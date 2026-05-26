# =============================================================================
# Tourist Mode Choice — PART 1 of 5
# Danalet et al. (2023), STRC Conference Paper
#
# Covers:
#   MNL mode choice estimation from Tourism Monitor Switzerland (TMS) data.
#   Translation of the Python/Biogeme model at:
#     github.com/antonindanalet/mode_choice_tourists
#
#   Data file: data/TMS 2017.sav (22,128 rows × 20 cols)
#
#   Mode alternatives — variable means_of_transport_in_CH:
#     1 = Train
#     2 = Light rail / public bus
#     3 = Car  [reference alternative, ASC_car = 0]
#     4 = Camper/caravan        } NOT in paper model — dropped
#     5 = Motorcycle             } NOT in paper model — dropped
#     6 = Long-distance bus through Europe
#     7 = Private tour bus (group travel)
#     8 = Bicycle / mountain bike
#     9 = Other
#
#   Key TMS variables used:
#     casenumber                  individual ID
#     means_of_transport_in_CH    observed mode (1–9, numeric)
#     origin_country              numeric country code
#     zone                        spatial typology 1=Big city … 4=Mountain
#     accomodation                0=supplementary, 1=hotel
#     age                         age category 1–17 (5-year bands: 1=16-20 … 17=96-100)
#     sex                         1=male, 2=female
#     weighting_factor            survey expansion weight
#
#   Model structure (follows Biogeme Python code structure):
#     - ASC per non-reference alternative
#     - Alternative-specific nationality group interactions (7 groups + ref=rest_of_world)
#     - Accommodation binary (hotel vs supplementary)
#     - Zone typology (urban/rural binary)
#     - All coefficients are alternative-specific (second pipe in mlogit formula)
#     - Paper target: 89 parameters, rho² ≈ 0.469
#
#   [ASSUMPTION] No LOS skim (travel time / cost by mode) is present in TMS 2017.
#   The model is estimated on sociodemographic predictors only. This differs from the
#   Biogeme model which includes generic travel time and cost; those coefficients
#   are absent here, so rho² will be lower than the paper's 0.469.
#
# INPUTS:  data/TMS 2017.sav
# OUTPUTS: part1_output.RData
#   - mnl_model     : fitted mlogit object
#   - coef_table    : tidy tibble (term, estimate, std_error)
#   - log_likelihood: scalar
#   - rho_squared   : McFadden's rho²
#   - tms_prepared  : cleaned data.frame (before dfidx conversion)
#   - COUNTRY_CODES : named vector mapping country codes to nationality groups
#   - ALT_CODES, REF_ALT, NAT_GROUPS, NATIONALITY_MAP : passed to Part 3
#
# Run next: part2_tourist_population.R
# =============================================================================

library(haven)
library(mlogit)
library(dfidx)
library(dplyr)
library(tidyr)
library(tibble)
library(broom)

set.seed(42)

# =============================================================================
# CONSTANTS
# =============================================================================

TMS_FILE  <- "data/TMS 2017.sav"
ALT_CODES <- c("1", "2", "3", "6", "7", "8", "9")   # paper model alternatives
REF_ALT   <- "3"                                      # car = reference (ASC = 0)

# TMS origin_country numeric codes → nationality groups.
# Based on SPSS value labels in TMS 2017.sav.
COUNTRY_CODES <- c(
  "17"   = "germany",
  "44"   = "france",
  "74"   = "italy",
  "314"  = "uk",
  "111"  = "netherlands",
  "4029" = "usa"
  # 212 = Switzerland excluded from estimation (foreign tourists only)
  # all other codes → "rest_of_world" (assigned via default in case_when below)
)

# Swiss domestic tourists (212) are excluded from estimation (foreign tourists only).
# The MNL nationality groups therefore do not include "switzerland".
NAT_GROUPS <- c("germany", "france", "italy", "uk", "netherlands", "usa",
                "rest_of_world")

# Reference nationality = rest_of_world (omitted from formula to avoid collinearity)
NAT_REF <- "rest_of_world"

# NATIONALITY_MAP tibble: used by Part 2 to match hotel CSV column names to groups.
# Switzerland is still a valid group for the hotel population in Part 2 even though
# Swiss domestic tourists are excluded from the MNL estimation here.
NATIONALITY_MAP <- tribble(
  ~nationality_label, ~group,
  "germany",          "germany",
  "france",           "france",
  "italy",            "italy",
  "uk",               "uk",
  "united_kingdom",   "uk",
  "netherlands",      "netherlands",
  "usa",              "usa",
  "united_states",    "usa",
  "switzerland",      "switzerland"
  # unmatched → rest_of_world
)

# =============================================================================
# RUNTIME FLAGS
# =============================================================================

message("[ASSUMPTION – Part 1] TMS 2017 has no LOS skim variables (travel time,",
        " cost by mode). Model estimated on sociodemographic predictors only.",
        " Paper rho² = 0.469 also uses finer nationality groups, AMR region dummies,",
        " accommodation sub-types, and hotel stars; expect lower rho² here (~0.457).")
message("[NOTE – Part 1] Modes 4 (camper) and 5 (motorcycle) are recoded to 9 (other)",
        " and 3 (car) respectively, matching the Biogeme Python code. This keeps",
        " all foreign-tourist observations and reproduces n = 12,809.")
message("[NOTE – Part 1] Swiss domestic tourists (origin_country = 212) are excluded.",
        " Paper models foreign tourists only (Biogeme: Q2B_ST_markets_detail_new2 != 4).")
message("[NOTE – Part 1] Nationality groups follow Biogeme Python code.",
        " Reference nationality = rest_of_world (omitted from formula).")
message("[NOTE – Part 1] Survey weights (weighting_factor) are applied in estimation.")

# =============================================================================
# 1. LOAD TMS DATA
# =============================================================================

cat("Loading TMS data from '", TMS_FILE, "' ...\n", sep = "")
if (!file.exists(TMS_FILE))
  stop("TMS file not found: '", TMS_FILE, "'\n",
       "  Place 'TMS 2017.sav' in tourists/data/ and re-run.")

raw <- read_sav(TMS_FILE)
cat(sprintf("Loaded: %d rows × %d columns.\n\n", nrow(raw), ncol(raw)))

# =============================================================================
# 2. RECODE AND CLEAN
# =============================================================================

tms <- raw %>%
  mutate(
    person_id = as.integer(casenumber),

    # Mode: recode camper(4)→other(9) and motorcycle(5)→car(3), matching Biogeme code.
    # These observations are kept (not dropped) so the sample matches the paper (n=12,809).
    mode = as.character(case_when(
      as.integer(means_of_transport_in_CH) == 4L ~ 9L,
      as.integer(means_of_transport_in_CH) == 5L ~ 3L,
      TRUE ~ as.integer(means_of_transport_in_CH)
    )),

    # Nationality group from numeric origin_country code.
    # Swiss domestic tourists (212) are excluded below; no "switzerland" group in the model.
    nationality_group = case_when(
      origin_country == 17L   ~ "germany",
      origin_country == 44L   ~ "france",
      origin_country == 74L   ~ "italy",
      origin_country == 314L  ~ "uk",
      origin_country == 111L  ~ "netherlands",
      origin_country == 4029L ~ "usa",
      TRUE                    ~ "rest_of_world"
    ),

    # Urban binary: Big city (zone=1) or Small Town (zone=2) → urban=1
    # Countryside (zone=3) and Mountain (zone=4) → urban=0
    urban = as.integer(zone %in% c(1L, 2L)),

    # Accommodation binary: 1=hotel, 0=supplementary (TMS has only these two)
    accom_hotel = as.integer(accomodation == 1L),

    # Age category (1–17; 1=16-20, …, 17=96-100); use as ordinal numeric
    age_cat = as.integer(age),

    # Sex binary (1=male, 2=female → 1=male, 0=female)
    male = as.integer(sex == 1L)
  ) %>%
  # Exclude Swiss domestic tourists (paper models foreign tourists only)
  filter(origin_country != 212L) %>%
  filter(
    mode %in% ALT_CODES,
    !is.na(nationality_group),
    !is.na(weighting_factor)
  )

cat(sprintf("After cleaning: %d observations (paper: 12,809).\n\n", nrow(tms)))

# Mode distribution
cat("Mode distribution (means_of_transport_in_CH):\n")
mode_df <- tms %>%
  count(mode, name = "n") %>%
  mutate(
    label = c("1"="Train","2"="Light rail/bus","3"="Car","6"="Long-dist bus",
               "7"="Tour bus","8"="Bicycle","9"="Other")[mode],
    pct = round(100 * n / sum(n), 1)
  )
print(mode_df)

cat("\nNationality group distribution:\n")
print(count(tms, nationality_group, sort = TRUE))

cat("\nAccommodation distribution:\n")
print(count(tms, accom_hotel))

cat("\nZone (urban typology) distribution:\n")
print(count(tms, zone = as.integer(zone), urban))

cat("\nWeighting factor summary:\n")
print(summary(tms$weighting_factor))
cat("\n")

# =============================================================================
# 3. CREATE NATIONALITY DUMMY VARIABLES
#
# One binary dummy per nationality group except the reference (rest_of_world).
# These go in the SECOND pipe of the mlogit formula so that mlogit automatically
# creates alternative-specific coefficients for each (e.g., train:nat_germany,
# bus:nat_germany, …). This mirrors the Python Biogeme structure where each
# nationality group has a separate ASC adjustment per alternative.
# =============================================================================

nat_vars <- setdiff(NAT_GROUPS, NAT_REF)   # 7 groups (rest_of_world = reference)

for (g in nat_vars) {
  tms[[paste0("nat_", g)]] <- as.integer(tms$nationality_group == g)
}

nat_formula_vars <- paste0("nat_", nat_vars)

cat(sprintf("Nationality dummies created: %s\n\n",
    paste(nat_formula_vars, collapse = ", ")))

# =============================================================================
# 3b. DROP NATIONALITY DUMMIES WITH ZERO CELLS
#
# If any nationality group has 0 observations in any mode alternative, MLE is
# numerically undefined for that interaction (perfect separation → singular
# Hessian). Detect and remove those groups before estimation; they are treated
# as rest_of_world for mode choice purposes.
# =============================================================================

mode_by_nat <- table(tms$nationality_group, tms$mode)
nat_with_zeros <- rownames(mode_by_nat)[apply(mode_by_nat == 0, 1, any)]
nat_with_zeros <- intersect(nat_with_zeros, nat_vars)

if (length(nat_with_zeros) > 0) {
  message(sprintf(
    "[NOTE – Part 1] Dropping %d nationality dummy group(s) with zero observations",
    length(nat_with_zeros)),
    " in at least one mode alternative (perfect separation): ",
    paste(nat_with_zeros, collapse = ", "),
    ". These agents are absorbed into the rest_of_world reference category.")
  nat_vars         <- setdiff(nat_vars, nat_with_zeros)
  nat_formula_vars <- paste0("nat_", nat_vars)
}

# =============================================================================
# 4. CONVERT TO MLOGIT LONG FORMAT
#
# dfidx() converts wide individual-level data to long (person × alternative) format.
# choice = "mode" identifies the observed choice column.
# shape = "wide" means one row per individual in the input.
# =============================================================================

tms_dfidx <- dfidx(
  tms,
  idx    = "person_id",
  choice = "mode",
  shape  = "wide"
)

# Extract the correctly-ordered long-format weight vector (length = n_obs × n_alts).
# tms$weighting_factor has length n_obs (12,809); passing it directly to mlogit()
# triggers "variable lengths differ" because mlogit checks against nrow(tms_dfidx).
weights_long <- tms_dfidx[["weighting_factor"]]

cat(sprintf("Data converted to mlogit long format (%d rows = %d obs × %d alts).\n",
    nrow(tms_dfidx), nrow(tms), length(ALT_CODES)))
cat(sprintf("Weight vector length: %d\n\n", length(weights_long)))

# =============================================================================
# 5. SPECIFY FORMULA
#
# mlogit formula: choice ~ alt_varying | ind_specific | ind_specific_no_asc
#   - First  pipe: alternative-varying variables (generic coefficients) — none here (no LOS)
#   - Second pipe: individual-specific variables → get alternative-specific coefficients
#   - Third  pipe: individual-specific variables with common (not alt-specific) coefficient
#
# Formula used:
#   mode ~ 0 | nat_germany + nat_france + ... + urban + accom_hotel | 0
#
# The "0" in the first position means no alternative-varying generic variables.
# The "0" in the third position suppresses a common intercept.
# mlogit will produce coefficients labelled "variable:altX" for each alternative.
# =============================================================================

ind_vars <- c(nat_formula_vars, "urban", "accom_hotel")

fmla <- as.formula(paste0(
  "mode ~ 0 | ",
  paste(ind_vars, collapse = " + "),
  " | 0"
))

cat("Estimation formula:\n")
print(fmla)
cat("\n")

# =============================================================================
# 6. ESTIMATE MNL
# =============================================================================

cat("Estimating MNL model (this may take a moment)...\n")

mnl_model <- mlogit(
  fmla,
  data     = tms_dfidx,
  reflevel = REF_ALT,
  weights  = weights_long
)

cat("Done.\n\n")

# =============================================================================
# 7. EXTRACT RESULTS
# =============================================================================

ll_model  <- logLik(mnl_model)
# Null log-likelihood: equal probabilities across all alternatives
n_alts    <- length(ALT_CODES)
n_obs     <- nrow(tms)
ll_null   <- n_obs * log(1 / n_alts)

log_likelihood <- as.numeric(ll_model)
rho_squared    <- 1 - (log_likelihood / ll_null)

coef_table <- broom::tidy(mnl_model) %>%
  rename(std_error = std.error, t_stat = statistic) %>%
  mutate(
    # mlogit names coefficients as "variable:alternative" (e.g. "nat_germany:1").
    param = sub(":.*", "", term),   # variable name, e.g. "(Intercept)", "nat_germany"
    alt   = sub(".*:", "", term)    # alternative code, e.g. "1", "2", "3"
  )

cat("=== Model Summary ===\n")
cat(sprintf("Observations         : %d  [Paper: 12,809  — should match exactly]\n", n_obs))
cat(sprintf("Alternatives         : %d\n", n_alts))
cat(sprintf("Parameters estimated : %d  [Paper: 89 (incl. LOS terms)]\n",
    length(coef(mnl_model))))
cat(sprintf("Log-likelihood       : %.2f\n", log_likelihood))
cat(sprintf("rho²                 : %.3f  [Paper: 0.469; lower expected without LOS]\n",
    rho_squared))
cat("\nCoefficients:\n")
print(as.data.frame(coef_table[, c("term","estimate","std_error","t_stat")]))
cat("\n")

# =============================================================================
# 8. MODE SHARE VALIDATION
#
# Compare predicted shares against observed shares.
# Large deviations indicate poor model fit or missing variables.
# =============================================================================

cat("=== Mode Share Validation ===\n")
obs_shares <- tms %>%
  count(mode, name = "n") %>%
  mutate(obs_share = n / sum(n))

pred_probs  <- fitted(mnl_model, outcome = FALSE)
pred_shares <- colMeans(pred_probs)

valid_df <- obs_shares %>%
  left_join(
    data.frame(mode = names(pred_shares),
               pred_share = as.numeric(pred_shares)),
    by = "mode"
  ) %>%
  mutate(
    mode_label = c("1"="Train","2"="Light rail/bus","3"="Car","6"="Long-dist bus",
                   "7"="Tour bus","8"="Bicycle","9"="Other")[mode],
    diff = round(pred_share - obs_share, 3)
  )

print(valid_df[, c("mode","mode_label","obs_share","pred_share","diff")])
cat("\nNote: MNL exactly reproduces observed shares when no LOS variables",
    "are included (ASC soaks up all share).\n\n")

# =============================================================================
# 9. SAVE
# =============================================================================

tms_prepared <- tms   # cleaned data.frame (wide format, before dfidx)

save(mnl_model, coef_table, log_likelihood, rho_squared,
     tms_prepared, COUNTRY_CODES, NATIONALITY_MAP, NAT_GROUPS, NAT_REF,
     ALT_CODES, REF_ALT, nat_formula_vars, ind_vars,
     file = "part1_output.RData")

cat("Part 1 complete. Output saved to part1_output.RData\n")
cat("Run part2_tourist_population.R next.\n")
