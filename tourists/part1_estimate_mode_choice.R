# =============================================================================
# Tourist Mode Choice — PART 1 of 5
# Danalet et al. (2023), STRC Conference Paper
#
# Covers:
#   MNL mode choice estimation from Tourism Monitor Switzerland (TMS) data.
#   Partial replication of the Python/Biogeme model at:
#     github.com/antonindanalet/mode_choice_tourists
#
#   Data files:
#     data/TMS 2017.sav              (22,128 rows × 20 cols)
#     data/commune_type_region.csv   (2,130 Swiss communes; FSO typology + AMR region)
#
#   Mode alternatives — variable means_of_transport_in_CH:
#     1 = Train
#     2 = Light rail / public bus
#     3 = Car  [reference alternative, ASC_car = 0]
#     4 = Camper/caravan   } recoded: 4→9 (other), 5→3 (car), matching Python
#     5 = Motorcycle        }
#     6 = Long-distance bus through Europe
#     7 = Private tour bus (group travel)
#     8 = Bicycle / mountain bike
#     9 = Other
#
#   Key TMS variables used:
#     casenumber                  individual ID
#     means_of_transport_in_CH    observed mode (1–9, numeric)
#     destination                 holiday commune (SPSS labelled, matched to CSV)
#     origin_country              numeric country code
#     accomodation                0=supplementary, 1=hotel
#     weighting_factor            survey expansion weight
#
#   Model structure (SIMPLIFIED relative to paper):
#     - ASC per non-reference alternative
#     - 6 nationality group dummies (rest_of_world = 7th group, used as reference)
#     - Accommodation binary (hotel vs supplementary)
#     - FSO urban/rural/intermediate typology (urban_fso, rural; intermediate = ref)
#       joined from commune_type_region.csv via destination label
#     - 15 AMR region dummies (amr_reg_2 … amr_reg_16; region 1 = reference)
#     - All coefficients are alternative-specific (second pipe in mlogit formula)
#     - Paper target: 89 parameters, rho² = 0.469 (unweighted estimation)
#
#   [GAP vs paper] This script omits several components present in the paper model:
#     - ~12 additional nationality groups (Austria, Spain, Russia, Denmark,
#       Scandinavia, North America, East Asia, Middle East, Brazil, India,
#       Southeast Asia, Australasia) — account for ~30 missing parameters
#     - 4-category accommodation (holiday homes, camping, collective) — ~13 params
#     - Hotel stars (continuous, 4 parameters)
#     - Region dummies included here as alternative-specific (15 × 6 = 90 params)
#       but paper specifies only 28 region parameters across selective alternatives
#     - Estimation is UNWEIGHTED in the Python code (weight column dropped in
#       get_data.py); this script applies weighting_factor — direct divergence
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
which_computer = "MR1"

if(which_computer == "MR1"){setwd("E:/ARE/ProjekteTIE/Turismusverkehr/RScript/tourists")}
if(which_computer == "mine"){setwd("")}

# =============================================================================
# CONSTANTS
# =============================================================================

TMS_FILE         <- "data/TMS 2017.sav"
COMMUNE_REF_FILE <- "data/commune_type_region.csv"
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

message("[NOTE – Part 1] Modes 4 (camper) → 9 (other) and 5 (motorcycle) → 3 (car),",
        " matching Biogeme Python get_data.py. Observations kept; n should match 12,809.")
message("[NOTE – Part 1] Swiss domestic tourists (origin_country = 212) excluded.",
        " Paper models foreign tourists only (Python: Q2B_ST_markets_detail_new2 != 4).")
message("[NOTE – Part 1] Only 6 nationality groups included (Germany, France, Italy,",
        " UK, Netherlands, USA); all others absorbed into rest_of_world reference.",
        " Paper estimates ~18 named groups — divergence from paper is expected.")
message("[NOTE – Part 1] FSO urban/rural typology and AMR region joined from commune_type_region.csv",
        " via TMS destination label (~97.9% match). Unmatched obs (2.1%) treated as intermediate/no-region.")
message("[NOTE – Part 1] Survey weights applied here. Python code is UNWEIGHTED",
        " (Gewichtung column dropped in get_data.py). This is a known divergence.")

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
# 1b. LOAD COMMUNE REFERENCE AND JOIN
#
# commune_type_region.csv provides the FSO urban/rural/intermediate typology
# (urban_rural_topology: 1=urban, 2=intermediate, 3=rural) and the AMR labour
# market region (amr_region: integer 1–16) for each Swiss commune.
#
# TMS identifies the holiday commune via the `destination` variable (SPSS
# labelled integer). We extract the label string and join on commune_name.
# A lookup table corrects English city names and pre-2017 commune mergers
# (~203 mappings); match rate ≈ 97.9%. The 2.1% unmatched are treated as
# intermediate typology (urban_fso = 0, rural = 0) and no-region (NA → dropped
# from region dummies).
# =============================================================================

commune_ref <- read.csv(COMMUNE_REF_FILE, stringsAsFactors = FALSE,
                        encoding = "UTF-8") %>%
  rename(amr_region = region)

# Lookup: TMS destination label -> CSV commune_name
# Covers English city names, tourist resort/hamlet -> post-merger commune name
name_lookup <- tribble(
  ~tms_name,                         ~csv_name,
  "Zurich",                          "Zürich",
  "Lucerne",                         "Luzern",
  "Geneva",                          "Genève",
  "Verbier",                         "Val de Bagnes",
  "Bagnes",                          "Val de Bagnes",
  "Fionnay",                         "Val de Bagnes",
  "Versegères",                      "Val de Bagnes",
  "Lourtier",                        "Val de Bagnes",
  "Champex-Lac",                     "Orsières",
  "La Tzoumaz",                      "Riddes",
  "Ovronnaz",                        "Leytron",
  "Anzère",                          "Ayent",
  "Montana",                         "Crans-Montana",
  "Randogne",                        "Crans-Montana",
  "Chermignon",                      "Crans-Montana",
  "St. Luc",                         "Anniviers",
  "Zinal",                           "Anniviers",
  "Grimentz",                        "Anniviers",
  "Chandolin",                       "Anniviers",
  "St. Jean",                        "Anniviers",
  "Vissoie",                         "Anniviers",
  "Vercorin",                        "Anniviers",
  "Münster-Geschinen",               "Obergoms",
  "Reckingen-Gluringen",             "Obergoms",
  "Grafschaft",                      "Obergoms",
  "Oberwald",                        "Obergoms",
  "Obergesteln",                     "Obergoms",
  "Ulrichen",                        "Obergoms",
  "Blitzingen",                      "Obergoms",
  "Niederwald",                      "Obergoms",
  "Hérémences",                      "Vex",
  "Mase",                            "Vex",
  "Nax",                             "Vex",
  "Miège",                           "Salgesch",
  "Sent",                            "Scuol",
  "Ftan",                            "Scuol",
  "Ardez",                           "Scuol",
  "Guarda",                          "Scuol",
  "Tarasp",                          "Scuol",
  "Vulpera",                         "Scuol",
  "Sur En",                          "Scuol",
  "Lavin",                           "Scuol",
  "Ramosch",                         "Scuol",
  "Tschlin",                         "Scuol",
  "Vnà",                             "Scuol",
  "Savognin",                        "Surses",
  "Bivio",                           "Surses",
  "Tinizong-Rona",                   "Surses",
  "Riom-Parsonz",                    "Surses",
  "Cinuos-chel",                     "Trun",
  "Bergün/Bravuogn",                 "Bergün Filisur",
  "Filisur",                         "Bergün Filisur",
  "Maloja",                          "Bregaglia",
  "Valbella",                        "Vaz/Obervaz",
  "Lenzerheide",                     "Vaz/Obervaz",
  "Klosters-Serneus",                "Klosters",
  "La Punt",                         "La Punt Chamues-ch",
  "Splügen",                         "Rheinwald",
  "Alvaneu Bad",                     "Albula/Alvra",
  "Lantsch / Lenz",                  "Lantsch/Lenz",
  "Vella",                           "Lumnezia",
  "Vrin",                            "Lumnezia",
  "Ilanz / Glion",                   "Ilanz/Glion",
  "Susch",                           "Zernez",
  "Wengen",                          "Lauterbrunnen",
  "Mürren",                          "Lauterbrunnen",
  "Kiental",                         "Reichenbach im Kandertal",
  "Gstaad",                          "Saanen",
  "Braunwald",                       "Glarus Süd",
  "Elm",                             "Glarus Süd",
  "Linthal",                         "Glarus Süd",
  "Wildhaus",                        "Wildhaus-Alt St. Johann",
  "Unterwasser",                     "Wildhaus-Alt St. Johann",
  "Alt St. Johann",                  "Wildhaus-Alt St. Johann",
  "Säntis-Schwägalp",                "Nesslau",
  "Gossau SG",                       "Gossau (SG)",
  "Brunnen",                         "Ingenbohl",
  "Stoos",                           "Morschach",
  "Sattel-Hochstuckli",              "Sattel",
  "Melchsee-Frutt",                  "Kerns",
  "Bauen",                           "Seedorf (UR)",
  "Villars-sur-Ollon",               "Ollon",
  "Les Diablerets",                  "Ormont-Dessus",
  "Torgon",                          "Val-d'Illiez",
  "Morgins",                         "Val-d'Illiez",
  "Champoussin",                     "Val-d'Illiez",
  "Blonay",                          "Blonay - Saint-Légier",
  "Cully",                           "Bourg-en-Lavaux",
  "Estavayer-le-Lac",                "Estavayer",
  "Saint Croix",                     "Sainte-Croix",
  "Breuleux",                        "Les Breuleux",
  "St. Aubin",                       "Saint-Aubin (FR)",
  "Schwarzsee",                      "Plaffeien",
  "Schmitten",                       "Schmitten (FR)",
  "Bad Zurzach",                     "Zurzach",
  "Tenero",                          "Tenero-Contra",
  "Gordevio",                        "Avegno Gordevio",
  "Avegno",                          "Avegno Gordevio",
  "Cugnasco",                        "Cugnasco-Gerra",
  "Magadino",                        "Gambarogno",
  "Vira (Gambarogno)",               "Gambarogno",
  "San Nazzaro",                     "Gambarogno",
  "Piazzogna",                       "Gambarogno",
  "Intragna",                        "Gambarogno",
  "Mosen",                           "Roggliswil",
  "Chernex",                         "Montreux",
  "Champfèr",                        "Silvaplana",
  "Susten",                          "Leuk",
  "Oberdorf NW",                     "Oberdorf (NW)",
  "Payern",                          "Payerne",
  "Affoltern i. E.",                 "Affoltern im Emmental",
  "Brione",                          "Brione sopra Minusio"
)

raw <- raw %>%
  mutate(
    commune_name_raw = as.character(as_factor(destination)),
    commune_name_csv = ifelse(
      commune_name_raw %in% name_lookup$tms_name,
      name_lookup$csv_name[match(commune_name_raw, name_lookup$tms_name)],
      commune_name_raw
    )
  ) %>%
  left_join(commune_ref, by = c("commune_name_csv" = "commune_name"))

n_matched   <- sum(!is.na(raw$urban_rural_topology))
n_unmatched <- sum(is.na(raw$urban_rural_topology))
cat(sprintf("Commune join: %d matched (%.1f%%), %d unmatched → treated as intermediate/no-region.\n\n",
    n_matched, 100 * n_matched / nrow(raw), n_unmatched))

# =============================================================================
# 2. RECODE AND CLEAN
# =============================================================================

tms <- raw %>%
  mutate(
    person_id = as.integer(casenumber),

    # Mode: recode camper(4)→other(9) and motorcycle(5)→car(3), matching Biogeme code.
    # Stored as factor with levels = ALT_CODES so mlogit.data() correctly identifies alternatives.
    mode = factor(case_when(
      as.integer(means_of_transport_in_CH) == 4L ~ "9",
      as.integer(means_of_transport_in_CH) == 5L ~ "3",
      TRUE ~ as.character(as.integer(means_of_transport_in_CH))
    ), levels = ALT_CODES),

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

    # FSO urban/rural/intermediate typology (1=urban, 2=intermediate, 3=rural).
    # Joined from commune_type_region.csv on destination commune label.
    # Unmatched observations (2.1%) default to intermediate (both dummies = 0).
    urban_fso = as.integer(!is.na(urban_rural_topology) & urban_rural_topology == 1L),
    rural     = as.integer(!is.na(urban_rural_topology) & urban_rural_topology == 3L),

    # Accommodation binary: 1=hotel, 0=supplementary.
    # Paper uses 4 categories (hotel, holiday homes, camping, collective) from
    # Q16_bereinigt_final_aggregiert sub-codes — this binary is a simplification.
    accom_hotel = as.integer(accomodation == 1L),

    # AMR Grossregion (1–16); NA for unmatched communes (dropped from region dummies)
    amr_region = amr_region,

    # Age and sex computed but not included in the current model formula
    age_cat = as.integer(age),
    male    = as.integer(sex == 1L)
  ) %>%
  # Exclude Swiss domestic tourists (paper models foreign tourists only)
  filter(origin_country != 212L) %>%
  filter(
    mode %in% ALT_CODES,
    !is.na(nationality_group),
    !is.na(weighting_factor),
    !is.na(accomodation)
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

cat("\nFSO typology distribution:\n")
print(count(tms, urban_rural_topology, urban_fso, rural))

cat("\nAMR region distribution:\n")
print(count(tms, amr_region, sort = TRUE))

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

nat_vars <- setdiff(NAT_GROUPS, NAT_REF)   # 6 dummies; rest_of_world is the 7th group (reference)

for (g in nat_vars) {
  tms[[paste0("nat_", g)]] <- as.integer(tms$nationality_group == g)
}

nat_formula_vars <- paste0("nat_", nat_vars)

cat(sprintf("Nationality dummies created: %s\n\n",
    paste(nat_formula_vars, collapse = ", ")))

# =============================================================================
# 3b. CREATE AMR REGION DUMMY VARIABLES
#
# Region 1 (Région lémanique) is the reference category (excluded dummy).
# Observations with NA amr_region (unmatched communes, ~2.1%) have all region
# dummies set to 0 — they contribute no region signal but remain in the sample.
# Note: including all 15 dummies in the second pipe gives 15 × 6 = 90 region
# parameters vs the paper's 28 (paper assigns region effects to select alts only).
# =============================================================================

AMR_REF_REGION <- 1L   # reference region (excluded dummy)
amr_reg_levels <- setdiff(1:16, AMR_REF_REGION)

for (r in amr_reg_levels) {
  tms[[paste0("amr_reg_", r)]] <- as.integer(!is.na(tms$amr_region) & tms$amr_region == r)
}

amr_reg_vars <- paste0("amr_reg_", amr_reg_levels)

cat(sprintf("AMR region dummies created: amr_reg_2 … amr_reg_16 (reference: region %d)\n\n",
    AMR_REF_REGION))

# Drop region dummies with zero observations in any mode alternative
# (perfect separation → singular Hessian; same logic as nationality check below)
mode_by_reg <- sapply(amr_reg_vars, function(v) table(tms[[v]], tms$mode))
reg_with_zeros <- amr_reg_vars[sapply(amr_reg_vars, function(v) {
  any(table(tms[[v]] == 1, tms$mode)["TRUE", ] == 0)
})]

if (length(reg_with_zeros) > 0) {
  message(sprintf(
    "[NOTE – Part 1] Dropping %d region dummy/dummies with zero obs in some mode: %s.",
    length(reg_with_zeros), paste(reg_with_zeros, collapse = ", ")))
  amr_reg_vars <- setdiff(amr_reg_vars, reg_with_zeros)
}

# =============================================================================
# 3d. DROP NATIONALITY DUMMIES WITH ZERO CELLS
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
# mlogit.data() converts wide individual-level data to long (person × alternative)
# format. Note: dfidx() — the newer API — has a version incompatibility with
# mlogit 1.1.3 + R 4.4.x that causes a model.matrix error; mlogit.data() is used
# here as the reliable alternative.
# =============================================================================

tms_mlogit <- mlogit.data(
  as.data.frame(tms),
  choice = "mode",
  shape  = "wide",
  id.var = "person_id"
)

# Extract the long-format weight vector. mlogit.data() expands individual-level
# data to n_obs × n_alts rows; the weight must match this expanded length.
weights_long <- tms_mlogit[["weighting_factor"]]

cat(sprintf("Data converted to mlogit long format (%d rows = %d obs × %d alts).\n",
    nrow(tms_mlogit), nrow(tms), length(ALT_CODES)))
cat(sprintf("Long-format weight vector length: %d\n\n", length(weights_long)))

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

ind_vars <- c(nat_formula_vars, "urban_fso", "rural", "accom_hotel", amr_reg_vars)

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
  data     = tms_mlogit,
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
cat(sprintf("Parameters estimated : %d  [Paper: 89 — gap due to fewer nationality groups,\n", length(coef(mnl_model))))
cat("                              simplified accommodation, no stars;\n")
cat("                              region params > paper (90 vs 28) due to full alt-specific spec]\n")
cat(sprintf("Log-likelihood       : %.2f\n", log_likelihood))
cat(sprintf("rho²                 : %.3f  [Paper: 0.469 (unweighted); lower expected here]\n",
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
