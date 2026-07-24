# =============================================================================
# Diagnostic sub-analysis: binary car-vs-foot mode choice for domestic
# (CH -> CH) trips, testing NON-LINEAR specifications of travel time, per
# request ("remove pt and bike and try non-linear specifications for walk
# and car travel times").
#
# Scope, explicitly narrowed from 03_estimate_logsum_CH.R's 4-alternative
# models:
#   - PT and bike are dropped entirely, not just from the utility functions
#     but from the CHOICE SET itself -- the estimation sample is restricted
#     to mc15 trips where the respondent actually chose car or foot (trips
#     where pt/bike was chosen have no valid outcome in a car/foot-only
#     choice set, so they're excluded, not recoded).
#   - Topology x travel-time interactions (Model 4a/4b in
#     README_trajets_CH_CH.md) are NOT included here -- this script isolates
#     the shape of the travel-time effect itself, and combining that with
#     topology interactions would multiply the number of non-linear terms
#     to compare. Cost (car only) and car network distance are kept, same as
#     Model 3's base spec, since the point is to test a different functional
#     form for travel time specifically, not to re-litigate the rest of the
#     specification.
# This is a diagnostic/comparison script, not a producer of a logsum used
# elsewhere -- unlike 03_estimate_logsum_CH.R, it does not write a
# tt_CH_CH_logsum*.fst file, since a car/foot-only choice set isn't a valid
# stand-in for full 4-mode accessibility.
#
# Three specifications of travel time are compared, all else equal
# (asc_car, b_cost_car, b_dist_car unchanged across the three):
#   A. Linear      : b_tt_car * tt_miv                  | b_tt_foot * tt_fgv                    (minutes)
#   B. Quadratic   : b_tt_car*tt_h + b_tt_car2*tt_h^2    | b_tt_foot*tt_h + b_tt_foot2*tt_h^2     (HOURS -- see note)
#   C. Log         : b_tt_car * log(tt_miv + 1)          | b_tt_foot * log(tt_fgv + 1)            (minutes)
# Quadratic and log are each compared to the linear baseline via AIC/BIC (log
# isn't nested in the other two, so no LR test there; quadratic IS nested in
# linear via b_tt_*2 = 0 once both are expressed in the same units, so an LR
# test applies for that comparison -- see the unit note below).
#
# Unit note for the quadratic model: tt_fgv (walking time to a candidate
# alternative, used regardless of whether foot was actually chosen) reaches
# up to ~2,113 minutes (~35 hours) for some long-distance car-chosen trips
# -- a real network-distance value (CH's diagonal is a very long walk), not
# a data error. Squaring that in MINUTES gives ~4.5 million, which combined
# with Apollo's small test perturbations of the starting values overflowed
# the utility scale and crashed estimation ("Log-likelihood calculation
# fails at values close to the starting values"). Fixed by expressing the
# quadratic model's travel times in HOURS (tt/60) instead of minutes, which
# keeps every squared term comfortably inside double-precision range.
# Model B's coefficients are therefore per-hour, not per-minute like A/C.
#
# All three apollo_probabilities() functions below build their utility list
# INLINE (no helper function returning V) -- an earlier version of this
# script factored V-construction into a shared build_V() closure, which
# silently broke Apollo's analytical-gradient pre-processor (same failure
# mode as a runtime `if` branch: "Apollo was not able to compute analytical
# gradients", falling back to slow numerical derivatives). Static, inline
# utility expressions are required for analytical gradients to work -- see
# 03_estimate_logsum_CH.R's note on this same issue.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(fst)
  library(apollo)
})

# -----------------------------------------------------------------------------
# 0. LOAD DATA
# -----------------------------------------------------------------------------

tt_ch_ch <- as.data.table(read_fst("data/output/tt_CH_CH.fst", columns = c("origin_zone", "dest_zone", "tt_miv", "tt_fgv", "cost_car", "dist_miv")))
setkey(tt_ch_ch, origin_zone, dest_zone)
cat(sprintf("Loaded data/output/tt_CH_CH.fst (%d rows)\n", nrow(tt_ch_ch)))

mc15 <- fread("data/output/mc15.csv")
cat(sprintf("Loaded data/output/mc15.csv (%d rows)\n\n", nrow(mc15)))

dir.create("results_output", showWarnings = FALSE)
invisible(capture.output(apollo_initialise()))

# -----------------------------------------------------------------------------
# 1. BUILD CAR/FOOT-ONLY ESTIMATION DATA
# -----------------------------------------------------------------------------
# Only trips where mc15's reported mode is "car" or "walk" (-> "foot") are
# kept; "pt"/"bike"/"other" trips are dropped, since they fall outside a
# car/foot-only choice set.

build_mode_data_carfoot <- function(mc15_dt, tt_dt) {
  md <- tt_dt[mc15_dt, on = c("origin_zone", "dest_zone")]

  md[, mode_chosen := fcase(
    mode == "car",  "car",
    mode == "walk", "foot",
    default         = NA_character_
  )]

  n_before <- nrow(md)
  md <- md[!is.na(mode_chosen) & !is.na(tt_miv) & !is.na(tt_fgv) & !is.na(cost_car) & !is.na(dist_miv)]
  n_dropped <- n_before - nrow(md)
  cat(sprintf("Dropped %d/%d respondents: chose pt/bike/other, or missing tt_miv/tt_fgv/cost_car/dist_miv\n", n_dropped, n_before))

  md[, choice := fcase(mode_chosen == "car", 1L, mode_chosen == "foot", 2L)]
  md[, `:=`(tt_miv_h = tt_miv / 60, tt_fgv_h = tt_fgv / 60)]   # hours, for the quadratic model only
  md[, agent_id := .I]
  md[]
}

mode_data <- build_mode_data_carfoot(mc15, tt_ch_ch)
cat(sprintf("Estimation sample: %d trips (%.1f%% car, %.1f%% foot)\n",
    nrow(mode_data), 100 * mean(mode_data$choice == 1), 100 * mean(mode_data$choice == 2)))
cat(sprintf("tt_miv range [%.1f, %.1f] min, tt_fgv range [%.1f, %.1f] min\n\n",
    min(mode_data$tt_miv), max(mode_data$tt_miv), min(mode_data$tt_fgv), max(mode_data$tt_fgv)))

# -----------------------------------------------------------------------------
# 2. MODEL A -- LINEAR TRAVEL TIME (baseline)
# -----------------------------------------------------------------------------
# Reference alternative: foot (asc_foot fixed at 0 by omission), same
# convention as 03_estimate_logsum_CH.R.

cat("\n\n===== Model A: linear travel time =====\n\n")

estimate_carfoot_linear <- function(mode_data, model_name) {
  database <- as.data.frame(mode_data[, .(agent_id, tt_miv, tt_fgv, cost_car, dist_miv, choice)])
  apollo_control <- list(modelName = model_name,
                          modelDescr = sprintf("Binary car/foot mode choice, linear travel time -- %s", model_name),
                          indivID = "agent_id", outputDirectory = "results_output/")
  apollo_beta <- c(asc_car = 0, b_tt_car = 0, b_tt_foot = 0, b_cost_car = 0, b_dist_car = 0)
  apollo_fixed <- c()

  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))
    P <- list()
    V <- list(
      car  = asc_car + b_tt_car * tt_miv + b_cost_car * cost_car + b_dist_car * dist_miv,
      foot =           b_tt_foot * tt_fgv
    )
    mnl_settings <- list(alternatives = c(car = 1, foot = 2), avail = list(car = 1, foot = 1), choiceVar = choice, V = V)
    P[["model"]] <- apollo_mnl(mnl_settings, functionality)
    P <- apollo_prepareProb(P, apollo_inputs, functionality)
    return(P)
  }

  apollo_inputs <- apollo_validateInputs(apollo_beta = apollo_beta, apollo_fixed = apollo_fixed,
                                          database = database, apollo_control = apollo_control, silent = TRUE)
  model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
  apollo_modelOutput(model, modelOutput_settings = list(printPVal = TRUE))
  model
}
model_A <- estimate_carfoot_linear(mode_data, "mode_choice_carfoot_linear")
saveRDS(model_A, "results_output/mode_choice_model_ch_carfoot_linear.rds")

# -----------------------------------------------------------------------------
# 3. MODEL B -- QUADRATIC TRAVEL TIME (hours, see unit note above)
# -----------------------------------------------------------------------------

cat("\n\n===== Model B: quadratic travel time (hours) =====\n\n")

estimate_carfoot_quad <- function(mode_data, model_name) {
  database <- as.data.frame(mode_data[, .(agent_id, tt_miv_h, tt_fgv_h, cost_car, dist_miv, choice)])
  apollo_control <- list(modelName = model_name,
                          modelDescr = sprintf("Binary car/foot mode choice, quadratic travel time (hours) -- %s", model_name),
                          indivID = "agent_id", outputDirectory = "results_output/")
  apollo_beta <- c(asc_car = 0, b_tt_car = 0, b_tt_car2 = 0, b_tt_foot = 0, b_tt_foot2 = 0, b_cost_car = 0, b_dist_car = 0)
  apollo_fixed <- c()

  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))
    P <- list()
    V <- list(
      car  = asc_car + b_tt_car * tt_miv_h + b_tt_car2 * tt_miv_h^2 + b_cost_car * cost_car + b_dist_car * dist_miv,
      foot =           b_tt_foot * tt_fgv_h + b_tt_foot2 * tt_fgv_h^2
    )
    mnl_settings <- list(alternatives = c(car = 1, foot = 2), avail = list(car = 1, foot = 1), choiceVar = choice, V = V)
    P[["model"]] <- apollo_mnl(mnl_settings, functionality)
    P <- apollo_prepareProb(P, apollo_inputs, functionality)
    return(P)
  }

  apollo_inputs <- apollo_validateInputs(apollo_beta = apollo_beta, apollo_fixed = apollo_fixed,
                                          database = database, apollo_control = apollo_control, silent = TRUE)
  model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
  apollo_modelOutput(model, modelOutput_settings = list(printPVal = TRUE))
  model
}
model_B <- estimate_carfoot_quad(mode_data, "mode_choice_carfoot_quadratic")
saveRDS(model_B, "results_output/mode_choice_model_ch_carfoot_quadratic.rds")

# Model A's LL isn't directly comparable to Model B's via a naive LR test,
# since B's V is a reparametrization of A's up to a units change (hours vs.
# minutes) plus the extra squared terms -- units alone don't change the
# likelihood (b_tt_car_hours = b_tt_car_minutes * 60 gives the same V), so
# the comparison is still valid: 2 extra parameters (b_tt_car2, b_tt_foot2).
lr_B <- 2 * (model_B$maximum - model_A$maximum)
p_B  <- pchisq(lr_B, df = 2, lower.tail = FALSE)
cat(sprintf("\nLR test (quadratic vs. linear): LR = %.4f (df=2), p = %.4g -- %s\n",
    lr_B, p_B, ifelse(p_B < 0.05, "quadratic is a significant improvement", "quadratic is NOT a significant improvement")))

b_B <- model_B$estimate
if (!is.na(b_B["b_tt_car2"]) && b_B["b_tt_car2"] != 0) {
  turn_car <- -b_B["b_tt_car"] / (2 * b_B["b_tt_car2"])
  cat(sprintf("Car utility turning point: tt_miv = %.2f h (%.0f min) (utility %s beyond this)\n",
      turn_car, turn_car * 60, ifelse(b_B["b_tt_car2"] < 0, "decreasing", "increasing")))
}
if (!is.na(b_B["b_tt_foot2"]) && b_B["b_tt_foot2"] != 0) {
  turn_foot <- -b_B["b_tt_foot"] / (2 * b_B["b_tt_foot2"])
  cat(sprintf("Foot utility turning point: tt_fgv = %.2f h (%.0f min) (utility %s beyond this)\n",
      turn_foot, turn_foot * 60, ifelse(b_B["b_tt_foot2"] < 0, "decreasing", "increasing")))
}

# -----------------------------------------------------------------------------
# 4. MODEL C -- LOG TRAVEL TIME
# -----------------------------------------------------------------------------
# log(tt + 1) instead of tt directly (the +1 avoids log(0), which does occur
# -- intrazonal trips have tt_fgv/tt_miv = 0). Captures diminishing marginal
# disutility as travel time grows -- the classic log-time form. No scale
# issue here (log(2113+1) = 7.66), so minutes are used directly.

cat("\n\n===== Model C: log travel time =====\n\n")

mode_data[, `:=`(log_tt_miv = log(tt_miv + 1), log_tt_fgv = log(tt_fgv + 1))]

estimate_carfoot_log <- function(mode_data, model_name) {
  database <- as.data.frame(mode_data[, .(agent_id, log_tt_miv, log_tt_fgv, cost_car, dist_miv, choice)])
  apollo_control <- list(modelName = model_name,
                          modelDescr = sprintf("Binary car/foot mode choice, log travel time -- %s", model_name),
                          indivID = "agent_id", outputDirectory = "results_output/")
  apollo_beta <- c(asc_car = 0, b_tt_car = 0, b_tt_foot = 0, b_cost_car = 0, b_dist_car = 0)
  apollo_fixed <- c()

  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))
    P <- list()
    V <- list(
      car  = asc_car + b_tt_car * log_tt_miv + b_cost_car * cost_car + b_dist_car * dist_miv,
      foot =           b_tt_foot * log_tt_fgv
    )
    mnl_settings <- list(alternatives = c(car = 1, foot = 2), avail = list(car = 1, foot = 1), choiceVar = choice, V = V)
    P[["model"]] <- apollo_mnl(mnl_settings, functionality)
    P <- apollo_prepareProb(P, apollo_inputs, functionality)
    return(P)
  }

  apollo_inputs <- apollo_validateInputs(apollo_beta = apollo_beta, apollo_fixed = apollo_fixed,
                                          database = database, apollo_control = apollo_control, silent = TRUE)
  model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
  apollo_modelOutput(model, modelOutput_settings = list(printPVal = TRUE))
  model
}
model_C <- estimate_carfoot_log(mode_data, "mode_choice_carfoot_log")
saveRDS(model_C, "results_output/mode_choice_model_ch_carfoot_log.rds")

# -----------------------------------------------------------------------------
# 5. COMPARISON SUMMARY
# -----------------------------------------------------------------------------

cat("\n\n===== Comparison: linear vs. quadratic vs. log travel time =====\n\n")
aic <- function(m) -2 * m$maximum + 2 * length(m$estimate)
bic <- function(m) -2 * m$maximum + log(nrow(mode_data)) * length(m$estimate)
cat(sprintf("%-12s %10s %6s %10s %10s\n", "Model", "LL", "npar", "AIC", "BIC"))
cat(sprintf("%-12s %10.2f %6d %10.2f %10.2f\n", "Linear",    model_A$maximum, length(model_A$estimate), aic(model_A), bic(model_A)))
cat(sprintf("%-12s %10.2f %6d %10.2f %10.2f\n", "Quadratic", model_B$maximum, length(model_B$estimate), aic(model_B), bic(model_B)))
cat(sprintf("%-12s %10.2f %6d %10.2f %10.2f\n", "Log",       model_C$maximum, length(model_C$estimate), aic(model_C), bic(model_C)))

cat("\nDone. Models saved to results_output/mode_choice_model_ch_carfoot_{linear,quadratic,log}.rds\n")
cat("(No logsum computed -- car/foot-only choice set is not a valid stand-in for full 4-mode accessibility.)\n")
