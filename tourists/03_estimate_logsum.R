# =============================================================================
# Mode choice model (car vs PT) + logsum for the destination choice model.
#
# The mode choice model is estimated on the REVEALED-PREFERENCE agqpv trips:
# each respondent's actual origin_zone -> dest_zone pair gives a real tt_miv
# (car) / tt_oev (PT) travel time from data/output/tt_agqpv.fst (built by
# 02_build_tt_lookups.R), and border_mode tells us which mode they actually
# used to cross into Switzerland (1 = road/car, 2 = rail/PT). tt_agqpv has
# 100% coverage for these real OD pairs, so it's the only database needed for
# estimation.
#
# Restricted to non-Swiss nationals (nationality != 1) to match the population
# used by the destination choice models in 04_destination_choice.R, which are
# scoped to foreign tourists only.
#
# The logsum (inclusive value / expected maximum utility)
#   logsum_od = ln( exp(V_car_od) + exp(V_pt_od) )
# is then computed for EVERY OD pair in TWO travel-time tables, using the
# estimated mode-choice coefficients, since the destination choice model
# (04_destination_choice.R) needs a logsum for arbitrary alternative zones,
# not just the ones respondents actually visited -- tt_agqpv's OD coverage is
# too sparse to be useful there:
#   - data/output/tt_ausland_CH.fst (every zone abroad x every Swiss
#     destination zone) -> data/output/tt_ausland_CH_logsum<tag>.fst -- the
#     EMU actually consumed by 04_destination_choice.R (foreign entry point
#     -> Swiss destination)
#   - data/output/tt_CH_CH.fst (every Swiss zone x every other Swiss zone)
#     -> data/output/tt_CH_CH_logsum<tag>.fst -- domestic zone-to-zone EMU,
#     not currently consumed by any other script
# (both built by 02_build_tt_lookups.R; their destination side is CH zones
# only, since Ausland/LI zones are never valid tourism destinations here).
#
# In addition to the pooled model, the mode choice model (and both logsums)
# is re-estimated separately on the Tagesreise (n_nights == 0) and Reise mit
# Ü. (n_nights > 0) subgroups, since day-trippers and overnight visitors may
# have different car/PT propensities -- their destination models should each
# use their own subgroup-specific EMU rather than the pooled one.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(fst)
  library(apollo)
})

# -----------------------------------------------------------------------------
# 0. LOAD DATA
# -----------------------------------------------------------------------------

tt_agqpv <- as.data.table(read_fst("data/output/tt_agqpv.fst"))
setkey(tt_agqpv, origin_zone, dest_zone)
cat(sprintf("Loaded data/output/tt_agqpv.fst (%d rows)\n", nrow(tt_agqpv)))

agqpv_full <- fread("data/output/agqpv.csv")
agqpv_full[, agent_id := .I]
agqpv <- agqpv_full[nationality != 1]
cat(sprintf("Loaded agqpv.csv: %d respondents (%d non-Swiss, %d Swiss excluded)\n",
    nrow(agqpv_full), nrow(agqpv), nrow(agqpv_full) - nrow(agqpv)))

tt_ausland_ch <- as.data.table(read_fst("data/output/tt_ausland_CH.fst"))
setkey(tt_ausland_ch, origin_zone, dest_zone)
cat(sprintf("Loaded data/output/tt_ausland_CH.fst (%d rows)\n", nrow(tt_ausland_ch)))

tt_ch_ch <- as.data.table(read_fst("data/output/tt_CH_CH.fst"))
setkey(tt_ch_ch, origin_zone, dest_zone)
cat(sprintf("Loaded data/output/tt_CH_CH.fst (%d rows)\n\n", nrow(tt_ch_ch)))

dir.create("results_output", showWarnings = FALSE)
invisible(capture.output(apollo_initialise()))

# -----------------------------------------------------------------------------
# 1. SAMPLE AGENTS
# -----------------------------------------------------------------------------

sample_agents <- function(data, n, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n <- min(n, nrow(data))
  data[sample(.N, n)]
}

# -----------------------------------------------------------------------------
# 2. BUILD MODE-CHOICE ESTIMATION DATA
# -----------------------------------------------------------------------------
# Two alternatives per agent: car (tt_miv) and pt (tt_oev). The chosen mode
# comes straight from the respondent's own reported border_mode.

build_mode_data <- function(agents_sub, tt_dt) {
  md <- tt_dt[agents_sub, on = c("origin_zone", "dest_zone")]

  md[, mode_chosen := fcase(
    border_mode == 1, "car",
    border_mode == 2, "pt",
    default          = NA_character_
  )]

  n_before <- nrow(md)
  md <- md[!is.na(mode_chosen) & !is.na(tt_miv) & !is.na(tt_oev)]
  n_dropped <- n_before - nrow(md)
  if (n_dropped > 0)
    cat(sprintf("WARNING: dropped %d/%d agents with unknown mode or missing travel time\n", n_dropped, n_before))

  md[, choice := fifelse(mode_chosen == "car", 1L, 2L)]
  md[]
}

# -----------------------------------------------------------------------------
# 3. COMPUTE LOGSUM
# -----------------------------------------------------------------------------
# Numerically-stable log-sum-exp of the two mode utilities, using the
# estimated mode-choice coefficients, for every OD pair in tt_dt.

compute_logsum <- function(tt_dt, model) {
  beta <- model$estimate

  V_car <- beta["asc_car"] + beta["b_tt_car"] * tt_dt$tt_miv
  V_pt  <-                   beta["b_tt_pt"]  * tt_dt$tt_oev

  # tt_miv/tt_oev are NA where 02_build_tt_lookups.R found that mode
  # unreachable (OMX sentinel travel time) -- treat a missing mode as
  # excluded from the logsum (utility -Inf), not as an unknown/missing OD
  # pair. If BOTH modes are unreachable, the pair truly has no logsum (NA).
  V_car[is.na(V_car)] <- -Inf
  V_pt[is.na(V_pt)]   <- -Inf

  v_max <- pmax(V_car, V_pt)
  logsum <- v_max + log(exp(V_car - v_max) + exp(V_pt - v_max))
  logsum[is.infinite(v_max)] <- NA_real_

  out <- copy(tt_dt)
  out[, logsum := logsum]
  out[]
}

# -----------------------------------------------------------------------------
# 4. ESTIMATE BINARY MNL MODE CHOICE MODEL
# -----------------------------------------------------------------------------
# apollo_validateInputs() is called with explicit apollo_beta/apollo_fixed/
# database/apollo_control arguments (rather than relying on its global-
# environment fallback), so this can safely live in a function and be reused
# across the pooled, Tagesreise and Reise mit Ü. subgroups.

estimate_mode_choice <- function(mode_data, model_name) {
  database <- as.data.frame(mode_data[, .(agent_id, tt_miv, tt_oev, choice)])
  database <- database[database$tt_oev < 1000, ]   # drop extreme PT travel-time outliers

  apollo_control <- list(
    modelName       = model_name,
    modelDescr      = sprintf("Binary MNL mode choice (car vs PT) -- %s", model_name),
    indivID         = "agent_id",
    outputDirectory = "results_output/"
  )

  apollo_beta  <- c(asc_car = 0, b_tt_car = 0, b_tt_pt = 0)
  apollo_fixed <- c()   # pt is the reference alternative (asc_pt fixed at 0)

  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))

    P <- list()
    V <- list(
      car = asc_car + b_tt_car * tt_miv,
      pt  =           b_tt_pt  * tt_oev
    )

    mnl_settings <- list(
      alternatives = c(car = 1, pt = 2),
      avail        = list(car = 1, pt = 1),
      choiceVar    = choice,
      V            = V
    )

    P[["model"]] <- apollo_mnl(mnl_settings, functionality)
    P <- apollo_prepareProb(P, apollo_inputs, functionality)
    return(P)
  }

  apollo_inputs <- apollo_validateInputs(
    apollo_beta    = apollo_beta,
    apollo_fixed   = apollo_fixed,
    database       = database,
    apollo_control = apollo_control,
    silent         = TRUE
  )

  model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
  apollo_modelOutput(model, modelOutput_settings = list(printPVal = TRUE))
  model
}

# Estimates the mode choice model on `agqpv_sub`, saves the model object to
# results_output/mode_choice_model_<tag>.rds, then computes the logsum for
# every OD pair in BOTH tt_ausland_ch and tt_ch_ch, saving them to
# data/output/tt_ausland_CH_logsum_<tag>.fst and
# data/output/tt_CH_CH_logsum_<tag>.fst ("" -> pooled files with no suffix).
save_logsum <- function(tt_dt, out_prefix, model, model_name, tag) {
  tt_logsum <- compute_logsum(tt_dt, model)
  logsum_path <- sprintf("%s_logsum%s.fst", out_prefix, tag)
  write_fst(tt_logsum, logsum_path, compress = 100)

  n_na_logsum <- sum(is.na(tt_logsum$logsum))
  cat(sprintf("[%s] logsum -> %s (range [%.3f, %.3f], mean %.3f, %d/%d NA -- both modes unreachable)\n",
      model_name, logsum_path,
      min(tt_logsum$logsum, na.rm = TRUE), max(tt_logsum$logsum, na.rm = TRUE),
      mean(tt_logsum$logsum, na.rm = TRUE), n_na_logsum, nrow(tt_logsum)))

  tt_logsum
}

run_mode_choice_and_logsum <- function(agqpv_sub, model_name, tag = "") {
  agents_sub <- sample_agents(agqpv_sub, nrow(agqpv_sub), seed = 42)
  mode_data  <- build_mode_data(agents_sub, tt_agqpv)
  cat(sprintf("[%s] estimation sample: %d agents (%.1f%% car, %.1f%% pt)\n",
      model_name, nrow(mode_data), 100 * mean(mode_data$choice == 1), 100 * mean(mode_data$choice == 2)))

  model <- estimate_mode_choice(mode_data, model_name)

  model_path <- sprintf("results_output/mode_choice_model%s.rds", tag)
  saveRDS(model, model_path)

  ausland_ch_logsum <- save_logsum(tt_ausland_ch, "data/output/tt_ausland_CH", model, model_name, tag)
  ch_ch_logsum      <- save_logsum(tt_ch_ch,      "data/output/tt_CH_CH",      model, model_name, tag)
  cat("\n")

  list(model = model, logsum = ausland_ch_logsum, logsum_ch_ch = ch_ch_logsum)
}

# -----------------------------------------------------------------------------
# 5. POOLED MODEL (all non-Swiss)
# -----------------------------------------------------------------------------

pooled <- run_mode_choice_and_logsum(agqpv, "mode_choice", tag = "")

# -----------------------------------------------------------------------------
# 6. TAGESREISE (n_nights == 0) AND REISE MIT Ü. (n_nights > 0), SEPARATELY
# -----------------------------------------------------------------------------

agqpv_tagesreise   <- agqpv[n_nights == 0]
agqpv_reise_mit_ue <- agqpv[n_nights > 0]
cat(sprintf("Tagesreise (n_nights==0): %d respondents\n", nrow(agqpv_tagesreise)))
cat(sprintf("Reise mit Ü. (n_nights>0): %d respondents\n\n", nrow(agqpv_reise_mit_ue)))

tagesreise   <- run_mode_choice_and_logsum(agqpv_tagesreise,   "mode_choice_tagesreise",   tag = "_tagesreise")
reise_mit_ue <- run_mode_choice_and_logsum(agqpv_reise_mit_ue, "mode_choice_reisemitue",   tag = "_reisemitue")

cat("Done. Logsum files:\n")
cat("  data/output/tt_ausland_CH_logsum.fst            (pooled, all non-Swiss)\n")
cat("  data/output/tt_ausland_CH_logsum_tagesreise.fst (n_nights == 0)\n")
cat("  data/output/tt_ausland_CH_logsum_reisemitue.fst (n_nights > 0)\n")
cat("  data/output/tt_CH_CH_logsum.fst                 (pooled, all non-Swiss)\n")
cat("  data/output/tt_CH_CH_logsum_tagesreise.fst      (n_nights == 0)\n")
cat("  data/output/tt_CH_CH_logsum_reisemitue.fst      (n_nights > 0)\n")
