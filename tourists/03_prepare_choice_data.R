# =============================================================================
# Mode choice model (car vs PT) + logsum for the destination choice model.
#
# The mode choice model is estimated on the REVEALED-PREFERENCE agqpv trips:
# each respondent's actual origin_zone -> dest_zone pair gives a real tt_miv
# (car) / tt_oev (PT) travel time from data/tt_light.fst (built by
# 02_build_tt_lookups.R), and border_mode tells us which mode they actually
# used to cross into Switzerland (1 = road/car, 2 = rail/PT). tt_light has
# 100% coverage for these real OD pairs, so it's the only database needed for
# estimation.
#
# The logsum (inclusive value / expected maximum utility)
#   logsum_od = ln( exp(V_car_od) + exp(V_pt_od) )
# is then computed for EVERY OD pair in data/tt_full.fst (all agqpv origins x
# all ~8000 reachable dest zones) using the estimated mode-choice coefficients,
# since the destination choice model (04_destination_choice.R) needs a logsum
# for arbitrary alternative zones, not just the ones respondents actually
# visited -- tt_light's OD coverage is too sparse to be useful there, so only
# the full version is written to disk.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(fst)
  library(apollo)
})

# -----------------------------------------------------------------------------
# 0. LOAD DATA
# -----------------------------------------------------------------------------

tt_light <- as.data.table(read_fst("data/tt_light.fst"))
setkey(tt_light, origin_zone, dest_zone)
cat(sprintf("Loaded data/tt_light.fst (%d rows)\n", nrow(tt_light)))

agqpv <- fread("data/agqpv.csv")
agqpv[, agent_id := .I]
cat(sprintf("Loaded agqpv.csv: %d respondents\n", nrow(agqpv)))

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
# 3. ESTIMATE BINARY MNL MODE CHOICE MODEL
# -----------------------------------------------------------------------------
# NOTE: apollo_validateInputs()/apollo_estimate() look up `database`,
# `apollo_control`, `apollo_beta`, etc. in the global environment, so (matching
# the convention used in 04_destination_choice.R / run_nalts.R) this section
# runs at top level rather than inside a function.

dir.create("output", showWarnings = FALSE)
invisible(capture.output(apollo_initialise()))

apollo_control <- list(
  modelName       = "mode_choice",
  modelDescr      = "Binary MNL mode choice (car vs PT) -- cross-border tourists",
  indivID         = "agent_id",
  outputDirectory = "output/"
)

apollo_beta <- c(
  asc_car  = 0,
  b_tt_car = 0,
  b_tt_pt  = 0
)
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

# -----------------------------------------------------------------------------
# 4. COMPUTE LOGSUM
# -----------------------------------------------------------------------------
# Numerically-stable log-sum-exp of the two mode utilities, using the
# estimated mode-choice coefficients, for every OD pair in tt_dt.

compute_logsum <- function(tt_dt, model) {
  beta <- model$estimate

  V_car <- beta["asc_car"] + beta["b_tt_car"] * tt_dt$tt_miv
  V_pt  <-                   beta["b_tt_pt"]  * tt_dt$tt_oev

  v_max <- pmax(V_car, V_pt)
  logsum <- v_max + log(exp(V_car - v_max) + exp(V_pt - v_max))

  out <- copy(tt_dt)
  out[, logsum := logsum]
  out[]
}

# -----------------------------------------------------------------------------
# 5. ESTIMATE
# -----------------------------------------------------------------------------

N_AGENTS <- nrow(agqpv)   # set lower (e.g. 1000) for a quick test run
SEED     <- 42

agents_sub <- sample_agents(agqpv, N_AGENTS, seed = SEED)
mode_data  <- build_mode_data(agents_sub, tt_light)
cat(sprintf("Mode-choice estimation sample: %d agents (%.1f%% car, %.1f%% pt)\n",
    nrow(mode_data), 100 * mean(mode_data$choice == 1), 100 * mean(mode_data$choice == 2)))

database <- as.data.frame(mode_data[, .(agent_id, tt_miv, tt_oev, choice)])
database <- database[database$tt_oev < 1000, ]   # drop extreme PT travel-time outliers
apollo_inputs <- apollo_validateInputs()

mode_model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(mode_model, modelOutput_settings = list(printPVal = TRUE))
saveRDS(mode_model, "output/mode_choice_model.rds")

# -----------------------------------------------------------------------------
# 6. LOGSUM FOR THE FULL OD MATRIX (needed by 04_destination_choice.R)
# -----------------------------------------------------------------------------

tt_full <- as.data.table(read_fst("data/tt_full.fst"))
setkey(tt_full, origin_zone, dest_zone)
cat(sprintf("Loaded data/tt_full.fst (%d rows)\n", nrow(tt_full)))

tt_full_logsum <- compute_logsum(tt_full, mode_model)
write_fst(tt_full_logsum, "data/tt_full_logsum.fst", compress = 100)

cat(sprintf("\nLogsum computed for %d OD pairs -> data/tt_full_logsum.fst\n", nrow(tt_full_logsum)))
cat(sprintf("logsum range: [%.3f, %.3f], mean %.3f\n",
    min(tt_full_logsum$logsum), max(tt_full_logsum$logsum), mean(tt_full_logsum$logsum)))
