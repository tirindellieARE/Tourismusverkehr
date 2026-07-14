# =============================================================================
# Destination Choice Model — Apollo MNL
#
# Baseline utility (both logsum and urban topology interacted with nationality):
#   V_j = sum_k beta_logsum_k * logsum_j        * I(nat == k)
#       + sum_k beta_topo2_k * I(topo_j == 2)   * I(nat == k)
#       + sum_k beta_topo3_k * I(topo_j == 3)   * I(nat == k)
#
# logsum_j = ln(exp(V_car) + exp(V_pt)), the inclusive value / expected-maximum-
#            utility from the mode choice model estimated in
#            03_prepare_choice_data.R (data/tt_full_logsum.fst). This replaces
#            a raw origin-destination travel time with the accessibility
#            measure implied by the lower-level (mode) choice -- standard
#            nested-logit practice (Ben-Akiva & Lerman / McFadden).
# origin_i = tourist's border-entry zone (foreign zone, outside CH)
#
# Swiss tourists (nationality == 1) excluded.
#
# run_destination_model(n_agents, n_alts)  -- one estimation run, your choice
#   of sample size and choice-set size.
# run_destination_batch(n_agents_vec, n_alts_vec) -- runs several models and
#   saves all of their estimates together under one output/destination_mnl_
#   batch_<batch_id>.csv, tagged with a shared batch_id and a run_id per model.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(fst)
  library(apollo)
})

# =============================================================================
# 1. LOAD DATA (shared across all runs)
# =============================================================================

agents   <- fread("data/agqpv.csv")
agents[, agent_id := .I]
zones_sf <- st_read("data/zones_communes.gpkg", quiet = TRUE)

zone_attrs <- as.data.table(st_drop_geometry(zones_sf))[, .(NO, STALAN2020)]
all_zones  <- zone_attrs$NO

agents_noswiss <- agents[nationality != 1]
cat(sprintf("Agents after removing Swiss : %d  (removed %d)\n",
    nrow(agents_noswiss), nrow(agents) - nrow(agents_noswiss)))

agents_noswiss[, nat_group := fcase(
  nationality == 2, "DE",
  nationality == 4, "FR",
  nationality == 5, "IT",
  default           = "other"
)]
NAT_LEVELS <- sort(unique(agents_noswiss$nat_group))
cat(sprintf("Nationality groups: %s\n", paste(NAT_LEVELS, collapse = ", ")))
print(agents_noswiss[, .N, by = nat_group][order(-N)])

LOGSUM_CACHE <- "data/tt_full_logsum.fst"
if (!file.exists(LOGSUM_CACHE)) {
  stop(sprintf(
    "%s not found. Run 03_prepare_choice_data.R first to estimate the mode choice model and build the logsum lookup.",
    LOGSUM_CACHE))
}
tt_dt <- as.data.table(read_fst(LOGSUM_CACHE, columns = c("origin_zone", "dest_zone", "logsum")))
setkey(tt_dt, origin_zone, dest_zone)
cat(sprintf("Loaded logsum lookup from %s (%d rows)\n\n", LOGSUM_CACHE, nrow(tt_dt)))

dir.create("output", showWarnings = FALSE)
invisible(capture.output(apollo_initialise()))

# =============================================================================
# 2. BUILD LONG/WIDE CHOICE DATA FOR ONE RUN
# =============================================================================
# n_agents : how many agqpv respondents to sample for this run
# n_alts   : choice-set size (chosen destination + n_alts-1 random alternatives,
#            drawn from all ~8000 zones)

build_destination_data <- function(n_agents, n_alts, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  agents_sub <- agents_noswiss[sample(.N, n_agents)]

  alt_matrix <- matrix(sample(all_zones, n_agents * n_alts, replace = TRUE),
                       nrow = n_agents, ncol = n_alts)
  chosen_col <- sample(n_alts, n_agents, replace = TRUE)
  for (i in seq_len(n_agents)) alt_matrix[i, chosen_col[i]] <- agents_sub$dest_zone[i]

  choice_long <- data.table(
    agent_id    = rep(agents_sub$agent_id,    each = n_alts),
    alt_id      = rep(seq_len(n_alts),        times = n_agents),
    alt_zone    = as.vector(t(alt_matrix)),
    chosen      = as.integer(rep(seq_len(n_alts), times = n_agents) ==
                              rep(chosen_col,      each  = n_alts)),
    nat_group   = rep(agents_sub$nat_group,   each = n_alts),
    origin_zone = rep(agents_sub$origin_zone, each = n_alts)
  )

  # Join zone topology
  choice_long <- zone_attrs[choice_long, on = c(NO = "alt_zone")]
  setnames(choice_long, c("NO", "STALAN2020"), c("alt_zone", "alt_topology"))
  choice_long[, alt_topology_num := as.integer(alt_topology)]

  # Join logsum (OD-specific accessibility)
  choice_long <- tt_dt[choice_long, on = c("origin_zone", dest_zone = "alt_zone")]
  setnames(choice_long, "dest_zone", "alt_zone")
  n_missing_logsum <- sum(is.na(choice_long$logsum))
  if (n_missing_logsum > 0)
    cat(sprintf("WARNING: %d rows missing logsum (will be NA in model)\n", n_missing_logsum))

  alt_ids <- as.character(seq_len(n_alts))

  indiv_dt    <- unique(choice_long[, .(agent_id, nat_group, origin_zone)])
  choice_dt   <- data.table(agent_id = agents_sub$agent_id, choice = chosen_col)
  wide_logsum <- dcast(choice_long, agent_id ~ alt_id, value.var = "logsum")
  wide_topo   <- dcast(choice_long, agent_id ~ alt_id, value.var = "alt_topology_num")

  setnames(wide_logsum, alt_ids, paste0("logsum_", alt_ids))
  setnames(wide_topo,   alt_ids, paste0("topo_",   alt_ids))

  as.data.frame(
    Reduce(function(a, b) merge(a, b, by = "agent_id"),
           list(indiv_dt, choice_dt, wide_logsum, wide_topo))
  )
}

# =============================================================================
# 3. RUN ONE MODEL
# =============================================================================

run_destination_model <- function(n_agents, n_alts, seed = 42, model_name = NULL) {
  if (is.null(model_name))
    model_name <- sprintf("destination_mnl_n%d_alts%d", n_agents, n_alts)

  database <- build_destination_data(n_agents, n_alts, seed = seed)
  cat(sprintf("[%s] Apollo database : %d rows x %d cols\n", model_name, nrow(database), ncol(database)))

  apollo_control <- list(
    modelName       = model_name,
    modelDescr      = sprintf("MNL destination choice -- logsum x nat, topo x nat (n_agents=%d, n_alts=%d)", n_agents, n_alts),
    indivID         = "agent_id",
    outputDirectory = "output/"
  )

  beta_names <- c(
    paste0("beta_logsum_", NAT_LEVELS),
    paste0("beta_topo2_",  NAT_LEVELS),
    paste0("beta_topo3_",  NAT_LEVELS)
  )
  apollo_beta  <- setNames(rep(0, length(beta_names)), beta_names)
  apollo_fixed <- c()

  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))

    P <- list()
    V <- setNames(vector("list", n_alts), paste0("alt", 1:n_alts))

    for (j in 1:n_alts) {
      logsum_j <- get(paste0("logsum_", j))
      topo_j   <- get(paste0("topo_",   j))

      v <- 0
      for (g in NAT_LEVELS) {
        is_g <- (nat_group == g)
        v <- v +
          get(paste0("beta_logsum_", g)) * logsum_j          * is_g +
          get(paste0("beta_topo2_",  g)) * (topo_j == 2)     * is_g +
          get(paste0("beta_topo3_",  g)) * (topo_j == 3)     * is_g
      }
      V[[paste0("alt", j)]] <- v
    }

    mnl_settings <- list(
      alternatives = setNames(1:n_alts, paste0("alt", 1:n_alts)),
      avail        = 1,
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

  t0      <- proc.time()
  model   <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
  elapsed <- (proc.time() - t0)["elapsed"]

  apollo_modelOutput(model, modelOutput_settings = list(printPVal = TRUE))
  apollo_saveOutput(model)

  rho2 <- if (!is.null(model$rho2_0) && length(model$rho2_0) == 1) model$rho2_0 else NA_real_

  estimates <- data.table(
    model_name = model_name,
    n_agents   = n_agents,
    n_alts     = n_alts,
    param      = names(model$estimate),
    estimate   = as.numeric(model$estimate),
    se         = as.numeric(model$se[names(model$estimate)]),
    final_ll   = model$LLout,
    rho2       = rho2,
    aic        = model$AIC,
    bic        = model$BIC,
    seconds    = round(elapsed, 1)
  )

  list(model = model, estimates = estimates)
}

# =============================================================================
# 4. RUN + SAVE A BATCH OF MODELS TOGETHER
# =============================================================================
# e.g. run_destination_batch(c(1000, 2000, 3000), 300) runs three models
# (1000/2000/3000 agents, all with a 300-alternative choice set) and saves all
# of their parameter estimates together in a single CSV, tagged with a shared
# batch_id and a run_id (1, 2, 3, ...) identifying each model within the batch.

run_destination_batch <- function(n_agents_vec, n_alts_vec = 300, seed = 42, batch_id = NULL) {
  if (is.null(batch_id)) batch_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
  n_alts_vec <- rep(n_alts_vec, length.out = length(n_agents_vec))

  all_estimates <- vector("list", length(n_agents_vec))
  for (i in seq_along(n_agents_vec)) {
    res <- run_destination_model(n_agents_vec[i], n_alts_vec[i], seed = seed)
    res$estimates[, `:=`(batch_id = batch_id, run_id = i)]
    all_estimates[[i]] <- res$estimates
  }

  batch_dt <- rbindlist(all_estimates)
  setcolorder(batch_dt, c("batch_id", "run_id", setdiff(names(batch_dt), c("batch_id", "run_id"))))

  out_file <- sprintf("output/destination_mnl_batch_%s.csv", batch_id)
  fwrite(batch_dt, out_file)
  cat(sprintf("\nBatch of %d model(s) saved to %s\n", length(n_agents_vec), out_file))
  batch_dt
}

# =============================================================================
# 5. BASELINE RUN
# =============================================================================

batch_results <- run_destination_batch(n_agents_vec = c(1000, 2000, 3000), n_alts_vec = 300)
