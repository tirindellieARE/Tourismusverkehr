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

# Attractivity indexes (Benzoni et al.) -- all 13 available, log1p + z-standardised.
# Pass any subset of these column names as `attr_vars` to build_destination_data() /
# run_destination_model() / run_destination_batch() to add them to the utility,
# each interacted with nationality.
ATTR_COLS <- c(
  "v01_gastronomy_count_log1p",     # restaurants, cafes, bars, fast food, ice cream
  "v02_resident_population_log1p",  # total residents per zone
  "v03_lake_shore_density_log1p",   # lake shoreline length / zone area
  "v04_hard_outdoor_count_log1p",   # cableways, viewpoints, huts, glaciers, caves, waterfalls
  "v05_soft_outdoor_count_log1p",   # parks, picnic sites, playgrounds, firepits, marinas
  "v06_land_use_mix_log1p",         # Shannon entropy of land-use composition
  "v07_cultural_count_log1p",       # museums, libraries, theatres, galleries, cinemas
  "v08_sport_count_log1p",          # pitches, fitness centres, tracks, swimming areas
  "v09_outdoor_route_length_log1p", # hiking/ski/snowshoe routes + via ferratas
  "v10_other_leisure_count_log1p",  # nightlife, social venues, fun parks, wellness
  "v11_diversity_index_log1p",      # count of distinct POI types present
  "v12_urban_POI_density_log1p",    # urban POIs / zone area
  "v13_support_services_log1p"      # fuel, ATMs, toilets, drinking water, fountains
)
benz <- fread("../benzoni_thesis/output/attractivity_indexes.csv",
              select = c("npvm_id", ATTR_COLS))
for (col in ATTR_COLS) {
  m <- mean(benz[[col]]); s <- sd(benz[[col]])
  benz[, (col) := (get(col) - m) / s]
}
cat(sprintf("Loaded %d attractivity indexes for %d zones\n\n", length(ATTR_COLS), nrow(benz)))

dir.create("output", showWarnings = FALSE)
invisible(capture.output(apollo_initialise()))

# =============================================================================
# 2. BUILD LONG/WIDE CHOICE DATA FOR ONE RUN
# =============================================================================
# n_agents       : how many agqpv respondents to sample for this run
# n_alts         : choice-set size (chosen destination + n_alts-1 random alternatives,
#                   drawn from all ~8000 zones)
# attr_vars      : optional character vector of ATTR_COLS names to add to the utility
# replace_agents : FALSE (default) samples n_agents distinct respondents; TRUE draws a
#                   bootstrap resample (same respondent can be drawn more than once)
# agent_pool     : which respondents to sample from -- defaults to all non-Swiss agents,
#                   pass a filtered subset (e.g. agents_noswiss[n_nights == 0]) to model a
#                   specific subgroup
# logsum_dt      : which EMU/logsum lookup to join -- defaults to the pooled tt_dt, pass a
#                   subgroup-specific one (e.g. tt_dt_tagesreise) to match agent_pool

build_destination_data <- function(n_agents, n_alts, attr_vars = character(0), seed = NULL,
                                    replace_agents = FALSE, agent_pool = agents_noswiss, logsum_dt = tt_dt) {
  if (!is.null(seed)) set.seed(seed)

  agents_sub <- agent_pool[sample(.N, n_agents, replace = replace_agents)]
  # Fresh per-draw ID: required when replace_agents = TRUE, since the same respondent
  # can be drawn more than once and each draw must still be a distinct Apollo row.
  agents_sub[, agent_id := .I]

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
  choice_long <- logsum_dt[choice_long, on = c("origin_zone", dest_zone = "alt_zone")]
  setnames(choice_long, "dest_zone", "alt_zone")
  n_missing_logsum <- sum(is.na(choice_long$logsum))
  if (n_missing_logsum > 0)
    cat(sprintf("WARNING: %d rows missing logsum (will be NA in model)\n", n_missing_logsum))

  # Join requested attractivity indexes (zone-level, no OD dimension)
  if (length(attr_vars) > 0) {
    choice_long <- benz[choice_long, on = c(npvm_id = "alt_zone")]
    setnames(choice_long, "npvm_id", "alt_zone")
    for (col in attr_vars) choice_long[is.na(get(col)), (col) := 0]
  }

  alt_ids <- as.character(seq_len(n_alts))

  indiv_dt    <- unique(choice_long[, .(agent_id, nat_group, origin_zone)])
  choice_dt   <- data.table(agent_id = agents_sub$agent_id, choice = chosen_col)
  wide_logsum <- dcast(choice_long, agent_id ~ alt_id, value.var = "logsum")
  wide_topo   <- dcast(choice_long, agent_id ~ alt_id, value.var = "alt_topology_num")

  setnames(wide_logsum, alt_ids, paste0("logsum_", alt_ids))
  setnames(wide_topo,   alt_ids, paste0("topo_",   alt_ids))

  wide_list <- list(indiv_dt, choice_dt, wide_logsum, wide_topo)
  for (col in attr_vars) {
    w <- dcast(choice_long, agent_id ~ alt_id, value.var = col)
    setnames(w, alt_ids, paste0(col, "_", alt_ids))
    wide_list <- c(wide_list, list(w))
  }

  as.data.frame(Reduce(function(a, b) merge(a, b, by = "agent_id"), wide_list))
}

# =============================================================================
# 3. RUN ONE MODEL
# =============================================================================

# Short label used in coefficient names, e.g. "v01_gastronomy_count_log1p" -> "v01"
attr_short_name <- function(attr_vars) sub("^(v[0-9]+).*", "\\1", attr_vars)

# Compact console summary: estimate / s.e. / t-stat / significance stars + fit stats
print_model_results <- function(estimates, model_name) {
  tab <- copy(estimates)
  tab[, tstat := estimate / se]
  tab[, sig := fifelse(abs(tstat) >= 2.576, "***",
               fifelse(abs(tstat) >= 1.96,  "**",
               fifelse(abs(tstat) >= 1.645, "*", "")))]
  cat(sprintf("\n=== %s ===\n", model_name))
  print(tab[, .(param, estimate = round(estimate, 4), se = round(se, 4),
                tstat = round(tstat, 2), sig)])
  cat(sprintf("LL = %.2f   rho2 = %.4f   AIC = %.1f   BIC = %.1f   n_agents = %d   n_alts = %d   time = %.1fs\n\n",
      estimates$final_ll[1], estimates$rho2[1], estimates$aic[1], estimates$bic[1],
      estimates$n_agents[1], estimates$n_alts[1], estimates$seconds[1]))
}

run_destination_model <- function(n_agents, n_alts, attr_vars = character(0), seed = 42, model_name = NULL,
                                   replace_agents = FALSE, agent_pool = agents_noswiss, logsum_dt = tt_dt,
                                   include_topo = TRUE) {
  if (is.null(model_name)) {
    attr_tag   <- if (length(attr_vars) > 0) paste0("_", paste(attr_short_name(attr_vars), collapse = "-")) else ""
    topo_tag   <- if (!include_topo) "_notopo" else ""
    model_name <- sprintf("destination_mnl_n%d_alts%d%s%s", n_agents, n_alts, attr_tag, topo_tag)
  }
  attr_short <- attr_short_name(attr_vars)

  database <- build_destination_data(n_agents, n_alts, attr_vars = attr_vars, seed = seed,
                                      replace_agents = replace_agents, agent_pool = agent_pool,
                                      logsum_dt = logsum_dt)
  cat(sprintf("[%s] Apollo database : %d rows x %d cols\n", model_name, nrow(database), ncol(database)))

  apollo_control <- list(
    modelName       = model_name,
    modelDescr      = sprintf("MNL destination choice -- logsum x nat%s%s (n_agents=%d, n_alts=%d)",
                               if (include_topo) ", topo x nat" else "",
                               if (length(attr_vars) > 0) paste0(", ", length(attr_vars), " attr x nat") else "",
                               n_agents, n_alts),
    indivID         = "agent_id",
    outputDirectory = "output/"
  )

  beta_names <- paste0("beta_logsum_", NAT_LEVELS)
  if (include_topo)
    beta_names <- c(beta_names, paste0("beta_topo2_", NAT_LEVELS), paste0("beta_topo3_", NAT_LEVELS))
  if (length(attr_vars) > 0)
    beta_names <- c(beta_names, as.vector(outer(paste0("beta_", attr_short), NAT_LEVELS, paste, sep = "_")))

  apollo_beta  <- setNames(rep(0, length(beta_names)), beta_names)
  apollo_fixed <- c()

  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))

    P <- list()
    V <- setNames(vector("list", n_alts), paste0("alt", 1:n_alts))

    for (j in 1:n_alts) {
      logsum_j <- get(paste0("logsum_", j))
      if (include_topo) topo_j <- get(paste0("topo_", j))

      v <- 0
      for (g in NAT_LEVELS) {
        is_g <- (nat_group == g)
        v <- v + get(paste0("beta_logsum_", g)) * logsum_j * is_g
        if (include_topo) {
          v <- v +
            get(paste0("beta_topo2_", g)) * (topo_j == 2) * is_g +
            get(paste0("beta_topo3_", g)) * (topo_j == 3) * is_g
        }
        if (length(attr_vars) > 0) {
          for (k in seq_along(attr_vars)) {
            attr_j <- get(paste0(attr_vars[k], "_", j))
            v <- v + get(paste0("beta_", attr_short[k], "_", g)) * attr_j * is_g
          }
        }
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

  print_model_results(estimates, model_name)

  list(model = model, estimates = estimates)
}

# =============================================================================
# 4. RUN + SAVE A BATCH OF MODELS TOGETHER
# =============================================================================
# e.g. run_destination_batch(c(1000, 2000, 3000), 300) runs three models
# (1000/2000/3000 agents, all with a 300-alternative choice set) and saves all
# of their parameter estimates together in a single CSV, tagged with a shared
# batch_id and a run_id (1, 2, 3, ...) identifying each model within the batch.

run_destination_batch <- function(n_agents_vec, n_alts_vec = 300, attr_vars = character(0), seed = 42, batch_id = NULL) {
  if (is.null(batch_id)) batch_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
  n_alts_vec <- rep(n_alts_vec, length.out = length(n_agents_vec))

  all_estimates <- vector("list", length(n_agents_vec))
  for (i in seq_along(n_agents_vec)) {
    res <- run_destination_model(n_agents_vec[i], n_alts_vec[i], attr_vars = attr_vars, seed = seed)
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
# 5. BOOTSTRAP: repeat the same spec with fresh with-replacement agent resamples
# =============================================================================
# Runs the same (n_agents, n_alts, attr_vars) specification n_reps times, each
# time drawing a fresh bootstrap resample of n_agents agents WITH replacement
# (duplicates allowed) -- alternatives are always sampled with replacement
# regardless, as in every other run. Saves every run's estimates together plus
# a mean/sd/min/max summary per parameter across the n_reps runs.

run_destination_bootstrap <- function(n_agents, n_alts, n_reps = 10, attr_vars = character(0),
                                       seeds = seq_len(n_reps), batch_id = NULL,
                                       agent_pool = agents_noswiss, logsum_dt = tt_dt, name_tag = "") {
  if (is.null(batch_id)) batch_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
  stopifnot(length(seeds) == n_reps)

  all_estimates <- vector("list", n_reps)
  for (i in seq_len(n_reps)) {
    model_name <- sprintf("destination_mnl_boot%02d%s_n%d_alts%d", i, name_tag, n_agents, n_alts)
    res <- run_destination_model(n_agents, n_alts, attr_vars = attr_vars, seed = seeds[i],
                                  model_name = model_name, replace_agents = TRUE,
                                  agent_pool = agent_pool, logsum_dt = logsum_dt)
    res$estimates[, `:=`(batch_id = batch_id, run_id = i)]
    all_estimates[[i]] <- res$estimates
  }

  boot_dt <- rbindlist(all_estimates)
  setcolorder(boot_dt, c("batch_id", "run_id", setdiff(names(boot_dt), c("batch_id", "run_id"))))

  out_file <- sprintf("output/destination_mnl_bootstrap_%s.csv", batch_id)
  fwrite(boot_dt, out_file)

  summary_dt <- boot_dt[, .(
    mean = mean(estimate),
    sd   = sd(estimate),
    min  = min(estimate),
    max  = max(estimate)
  ), by = param]

  cat(sprintf("\nBootstrap of %d run(s) saved to %s\n", n_reps, out_file))
  print(summary_dt, digits = 4)

  list(runs = boot_dt, summary = summary_dt)
}

# =============================================================================
# 6. RUN A SPECIFICATION
# =============================================================================
# Edit ATTR_VARS to test a different specification -- any subset (incl. none)
# of ATTR_COLS, e.g.:
#   ATTR_VARS <- character(0)                                   # baseline, no attractivity
#   ATTR_VARS <- c("v01_gastronomy_count_log1p")                 # single variable
#   ATTR_VARS <- ATTR_COLS                                       # all 13
#   ATTR_VARS <- c("v01_gastronomy_count_log1p",
#                  "v02_resident_population_log1p",
#                  "v03_lake_shore_density_log1p",
#                  "v07_cultural_count_log1p",
#                  "v09_outdoor_route_length_log1p",
#                  "v12_urban_POI_density_log1p")                # "minimal" attractivity set

ATTR_VARS <- c(
  "v01_gastronomy_count_log1p",
  "v02_resident_population_log1p",
  "v03_lake_shore_density_log1p",
  "v07_cultural_count_log1p",
  "v09_outdoor_route_length_log1p",
  "v12_urban_POI_density_log1p"
)

result <- run_destination_model(n_agents = 1000, n_alts = 300, attr_vars = ATTR_VARS)

# For a sweep across sample sizes / choice-set sizes with the same attr_vars, use:
# batch_results <- run_destination_batch(n_agents_vec = c(1000, 2000, 3000), n_alts_vec = 300, attr_vars = ATTR_VARS)
