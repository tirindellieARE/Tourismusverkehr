library(data.table)
library(sf)
library(hdf5r)
library(apollo)

N_ALTS    <- 100
N_AGENTS  <- 1000
N_SAMPLES <- 10

# =============================================================================
# 1. LOAD DATA (once)
# =============================================================================

agents   <- fread("data/output/agqpv.csv")
agents[, agent_id := .I]
zones_sf <- st_read("data/input/zones_communes.gpkg", quiet = TRUE)

zone_attrs <- as.data.table(st_drop_geometry(zones_sf))[, .(NO, STALAN2020)]
all_zones  <- zone_attrs$NO

agents_noswiss <- agents[nationality != 1]
agents_noswiss[, nat_group := fcase(
  nationality == 2, "DE",
  nationality == 3, "AT",
  nationality == 4, "FR",
  nationality == 5, "IT",
  default           = "other"
)]

tt_dt <- readRDS("data/input/tt_avg_lookup.rds")
cat("Data loaded.\n\n")

# =============================================================================
# 2. APOLLO PROBABILITY FUNCTION (fixed across all runs)
# =============================================================================

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = setNames(vector("list", N_ALTS), paste0("alt", 1:N_ALTS))
  for (j in 1:N_ALTS) {
    tt_j   <- get(paste0("tt_",   j))
    topo_j <- get(paste0("topo_", j))
    V[[paste0("alt", j)]] <-
      beta_tt_DE    * tt_j * (nat_group == "DE")    +
      beta_tt_AT    * tt_j * (nat_group == "AT")    +
      beta_tt_FR    * tt_j * (nat_group == "FR")    +
      beta_tt_IT    * tt_j * (nat_group == "IT")    +
      beta_tt_other * tt_j * (nat_group == "other") +
      beta_topo2 * (topo_j == 2) +
      beta_topo3 * (topo_j == 3)
  }
  mnl_settings = list(
    alternatives = setNames(1:N_ALTS, paste0("alt", 1:N_ALTS)),
    avail        = 1,
    choiceVar    = choice,
    V            = V
  )
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

apollo_beta = c(
  beta_tt_DE    = 0,
  beta_tt_AT    = 0,
  beta_tt_FR    = 0,
  beta_tt_IT    = 0,
  beta_tt_other = 0,
  beta_topo2    = 0,
  beta_topo3    = 0
)
# "other" group has only 21 agents in the full population — rarely sampled,
# so its parameter is fixed at 0 to avoid identification failures.
# apollo_fixed takes a character vector of names; the fixed value comes from apollo_beta.
apollo_fixed = c("beta_tt_other")

# Suppress Apollo's banner
invisible(capture.output(apollo_initialise()))

# =============================================================================
# 3. LOOP OVER 10 SAMPLES
# =============================================================================

results <- vector("list", N_SAMPLES)

for (s in seq_len(N_SAMPLES)) {
  cat(sprintf("=== Sample %d / %d ===\n", s, N_SAMPLES))
  set.seed(s)

  # Sample agents
  agents_sub <- agents_noswiss[sample(.N, N_AGENTS)]

  # Build choice matrix
  alt_matrix <- matrix(sample(all_zones, N_AGENTS * N_ALTS, replace = TRUE),
                       nrow = N_AGENTS, ncol = N_ALTS)
  chosen_col <- sample(N_ALTS, N_AGENTS, replace = TRUE)
  for (i in seq_len(N_AGENTS)) alt_matrix[i, chosen_col[i]] <- agents_sub$dest_zone[i]

  choice_long <- data.table(
    agent_id    = rep(agents_sub$agent_id,    each = N_ALTS),
    alt_id      = rep(seq_len(N_ALTS),        times = N_AGENTS),
    alt_zone    = as.vector(t(alt_matrix)),
    chosen      = as.integer(rep(seq_len(N_ALTS), times = N_AGENTS) ==
                              rep(chosen_col,      each  = N_ALTS)),
    nat_group   = rep(agents_sub$nat_group,   each = N_ALTS),
    origin_zone = rep(agents_sub$origin_zone, each = N_ALTS)
  )

  choice_long <- zone_attrs[choice_long, on = c(NO = "alt_zone")]
  setnames(choice_long, c("NO", "STALAN2020"), c("alt_zone", "alt_topology"))
  choice_long[, alt_topology_num := as.integer(alt_topology)]
  choice_long <- tt_dt[choice_long, on = c("origin_zone", alt_zone = "alt_zone")]

  alt_ids   <- as.character(1:N_ALTS)
  indiv_dt  <- unique(choice_long[, .(agent_id, nat_group, origin_zone)])
  choice_dt <- data.table(agent_id = agents_sub$agent_id, choice = chosen_col)
  wide_tt   <- dcast(choice_long, agent_id ~ alt_id, value.var = "tt_avg")
  wide_topo <- dcast(choice_long, agent_id ~ alt_id, value.var = "alt_topology_num")
  setnames(wide_tt,   alt_ids, paste0("tt_",   alt_ids))
  setnames(wide_topo, alt_ids, paste0("topo_", alt_ids))

  database <- as.data.frame(
    Reduce(function(a, b) merge(a, b, by = "agent_id"),
           list(indiv_dt, choice_dt, wide_tt, wide_topo))
  )

  apollo_control <- list(
    modelName       = sprintf("mnl_sample%02d", s),
    modelDescr      = "MNL destination choice — OD travel time",
    indivID         = "agent_id",
    outputDirectory = "results_output/",
    silent          = TRUE
  )

  apollo_inputs <- tryCatch(
    suppressMessages(suppressWarnings(apollo_validateInputs())),
    error = function(e) { cat("  validateInputs failed:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(apollo_inputs)) next
  apollo_inputs$N_ALTS <- N_ALTS

  model <- tryCatch(
    suppressMessages(suppressWarnings(
      apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,
                      estimate_settings = list(silent = TRUE))
    )),
    error = function(e) { cat("  Estimation failed:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(model)) next

  est <- as.data.table(model$estimate, keep.rownames = "param")
  setnames(est, "V1", "estimate")
  est[, sample := s]
  est[, final_ll := model$LLout]
  results[[s]] <- est

  # Write incrementally so results survive Apollo stdout hijacking
  fwrite(est, "results_output/bootstrap_results.csv", append = (s > 1))
  cat(sprintf("  LL = %.4f\n", model$LLout))
}

# =============================================================================
# 4. SUMMARY STATISTICS  (read from CSV in case stdout was hijacked)
# =============================================================================

all_results <- fread("results_output/bootstrap_results.csv")
cat("\n\n")
cat("=================================================================\n")
cat("SUMMARY STATISTICS ACROSS 10 SAMPLES (1000 agents each)\n")
cat("=================================================================\n\n")

summary_dt <- all_results[, .(
  mean   = mean(estimate),
  sd     = sd(estimate),
  min    = min(estimate),
  median = median(estimate),
  max    = max(estimate)
), by = param]

print(summary_dt, digits = 4)

cat("\n\nFinal LL per sample:\n")
ll_dt <- unique(all_results[, .(sample, final_ll)])
print(ll_dt)
cat(sprintf("\nMean LL: %.2f  |  SD: %.2f\n", mean(ll_dt$final_ll), sd(ll_dt$final_ll)))
