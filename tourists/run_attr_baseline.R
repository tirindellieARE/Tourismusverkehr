suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(hdf5r)
  library(apollo)
})

N_ALTS   <- 300
N_AGENTS <- 1000
SEED     <- 42

agents   <- fread("data/agqpv.csv")
agents[, agent_id := .I]
zones_sf <- st_read("data/zones_communes.gpkg", quiet = TRUE)
zone_attrs <- as.data.table(st_drop_geometry(zones_sf))[, .(NO, STALAN2020)]
all_zones  <- zone_attrs$NO

agents_noswiss <- agents[nationality != 1]
agents_noswiss[, nat_group := fcase(
  nationality == 2, "DE",
  nationality == 4, "FR",
  nationality == 5, "IT",
  default           = "other"
)]

tt_dt <- readRDS("data/tt_avg_lookup.rds")

# Only v02 population
benz <- fread("../benzoni_thesis/output/attractivity_indexes.csv",
              select = c("npvm_id", "v02_resident_population_log1p"))
m <- mean(benz$v02_resident_population_log1p)
s <- sd(benz$v02_resident_population_log1p)
benz[, v02_resident_population_log1p := (v02_resident_population_log1p - m) / s]

set.seed(SEED)
agents_sub <- agents_noswiss[sample(.N, N_AGENTS)]

alt_matrix <- matrix(sample(all_zones, N_AGENTS * N_ALTS, replace = TRUE),
                     nrow = N_AGENTS, ncol = N_ALTS)
chosen_col <- sample(N_ALTS, N_AGENTS, replace = TRUE)
for (i in seq_len(N_AGENTS)) alt_matrix[i, chosen_col[i]] <- agents_sub$dest_zone[i]

alt_ids <- as.character(1:N_ALTS)

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
choice_long <- benz[choice_long, on = c(npvm_id = "alt_zone")]
choice_long[is.na(v02_resident_population_log1p), v02_resident_population_log1p := 0]

indiv_dt  <- unique(choice_long[, .(agent_id, nat_group, origin_zone)])
choice_dt <- data.table(agent_id = agents_sub$agent_id, choice = chosen_col)
wide_tt   <- dcast(choice_long, agent_id ~ alt_id, value.var = "tt_avg")
wide_topo <- dcast(choice_long, agent_id ~ alt_id, value.var = "alt_topology_num")
wide_pop  <- dcast(choice_long, agent_id ~ alt_id, value.var = "v02_resident_population_log1p")
setnames(wide_tt,   alt_ids, paste0("tt_",   alt_ids))
setnames(wide_topo, alt_ids, paste0("topo_", alt_ids))
setnames(wide_pop,  alt_ids, paste0("pop_",  alt_ids))

database <- as.data.frame(
  Reduce(function(a, b) merge(a, b, by = "agent_id"),
         list(indiv_dt, choice_dt, wide_tt, wide_topo, wide_pop))
)

invisible(capture.output(apollo_initialise()))

apollo_control <- list(
  modelName       = "mnl_baseline",
  modelDescr      = "Baseline: tt + topology + population, all x nationality",
  indivID         = "agent_id",
  outputDirectory = "output/"
)

nats <- c("DE", "FR", "IT")

apollo_beta <- c(
  setNames(rep(0, 3), paste0("beta_tt_",    nats)),
  setNames(rep(0, 6), c(paste0("beta_topo2_", nats), paste0("beta_topo3_", nats))),
  setNames(rep(0, 3), paste0("beta_pop_",   nats))
)
apollo_fixed <- character(0)

apollo_inputs <- apollo_validateInputs()
apollo_inputs$N_ALTS <- N_ALTS

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = setNames(vector("list", N_ALTS), paste0("alt", 1:N_ALTS))
  for (j in 1:N_ALTS) {
    tt_j   <- get(paste0("tt_",   j))
    topo_j <- get(paste0("topo_", j))
    pop_j  <- get(paste0("pop_",  j))
    V[[paste0("alt", j)]] <-
      beta_tt_DE    * tt_j * (nat_group == "DE") +
      beta_tt_FR    * tt_j * (nat_group == "FR") +
      beta_tt_IT    * tt_j * (nat_group == "IT") +
      beta_topo2_DE * (topo_j == 2) * (nat_group == "DE") +
      beta_topo2_FR * (topo_j == 2) * (nat_group == "FR") +
      beta_topo2_IT * (topo_j == 2) * (nat_group == "IT") +
      beta_topo3_DE * (topo_j == 3) * (nat_group == "DE") +
      beta_topo3_FR * (topo_j == 3) * (nat_group == "FR") +
      beta_topo3_IT * (topo_j == 3) * (nat_group == "IT") +
      beta_pop_DE   * pop_j * (nat_group == "DE") +
      beta_pop_FR   * pop_j * (nat_group == "FR") +
      beta_pop_IT   * pop_j * (nat_group == "IT")
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

cat(sprintf("Running baseline MNL: %d agents, %d alts, %d free params\n",
            N_AGENTS, N_ALTS, length(apollo_beta)))

model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

est <- data.table(
  param    = names(model$estimate),
  estimate = as.numeric(model$estimate),
  se       = tryCatch(sqrt(diag(model$varcov)), error = function(e) rep(NA_real_, length(model$estimate))),
  final_ll = model$LLout,
  rho2     = model$rho2_0
)
est[, tstat := estimate / se]
fwrite(est, "output/attr_baseline_results.csv")
cat(sprintf("Done  LL=%.4f  rho2=%.4f\n", model$LLout, model$rho2_0))
