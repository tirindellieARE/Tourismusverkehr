suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(hdf5r)
  library(apollo)
})

N_ALTS   <- 300
N_AGENTS <- 1000
SEED     <- 42

# --- Load base data ---
agents   <- fread("data/agents.csv")
zones_sf <- st_read("data/zones_communes.gpkg", quiet = TRUE)
zone_attrs <- as.data.table(st_drop_geometry(zones_sf))[, .(NO, STALAN2020)]
all_zones  <- zone_attrs$NO

# AT merged into "other"
agents_noswiss <- agents[nationality != 1]
agents_noswiss[, nat_group := fcase(
  nationality == 2, "DE",
  nationality == 4, "FR",
  nationality == 5, "IT",
  default           = "other"   # AT + remaining
)]

tt_dt <- readRDS("data/tt_avg_lookup.rds")

# --- Load attractivity indexes (log1p + z-standardised) ---
attr_cols <- c(
  "v01_gastronomy_count_log1p",
  "v02_resident_population_log1p",
  "v03_lake_shore_density_log1p",
  "v04_hard_outdoor_count_log1p",
  "v05_soft_outdoor_count_log1p",
  "v06_land_use_mix_log1p",
  "v07_cultural_count_log1p",
  "v08_sport_count_log1p",
  "v09_outdoor_route_length_log1p",
  "v10_other_leisure_count_log1p",
  "v11_diversity_index_log1p",
  "v12_urban_POI_density_log1p",
  "v13_support_services_log1p"
)
benz <- fread("../benzoni_thesis/output/attractivity_indexes.csv",
              select = c("npvm_id", attr_cols))
for (col in attr_cols) {
  m <- mean(benz[[col]]); s <- sd(benz[[col]])
  benz[, (col) := (get(col) - m) / s]
}

# --- Sample agents ---
set.seed(SEED)
agents_sub <- agents_noswiss[sample(.N, N_AGENTS)]

# --- Build alternative matrix ---
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
for (col in attr_cols) choice_long[is.na(get(col)), (col) := 0]

# --- Wide format ---
indiv_dt  <- unique(choice_long[, .(agent_id, nat_group, origin_zone)])
choice_dt <- data.table(agent_id = agents_sub$agent_id, choice = chosen_col)

wide_tt   <- dcast(choice_long, agent_id ~ alt_id, value.var = "tt_avg")
wide_topo <- dcast(choice_long, agent_id ~ alt_id, value.var = "alt_topology_num")
setnames(wide_tt,   alt_ids, paste0("tt_",   alt_ids))
setnames(wide_topo, alt_ids, paste0("topo_", alt_ids))

wide_list <- list(indiv_dt, choice_dt, wide_tt, wide_topo)
for (col in attr_cols) {
  w <- dcast(choice_long, agent_id ~ alt_id, value.var = col)
  setnames(w, alt_ids, paste0(col, "_", alt_ids))
  wide_list <- c(wide_list, list(w))
}

database <- as.data.frame(
  Reduce(function(a, b) merge(a, b, by = "agent_id"), wide_list)
)

# --- Apollo setup ---
invisible(capture.output(apollo_initialise()))

apollo_control <- list(
  modelName       = "mnl_attr_nat2",
  modelDescr      = "MNL: 13 indexes x nat + tt x nat + topology x nat (AT merged into other)",
  indivID         = "agent_id",
  outputDirectory = "output/"
)

nats <- c("DE", "FR", "IT")   # "other" is base (fixed at 0)

# Travel time x nationality (other = 0 fixed)
tt_betas <- setNames(rep(0, 3), paste0("beta_tt_", nats))

# Topology x nationality: topo2 and topo3, each for DE/FR/IT
topo_betas <- setNames(
  rep(0, 6),
  c(paste0("beta_topo2_", nats), paste0("beta_topo3_", nats))
)

# Attractivity x nationality: 13 vars x 3 nationalities
attr_short <- sprintf("v%02d", 1:13)
attr_betas <- setNames(
  rep(0, length(attr_short) * length(nats)),
  as.vector(outer(paste0("beta_", attr_short), nats, paste, sep = "_"))
)

apollo_beta  <- c(tt_betas, topo_betas, attr_betas)
apollo_fixed <- character(0)   # nothing fixed — "other" has no params

apollo_inputs <- apollo_validateInputs()
apollo_inputs$N_ALTS    <- N_ALTS
apollo_inputs$attr_cols <- attr_cols
apollo_inputs$nats      <- nats

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = setNames(vector("list", N_ALTS), paste0("alt", 1:N_ALTS))

  for (j in 1:N_ALTS) {
    tt_j   <- get(paste0("tt_",   j))
    topo_j <- get(paste0("topo_", j))

    # Travel time x nationality (other = 0)
    v <- beta_tt_DE * tt_j * (nat_group == "DE") +
         beta_tt_FR * tt_j * (nat_group == "FR") +
         beta_tt_IT * tt_j * (nat_group == "IT")

    # Topology x nationality (other = 0)
    v <- v +
      beta_topo2_DE * (topo_j == 2) * (nat_group == "DE") +
      beta_topo2_FR * (topo_j == 2) * (nat_group == "FR") +
      beta_topo2_IT * (topo_j == 2) * (nat_group == "IT") +
      beta_topo3_DE * (topo_j == 3) * (nat_group == "DE") +
      beta_topo3_FR * (topo_j == 3) * (nat_group == "FR") +
      beta_topo3_IT * (topo_j == 3) * (nat_group == "IT")

    # Attractivity x nationality (other = 0)
    for (k in seq_along(attr_cols)) {
      col_j <- get(paste0(attr_cols[k], "_", j))
      bk    <- sprintf("beta_v%02d", k)
      v <- v +
        get(paste0(bk, "_DE")) * col_j * (nat_group == "DE") +
        get(paste0(bk, "_FR")) * col_j * (nat_group == "FR") +
        get(paste0(bk, "_IT")) * col_j * (nat_group == "IT")
    }

    V[[paste0("alt", j)]] <- v
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

cat(sprintf("Running MNL: %d agents, %d alts, %d free params\n",
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
fwrite(est, "output/attr_nat2_results.csv")
cat(sprintf("Done  LL=%.4f  rho2=%.4f\n", model$LLout, model$rho2_0))
