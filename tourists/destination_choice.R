# =============================================================================
# Destination Choice Model — Apollo MNL
#
# Utility:
#   V_j = beta_tt_k * tt_avg_j * I(nat==k)
#       + beta_topo2 * I(topo_j == 2)
#       + beta_topo3 * I(topo_j == 3)
#
# tt_avg_j = 0.9 * TTC[origin_i, j] + 0.1 * (RITA + EGT + ACT)[origin_i, j]
# origin_i = tourist's border-entry zone (foreign zone, outside CH)
#
# Swiss tourists (nationality == 1) excluded.
# =============================================================================

library(data.table)
library(sf)
library(hdf5r)
library(apollo)

set.seed(42)
N_ALTS   <- 100
N_AGENTS <- 1000

# =============================================================================
# 1. LOAD DATA
# =============================================================================

agents    <- fread("data/agents.csv")
zones_sf  <- st_read("data/zones_communes.gpkg", quiet = TRUE)

zone_attrs <- as.data.table(st_drop_geometry(zones_sf))[, .(NO, STALAN2020)]
all_zones  <- zone_attrs$NO

agents_noswiss <- agents[nationality != 1]
cat(sprintf("Agents after removing Swiss : %d  (removed %d)\n",
    nrow(agents_noswiss), nrow(agents) - nrow(agents_noswiss)))

agents_noswiss[, nat_group := fcase(
  nationality == 2, "DE",
  nationality == 3, "AT",
  nationality == 4, "FR",
  nationality == 5, "IT",
  default           = "other"
)]
cat("Nationality group distribution:\n")
print(agents_noswiss[, .N, by = nat_group][order(-N)])

# =============================================================================
# 2. BUILD tt_avg LOOKUP  (cache to RDS after first run)
# =============================================================================

TT_CACHE <- "data/tt_avg_lookup.rds"

if (!file.exists(TT_CACHE)) {
  cat("\nBuilding tt_avg lookup from OMX matrices...\n")

  orig_zones <- unique(agents_noswiss$origin_zone)

  open_no <- function(path) {
    h  <- H5File$new(path, mode = "r")
    no <- h[["lookup/NO"]][]
    h$close_all()
    no
  }
  no_ttc  <- open_no("data/TTC.omx")
  no_rita <- open_no("data/RITA.omx")
  no_egt  <- open_no("data/EGT.omx")
  no_act  <- open_no("data/ACT.omx")

  # Destination zones present in all 4 matrices
  dest_zones <- all_zones[all_zones %in% no_ttc &
                           all_zones %in% no_rita &
                           all_zones %in% no_egt  &
                           all_zones %in% no_act]
  cat(sprintf("Origin zones   : %d\nDest zones     : %d\n",
      length(orig_zones), length(dest_zones)))

  ci_ttc  <- match(dest_zones, no_ttc)
  ci_rita <- match(dest_zones, no_rita)
  ci_egt  <- match(dest_zones, no_egt)
  ci_act  <- match(dest_zones, no_act)

  h_ttc  <- H5File$new("data/TTC.omx",  mode = "r")
  h_rita <- H5File$new("data/RITA.omx", mode = "r")
  h_egt  <- H5File$new("data/EGT.omx",  mode = "r")
  h_act  <- H5File$new("data/ACT.omx",  mode = "r")

  tt_list <- vector("list", length(orig_zones))
  for (i in seq_along(orig_zones)) {
    oz      <- orig_zones[i]
    ri_ttc  <- match(oz, no_ttc)
    ri_rita <- match(oz, no_rita)
    ri_egt  <- match(oz, no_egt)
    ri_act  <- match(oz, no_act)

    ttc  <- h_ttc[["data/131"]][ri_ttc,  ][ci_ttc]
    rita <- h_rita[["data/141"]][ri_rita, ][ci_rita]
    egt  <- h_egt[["data/143"]][ri_egt,  ][ci_egt]
    act  <- h_act[["data/142"]][ri_act,  ][ci_act]

    tt_list[[i]] <- data.table(
      origin_zone = oz,
      alt_zone    = dest_zones,
      tt_avg      = 0.9 * ttc + 0.1 * (rita + egt + act)
    )
    if (i %% 50 == 0) cat(sprintf("  %d / %d origin zones processed\n", i, length(orig_zones)))
  }

  h_ttc$close_all(); h_rita$close_all(); h_egt$close_all(); h_act$close_all()

  tt_dt <- rbindlist(tt_list)
  saveRDS(tt_dt, TT_CACHE)
  cat(sprintf("tt_avg lookup saved to %s  (%d rows)\n\n", TT_CACHE, nrow(tt_dt)))
} else {
  cat(sprintf("\nLoading cached tt_avg lookup from %s\n\n", TT_CACHE))
  tt_dt <- readRDS(TT_CACHE)
}

# =============================================================================
# 3. SUBSAMPLE AND BUILD LONG-FORMAT CHOICE DATA
# =============================================================================

agents_sub <- agents_noswiss[sample(.N, N_AGENTS)]
cat(sprintf("Subsample : %d agents\n", nrow(agents_sub)))

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

# Join zone topology
choice_long <- zone_attrs[choice_long, on = c(NO = "alt_zone")]
setnames(choice_long, c("NO", "STALAN2020"), c("alt_zone", "alt_topology"))
choice_long[, alt_topology_num := as.integer(alt_topology)]

# Join tt_avg (OD-specific travel time)
choice_long <- tt_dt[choice_long, on = c("origin_zone", alt_zone = "alt_zone")]
n_missing_tt <- sum(is.na(choice_long$tt_avg))
if (n_missing_tt > 0)
  cat(sprintf("WARNING: %d rows missing tt_avg (will be NA in model)\n", n_missing_tt))

# =============================================================================
# 4. PIVOT TO WIDE FORMAT FOR APOLLO
# =============================================================================

alt_ids <- as.character(1:N_ALTS)

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

cat(sprintf("Apollo database : %d rows x %d cols\n\n", nrow(database), ncol(database)))

# =============================================================================
# 5. APOLLO SETUP
# =============================================================================

dir.create("output", showWarnings = FALSE)
apollo_initialise()

apollo_control = list(
  modelName       = "destination_mnl",
  modelDescr      = "MNL tourist destination choice — OD travel time by nationality",
  indivID         = "agent_id",
  outputDirectory = "output/"
)

apollo_beta = c(
  beta_tt_DE    = 0,
  beta_tt_AT    = 0,
  beta_tt_FR    = 0,
  beta_tt_IT    = 0,
  beta_tt_other = 0,
  beta_topo2    = 0,
  beta_topo3    = 0
)

apollo_fixed  = c()
apollo_inputs = apollo_validateInputs()
apollo_inputs$N_ALTS = N_ALTS

# =============================================================================
# 6. PROBABILITY FUNCTION
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

# =============================================================================
# 7. ESTIMATE
# =============================================================================

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model, modelOutput_settings = list(printPVal = TRUE))
