# =============================================================================
# Destination Choice Model — AMR-region choice set (Apollo MNL)
#
# Same modelling idea as 04_destination_choice.R (EMU + zone-level attractivity
# variables, optionally interacted with nationality), but the choice set is now
# the 101 AMR regions from data/amr101.gpkg instead of individual NPVM zones,
# using the population-weighted region-level EMU / Erreichbarkeit / population
# built by aggregate_regions.R.
#
# Because there are only 101 regions (vs. ~8,700 zones), the FULL choice set is
# used for every agent -- no McFadden alternative sampling is needed. This is a
# genuine full-information MNL, not a sampled-alternatives approximation.
#
#   V_j = sum_k beta_logsum_k * EMU_j          * I(nat == k)
#       + sum_k beta_<attr>_k * attr_j         * I(nat == k)     (per attr_var)
#
# EMU_j    = population-weighted region-level logsum for this agent's
#            origin_zone -> region j (data/output/tt_full_logsum_amr101*.fst)
# attr_j   = region-level attribute (e.g. population, Erreichbarkeit),
#            population-weighted from zone level, z-standardised
# origin_i = tourist's border-entry zone (unchanged -- only the DESTINATION
#            side is aggregated to regions, origin stays a zone)
#
# The agent's chosen alternative is the AMR region containing their actual
# destination zone (data/output/zone_to_amr101.csv). Swiss tourists
# (nationality == 1) excluded, matching 04_destination_choice.R.
#
# run_destination_model_amr(n_agents)  -- one estimation run, your choice of
#   sample size, attractivity variable subset, agent subgroup, EMU lookup, and
#   whether variables are interacted with nationality (interact_nationality).
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(fst)
  library(apollo)
})

# =============================================================================
# 1. LOAD DATA
# =============================================================================

agents <- fread("data/output/agqpv.csv")
agents[, agent_id := .I]

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

# --- zone -> region lookup, applied to each respondent's actual destination ---
# NOTE: BAE2018 region codes are zero-padded strings (e.g. "01012" for Geneve) --
# fread would silently parse them as integer and strip the leading zero, so every
# read of this column is forced to character to match the .fst files below (which
# keep it as character throughout, having never round-tripped through a CSV).
zone_region <- fread("data/output/zone_to_amr101.csv", select = c("npvm_id", "BAE2018"),
                      colClasses = c(BAE2018 = "character"))
agents_noswiss <- merge(agents_noswiss, zone_region, by.x = "dest_zone", by.y = "npvm_id", all.x = TRUE)
setnames(agents_noswiss, "BAE2018", "dest_region")
n_missing_region <- sum(is.na(agents_noswiss$dest_region))
if (n_missing_region > 0) {
  cat(sprintf("WARNING: %d agents have no region for their destination zone -- dropped\n", n_missing_region))
  agents_noswiss <- agents_noswiss[!is.na(dest_region)]
}

# --- region master list + attributes (population, Erreichbarkeit), z-standardised ---
region_pop <- fread("data/output/amr101_population.csv", colClasses = c(BAE2018 = "character"))
region_pop[, pop := as.numeric(scale(log1p(region_population)))]

region_err <- fread("data/output/erreichbarkeit_amr101.csv", select = c("BAE2018", "erreichbarkeit_avg_z"),
                     colClasses = c(BAE2018 = "character"))

region_attrs <- merge(region_pop[, .(BAE2018, pop)], region_err, by = "BAE2018")
REGION_IDS <- sort(region_attrs$BAE2018)
cat(sprintf("Regions in choice set: %d\n\n", length(REGION_IDS)))

# --- region-level EMU lookups (built by aggregate_regions.R) ---
LOGSUM_CACHE_AMR <- "data/output/tt_full_logsum_amr101.fst"
if (!file.exists(LOGSUM_CACHE_AMR)) {
  stop(sprintf("%s not found. Run aggregate_regions.R first.", LOGSUM_CACHE_AMR))
}
tt_dt_amr <- as.data.table(read_fst(LOGSUM_CACHE_AMR))
setkey(tt_dt_amr, origin_zone, dest_region)
cat(sprintf("Loaded region-level EMU lookup from %s (%d rows)\n\n", LOGSUM_CACHE_AMR, nrow(tt_dt_amr)))

dir.create("results_output", showWarnings = FALSE)
invisible(capture.output(apollo_initialise()))

# =============================================================================
# 2. BUILD LONG/WIDE CHOICE DATA FOR ONE RUN
# =============================================================================
# n_agents       : how many agqpv respondents to sample for this run
# attr_vars      : optional character vector of region_attrs column names ("pop",
#                   "erreichbarkeit_avg_z") to add to the utility
# replace_agents : FALSE (default) samples n_agents distinct respondents; TRUE draws a
#                   bootstrap resample (same respondent can be drawn more than once)
# agent_pool     : which respondents to sample from -- defaults to all non-Swiss agents,
#                   pass a filtered subset (e.g. agents_noswiss[n_nights == 0]) to model a
#                   specific subgroup
# logsum_dt      : which region-level EMU lookup to join -- defaults to the pooled
#                   tt_dt_amr, pass a subgroup-specific one (e.g. tt_dt_amr_tagesreise)
#
# Unlike build_destination_data() in 04_destination_choice.R, the alternative set is
# NOT sampled -- every agent gets all 101 regions as their choice set, and "chosen"
# is simply the region containing their real destination zone.

build_destination_data_amr <- function(n_agents, attr_vars = character(0), seed = NULL,
                                        replace_agents = FALSE, agent_pool = agents_noswiss,
                                        logsum_dt = tt_dt_amr) {
  if (!is.null(seed)) set.seed(seed)

  agents_sub <- agent_pool[sample(.N, n_agents, replace = replace_agents)]
  # Fresh per-draw ID: required when replace_agents = TRUE, since the same respondent
  # can be drawn more than once and each draw must still be a distinct Apollo row.
  agents_sub[, agent_id := .I]

  n_alts <- length(REGION_IDS)
  chosen_col <- match(agents_sub$dest_region, REGION_IDS)

  choice_long <- data.table(
    agent_id    = rep(agents_sub$agent_id,    each = n_alts),
    alt_id      = rep(seq_len(n_alts),        times = n_agents),
    alt_region  = rep(REGION_IDS,             times = n_agents),
    chosen      = as.integer(rep(seq_len(n_alts), times = n_agents) ==
                              rep(chosen_col,      each  = n_alts)),
    nat_group   = rep(agents_sub$nat_group,   each = n_alts),
    origin_zone = rep(agents_sub$origin_zone, each = n_alts)
  )

  # Join region-level EMU (origin-zone x dest-region specific accessibility)
  choice_long <- logsum_dt[choice_long, on = c("origin_zone", dest_region = "alt_region")]
  setnames(choice_long, "dest_region", "alt_region")
  n_missing_logsum <- sum(is.na(choice_long$logsum))
  if (n_missing_logsum > 0)
    cat(sprintf("WARNING: %d rows missing logsum (will be NA in model)\n", n_missing_logsum))

  # Join requested region-level attributes (no OD dimension)
  if (length(attr_vars) > 0) {
    choice_long <- region_attrs[choice_long, on = c(BAE2018 = "alt_region")]
    setnames(choice_long, "BAE2018", "alt_region")
    for (col in attr_vars) choice_long[is.na(get(col)), (col) := 0]
  }

  alt_ids <- as.character(seq_len(n_alts))

  indiv_dt    <- unique(choice_long[, .(agent_id, nat_group, origin_zone)])
  choice_dt   <- data.table(agent_id = agents_sub$agent_id, choice = chosen_col)
  wide_logsum <- dcast(choice_long, agent_id ~ alt_id, value.var = "logsum")
  setnames(wide_logsum, alt_ids, paste0("logsum_", alt_ids))

  wide_list <- list(indiv_dt, choice_dt, wide_logsum)
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

attr_short_name_amr <- function(attr_vars) sub("^(v[0-9]+).*", "\\1", attr_vars)

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

run_destination_model_amr <- function(n_agents, attr_vars = character(0), seed = 42, model_name = NULL,
                                       replace_agents = FALSE, agent_pool = agents_noswiss, logsum_dt = tt_dt_amr,
                                       interact_nationality = TRUE) {
  n_alts <- length(REGION_IDS)
  if (is.null(model_name)) {
    attr_tag   <- if (length(attr_vars) > 0) paste0("_", paste(attr_short_name_amr(attr_vars), collapse = "-")) else ""
    nat_tag    <- if (!interact_nationality) "_pooled" else ""
    model_name <- sprintf("destination_mnl_amr_n%d%s%s", n_agents, attr_tag, nat_tag)
  }
  attr_short <- attr_short_name_amr(attr_vars)

  database <- build_destination_data_amr(n_agents, attr_vars = attr_vars, seed = seed,
                                          replace_agents = replace_agents, agent_pool = agent_pool,
                                          logsum_dt = logsum_dt)
  cat(sprintf("[%s] Apollo database : %d rows x %d cols\n", model_name, nrow(database), ncol(database)))

  apollo_control <- list(
    modelName       = model_name,
    modelDescr      = sprintf("MNL destination choice (AMR regions) -- logsum%s%s (n_agents=%d, n_alts=%d)",
                               if (interact_nationality) " x nat" else " (pooled)",
                               if (length(attr_vars) > 0) sprintf(", %d attr%s", length(attr_vars), if (interact_nationality) " x nat" else "") else "",
                               n_agents, n_alts),
    indivID         = "agent_id",
    outputDirectory = "results_output/"
  )

  if (interact_nationality) {
    beta_names <- paste0("beta_logsum_", NAT_LEVELS)
    if (length(attr_vars) > 0)
      beta_names <- c(beta_names, as.vector(outer(paste0("beta_", attr_short), NAT_LEVELS, paste, sep = "_")))
  } else {
    beta_names <- "beta_logsum"
    if (length(attr_vars) > 0)
      beta_names <- c(beta_names, paste0("beta_", attr_short))
  }

  apollo_beta  <- setNames(rep(0, length(beta_names)), beta_names)
  apollo_fixed <- c()

  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))

    P <- list()
    V <- setNames(vector("list", n_alts), paste0("alt", 1:n_alts))

    for (j in 1:n_alts) {
      logsum_j <- get(paste0("logsum_", j))

      v <- 0
      if (interact_nationality) {
        for (g in NAT_LEVELS) {
          is_g <- (nat_group == g)
          v <- v + get(paste0("beta_logsum_", g)) * logsum_j * is_g
          if (length(attr_vars) > 0) {
            for (k in seq_along(attr_vars)) {
              attr_j <- get(paste0(attr_vars[k], "_", j))
              v <- v + get(paste0("beta_", attr_short[k], "_", g)) * attr_j * is_g
            }
          }
        }
      } else {
        v <- v + beta_logsum * logsum_j
        if (length(attr_vars) > 0) {
          for (kk in seq_along(attr_vars)) {
            attr_j <- get(paste0(attr_vars[kk], "_", j))
            v <- v + get(paste0("beta_", attr_short[kk])) * attr_j
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
# 4. RUN A SPECIFICATION
# =============================================================================
# EMU + population + Erreichbarkeit, interacted with nationality, full sample,
# own EMU per subgroup, all 101 AMR regions in the choice set.

tt_dt_amr_tagesreise <- as.data.table(read_fst("data/output/tt_full_logsum_amr101_tagesreise.fst"))
setkey(tt_dt_amr_tagesreise, origin_zone, dest_region)
tt_dt_amr_reisemitue <- as.data.table(read_fst("data/output/tt_full_logsum_amr101_reisemitue.fst"))
setkey(tt_dt_amr_reisemitue, origin_zone, dest_region)

agents_tagesreise <- agents_noswiss[n_nights == 0]
agents_reisemitue <- agents_noswiss[n_nights > 0]

ATTR_VARS_AMR <- c("pop", "erreichbarkeit_avg_z")

res_tagesreise_amr <- run_destination_model_amr(
  nrow(agents_tagesreise), attr_vars = ATTR_VARS_AMR, seed = 42,
  model_name = "destination_mnl_amr_tagesreise_pop_erreichbarkeit_full",
  agent_pool = agents_tagesreise, logsum_dt = tt_dt_amr_tagesreise,
  interact_nationality = TRUE
)

res_reisemitue_amr <- run_destination_model_amr(
  nrow(agents_reisemitue), attr_vars = ATTR_VARS_AMR, seed = 42,
  model_name = "destination_mnl_amr_reise_mit_ue_pop_erreichbarkeit_full",
  agent_pool = agents_reisemitue, logsum_dt = tt_dt_amr_reisemitue,
  interact_nationality = TRUE
)

out_amr <- rbind(res_tagesreise_amr$estimates, res_reisemitue_amr$estimates)
fwrite(out_amr, "results_output/destination_mnl_amr_tagesreise_vs_reisemitue_pop_erreichbarkeit.csv")
cat("\nSaved combined results to results_output/destination_mnl_amr_tagesreise_vs_reisemitue_pop_erreichbarkeit.csv\n")
