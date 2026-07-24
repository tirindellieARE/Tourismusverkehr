# =============================================================================
# Mode choice model (car / PT / foot / bike) + logsum for domestic (CH -> CH)
# trips, pooled (not segmented by distance), with urban/rural topology
# interacted with travel time.
#
# Companion script: 03_estimate_logsum_Ausland_CH.R estimates the mode choice
# model for foreign-entry trips and computes logsum over tt_ausland_CH.fst.
#
# The mode choice model is estimated on REVEALED-PREFERENCE domestic trips
# from data/output/mc15.csv (origin_zone, dest_zone, mode), joined to the
# actual travel time, cost (car/PT only) and car distance for each mode from
# data/output/tt_CH_CH.fst (tt_miv/cost_car/dist_miv = car, tt_oev/cost_pt =
# PT, tt_fgv = foot, tt_velo = bike; built by 02_build_tt_lookups.R), plus the
# DESTINATION zone's urban/rural topology (STALAN2020, from
# data/input/zones_communes.gpkg: 1 = urban/flat (base), 2 = hilly,
# 3 = mountain -- same coding as the Ausland/CH destination choice models,
# which also attribute topology to the destination/candidate zone).
# Destination (not origin) topology is used here per request -- an origin-
# zone version was tried first (see README_trajets_CH_CH.md, Model 4); this
# run tests whether the built environment of the trip's END point (rather
# than its start) better explains time-sensitivity by mode.
#
# mc15's raw "walk" mode is relabelled "foot" to match the tt_fgv column; the
# small "other" category (183 / 8,730 rows) is dropped, since it isn't one of
# the four modelled alternatives. No cost/distance data for foot/bike. This
# is the same base specification as "Model 3" in README_trajets_CH_CH.md
# (car: time+cost+distance; PT: time+cost; foot/bike: time-only) -- distance-
# band segmentation is NOT used here (reverted per request); instead,
# topology x travel-time interactions are added to see whether urban/rural
# character explains some of what segmentation was addressing.
#
# Topology interaction: for each mode, travel time gets a base coefficient
# (the urban/topo==1 rate) plus two interaction shifts for hilly/mountain
# zones, e.g. for car:
#   b_tt_car + b_tt_car_topo2 * I(topo==2) + b_tt_car_topo3 * I(topo==3)
# 7 / 7,966 CH zones have no STALAN2020 value (see 02_build_tt_lookups.R /
# 04_destination_choice.R's notes on this) -- rows whose DESTINATION zone is
# one of these 7 get a NA topology, and so a NA utility/logsum for every
# alternative.
#
# No pooled/Tagesreise/Reise-mit-Ü. split here (unlike
# 03_estimate_logsum_Ausland_CH.R) -- domestic mode choice has no such
# distinction in mc15.
#
# The logsum (inclusive value / expected maximum utility)
#   logsum_od = ln( exp(V_car_od) + exp(V_pt_od) + exp(V_foot_od) + exp(V_bike_od) )
# is computed for every OD pair in data/output/tt_CH_CH.fst and saved to
# data/output/tt_CH_CH_logsum.fst (column `logsum`).
#
# Section 5 additionally estimates a NESTED logit version of the same
# specification -- nest 1 = {foot} (walkable-distance choice), nest 2 =
# {car, pt, bike} (which motorised/non-walking mode, given the trip isn't
# walkable) -- to relax MNL's IIA assumption between the three non-foot
# modes. Saved separately (mode_choice_model_ch_nl.rds,
# tt_CH_CH_logsum_nl.fst) alongside the flat MNL above, not in place of it.
#
# Section 6 estimates a second, alternate nested logit tree, per request --
# nest 1 = {foot, bike} ("active"/self-propelled modes), nest 2 = {car, pt}
# ("powered"/priced modes) -- saved separately again
# (mode_choice_model_ch_nl2.rds, tt_CH_CH_logsum_nl2.fst).
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(fst)
  library(apollo)
})

# -----------------------------------------------------------------------------
# 0. LOAD DATA
# -----------------------------------------------------------------------------

tt_ch_ch <- as.data.table(read_fst("data/output/tt_CH_CH.fst"))
setkey(tt_ch_ch, origin_zone, dest_zone)
cat(sprintf("Loaded data/output/tt_CH_CH.fst (%d rows)\n", nrow(tt_ch_ch)))

mc15 <- fread("data/output/mc15.csv")
cat(sprintf("Loaded data/output/mc15.csv (%d rows)\n", nrow(mc15)))

zones_sf <- st_read("data/input/zones_communes.gpkg", quiet = TRUE)
zone_topo <- as.data.table(st_drop_geometry(zones_sf))[, .(NO, topo = as.integer(STALAN2020))]
cat(sprintf("Loaded topology for %d zones (%d missing STALAN2020)\n\n", nrow(zone_topo), sum(is.na(zone_topo$topo))))

# Join destination-zone topology into tt_CH_CH once, up front, since both the
# estimation data and the full logsum computation need it.
tt_ch_ch[zone_topo, on = c(dest_zone = "NO"), topo := i.topo]

dir.create("results_output", showWarnings = FALSE)
invisible(capture.output(apollo_initialise()))

# -----------------------------------------------------------------------------
# 1. BUILD MODE-CHOICE ESTIMATION DATA
# -----------------------------------------------------------------------------
# Four alternatives: car (tt_miv, cost_car, dist_miv), pt (tt_oev, cost_pt),
# foot (tt_fgv), bike (tt_velo), each with topology x travel-time
# interactions. The chosen mode comes from mc15's own reported mode; "walk"
# -> "foot" to match the travel-time column, "other" is dropped (not one of
# the four modelled alternatives).

build_mode_data_ch <- function(mc15_dt, tt_dt) {
  md <- tt_dt[mc15_dt, on = c("origin_zone", "dest_zone")]

  md[, mode_chosen := fcase(
    mode == "car",  "car",
    mode == "pt",   "pt",
    mode == "walk", "foot",
    mode == "bike", "bike",
    default         = NA_character_
  )]

  n_before <- nrow(md)
  md <- md[!is.na(mode_chosen) & !is.na(tt_miv) & !is.na(tt_oev) & !is.na(tt_fgv) & !is.na(tt_velo) &
           !is.na(cost_car) & !is.na(cost_pt) & !is.na(dist_miv) & !is.na(topo)]
  n_dropped <- n_before - nrow(md)
  if (n_dropped > 0)
    cat(sprintf("WARNING: dropped %d/%d respondents with unknown mode or missing travel time/cost/topology\n", n_dropped, n_before))

  md[, choice := fcase(
    mode_chosen == "car",  1L,
    mode_chosen == "pt",   2L,
    mode_chosen == "foot", 3L,
    mode_chosen == "bike", 4L
  )]
  md[, `:=`(is_topo2 = as.integer(topo == 2), is_topo3 = as.integer(topo == 3))]
  md[, agent_id := .I]
  md[]
}

# -----------------------------------------------------------------------------
# 2. COMPUTE LOGSUM
# -----------------------------------------------------------------------------
# Numerically-stable log-sum-exp of the four mode utilities, using the
# estimated mode-choice coefficients, for every OD pair in tt_dt.

compute_logsum_ch <- function(tt_dt, model) {
  beta <- model$estimate
  is_topo2 <- as.integer(tt_dt$topo == 2)
  is_topo3 <- as.integer(tt_dt$topo == 3)

  V_car  <- beta["asc_car"]  + (beta["b_tt_car"]  + beta["b_tt_car_topo2"]  * is_topo2 + beta["b_tt_car_topo3"]  * is_topo3) * tt_dt$tt_miv  +
            beta["b_cost_car"] * tt_dt$cost_car + beta["b_dist_car"] * tt_dt$dist_miv
  V_pt   <- beta["asc_pt"]   + (beta["b_tt_pt"]   + beta["b_tt_pt_topo2"]   * is_topo2 + beta["b_tt_pt_topo3"]   * is_topo3) * tt_dt$tt_oev  +
            beta["b_cost_pt"] * tt_dt$cost_pt
  V_foot <-                    (beta["b_tt_foot"] + beta["b_tt_foot_topo2"] * is_topo2 + beta["b_tt_foot_topo3"] * is_topo3) * tt_dt$tt_fgv
  V_bike <- beta["asc_bike"] + (beta["b_tt_bike"] + beta["b_tt_bike_topo2"] * is_topo2 + beta["b_tt_bike_topo3"] * is_topo3) * tt_dt$tt_velo

  # A missing travel time OR missing topology means that mode's utility is
  # unavailable/excluded from the logsum (utility -Inf), not that the whole
  # OD pair is missing -- same convention as compute_logsum() in
  # 03_estimate_logsum_Ausland_CH.R. Since topology is per-destination (not
  # per-mode), a missing topo makes ALL four utilities NA at once (is_topo2/3
  # become NA, propagating through every V), which is the point: it isn't
  # meaningfully "some modes reachable" the way missing travel time is.
  V_car[is.na(V_car)]   <- -Inf
  V_pt[is.na(V_pt)]     <- -Inf
  V_foot[is.na(V_foot)] <- -Inf
  V_bike[is.na(V_bike)] <- -Inf

  v_max <- pmax(V_car, V_pt, V_foot, V_bike)
  logsum <- v_max + log(exp(V_car - v_max) + exp(V_pt - v_max) + exp(V_foot - v_max) + exp(V_bike - v_max))
  logsum[is.infinite(v_max)] <- NA_real_

  out <- copy(tt_dt)
  out[, logsum := logsum]
  out[]
}

# -----------------------------------------------------------------------------
# 2b. COMPUTE LOGSUM (NESTED LOGIT VERSION)
# -----------------------------------------------------------------------------
# Same utilities as compute_logsum_ch(), but combined via the two-level nested
# logit tree's own inclusive-value formula instead of a flat log-sum-exp:
#   root: {foot, moto}                 (root scale fixed at 1)
#   moto: {car, pt, bike}              (own scale/nesting parameter lambda_moto)
# IV_moto  = ln( exp(V_car/lambda_moto) + exp(V_pt/lambda_moto) + exp(V_bike/lambda_moto) )
# W_moto   = lambda_moto * IV_moto     (moto nest's utility as seen from the root)
# logsum   = ln( exp(V_foot) + exp(W_moto) )
# This is the standard nested-logit expected-maximum-utility (accessibility)
# measure for the whole tree -- reduces to the flat MNL log-sum-exp exactly
# when lambda_moto = 1. lambda_moto itself is recovered from the estimated
# raw_lambda_moto via the same logistic transform used in estimation (see
# estimate_mode_choice_ch_nl()'s note on why lambda is reparametrized).

compute_logsum_ch_nl <- function(tt_dt, model) {
  beta <- model$estimate
  is_topo2 <- as.integer(tt_dt$topo == 2)
  is_topo3 <- as.integer(tt_dt$topo == 3)
  lambda_moto <- 1 / (1 + exp(-beta["raw_lambda_moto"]))

  V_car  <- beta["asc_car"]  + (beta["b_tt_car"]  + beta["b_tt_car_topo2"]  * is_topo2 + beta["b_tt_car_topo3"]  * is_topo3) * tt_dt$tt_miv  +
            beta["b_cost_car"] * tt_dt$cost_car + beta["b_dist_car"] * tt_dt$dist_miv
  V_pt   <- beta["asc_pt"]   + (beta["b_tt_pt"]   + beta["b_tt_pt_topo2"]   * is_topo2 + beta["b_tt_pt_topo3"]   * is_topo3) * tt_dt$tt_oev  +
            beta["b_cost_pt"] * tt_dt$cost_pt
  V_foot <-                    (beta["b_tt_foot"] + beta["b_tt_foot_topo2"] * is_topo2 + beta["b_tt_foot_topo3"] * is_topo3) * tt_dt$tt_fgv
  V_bike <- beta["asc_bike"] + (beta["b_tt_bike"] + beta["b_tt_bike_topo2"] * is_topo2 + beta["b_tt_bike_topo3"] * is_topo3) * tt_dt$tt_velo

  V_car[is.na(V_car)]   <- -Inf
  V_pt[is.na(V_pt)]     <- -Inf
  V_foot[is.na(V_foot)] <- -Inf
  V_bike[is.na(V_bike)] <- -Inf

  # Moto nest {car, pt, bike}: numerically-stable log-sum-exp of V/lambda_moto.
  v_moto <- pmax(V_car, V_pt, V_bike) / lambda_moto
  iv_moto <- v_moto + log(exp(V_car / lambda_moto - v_moto) + exp(V_pt / lambda_moto - v_moto) + exp(V_bike / lambda_moto - v_moto))
  w_moto <- lambda_moto * iv_moto
  # If all three moto alternatives are -Inf, iv_moto/w_moto come out NaN
  # (pmax of three -Inf is -Inf, 0/lambda - (-Inf) is NaN) -- force -Inf so
  # the root sum below treats the whole nest as unavailable, not "missing".
  w_moto[is.na(w_moto)] <- -Inf

  v_max <- pmax(V_foot, w_moto)
  logsum <- v_max + log(exp(V_foot - v_max) + exp(w_moto - v_max))
  logsum[is.infinite(v_max)] <- NA_real_

  out <- copy(tt_dt)
  out[, logsum := logsum]
  out[]
}

# -----------------------------------------------------------------------------
# 2c. COMPUTE LOGSUM (NESTED LOGIT, ALTERNATE TREE: {foot,bike} vs {car,pt})
# -----------------------------------------------------------------------------
# Same utilities, different tree from compute_logsum_ch_nl():
#   root: {active, moto}     (root scale fixed at 1)
#   active: {foot, bike}     (own scale lambda_active -- "unpowered" modes)
#   moto:   {car, pt}        (own scale lambda_moto2 -- "powered" modes)
# Both nests now have 2 real alternatives (neither is a singleton), so both
# get their own nesting parameter, unlike the {foot} vs {car,pt,bike} tree
# where only the 3-alternative nest needed one.

compute_logsum_ch_nl2 <- function(tt_dt, model) {
  beta <- model$estimate
  is_topo2 <- as.integer(tt_dt$topo == 2)
  is_topo3 <- as.integer(tt_dt$topo == 3)
  lambda_active <- 1 / (1 + exp(-beta["raw_lambda_active"]))
  lambda_moto2  <- 1 / (1 + exp(-beta["raw_lambda_moto2"]))

  V_car  <- beta["asc_car"]  + (beta["b_tt_car"]  + beta["b_tt_car_topo2"]  * is_topo2 + beta["b_tt_car_topo3"]  * is_topo3) * tt_dt$tt_miv  +
            beta["b_cost_car"] * tt_dt$cost_car + beta["b_dist_car"] * tt_dt$dist_miv
  V_pt   <- beta["asc_pt"]   + (beta["b_tt_pt"]   + beta["b_tt_pt_topo2"]   * is_topo2 + beta["b_tt_pt_topo3"]   * is_topo3) * tt_dt$tt_oev  +
            beta["b_cost_pt"] * tt_dt$cost_pt
  V_foot <-                    (beta["b_tt_foot"] + beta["b_tt_foot_topo2"] * is_topo2 + beta["b_tt_foot_topo3"] * is_topo3) * tt_dt$tt_fgv
  V_bike <- beta["asc_bike"] + (beta["b_tt_bike"] + beta["b_tt_bike_topo2"] * is_topo2 + beta["b_tt_bike_topo3"] * is_topo3) * tt_dt$tt_velo

  V_car[is.na(V_car)]   <- -Inf
  V_pt[is.na(V_pt)]     <- -Inf
  V_foot[is.na(V_foot)] <- -Inf
  V_bike[is.na(V_bike)] <- -Inf

  # active nest {foot, bike}
  v_active <- pmax(V_foot, V_bike) / lambda_active
  iv_active <- v_active + log(exp(V_foot / lambda_active - v_active) + exp(V_bike / lambda_active - v_active))
  w_active <- lambda_active * iv_active
  w_active[is.na(w_active)] <- -Inf

  # moto nest {car, pt}
  v_moto <- pmax(V_car, V_pt) / lambda_moto2
  iv_moto <- v_moto + log(exp(V_car / lambda_moto2 - v_moto) + exp(V_pt / lambda_moto2 - v_moto))
  w_moto <- lambda_moto2 * iv_moto
  w_moto[is.na(w_moto)] <- -Inf

  v_max <- pmax(w_active, w_moto)
  logsum <- v_max + log(exp(w_active - v_max) + exp(w_moto - v_max))
  logsum[is.infinite(v_max)] <- NA_real_

  out <- copy(tt_dt)
  out[, logsum := logsum]
  out[]
}

# -----------------------------------------------------------------------------
# 3. ESTIMATE MNL MODE CHOICE MODEL (4 alternatives)
# -----------------------------------------------------------------------------
# apollo_validateInputs() is called with explicit apollo_beta/apollo_fixed/
# database/apollo_control arguments (rather than relying on its global-
# environment fallback), so this can safely live in a function.
#
# Reference alternative: foot (asc_foot fixed at 0 by simply not including it
# as a parameter) -- it's the most common mode in mc15 (65.5%), making it a
# natural, well-populated baseline. Topology reference category: urban/flat
# (topo == 1), same convention as the Ausland/CH destination choice models.
#
# The utility expressions are always the same static form (no runtime `if`
# branches) -- Apollo's analytical-gradient pre-processor cannot parse a
# conditional utility expression (this broke analytical differentiation and
# caused a saddle-point convergence in an earlier version of this script; see
# README_trajets_CH_CH.md).

estimate_mode_choice_ch <- function(mode_data, model_name) {
  database <- as.data.frame(mode_data[, .(agent_id, tt_miv, tt_oev, tt_fgv, tt_velo, cost_car, cost_pt,
                                           dist_miv, is_topo2, is_topo3, choice)])

  apollo_control <- list(
    modelName       = model_name,
    modelDescr      = sprintf("MNL mode choice (car/pt/foot/bike, cost + distance + topo x tt) -- %s", model_name),
    indivID         = "agent_id",
    outputDirectory = "results_output/"
  )

  apollo_beta <- c(asc_car = 0, asc_pt = 0, asc_bike = 0,
                    b_tt_car = 0, b_tt_pt = 0, b_tt_foot = 0, b_tt_bike = 0,
                    b_tt_car_topo2 = 0, b_tt_car_topo3 = 0,
                    b_tt_pt_topo2 = 0, b_tt_pt_topo3 = 0,
                    b_tt_foot_topo2 = 0, b_tt_foot_topo3 = 0,
                    b_tt_bike_topo2 = 0, b_tt_bike_topo3 = 0,
                    b_cost_car = 0, b_cost_pt = 0, b_dist_car = 0)
  apollo_fixed <- c()   # foot is the reference alternative (asc_foot fixed at 0)

  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))

    P <- list()
    V <- list(
      car  = asc_car  + (b_tt_car  + b_tt_car_topo2  * is_topo2 + b_tt_car_topo3  * is_topo3) * tt_miv  + b_cost_car * cost_car + b_dist_car * dist_miv,
      pt   = asc_pt   + (b_tt_pt   + b_tt_pt_topo2   * is_topo2 + b_tt_pt_topo3   * is_topo3) * tt_oev  + b_cost_pt  * cost_pt,
      foot =            (b_tt_foot + b_tt_foot_topo2 * is_topo2 + b_tt_foot_topo3 * is_topo3) * tt_fgv,
      bike = asc_bike + (b_tt_bike + b_tt_bike_topo2 * is_topo2 + b_tt_bike_topo3 * is_topo3) * tt_velo
    )

    mnl_settings <- list(
      alternatives = c(car = 1, pt = 2, foot = 3, bike = 4),
      avail        = list(car = 1, pt = 1, foot = 1, bike = 1),
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

# -----------------------------------------------------------------------------
# 3b. ESTIMATE NESTED LOGIT MODE CHOICE MODEL (2 nests)
# -----------------------------------------------------------------------------
# Same utility specification as estimate_mode_choice_ch() (same variables,
# same coefficients, same reference alternative/category), but combined via a
# two-level nested logit tree instead of a flat MNL, per request:
#   root
#   |-- foot                     (walkable-distance choice, its own singleton
#   |                              nest -- no separate scale parameter needed,
#   |                              it's degenerate with a single alternative)
#   `-- moto = {car, pt, bike}   (motorised/non-walkable choice: given the
#                                  trip isn't walkable, which of these three)
# lambda_moto is the moto nest's scale parameter (0 < lambda_moto <= 1 for RUM
# consistency; lambda_moto = 1 collapses the nest back to the flat MNL, so a
# nesting parameter significantly below 1 is evidence that car/pt/bike's
# unobserved utility components are correlated -- e.g. an agent's latent
# "willingness to consider anything but walking" -- in a way IIA can't
# capture, exactly the correlation this tree structure is meant to test for.
#
# lambda_moto is NOT estimated directly. A first attempt that estimated it as
# a free parameter (starting at 0.5, unconstrained) converged to 3.14 --
# outside the valid (0,1] range, a sign the optimizer was using it to rescale
# utilities rather than capture genuine within-nest correlation (the whole
# coefficient vector inflated by roughly the same factor). Apollo does not
# support bounded parameters directly, so lambda_moto is instead recovered
# from an unconstrained raw_lambda_moto via a logistic transform,
# lambda_moto = 1 / (1 + exp(-raw_lambda_moto)), which can never leave (0,1)
# regardless of what value the optimizer picks for raw_lambda_moto. This is a
# static, differentiable formula (not a runtime branch), so it doesn't break
# Apollo's analytical-gradient pre-processing -- see the note on
# apollo_probabilities' static-utility requirement above.

estimate_mode_choice_ch_nl <- function(mode_data, model_name) {
  database <- as.data.frame(mode_data[, .(agent_id, tt_miv, tt_oev, tt_fgv, tt_velo, cost_car, cost_pt,
                                           dist_miv, is_topo2, is_topo3, choice)])

  apollo_control <- list(
    modelName       = model_name,
    modelDescr      = sprintf("Nested logit mode choice (car/pt/foot/bike, cost + distance + topo x tt; nest = {car,pt,bike} vs. foot) -- %s", model_name),
    indivID         = "agent_id",
    outputDirectory = "results_output/"
  )

  apollo_beta <- c(asc_car = 0, asc_pt = 0, asc_bike = 0,
                    b_tt_car = 0, b_tt_pt = 0, b_tt_foot = 0, b_tt_bike = 0,
                    b_tt_car_topo2 = 0, b_tt_car_topo3 = 0,
                    b_tt_pt_topo2 = 0, b_tt_pt_topo3 = 0,
                    b_tt_foot_topo2 = 0, b_tt_foot_topo3 = 0,
                    b_tt_bike_topo2 = 0, b_tt_bike_topo3 = 0,
                    b_cost_car = 0, b_cost_pt = 0, b_dist_car = 0,
                    raw_lambda_moto = 0)   # logistic(0) = 0.5, a neutral starting nesting parameter
  apollo_fixed <- c()   # foot is the reference alternative (asc_foot fixed at 0); root scale fixed at 1 inside nlNests, not here

  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))

    P <- list()
    V <- list(
      car  = asc_car  + (b_tt_car  + b_tt_car_topo2  * is_topo2 + b_tt_car_topo3  * is_topo3) * tt_miv  + b_cost_car * cost_car + b_dist_car * dist_miv,
      pt   = asc_pt   + (b_tt_pt   + b_tt_pt_topo2   * is_topo2 + b_tt_pt_topo3   * is_topo3) * tt_oev  + b_cost_pt  * cost_pt,
      foot =            (b_tt_foot + b_tt_foot_topo2 * is_topo2 + b_tt_foot_topo3 * is_topo3) * tt_fgv,
      bike = asc_bike + (b_tt_bike + b_tt_bike_topo2 * is_topo2 + b_tt_bike_topo3 * is_topo3) * tt_velo
    )

    lambda_moto <- 1 / (1 + exp(-raw_lambda_moto))   # constrained to (0,1) -- see note above
    nlNests <- list(root = 1, moto = lambda_moto)
    nlStructure <- list()
    nlStructure[["root"]] <- c("foot", "moto")
    nlStructure[["moto"]] <- c("car", "pt", "bike")

    nl_settings <- list(
      alternatives = c(car = 1, pt = 2, foot = 3, bike = 4),
      avail        = list(car = 1, pt = 1, foot = 1, bike = 1),
      choiceVar    = choice,
      V            = V,
      nlNests      = nlNests,
      nlStructure  = nlStructure
    )

    P[["model"]] <- apollo_nl(nl_settings, functionality)
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

# -----------------------------------------------------------------------------
# 3c. ESTIMATE NESTED LOGIT MODE CHOICE MODEL, ALTERNATE TREE
#     ({foot, bike} vs {car, pt} -- "active" vs "powered/paid" modes)
# -----------------------------------------------------------------------------
# Per request: an alternative nesting hypothesis to estimate_mode_choice_ch_nl()
# -- instead of grouping by "walkable or not", group by "self-propelled and
# free" (foot, bike) vs "powered and priced" (car, pt), on the idea that
# foot/bike share unobserved determinants (e.g. fitness, weather sensitivity)
# distinct from car/pt's (e.g. schedule/cost sensitivity, licence ownership):
#   root
#   |-- active = {foot, bike}
#   `-- moto   = {car, pt}
# Both nests have 2 real alternatives, so BOTH need their own nesting
# parameter (unlike the {foot} vs {car,pt,bike} tree, where foot was a
# singleton and needed none). Both are reparametrized via the same logistic
# transform as lambda_moto above, for the same reason (an unconstrained
# nesting parameter can converge outside the valid (0,1) range with only a
# post-hoc warning, and inflates the whole coefficient vector when it does).

estimate_mode_choice_ch_nl2 <- function(mode_data, model_name) {
  database <- as.data.frame(mode_data[, .(agent_id, tt_miv, tt_oev, tt_fgv, tt_velo, cost_car, cost_pt,
                                           dist_miv, is_topo2, is_topo3, choice)])

  apollo_control <- list(
    modelName       = model_name,
    modelDescr      = sprintf("Nested logit mode choice (car/pt/foot/bike, cost + distance + topo x tt; nest = {foot,bike} vs. {car,pt}) -- %s", model_name),
    indivID         = "agent_id",
    outputDirectory = "results_output/"
  )

  apollo_beta <- c(asc_car = 0, asc_pt = 0, asc_bike = 0,
                    b_tt_car = 0, b_tt_pt = 0, b_tt_foot = 0, b_tt_bike = 0,
                    b_tt_car_topo2 = 0, b_tt_car_topo3 = 0,
                    b_tt_pt_topo2 = 0, b_tt_pt_topo3 = 0,
                    b_tt_foot_topo2 = 0, b_tt_foot_topo3 = 0,
                    b_tt_bike_topo2 = 0, b_tt_bike_topo3 = 0,
                    b_cost_car = 0, b_cost_pt = 0, b_dist_car = 0,
                    raw_lambda_active = 0, raw_lambda_moto2 = 0)   # logistic(0) = 0.5 each
  apollo_fixed <- c()   # foot is the reference alternative (asc_foot fixed at 0); root scale fixed at 1 inside nlNests, not here

  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))

    P <- list()
    V <- list(
      car  = asc_car  + (b_tt_car  + b_tt_car_topo2  * is_topo2 + b_tt_car_topo3  * is_topo3) * tt_miv  + b_cost_car * cost_car + b_dist_car * dist_miv,
      pt   = asc_pt   + (b_tt_pt   + b_tt_pt_topo2   * is_topo2 + b_tt_pt_topo3   * is_topo3) * tt_oev  + b_cost_pt  * cost_pt,
      foot =            (b_tt_foot + b_tt_foot_topo2 * is_topo2 + b_tt_foot_topo3 * is_topo3) * tt_fgv,
      bike = asc_bike + (b_tt_bike + b_tt_bike_topo2 * is_topo2 + b_tt_bike_topo3 * is_topo3) * tt_velo
    )

    lambda_active <- 1 / (1 + exp(-raw_lambda_active))   # constrained to (0,1)
    lambda_moto2  <- 1 / (1 + exp(-raw_lambda_moto2))    # constrained to (0,1)
    nlNests <- list(root = 1, active = lambda_active, moto = lambda_moto2)
    nlStructure <- list()
    nlStructure[["root"]]   <- c("active", "moto")
    nlStructure[["active"]] <- c("foot", "bike")
    nlStructure[["moto"]]   <- c("car", "pt")

    nl_settings <- list(
      alternatives = c(car = 1, pt = 2, foot = 3, bike = 4),
      avail        = list(car = 1, pt = 1, foot = 1, bike = 1),
      choiceVar    = choice,
      V            = V,
      nlNests      = nlNests,
      nlStructure  = nlStructure
    )

    P[["model"]] <- apollo_nl(nl_settings, functionality)
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

# -----------------------------------------------------------------------------
# 4. ESTIMATE, THEN SAVE MODEL + LOGSUM (pooled, no distance-band split)
# -----------------------------------------------------------------------------

mode_data <- build_mode_data_ch(mc15, tt_ch_ch)
cat(sprintf("Estimation sample: %d trips (%.1f%% car, %.1f%% pt, %.1f%% foot, %.1f%% bike)\n",
    nrow(mode_data),
    100 * mean(mode_data$choice == 1), 100 * mean(mode_data$choice == 2),
    100 * mean(mode_data$choice == 3), 100 * mean(mode_data$choice == 4)))
print(mode_data[, .N, by = topo][order(topo)])
cat("\n")

model <- estimate_mode_choice_ch(mode_data, "mode_choice_ch")
saveRDS(model, "results_output/mode_choice_model_ch.rds")

tt_ch_ch_logsum <- compute_logsum_ch(tt_ch_ch, model)
write_fst(tt_ch_ch_logsum, "data/output/tt_CH_CH_logsum.fst", compress = 100)

n_na_logsum <- sum(is.na(tt_ch_ch_logsum$logsum))
cat(sprintf("logsum -> data/output/tt_CH_CH_logsum.fst (range [%.3f, %.3f], mean %.3f, %d/%d NA -- missing topology or all four modes unreachable)\n",
    min(tt_ch_ch_logsum$logsum, na.rm = TRUE), max(tt_ch_ch_logsum$logsum, na.rm = TRUE),
    mean(tt_ch_ch_logsum$logsum, na.rm = TRUE), n_na_logsum, nrow(tt_ch_ch_logsum)))

cat("\nDone. Model saved to results_output/mode_choice_model_ch.rds\n")
cat("Logsum saved to data/output/tt_CH_CH_logsum.fst (pooled, topology x travel-time interactions, no distance-band split)\n")

# -----------------------------------------------------------------------------
# 5. ESTIMATE, THEN SAVE NESTED LOGIT MODEL + LOGSUM
# -----------------------------------------------------------------------------
# Saved to separate files from the MNL model above (does not overwrite it),
# since this is a distinct model to compare against, not a replacement.

cat("\n\n===== Nested logit model =====\n\n")

model_nl <- estimate_mode_choice_ch_nl(mode_data, "mode_choice_ch_nl")
saveRDS(model_nl, "results_output/mode_choice_model_ch_nl.rds")

lr_stat <- 2 * (model_nl$maximum - model$maximum)
lr_p    <- pchisq(lr_stat, df = 1, lower.tail = FALSE)
cat(sprintf("\nLR test vs. flat MNL: LR = %.4f (df=1), p = %.4g -- %s\n",
    lr_stat, lr_p, ifelse(lr_p < 0.05, "nesting is a significant improvement", "nesting is NOT a significant improvement")))

tt_ch_ch_logsum_nl <- compute_logsum_ch_nl(tt_ch_ch, model_nl)
write_fst(tt_ch_ch_logsum_nl, "data/output/tt_CH_CH_logsum_nl.fst", compress = 100)

n_na_logsum_nl <- sum(is.na(tt_ch_ch_logsum_nl$logsum))
cat(sprintf("logsum -> data/output/tt_CH_CH_logsum_nl.fst (range [%.3f, %.3f], mean %.3f, %d/%d NA -- missing topology or all four modes unreachable)\n",
    min(tt_ch_ch_logsum_nl$logsum, na.rm = TRUE), max(tt_ch_ch_logsum_nl$logsum, na.rm = TRUE),
    mean(tt_ch_ch_logsum_nl$logsum, na.rm = TRUE), n_na_logsum_nl, nrow(tt_ch_ch_logsum_nl)))

cat("\nDone. Model saved to results_output/mode_choice_model_ch_nl.rds\n")
cat("Logsum saved to data/output/tt_CH_CH_logsum_nl.fst (pooled, nested logit: {foot} vs. {car,pt,bike})\n")

# -----------------------------------------------------------------------------
# 6. ESTIMATE, THEN SAVE ALTERNATE NESTED LOGIT MODEL + LOGSUM
#    ({foot, bike} vs {car, pt})
# -----------------------------------------------------------------------------
# Saved to its own separate files -- three models now coexist on disk: flat
# MNL, nested {foot} vs {car,pt,bike}, nested {foot,bike} vs {car,pt}.

cat("\n\n===== Nested logit model (alternate tree: {foot,bike} vs {car,pt}) =====\n\n")

model_nl2 <- estimate_mode_choice_ch_nl2(mode_data, "mode_choice_ch_nl2")
saveRDS(model_nl2, "results_output/mode_choice_model_ch_nl2.rds")

lr_stat2 <- 2 * (model_nl2$maximum - model$maximum)
lr_p2    <- pchisq(lr_stat2, df = 2, lower.tail = FALSE)   # 2 extra params vs. flat MNL (lambda_active, lambda_moto2)
cat(sprintf("\nLR test vs. flat MNL: LR = %.4f (df=2), p = %.4g -- %s\n",
    lr_stat2, lr_p2, ifelse(lr_p2 < 0.05, "nesting is a significant improvement", "nesting is NOT a significant improvement")))

tt_ch_ch_logsum_nl2 <- compute_logsum_ch_nl2(tt_ch_ch, model_nl2)
write_fst(tt_ch_ch_logsum_nl2, "data/output/tt_CH_CH_logsum_nl2.fst", compress = 100)

n_na_logsum_nl2 <- sum(is.na(tt_ch_ch_logsum_nl2$logsum))
cat(sprintf("logsum -> data/output/tt_CH_CH_logsum_nl2.fst (range [%.3f, %.3f], mean %.3f, %d/%d NA -- missing topology or all four modes unreachable)\n",
    min(tt_ch_ch_logsum_nl2$logsum, na.rm = TRUE), max(tt_ch_ch_logsum_nl2$logsum, na.rm = TRUE),
    mean(tt_ch_ch_logsum_nl2$logsum, na.rm = TRUE), n_na_logsum_nl2, nrow(tt_ch_ch_logsum_nl2)))

cat("\nDone. Model saved to results_output/mode_choice_model_ch_nl2.rds\n")
cat("Logsum saved to data/output/tt_CH_CH_logsum_nl2.fst (pooled, nested logit: {foot,bike} vs. {car,pt})\n")

# -----------------------------------------------------------------------------
# 7. POOLED MODEL, LOG + QUADRATIC TRAVEL TIME FOR ALL FOUR MODES
# -----------------------------------------------------------------------------
# Per request ("try log travel time and squared for all four modes in the
# pooled model"): extends 03e_carfoot_nonlinear_CH.R's car/foot finding --
# log(tt+1) alone fixed car's "wrong-sign" confound in a binary car/foot
# model -- to the full 4-alternative pooled model, combining BOTH a log term
# AND a quadratic (hours-scaled) term for every mode's travel time:
#   V_mode = ... + b_tt_mode_log * log(tt_mode + 1) + b_tt_mode_sq * (tt_mode/60)^2 + ...
# Both terms together give a flexible functional form: the log term captures
# diminishing marginal disutility at low/moderate travel times (the effect
# that fixed the sign confound for car/foot alone), the quadratic term
# additionally allows curvature/acceleration at the high end.
#
# Topology x travel-time interactions (Model 4a/4b) are NOT included here --
# combining them with two travel-time terms per mode (instead of one) would
# double the interaction terms (4 shifts per mode instead of 2), and the
# point of this run is to isolate the non-linear time functional form itself
# across all four modes, matching the scope of 03e_carfoot_nonlinear_CH.R.
# Cost (car/PT) and car network distance are kept, matching Model 3/4b's
# base spec.
#
# Own estimation sample is built without a topology filter (topology isn't
# used here), so it retains slightly more rows than the topology-based
# models above (mode/tt/cost/dist completeness only).
#
# tt_mode/60 (hours) is used for the SQUARED term specifically -- squaring
# tt_fgv (up to ~2,113 minutes for some trips) in raw minutes overflowed the
# utility scale in 03e_carfoot_nonlinear_CH.R's quadratic model; hours keeps
# every squared term comfortably scaled. The log term uses raw minutes
# (log(2113+1) = 7.66, no overflow risk).

cat("\n\n===== Pooled model, log + quadratic travel time (all 4 modes) =====\n\n")

build_mode_data_ch_nonlinear <- function(mc15_dt, tt_dt) {
  md <- tt_dt[mc15_dt, on = c("origin_zone", "dest_zone")]

  md[, mode_chosen := fcase(
    mode == "car",  "car",
    mode == "pt",   "pt",
    mode == "walk", "foot",
    mode == "bike", "bike",
    default         = NA_character_
  )]

  n_before <- nrow(md)
  md <- md[!is.na(mode_chosen) & !is.na(tt_miv) & !is.na(tt_oev) & !is.na(tt_fgv) & !is.na(tt_velo) &
           !is.na(cost_car) & !is.na(cost_pt) & !is.na(dist_miv)]
  n_dropped <- n_before - nrow(md)
  if (n_dropped > 0)
    cat(sprintf("WARNING: dropped %d/%d respondents with unknown mode or missing travel time/cost\n", n_dropped, n_before))

  md[, choice := fcase(
    mode_chosen == "car",  1L,
    mode_chosen == "pt",   2L,
    mode_chosen == "foot", 3L,
    mode_chosen == "bike", 4L
  )]
  md[, `:=`(log_tt_miv  = log(tt_miv + 1),  tt_miv_h_sq  = (tt_miv / 60)^2,
            log_tt_oev  = log(tt_oev + 1),  tt_oev_h_sq  = (tt_oev / 60)^2,
            log_tt_fgv  = log(tt_fgv + 1),  tt_fgv_h_sq  = (tt_fgv / 60)^2,
            log_tt_velo = log(tt_velo + 1), tt_velo_h_sq = (tt_velo / 60)^2)]
  md[, agent_id := .I]
  md[]
}

mode_data_nl <- build_mode_data_ch_nonlinear(mc15, tt_ch_ch)
cat(sprintf("Estimation sample: %d trips (%.1f%% car, %.1f%% pt, %.1f%% foot, %.1f%% bike)\n\n",
    nrow(mode_data_nl),
    100 * mean(mode_data_nl$choice == 1), 100 * mean(mode_data_nl$choice == 2),
    100 * mean(mode_data_nl$choice == 3), 100 * mean(mode_data_nl$choice == 4)))

estimate_mode_choice_ch_nonlinear <- function(mode_data, model_name) {
  database <- as.data.frame(mode_data[, .(agent_id, log_tt_miv, tt_miv_h_sq, log_tt_oev, tt_oev_h_sq,
                                           log_tt_fgv, tt_fgv_h_sq, log_tt_velo, tt_velo_h_sq,
                                           cost_car, cost_pt, dist_miv, choice)])

  apollo_control <- list(
    modelName       = model_name,
    modelDescr      = sprintf("MNL mode choice (car/pt/foot/bike, cost + distance + log+quadratic tt) -- %s", model_name),
    indivID         = "agent_id",
    outputDirectory = "results_output/"
  )

  apollo_beta <- c(asc_car = 0, asc_pt = 0, asc_bike = 0,
                    b_tt_car_log = 0, b_tt_car_sq = 0,
                    b_tt_pt_log = 0, b_tt_pt_sq = 0,
                    b_tt_foot_log = 0, b_tt_foot_sq = 0,
                    b_tt_bike_log = 0, b_tt_bike_sq = 0,
                    b_cost_car = 0, b_cost_pt = 0, b_dist_car = 0)
  apollo_fixed <- c()   # foot is the reference alternative (asc_foot fixed at 0)

  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))

    P <- list()
    V <- list(
      car  = asc_car  + b_tt_car_log  * log_tt_miv  + b_tt_car_sq  * tt_miv_h_sq  + b_cost_car * cost_car + b_dist_car * dist_miv,
      pt   = asc_pt   + b_tt_pt_log   * log_tt_oev  + b_tt_pt_sq   * tt_oev_h_sq  + b_cost_pt  * cost_pt,
      foot =            b_tt_foot_log * log_tt_fgv  + b_tt_foot_sq * tt_fgv_h_sq,
      bike = asc_bike + b_tt_bike_log * log_tt_velo + b_tt_bike_sq * tt_velo_h_sq
    )

    mnl_settings <- list(
      alternatives = c(car = 1, pt = 2, foot = 3, bike = 4),
      avail        = list(car = 1, pt = 1, foot = 1, bike = 1),
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

model_nonlinear <- estimate_mode_choice_ch_nonlinear(mode_data_nl, "mode_choice_ch_nonlinear")
saveRDS(model_nonlinear, "results_output/mode_choice_model_ch_nonlinear.rds")

compute_logsum_ch_nonlinear <- function(tt_dt, model) {
  beta <- model$estimate

  log_tt_miv  <- log(tt_dt$tt_miv + 1);  tt_miv_h_sq  <- (tt_dt$tt_miv / 60)^2
  log_tt_oev  <- log(tt_dt$tt_oev + 1);  tt_oev_h_sq  <- (tt_dt$tt_oev / 60)^2
  log_tt_fgv  <- log(tt_dt$tt_fgv + 1);  tt_fgv_h_sq  <- (tt_dt$tt_fgv / 60)^2
  log_tt_velo <- log(tt_dt$tt_velo + 1); tt_velo_h_sq <- (tt_dt$tt_velo / 60)^2

  V_car  <- beta["asc_car"]  + beta["b_tt_car_log"]  * log_tt_miv  + beta["b_tt_car_sq"]  * tt_miv_h_sq  +
            beta["b_cost_car"] * tt_dt$cost_car + beta["b_dist_car"] * tt_dt$dist_miv
  V_pt   <- beta["asc_pt"]   + beta["b_tt_pt_log"]   * log_tt_oev  + beta["b_tt_pt_sq"]   * tt_oev_h_sq  +
            beta["b_cost_pt"] * tt_dt$cost_pt
  V_foot <-                    beta["b_tt_foot_log"] * log_tt_fgv  + beta["b_tt_foot_sq"] * tt_fgv_h_sq
  V_bike <- beta["asc_bike"] + beta["b_tt_bike_log"] * log_tt_velo + beta["b_tt_bike_sq"]  * tt_velo_h_sq

  V_car[is.na(V_car)]   <- -Inf
  V_pt[is.na(V_pt)]     <- -Inf
  V_foot[is.na(V_foot)] <- -Inf
  V_bike[is.na(V_bike)] <- -Inf

  v_max <- pmax(V_car, V_pt, V_foot, V_bike)
  logsum <- v_max + log(exp(V_car - v_max) + exp(V_pt - v_max) + exp(V_foot - v_max) + exp(V_bike - v_max))
  logsum[is.infinite(v_max)] <- NA_real_

  out <- copy(tt_dt)
  out[, logsum := logsum]
  out[]
}

tt_ch_ch_logsum_nonlinear <- compute_logsum_ch_nonlinear(tt_ch_ch, model_nonlinear)
write_fst(tt_ch_ch_logsum_nonlinear, "data/output/tt_CH_CH_logsum_nonlinear.fst", compress = 100)

n_na_logsum_nonlinear <- sum(is.na(tt_ch_ch_logsum_nonlinear$logsum))
cat(sprintf("logsum -> data/output/tt_CH_CH_logsum_nonlinear.fst (range [%.3f, %.3f], mean %.3f, %d/%d NA -- all four modes unreachable)\n",
    min(tt_ch_ch_logsum_nonlinear$logsum, na.rm = TRUE), max(tt_ch_ch_logsum_nonlinear$logsum, na.rm = TRUE),
    mean(tt_ch_ch_logsum_nonlinear$logsum, na.rm = TRUE), n_na_logsum_nonlinear, nrow(tt_ch_ch_logsum_nonlinear)))

# Model 4b (linear tt + topology interactions, 18 params) and this model
# (log+quadratic tt, no topology, 14 params) are NOT nested -- neither
# functional form is a restricted special case of the other -- so a
# chi-square LR test does not apply here (it would need one model's
# parameter space to contain the other's). AIC/BIC (which don't require
# nesting) are the correct comparison instead.
aic_4b   <- -2 * model$maximum + 2 * length(model$estimate)
bic_4b   <- -2 * model$maximum + log(nrow(mode_data)) * length(model$estimate)
aic_nl4  <- -2 * model_nonlinear$maximum + 2 * length(model_nonlinear$estimate)
bic_nl4  <- -2 * model_nonlinear$maximum + log(nrow(mode_data_nl)) * length(model_nonlinear$estimate)
cat(sprintf("\nComparison vs. Model 4b (linear tt + topology, NOT nested -- AIC/BIC only, no LR test):\n"))
cat(sprintf("  Model 4b (linear tt + topo x tt): LL=%.2f, npar=%d, N=%d, AIC=%.1f, BIC=%.1f\n",
    model$maximum, length(model$estimate), nrow(mode_data), aic_4b, bic_4b))
cat(sprintf("  Log+quadratic tt (no topo):       LL=%.2f, npar=%d, N=%d, AIC=%.1f, BIC=%.1f\n",
    model_nonlinear$maximum, length(model_nonlinear$estimate), nrow(mode_data_nl), aic_nl4, bic_nl4))

cat("\nDone. Model saved to results_output/mode_choice_model_ch_nonlinear.rds\n")
cat("Logsum saved to data/output/tt_CH_CH_logsum_nonlinear.fst (pooled, log+quadratic travel time, all 4 modes, no topology interaction)\n")

cat("\nDone. Model saved to results_output/mode_choice_model_ch_nl2.rds\n")
cat("Logsum saved to data/output/tt_CH_CH_logsum_nl2.fst (pooled, nested logit: {foot,bike} vs. {car,pt})\n")
