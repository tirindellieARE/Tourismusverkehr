# =============================================================================
# SIMBA MOBi Simulation — PART 2 of 5
# Scherr et al. (2020), EJTIR 20(4), pp. 152-172
#
# Covers:
#   6. Destination & mode choice — Nested Logit (Equations 1-5, Section 2.4)
#      Mode chosen at TOUR level (one mode per tour, consistent across trips)
#
# INPUTS:  part1_output.RData
# OUTPUTS: part2_output.RData
#   - agent_plans_p2 : list with tours, destinations and modes per agent
#
# Run next: part3_durations.R
# =============================================================================

set.seed(42)

# =============================================================================
# CORRECTIONS APPLIED IN THIS VERSION
# =============================================================================
# The following deviations from the paper have been CORRECTED:
#
# [CORRECTED A4] Rubber banding (Eq. 4, Section 2.4): choose_destination() now
#   accepts a primary_zone argument. When supplied for stop activities,
#   destination utility is a weighted combination of EMU from the trip origin (i)
#   and EMU from the primary location (k) — matching Eq. 4. build_one_tour()
#   passes the primary zone for all non-primary stops.
#
# [CORRECTED A5] Mode utility (Table 2): mode_utility() now includes all 7
#   variable types: constant, travel time, parking search time (walk/car),
#   service frequency (pt), number of transfers (pt), parking cost (car),
#   and distance (pt/car). Mock data are generated for the additional variables.
#   Beta coefficients remain invented/directionally plausible (not from data).
#
# [CORRECTED A6] Activity-type-specific zone attractions (Section 2.4):
#   zone_attr_by_type provides separate attraction vectors per activity type
#   (Work, Education, Shopping, Leisure, Business, Accompany, Other).
#   choose_destination() selects the appropriate vector via get_zone_attr().
#
# [BUG FIX] n_trips corrected from length(activities)*2 to
#   length(activities) + n_tours. See inline comment.
#
# [CHANGE 1] Subtour-specific mode choice (Section 2.4): subtour activities
#   now receive their own mode, constrained by the main tour mode:
#     car_driver    -> car_driver (small preference bonus) or walk
#     car_passenger -> pt or walk
#     pt            -> pt or walk
#     bicycle       -> bicycle or walk
#     walk          -> walk only
#   choose_subtour_mode() implements the constrained MNL. build_one_tour()
#   returns a per-activity modes vector; the calling loop in the main run
#   section uses tr$modes instead of replicating a single tr$mode.
#   subtour_idx (from Part 1 tour structures) identifies the subtour position.
#
# [CHANGE 2] Rubber banding always applied for intermediate stops (Eq. 4):
#   Previously choose_destination() skipped rubber banding when
#   origin == primary_zone. The condition primary_zone != origin has been
#   removed so Eq. 4 is applied whenever primary_zone is supplied, regardless
#   of whether the trip origin and primary location coincide.
# =============================================================================
message("[Part 2] Corrections A4 (rubber banding), A5 (full Table 2 mode utility),",
        " and A6 (activity-type attractions) are now implemented.")
message("[Part 2] CHANGE 1: subtour-specific mode choice constrained by main tour mode.")
message("[Part 2] CHANGE 2: rubber banding (Eq. 4) always applied when primary_zone",
        " is supplied, even when origin == primary_zone.")

# Load Part 1 outputs
load("part1_output.RData")
cat("Part 1 data loaded.\n\n")

# =============================================================================
# NETWORK: Travel time matrices and zone attractions
#
# In the real SIMBA MOBi model these come from the MATSim/Visum network.
# Here we use random mock data as a substitute.
# =============================================================================

N_ZONES <- 20

gen_tt_matrix <- function(n_zones) {
  d <- matrix(runif(n_zones^2, 5, 90), n_zones, n_zones)
  diag(d) <- 0
  list(
    walk     = pmin(d * 3.0, 120),
    bicycle  = pmin(d * 0.8,  90),
    pt       = pmin(d * 1.2,  80),
    car      = pmin(d * 0.7,  70),
    car_pass = pmin(d * 0.7,  70)
  )
}

tt        <- gen_tt_matrix(N_ZONES)
zone_attr <- runif(N_ZONES, 500, 12000)   # kept for backward compatibility

# =============================================================================
# [CORRECTED A5] Additional level-of-service measures (Table 2)
# All mock data — in the real model these come from the MATSim/Visum network.
# =============================================================================

# Parking search time matrix (minutes) — applies to walk, car_driver, car_passenger
parking_search_time <- {
  m <- matrix(runif(N_ZONES^2, 0, 15), N_ZONES, N_ZONES)
  diag(m) <- 0
  m
}

# Service frequency at destination zone (departures per hour) — pt only
service_freq <- runif(N_ZONES, 2, 12)

# Number of transfers — pt only; proxy: longer OD pairs have more transfers
n_transfers <- {
  m <- matrix(pmax(0L, round(runif(N_ZONES^2, -0.5, 3.5))), N_ZONES, N_ZONES)
  diag(m) <- 0L
  m
}

# Parking cost at destination (CHF/h) — car_driver, car_passenger
parking_cost <- runif(N_ZONES, 0, 5)

# Distance proxy (km): car travel time in minutes ~ km at 60 km/h
distance_km <- tt$car   # 1 min ≈ 1 km; internally consistent with mock data

# =============================================================================
# [CORRECTED A6] Activity-type-specific zone attractions (Section 2.4)
# The paper estimates separate destination probability matrices per activity
# type and person group. Here we use separate random attraction vectors as a
# structural proxy. Calibration from data is NOT performed.
# =============================================================================

zone_attr_by_type <- list(
  Work      = runif(N_ZONES,  500, 12000),  # proportional to number of jobs
  Education = runif(N_ZONES,  100,  5000),  # school/university capacity proxy
  Shopping  = runif(N_ZONES,  200,  8000),  # retail floor space proxy
  Leisure   = runif(N_ZONES,  100,  6000),  # leisure facility count proxy
  Business  = runif(N_ZONES,  500, 10000),  # co-located with employment
  Accompany = runif(N_ZONES,  300,  5000),
  Other     = runif(N_ZONES,  200,  6000)
)

get_zone_attr <- function(activity_type) {
  if (activity_type %in% names(zone_attr_by_type))
    zone_attr_by_type[[activity_type]]
  else
    zone_attr_by_type[["Other"]]
}

# =============================================================================
# MODE UTILITY (Equation 1, Table 2)
# Parameters are trip-based as stated in Section 2.4.
# All beta coefficients are invented (directionally plausible) — not estimated.
# =============================================================================

# [CORRECTED A5]: Now includes all 7 variable types from Table 2.
# Column coverage matches Table 2 exactly:
#   walk          : constant, travel time, parking search time
#   bicycle       : constant, travel time
#   pt            : constant, travel time, service frequency, transfers, distance
#   car_driver    : constant, travel time, parking search time, parking cost, distance
#   car_passenger : constant, travel time, parking search time, parking cost, distance
mode_utility <- function(origin, dest, agent, tt_list) {

  # --- Alternative-specific constants (Table 2, "const.") ---
  asc <- c(walk = -1.50, bicycle = -0.80, pt =  0.00,
           car_driver =  0.50, car_passenger = -0.50)

  # --- Travel time: all modes (Table 2) ---
  beta_tt <- c(walk = -0.060, bicycle = -0.050, pt = -0.040,
               car_driver = -0.030, car_passenger = -0.030)

  # --- Parking search time: walk, car_driver, car_passenger (Table 2) ---
  beta_park_search <- c(walk = -0.040, bicycle =  0.000, pt =  0.000,
                        car_driver = -0.020, car_passenger = -0.020)

  # --- Service frequency (arrivals/h): pt only (Table 2) ---
  beta_freq <- c(walk =  0.000, bicycle =  0.000, pt =  0.080,
                 car_driver =  0.000, car_passenger =  0.000)

  # --- Number of transfers: pt only (Table 2) ---
  beta_trans <- c(walk =  0.000, bicycle =  0.000, pt = -0.200,
                  car_driver =  0.000, car_passenger =  0.000)

  # --- Parking cost (CHF/h): car_driver, car_passenger (Table 2) ---
  beta_park_cost <- c(walk =  0.000, bicycle =  0.000, pt =  0.000,
                      car_driver = -0.150, car_passenger = -0.100)

  # --- Distance (km): pt, car_driver, car_passenger (Table 2) ---
  beta_dist <- c(walk =  0.000, bicycle =  0.000, pt = -0.010,
                 car_driver = -0.005, car_passenger = -0.005)

  # --- Variable vectors (one element per mode, in asc order) ---
  tt_vec    <- c(tt_list$walk[origin, dest], tt_list$bicycle[origin, dest],
                 tt_list$pt[origin, dest],   tt_list$car[origin, dest],
                 tt_list$car_pass[origin, dest])

  ps_vec    <- c(parking_search_time[origin, dest], 0, 0,
                 parking_search_time[origin, dest],
                 parking_search_time[origin, dest])

  freq_vec  <- c(0, 0, service_freq[dest], 0, 0)
  trans_vec <- c(0, 0, n_transfers[origin, dest], 0, 0)
  cost_vec  <- c(0, 0, 0, parking_cost[dest], parking_cost[dest])
  dist_vec  <- c(0, 0, distance_km[origin, dest],
                 distance_km[origin, dest], distance_km[origin, dest])

  u <- asc              +
       beta_tt          * tt_vec    +
       beta_park_search * ps_vec    +
       beta_freq        * freq_vec  +
       beta_trans       * trans_vec +
       beta_park_cost   * cost_vec  +
       beta_dist        * dist_vec

  # Constraint: no car driver if no car available
  if (agent$car_avail == 0) u["car_driver"] <- -Inf

  # PT bonus by subscription type (General-Abo holders use PT much more)
  pt_bonus <- switch(agent$pt_sub,
    general = 1.5, verbund = 0.8, halbtax = 0.3, none = 0.0
  )
  u["pt"] <- u["pt"] + pt_bonus
  u
}

# =============================================================================
# LOGSUM / EXPECTED MAXIMUM UTILITY (Equation 2)
# Feeds into destination choice to capture mode availability
# =============================================================================

compute_logsum <- function(utils, theta = 1) {
  valid <- utils[is.finite(utils)]
  mx <- max(valid/theta)
  theta * (log(sum(exp(valid/theta - mx))) + mx)
}

# =============================================================================
# DESTINATION CHOICE (Equations 3–5, Section 2.4)
#
# [CORRECTED A4] Rubber banding (Eq. 4) is now implemented via the
#   primary_zone argument. When primary_zone is supplied (intermediate stops),
#   the destination utility is a weighted combination:
#     V(j|i) = ln(Aj) + alpha * theta * EMU_ij + beta * theta * EMU_kj
#   where i = trip origin, k = primary location.
#   When primary_zone is NULL (home-based or unconstrained choice), Eq. 3 is
#   used: V(j|i) = ln(Aj) + theta * EMU_ij.
#
# [CORRECTED A6] Activity-type-specific attractions via get_zone_attr().
#
# Shadow prices (lambda_j, lambda_ij) are still omitted — they require
# iterative calibration against empirical OD data not available here.
# =============================================================================

choose_destination <- function(origin, agent, tt_list,
                               activity_type = "Other",
                               primary_zone  = NULL,   # k in Eq. 4
                               alpha = 0.7,            # weight of trip origin (i)
                               beta  = 0.3,            # weight of primary location (k)
                               theta = 0.6,            # logsum scale (Eq. 3)
                               n_zones = N_ZONES) {
  attr_vec <- get_zone_attr(activity_type)  # [CORRECTED A6]

  dest_utils <- sapply(1:n_zones, function(j) {
    if (j == origin) return(-Inf)

    mu_ij  <- mode_utility(origin, j, agent, tt_list)
    emu_ij <- compute_logsum(mu_ij)

    # [CHANGE 2] Rubber banding applied whenever primary_zone is supplied.
    # The previous condition excluded origin == primary_zone; that guard has
    # been removed so Eq. 4 is always used for stop activities.
    # (When origin == primary_zone the two EMU terms are equal and the formula
    # reduces to Eq. 3, so results are mathematically equivalent in that edge case.)
    if (!is.null(primary_zone) && !is.na(primary_zone)) {
      # [CHANGE 2] Rubber banding — Eq. 4 — always applied when primary_zone given
      mu_kj  <- mode_utility(primary_zone, j, agent, tt_list)
      emu_kj <- compute_logsum(mu_kj)
      log(attr_vec[j]) + alpha * theta * emu_ij + beta * theta * emu_kj
    } else {
      # Eq. 3: no primary location supplied (home-based destination choice)
      log(attr_vec[j]) + theta * emu_ij
    }
  })

  exp_u <- exp(dest_utils - max(dest_utils[is.finite(dest_utils)]))
  exp_u[!is.finite(exp_u)] <- 0
  probs <- exp_u / sum(exp_u)
  sample(1:n_zones, 1, prob = probs)
}

# =============================================================================
# TOUR-LEVEL MODE CHOICE (Section 2.4)
# "A mode is assigned to each tour and subtour based on the zonal
#  level of service measures in MOBi.plans."
# Called ONCE per tour using home -> primary destination OD pair
# =============================================================================

choose_tour_mode <- function(home_zone, primary_dest, agent, tt_list) {
  mu    <- mode_utility(home_zone, primary_dest, agent, tt_list)
  valid <- mu[is.finite(mu)]
  exp_u <- exp(valid - max(valid))
  probs <- exp_u / sum(exp_u)
  sample(names(valid), 1, prob = probs)
}

# =============================================================================
# [CHANGE 1] SUBTOUR MODE CHOICE — constrained by the main tour mode
#
# Subtours originate and terminate at the primary work location, so the
# available modes depend on what the agent used to reach that location:
#   car_driver    -> car_driver (with a +0.5 utility bonus) or walk
#   car_passenger -> pt or walk  (no car available as driver)
#   pt            -> pt or walk  (no car)
#   bicycle       -> bicycle or walk
#   walk          -> walk only
#
# All other modes (not in the allowed set) receive -Inf utility.
# A MNL is then applied over the allowed modes in the usual way.
# =============================================================================

choose_subtour_mode <- function(subtour_origin, subtour_dest, agent, tt_list,
                                main_tour_mode) {
  mu <- mode_utility(subtour_origin, subtour_dest, agent, tt_list)

  # [CHANGE 1] Allowed subtour modes keyed by main tour mode
  allowed <- switch(main_tour_mode,
    car_driver    = c("car_driver", "walk"),
    car_passenger = c("pt", "walk"),
    pt            = c("pt", "walk"),
    bicycle       = c("bicycle", "walk"),
    walk          = c("walk"),
    c("walk", "pt")   # safe default
  )

  # [CHANGE 1] Small car preference when main tour is by car (the agent already
  # has the car at the primary location, so marginal cost of using it is low)
  if (main_tour_mode == "car_driver") {
    mu["car_driver"] <- mu["car_driver"] + 0.5
  }

  # Suppress unavailable modes
  mu[!names(mu) %in% allowed] <- -Inf

  valid <- mu[is.finite(mu)]
  if (length(valid) == 0) return("walk")   # fallback

  exp_u <- exp(valid - max(valid))
  probs <- exp_u / sum(exp_u)
  sample(names(valid), 1, prob = probs)
}

# Travel time lookup (minutes -> hours)
get_travel_time <- function(origin, dest, mode, tt_list) {
  tt_min <- switch(mode,
    walk          = tt_list$walk[origin, dest],
    bicycle       = tt_list$bicycle[origin, dest],
    pt            = tt_list$pt[origin, dest],
    car_driver    = tt_list$car[origin, dest],
    car_passenger = tt_list$car_pass[origin, dest],
    30
  )
  tt_min / 60
}

# =============================================================================
# BUILD ONE TOUR: destinations chosen sequentially, then mode chosen
#
# [CORRECTED A4 + A6]: Activity type and primary zone are now passed to
#   choose_destination(), enabling rubber banding (Eq. 4) for stop activities
#   and activity-type-specific zone attractions (Section 2.4).
#
# [CHANGE 1] subtour_idx: integer index of the subtour activity in `activities`
#   (NA if no subtour). When supplied, that activity receives a separate mode
#   via choose_subtour_mode() constrained by the main tour mode. All other
#   activities share the main tour mode. The returned list contains:
#     $main_mode   : tour-level mode (one string, for reporting)
#     $subtour_mode: subtour mode or NA_character_
#     $modes       : per-activity character vector (length == length(activities))
# =============================================================================

build_one_tour <- function(activities, agent, tt_list,
                           subtour_idx = NA_integer_) {   # [CHANGE 1]
  home_zone    <- agent$home_zone
  primary_zone <- agent$primary_dest_zone   # k in rubber banding (Eq. 4)
  current      <- home_zone

  dests <- sapply(seq_along(activities), function(k) {
    act <- activities[[k]]
    if (act %in% c("Work", "Education")) {
      # Primary activity: destination pre-assigned from location choice (Sec 2.3)
      d <- primary_zone
    } else {
      # Stop activity: [CORRECTED A4] rubber banding towards primary_zone,
      #                [CORRECTED A6] with activity-type-specific attraction
      d <- choose_destination(current, agent, tt_list,
                               activity_type = act,
                               primary_zone  = primary_zone)
    }
    current <<- d
    d
  })

  # Resolve primary destination for tour-level mode choice
  primary_dest <- if (!is.na(primary_zone)) primary_zone else dests[1]

  # Choose main tour mode ONCE for the whole tour (Section 2.4)
  main_mode <- choose_tour_mode(home_zone, primary_dest, agent, tt_list)

  # [CHANGE 1] Subtour-specific mode: constrained by main_mode
  subtour_mode <- NA_character_
  if (!is.na(subtour_idx)) {
    # Subtour origin is the activity immediately before it (Work location)
    sub_origin <- if (subtour_idx > 1L) dests[subtour_idx - 1L] else home_zone
    sub_dest   <- dests[subtour_idx]
    subtour_mode <- choose_subtour_mode(sub_origin, sub_dest, agent, tt_list,
                                        main_mode)
  }

  # [CHANGE 1] Per-activity mode vector: main_mode for all except subtour
  modes <- rep(main_mode, length(activities))
  if (!is.na(subtour_idx)) modes[subtour_idx] <- subtour_mode

  list(activities   = activities,
       dests        = dests,
       main_mode    = main_mode,       # [CHANGE 1] renamed from $mode
       subtour_mode = subtour_mode,    # [CHANGE 1] NA if no subtour
       modes        = modes)           # [CHANGE 1] per-activity vector
}

# =============================================================================
# TESTING SECTION — Part 2
# Run this block to test each function on a single agent or small subset.
# Produces dest_mode_test: one row per activity across the first N_TEST agents,
# showing chosen destination zone, mode, and travel time for each trip.
# =============================================================================

N_TEST    <- 10
agent_idx <- 1   # single agent to inspect in detail

# --- Test mode_utility() for one OD pair ---
agent_single <- pop[agent_idx, ]
cat("=== Mode utilities for Agent", agent_idx,
    "(origin=1, dest=5) ===\n")
print(round(mode_utility(1, 5, agent_single, tt), 3))

# --- Test compute_logsum() ---
utils_test <- mode_utility(1, 5, agent_single, tt)
cat(sprintf("\nLogsum (EMU) for that OD pair: %.3f\n", compute_logsum(utils_test)))

# --- Test choose_destination() — now with activity type and rubber banding ---
cat(sprintf("\nSampled destination from zone 1 for Agent %d\n", agent_idx))
cat(sprintf("  Leisure stop (no rubber banding, no primary zone): zone %d\n",
    choose_destination(1, agent_single, tt, activity_type = "Leisure")))
cat(sprintf("  Shopping stop (rubber banding to primary zone %s): zone %d\n",
    agent_single$primary_dest_zone,
    choose_destination(1, agent_single, tt, activity_type = "Shopping",
                       primary_zone = agent_single$primary_dest_zone)))

# --- Test choose_tour_mode() ---
cat(sprintf("Tour mode chosen (home=1, primary_dest=5): %s\n",
    choose_tour_mode(1, 5, agent_single, tt)))

# --- Test build_one_tour() on a simple activity list ---
# [CHANGE 1] Index 3 (Leisure) is treated as a subtour activity for this test
test_acts <- c("Shopping", "Work", "Leisure")
test_tour <- build_one_tour(test_acts, agent_single, tt, subtour_idx = 3L)  # [CHANGE 1]
cat("\n=== build_one_tour() test (Shopping -> Work -> Leisure[subtour]) ===\n")
cat(sprintf("Activities  : %s\n", paste(test_tour$activities, collapse=" -> ")))
cat(sprintf("Destinations: %s\n", paste(test_tour$dests,      collapse=" -> ")))
cat(sprintf("Main mode   : %s\n", test_tour$main_mode))                    # [CHANGE 1]
if (!is.na(test_tour$subtour_mode))                                         # [CHANGE 1]
  cat(sprintf("Subtour mode: %s  (constrained by main mode)\n",            # [CHANGE 1]
              test_tour$subtour_mode))                                      # [CHANGE 1]
cat(sprintf("Per-activity: %s\n", paste(test_tour$modes, collapse=" -> "))) # [CHANGE 1]

# --- Build dest_mode_test: one row per activity for first N_TEST agents ---
dest_mode_test <- dplyr::bind_rows(lapply(1:N_TEST, function(i) {
  agent     <- pop[i, ]
  home_zone <- agent$home_zone
  tours_in  <- agent_tours[[i]]$tours
  if (length(tours_in) == 0) return(NULL)

  dplyr::bind_rows(lapply(seq_along(tours_in), function(t) {
    # [CHANGE 1] Pass subtour_idx so the subtour activity gets its own mode
    tr <- build_one_tour(tours_in[[t]]$activities, agent, tt,
                         subtour_idx = tours_in[[t]]$subtour_idx)   # [CHANGE 1]
    n_acts  <- length(tr$activities)
    origins <- c(home_zone, tr$dests[-n_acts])
    # [CHANGE 1] Use per-activity mode for each trip's travel time
    tt_trips <- mapply(function(o, d, m) get_travel_time(o, d, m, tt),
                       origins, tr$dests, tr$modes)                 # [CHANGE 1]
    data.frame(
      agent_id      = i,
      tour_num      = t,
      tour_type     = tours_in[[t]]$type,
      activity      = tr$activities,
      origin_zone   = origins,
      dest_zone     = tr$dests,
      tour_mode     = tr$main_mode,    # [CHANGE 1] renamed from $mode
      activity_mode = tr$modes,        # [CHANGE 1] per-activity (subtour may differ)
      travel_time_h = round(tt_trips, 3),
      stringsAsFactors = FALSE
    )
  }))
}))

cat("\n=== dest_mode_test dataframe (Part 2) ===\n")
print(head(dest_mode_test, 15))
cat(sprintf("\nMode distribution across test trips (per-activity):\n"))  # [CHANGE 1]
print(round(100 * prop.table(table(dest_mode_test$activity_mode)), 1))   # [CHANGE 1]
cat("\n")

# =============================================================================
# RUN PART 2: Add destinations and tour-level modes to each agent's tours
# =============================================================================

cat("Running destination and tour-level mode choice for all agents...\n")

agent_plans_p2 <- vector("list", N_AGENTS)

for (i in 1:N_AGENTS) {
  agent     <- pop[i, ]
  home_zone <- agent$home_zone
  tours_in  <- agent_tours[[i]]$tours

  # Apply destination + mode choice to each tour
  # [CHANGE 1] Pass subtour_idx from Part 1 tour structure
  tours_out <- lapply(tours_in, function(tr) {
    build_one_tour(tr$activities, agent, tt,
                   subtour_idx = tr$subtour_idx)   # [CHANGE 1]
  })

  # Flatten tours into single vectors
  activities <- unlist(lapply(tours_out, function(tr) tr$activities))
  dests_flat <- unlist(lapply(tours_out, function(tr) tr$dests))
  # [CHANGE 1] Use per-activity modes vector (subtour activity may differ)
  modes_flat <- unlist(lapply(tours_out, function(tr) tr$modes))   # [CHANGE 1]

  # Compute total travel time (fixed — destinations and modes are set)
  current  <- home_zone
  tt_total <- 0
  for (k in seq_along(activities)) {
    tt_total <- tt_total + get_travel_time(current, dests_flat[k], modes_flat[k], tt)
    current  <- dests_flat[k]
  }
  if (length(activities) > 0)
    tt_total <- tt_total + get_travel_time(current, home_zone,
                                           modes_flat[length(modes_flat)], tt)

  agent_plans_p2[[i]] <- list(
    n_work       = agent_tours[[i]]$n_work,
    n_edu        = agent_tours[[i]]$n_edu,
    n_bus        = agent_tours[[i]]$n_bus,
    n_other      = agent_tours[[i]]$n_other,
    n_tours      = agent_tours[[i]]$n_tours,
    home_zone    = home_zone,
    activities   = as.list(activities),
    dests        = dests_flat,
    modes        = modes_flat,
    total_travel = tt_total,
    # One outbound trip per activity + one return-home trip per tour.
    # (Previously was length(activities) * 2, which double-counted.)
    n_trips      = length(activities) + agent_tours[[i]]$n_tours
  )
}

cat("Done.\n\n")

# Quick summary
cat("=== Part 2: Destination & mode choice summary ===\n")
mean_tt   <- mean(sapply(agent_plans_p2, function(p) p$total_travel))
all_modes <- unlist(lapply(agent_plans_p2, function(p) p$modes))
cat(sprintf("Mean total travel time [h]: %.2f  [Paper: 1.52]\n", mean_tt))
cat("Mode share across all trips:\n")
mode_tab <- round(100 * prop.table(table(all_modes)), 1)
for (m in names(mode_tab)) cat(sprintf("  %-15s: %.1f%%\n", m, mode_tab[m]))
cat("\n")

# =============================================================================
# SAVE OUTPUT for Part 3
# =============================================================================

save(pop, agent_plans_p2, N_AGENTS, N_ZONES, tt, zone_attr,
     parking_search_time, service_freq, n_transfers, parking_cost, distance_km,
     zone_attr_by_type, get_zone_attr,
     mode_utility, compute_logsum, choose_destination,
     choose_tour_mode, choose_subtour_mode,   # [CHANGE 1] added choose_subtour_mode
     get_travel_time, build_one_tour,
     file = "part2_output.RData")

cat("Part 2 complete. Output saved to part2_output.RData\n")
cat("Run part3_durations.R next.\n")
