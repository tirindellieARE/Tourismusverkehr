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
zone_attr <- runif(N_ZONES, 500, 12000)

# =============================================================================
# MODE UTILITY (Equation 1, Table 2)
# Parameters are trip-based as stated in Section 2.4.
# ASC and beta_tt values are invented — not estimated from data.
# =============================================================================

mode_utility <- function(origin, dest, agent, tt_list) {
  asc     <- c(walk=-1.5, bicycle=-0.8, pt=0.0, car_driver=0.5, car_passenger=-0.5)
  beta_tt <- c(walk=-0.06, bicycle=-0.05, pt=-0.04,
               car_driver=-0.03, car_passenger=-0.03)

  u <- asc + beta_tt * c(
    tt_list$walk[origin, dest],
    tt_list$bicycle[origin, dest],
    tt_list$pt[origin, dest],
    tt_list$car[origin, dest],
    tt_list$car_pass[origin, dest]
  )

  # Constraint: no car driver if no car available
  if (agent$car_avail == 0) u["car_driver"] <- -Inf

  # PT bonus by subscription type (General-Abo holders use PT much more)
  pt_bonus <- switch(agent$pt_sub,
    general  = 1.5,
    verbund  = 0.8,
    halbtax  = 0.3,
    none     = 0.0
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
  log(sum(exp((valid - max(valid)) / theta))) + max(valid) / theta
}

# =============================================================================
# DESTINATION CHOICE (Equation 3)
# V(j|i) = log(Aj) + theta * EMU_ij + shadow prices
# Shadow prices omitted here (not calibrated from data)
# destination attractiveness is independent of tour/activity type!
# =============================================================================

choose_destination <- function(origin, agent, tt_list, n_zones = N_ZONES) {
  dest_utils <- sapply(1:n_zones, function(j) {
    if (j == origin) return(-Inf)
    mu  <- mode_utility(origin, j, agent, tt_list)
    emu <- compute_logsum(mu)
    log(zone_attr[j]) + 0.6 * emu
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
# BUILD ONE TOUR: destinations chosen sequentially, then mode chosen once
# =============================================================================

build_one_tour <- function(activities, agent, tt_list) {

  # Choose destinations sequentially starting from home
  home_zone = agent$home_zone
  current <- home_zone
  primary_dest = agent$primary_dest_zone
  dests   <- sapply(seq_along(activities), function(k) {
    if(activities[[k]] %in% c("Work","Education")){
      d = primary_dest
    } else d <- choose_destination(current, agent, tt_list)
    current <<- d
    d
  })

  # Primary destination = Work/Education if present, else first activity
  primary_dest = agent$primary_dest_zone
  primary_dest = if (is.na(primary_dest)) dests[1] else primary_dest

  # Choose mode ONCE for the whole tour
  tour_mode <- choose_tour_mode(home_zone, primary_dest, agent, tt_list)

  list(
    activities = activities,
    dests      = dests,
    mode       = tour_mode   # same mode for all trips in this tour
  )
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

# --- Test choose_destination() ---
cat(sprintf("\nSampled destination from zone 1 for Agent %d: zone %d\n",
    agent_idx, choose_destination(1, agent_single, tt)))

# --- Test choose_tour_mode() ---
cat(sprintf("Tour mode chosen (home=1, primary_dest=5): %s\n",
    choose_tour_mode(1, 5, agent_single, tt)))

# --- Test build_one_tour() on a simple activity list ---
test_acts <- c("Shopping", "Work", "Leisure")
test_tour <- build_one_tour(test_acts, agent_single, tt)
cat("\n=== build_one_tour() test (Shopping -> Work -> Leisure) ===\n")
cat(sprintf("Activities  : %s\n", paste(test_tour$activities, collapse=" -> ")))
cat(sprintf("Destinations: %s\n", paste(test_tour$dests,      collapse=" -> ")))
cat(sprintf("Tour mode   : %s  (same for all trips in this tour)\n", test_tour$mode))

# --- Build dest_mode_test: one row per activity for first N_TEST agents ---
dest_mode_test <- dplyr::bind_rows(lapply(1:N_TEST, function(i) {
  agent     <- pop[i, ]
  home_zone <- agent$home_zone
  tours_in  <- agent_tours[[i]]$tours
  if (length(tours_in) == 0) return(NULL)

  dplyr::bind_rows(lapply(seq_along(tours_in), function(t) {
    tr      <- build_one_tour(tours_in[[t]]$activities, agent, tt)
    n_acts  <- length(tr$activities)
    origins <- c(home_zone, tr$dests[-n_acts])
    tt_trips <- mapply(function(o, d) get_travel_time(o, d, tr$mode, tt),
                       origins, tr$dests)
    data.frame(
      agent_id    = i,
      tour_num    = t,
      tour_type   = tours_in[[t]]$type,
      activity    = tr$activities,
      origin_zone = origins,
      dest_zone   = tr$dests,
      tour_mode   = tr$mode,
      travel_time_h = round(tt_trips, 3),
      stringsAsFactors = FALSE
    )
  }))
}))

cat("\n=== dest_mode_test dataframe (Part 2) ===\n")
print(head(dest_mode_test, 15))
cat(sprintf("\nMode distribution across test trips:\n"))
print(round(100 * prop.table(table(dest_mode_test$tour_mode)), 1))
cat("\n")

# =============================================================================
# RUN PART 2: Add destinations and tour-level modes to each agent's tours
# =============================================================================

cat("Running destination and tour-level mode choice for all agents...\n")

agent_plans_p2 <- vector("list", N_AGENTS)

for (i in 1:N_AGENTS) {
  agent     <- pop[i, ]
  home_zone <- sample(1:N_ZONES, 1)
  tours_in  <- agent_tours[[i]]$tours

  # Apply destination + mode choice to each tour
  tours_out <- lapply(tours_in, function(tr) {
    build_one_tour(tr$activities, agent, tt)
  })

  # Flatten tours into single vectors
  activities <- unlist(lapply(tours_out, function(tr) tr$activities))
  dests_flat <- unlist(lapply(tours_out, function(tr) tr$dests))
  modes_flat <- unlist(lapply(tours_out, function(tr)
                  rep(tr$mode, length(tr$activities))))

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
    n_trips      = length(activities) * 2
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
     mode_utility, compute_logsum, choose_destination,
     choose_tour_mode, get_travel_time, build_one_tour,
     file = "part2_output.RData")

cat("Part 2 complete. Output saved to part2_output.RData\n")
cat("Run part3_durations.R next.\n")
