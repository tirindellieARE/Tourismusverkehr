# =============================================================================
# Tourist Mode Choice — PART 4 of 5
# Danalet et al. (2023), STRC Conference Paper
# Activity simulation adapted from Scherr et al. (2020), EJTIR 20(4), pp.152-172
#
# STANDALONE script — does NOT depend on activity_simulation/ scripts.
#
# Covers:
#   Rule-based excursion activity chain generation for each synthetic tourist
#   agent per stay-day. Adapted from the Scherr et al. MOBi.plans algorithm
#   with the following structural differences:
#
#   | Aspect          | Residents (Scherr)           | Tourists (this script)          |
#   |-----------------|------------------------------|---------------------------------|
#   | Home base       | Residential zone             | Hotel zone                      |
#   | Activities      | Work/Educ/Business/Leisure/  | Leisure / Shopping /            |
#   |                 | Shopping/Accompany/Other     | Sightseeing                     |
#   | Plan horizon    | 1 day (24h)                  | 1 excursion per stay-day        |
#   | Tour types      | Primary (W/E) + secondary    | Excursion tours only            |
#   | Destination     | MNL nested logit + rubber    | Hotel zone = origin; excursion  |
#   |                 | banding                      | destination drawn by frequency  |
#   | Mode            | Scherr MNL (5 modes)         | From Part 3 (7 modes, fixed)    |
#   | No rubber band  | Only for W/E tours           | Not applied [ASSUMPTION – A-7]  |
#
# All invented parameters are flagged [INVENTED PARAMETER].
# All non-paper assumptions are flagged [ASSUMPTION].
#
# INPUTS:  part3_output.RData
# OUTPUTS: part4_output.RData
#   - tourist_plans : named list per agent_id; each entry is a list of day_plans,
#                     one per stay-day, each containing:
#                       $activities  character vector
#                       $durations   numeric vector [hours]
#                       $start_times numeric vector [hours from midnight]
#                       $ooh         total out-of-hotel time [hours]
#                       $excursion_zone : destination zone (hotel zone for now)
#
# Run next: part5_od_matrices.R
# =============================================================================

library(dplyr)
library(truncnorm)

set.seed(42)

# =============================================================================
# RUNTIME FLAGS
# =============================================================================

message("[ASSUMPTION – Part 4] Hotel zone used as excursion origin. Destination",
        " zone assignment: for now, all excursions depart from and return to the",
        " hotel zone. A trip-level destination model is not implemented here since",
        " the paper does not specify one for tourists. Add zone-level destination",
        " choice if OD matrices should reflect within-Switzerland trip distribution.")
message("[ASSUMPTION – Part 4] No rubber banding. All tourist activities are",
        " secondary-type; the long-term primary location anchor does not apply.")
message("[ASSUMPTION – Part 4] Excursion mode assigned in Part 3 is the main",
        " hotel→destination leg. Within-excursion local legs (e.g. walk between",
        " sights) are not modelled separately.")
message("[INVENTED PARAMETER – Part 4] All MNL beta values in tourist_excursion_freq(),",
        " tourist_stop_freq(), assign_tourist_activity(), tourist_duration_sample(),",
        " and tourist_time_budgets are invented. Replace with estimated values from TMS.")

# =============================================================================
# 1. LOAD INPUTS
# =============================================================================

load("part3_output.RData")  # tourist_pop, agent_modes, SCALE_FACTOR

cat(sprintf("Agents loaded: %d\n", nrow(tourist_pop)))
n_group <- sum(agent_modes$group_tour)
cat(sprintf("Group tour agents (mode 7, excluded): %d\n\n", n_group))

# Merge mode assignment into pop
pop <- dplyr::left_join(tourist_pop, agent_modes[, c("agent_id","chosen_mode","group_tour")],
                        by = "agent_id")

# Exclude tour bus agents from individual simulation
pop_ind <- pop[!pop$group_tour, ]
N_AGENTS <- nrow(pop_ind)
cat(sprintf("Agents for individual simulation: %d\n\n", N_AGENTS))

# =============================================================================
# 2. HELPER: MNL CHOICE
# =============================================================================

mnl_choice <- function(V) {
  V <- V - max(V)
  p <- exp(V) / sum(exp(V))
  as.integer(sample(names(V), 1, prob = p))
}

# =============================================================================
# 3. EXCURSION FREQUENCY MODEL
#
# Predicts the number of excursions a tourist makes per stay-day (0, 1, or 2).
# [INVENTED PARAMETER] All beta values below. Paper does not model excursion frequency.
# Variables used: nationality_group.
# =============================================================================

tourist_excursion_freq <- function(agent) {
  # [INVENTED PARAMETER] beta values approximated; no paper source.
  nat_bonus <- switch(agent$nationality_group,
    germany      = 0.20,
    france       = 0.10,
    italy        = 0.15,
    uk           = 0.25,
    netherlands  = 0.20,
    usa          = 0.30,
    switzerland  = 0.10,
    rest_of_world = 0.15
  )
  V0 <- 0
  V1 <- -0.30 + nat_bonus   # [INVENTED PARAMETER]
  V2 <- -2.00 + nat_bonus   # [INVENTED PARAMETER]

  mnl_choice(c("0" = V0, "1" = V1, "2" = V2))
}

# =============================================================================
# 4. STOP FREQUENCY MODEL
#
# Predicts the number of stops within an excursion tour (0, 1, or 2).
# Single MNL (total stops) — no outbound/inbound split for tourist tours.
# [INVENTED PARAMETER] All beta values below.
# =============================================================================

tourist_stop_freq <- function(agent, n_excursions) {
  V0 <- 0
  V1 <- -0.50 - 0.20 * n_excursions   # [INVENTED PARAMETER]
  V2 <- -1.80 - 0.40 * n_excursions   # [INVENTED PARAMETER]

  mnl_choice(c("0" = V0, "1" = V1, "2" = V2))
}

# =============================================================================
# 5. ACTIVITY TYPE ASSIGNMENT
#
# Activity types for tourists: Leisure, Shopping, Sightseeing.
# direction: "outbound" | "primary" | "inbound"
# [INVENTED PARAMETER] All probabilities below. Paper does not publish tables for tourists.
# =============================================================================

assign_tourist_activity <- function(direction) {
  probs <- switch(direction,
    outbound = c(Leisure = 0.30, Shopping = 0.20, Sightseeing = 0.50),  # [INVENTED PARAMETER]
    primary  = c(Leisure = 0.35, Shopping = 0.15, Sightseeing = 0.50),  # [INVENTED PARAMETER]
    inbound  = c(Leisure = 0.40, Shopping = 0.35, Sightseeing = 0.25)   # [INVENTED PARAMETER]
  )
  sample(names(probs), 1, prob = probs)
}

# =============================================================================
# 6. DURATION SAMPLING
#
# Truncated normal distributions by activity type.
# [INVENTED PARAMETER] Mean and SD below; no TMS duration CDFs published.
# =============================================================================

# Mean and SD in hours [INVENTED PARAMETER]
DURATION_PARAMS <- list(
  Leisure     = list(mean = 2.5, sd = 1.2, min = 0.5, max = 6.0),
  Shopping    = list(mean = 1.5, sd = 0.7, min = 0.25, max = 3.5),
  Sightseeing = list(mean = 2.0, sd = 1.0, min = 0.5, max = 5.0)
)

tourist_duration_sample <- function(activity_type) {
  p <- DURATION_PARAMS[[activity_type]]
  if (is.null(p)) p <- list(mean = 1.5, sd = 0.8, min = 0.25, max = 4.0)
  rtruncnorm(1, a = p$min, b = p$max, mean = p$mean, sd = p$sd)
}

# =============================================================================
# 7. TIME BUDGET
#
# Maximum out-of-hotel time (ooh) per excursion day.
# Adapted from Scherr et al. Table 3 but relaxed for tourists.
# [INVENTED PARAMETER] All values below.
# =============================================================================

TOURIST_OOH_BUDGET <- 14.0   # hours [INVENTED PARAMETER]
N_TRIES            <- 20     # max duration-combination attempts per excursion

# =============================================================================
# 8. SCHEDULE A SINGLE EXCURSION
#
# Places activities sequentially starting from a sampled departure time.
# "Outward" approach (Castiglione et al. 2015): anchor on the primary activity.
#
# [INVENTED PARAMETER] departure time distribution: uniform 7h–11h.
# =============================================================================

DEPARTURE_MIN <- 7.0    # [INVENTED PARAMETER]
DEPARTURE_MAX <- 11.0   # [INVENTED PARAMETER]
TRAVEL_PER_LEG <- 0.5   # hours per leg (placeholder; wire real LOS when available)
                         # [INVENTED PARAMETER — replace with actual OD skims]

schedule_excursion <- function(activities, durations) {
  n_acts  <- length(activities)
  n_legs  <- n_acts + 1L   # outbound + between acts + return
  dep_time <- runif(1, DEPARTURE_MIN, DEPARTURE_MAX)

  times  <- numeric(n_acts)
  t_curr <- dep_time

  for (k in seq_len(n_acts)) {
    times[k] <- t_curr + TRAVEL_PER_LEG
    t_curr   <- times[k] + durations[k]
  }

  return_time <- t_curr + TRAVEL_PER_LEG
  ooh         <- return_time - dep_time

  list(
    departure  = dep_time,
    start_times = times,
    return_time = return_time,
    ooh         = ooh
  )
}

# =============================================================================
# 9. BUILD ONE EXCURSION PLAN
# =============================================================================

build_excursion <- function(agent, n_excursions_today) {
  n_stops <- tourist_stop_freq(agent, n_excursions_today)

  # Build activity sequence
  acts <- character(0)
  if (n_stops >= 2L) acts <- c(acts, assign_tourist_activity("outbound"))
  acts <- c(acts, assign_tourist_activity("primary"))
  if (n_stops >= 1L) acts <- c(acts, assign_tourist_activity("inbound"))

  # Sample durations respecting ooh budget
  valid <- FALSE
  for (try in seq_len(N_TRIES)) {
    durs <- sapply(acts, tourist_duration_sample)
    sched <- schedule_excursion(acts, durs)
    if (sched$ooh <= TOURIST_OOH_BUDGET) { valid <- TRUE; break }
  }
  if (!valid) {
    # Fallback: minimal durations [INVENTED PARAMETER]
    durs  <- sapply(acts, function(a) DURATION_PARAMS[[a]]$min %||% 0.25)
    sched <- schedule_excursion(acts, durs)
  }

  list(
    activities   = acts,
    durations    = durs,
    start_times  = sched$start_times,
    departure    = sched$departure,
    return_time  = sched$return_time,
    ooh          = sched$ooh,
    excursion_zone = agent$excursion_zone,   # placeholder; extend with destination model
    mode         = agent$chosen_mode,
    plan_valid   = valid
  )
}

# Null-coalescing helper
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b

# =============================================================================
# 10. MAIN SIMULATION LOOP
# =============================================================================

cat("Running tourist activity simulation...\n")

tourist_plans <- vector("list", N_AGENTS)

for (i in seq_len(N_AGENTS)) {
  agent <- pop_ind[i, ]

  day_plans <- vector("list", agent$n_nights)

  for (day in seq_len(agent$n_nights)) {
    n_ex <- tourist_excursion_freq(agent)

    excursions <- vector("list", n_ex)
    for (ex in seq_len(max(n_ex, 1L))) {
      if (n_ex == 0L) break
      excursions[[ex]] <- build_excursion(agent, n_ex)
    }

    day_plans[[day]] <- list(
      day        = day,
      n_excursions = n_ex,
      excursions = excursions[seq_len(n_ex)]
    )
  }

  tourist_plans[[i]] <- list(
    agent_id       = agent$agent_id,
    hotel_id       = agent$hotel_id,
    nationality_group = agent$nationality_group,
    chosen_mode    = agent$chosen_mode,
    n_nights       = agent$n_nights,
    day_plans      = day_plans
  )
}

cat("Done.\n\n")

# =============================================================================
# 11. VALIDATION SUMMARY
# =============================================================================

cat("=== Part 4: Tourist Activity Simulation Summary ===\n")

all_excursions <- do.call(c, lapply(tourist_plans, function(tp) {
  do.call(c, lapply(tp$day_plans, function(dp) dp$excursions))
}))

n_ex_total   <- length(all_excursions)
n_valid      <- sum(sapply(all_excursions, function(e) isTRUE(e$plan_valid)))
mean_ooh     <- mean(sapply(all_excursions, function(e) e$ooh))
mean_n_acts  <- mean(sapply(all_excursions, function(e) length(e$activities)))
act_type_tab <- table(unlist(lapply(all_excursions, function(e) e$activities)))

cat(sprintf("Agents simulated    : %d\n", N_AGENTS))
cat(sprintf("Total excursions    : %d\n", n_ex_total))
cat(sprintf("Valid plans         : %d / %d (%.1f%%)\n",
    n_valid, n_ex_total, 100 * n_valid / max(n_ex_total, 1)))
cat(sprintf("Mean ooh [h]        : %.2f  [benchmark: TMS data]\n", mean_ooh))
cat(sprintf("Mean activities/exc : %.1f\n", mean_n_acts))
cat("\nActivity type distribution:\n")
print(act_type_tab)
cat("\n")

# =============================================================================
# 12. SAVE
# =============================================================================

save(tourist_plans, pop_ind, N_AGENTS, SCALE_FACTOR,
     DURATION_PARAMS, TOURIST_OOH_BUDGET, TRAVEL_PER_LEG,
     file = "part4_output.RData")

cat("Part 4 complete. Output saved to part4_output.RData\n")
cat("Run part5_od_matrices.R next.\n")
