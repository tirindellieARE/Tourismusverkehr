# =============================================================================
# SIMBA MOBi Simulation — PART 4 of 5
# Scherr et al. (2020), EJTIR 20(4), pp. 152-172
#
# Covers:
#   8. Rule-based plan-building with time budgets (Section 2.5, Table 3)
#
#      The plan-building procedure finds a valid combination of activity
#      durations such that the agent's full day plan fits within 24 hours.
#      Destinations and modes are already fixed from Part 2.
#
#      Time budgets (Table 3):
#        Iteration | bud_travel | bud_perf | bud_ooh
#             1    |    12h     |   14h    |  13.5h
#             2    |     5h     |   12h    |  14.0h
#             3    |     4h     |   11h    |  15.0h
#             4    |     3h     |   10h    |  16.5h
#
#      Algorithm per iteration:
#        1. Filter duration alternatives by bud_perf
#        2. Check travel time against bud_travel
#        3. Randomly combine until ooh < bud_ooh (max N_TRIES attempts)
#        4. If no valid plan after 4 iterations: fallback minimal plan
#
# INPUTS:  part3_output.RData
# OUTPUTS: part4_output.RData
#   - agent_plans_p4 : finalized plans with chosen durations and budget info
#
# Run next: part5_scheduling.R
# =============================================================================

set.seed(42)

# =============================================================================
# ASSUMPTIONS AND DEVIATIONS FROM PAPER — emitted as messages at runtime
# =============================================================================
message("[ASSUMPTION 9 – Part 4] Destination alternatives (Section 2.5): The paper",
        " generates |D|=3 DESTINATION alternative sets and |P|=10 duration sets,",
        " then randomly combines them. Here, destinations are FIXED from Part 2.",
        " Only duration alternatives (P) are varied; D always has one element.")
message("[ASSUMPTION 10 – Part 4] Iteration behaviour (Section 2.5): When no valid",
        " plan is found, the paper has agents REDO ALL DAILY CHOICES (tour/stop",
        " generation, destination, mode) with progressively tighter budgets.",
        " Here, only the time budgets are relaxed across iterations; activity",
        " choices are NOT regenerated.")
message("[ASSUMPTION 11 – Part 4] bud_travel across iterations: The paper tightens",
        " bud_travel (12→5→4→3 h) across iterations to force agents who chose",
        " far destinations to re-choose closer ones when redoing daily choices.",
        " Since destinations are FIXED here, a high total travel time will cause",
        " the agent to fail ALL later iterations (which have stricter travel",
        " budgets), making iterations 2-4 unreachable for such agents.")

# Load Part 3 outputs
load("part3_output.RData")
cat("Part 3 data loaded.\n\n")

# =============================================================================
# TIME BUDGETS (Table 3, Section 2.5)
# =============================================================================

TIME_BUDGETS <- data.frame(
  iteration  = 1:4,
  bud_travel = c(12,  5,  4,  3),
  bud_perf   = c(14, 12, 11, 10),
  bud_ooh    = c(13.5, 14.0, 15.0, 16.5)
)

N_TRIES <- 20   # max random combination attempts per budget iteration

# =============================================================================
# PLAN-BUILDING ALGORITHM (Section 2.5)
#
# Inputs (all fixed from Parts 1-3):
#   - activities  : character vector of activity types
#   - modes_flat  : mode for each activity (tour-consistent from Part 2)
#   - dests_flat  : destination zone for each activity (from Part 2)
#   - tt_total    : total travel time already computed in Part 2
#   - dur_alts    : list of N_DUR_ALTS duration alternatives (from Part 3)
#
# The algorithm only selects among duration alternatives here.
# Travel time is fixed since destinations and modes were already chosen.
# =============================================================================

build_plan_with_budgets <- function(plan) {

  acts     <- unlist(plan$activities)
  tt_total <- plan$total_travel
  dur_alts <- plan$dur_alts

  # Fallback for empty activity list
  if (length(acts) == 0 || length(dur_alts) == 0) {
    plan$durations    <- numeric(0)
    plan$total_dur    <- 0
    plan$ooh          <- tt_total
    plan$iteration    <- 0
    plan$plan_valid   <- TRUE
    return(plan)
  }

  valid_plan_found <- FALSE

  for (iter in 1:4) {
    bud <- TIME_BUDGETS[iter, ]

    # Step 1: Check travel time budget.
    # [ASSUMPTION 11 – deviation from Section 2.5]: bud_travel tightens from
    # 12→5→4→3 h across iterations. In the paper this forces agents who re-do
    # daily choices to pick closer destinations. Here destinations are FIXED, so
    # tt_total never changes. If tt_total >= bud_travel in iteration 1 (>12h),
    # it will also exceed all later budgets, making those iterations unreachable.
    # For agents with tt_total between 5 and 12 h, iterations 2-4 are similarly
    # unreachable. This is a structural limitation of fixing destinations in Part 2.
    if (tt_total >= bud$bud_travel) next

    # Step 2: Filter duration alternatives by bud_perf (Eq. 6-7)
    dur_totals <- sapply(dur_alts, sum)
    valid_durs <- dur_alts[dur_totals < bud$bud_perf]
    if (length(valid_durs) == 0)
      valid_durs <- list(dur_alts[[which.min(dur_totals)]])

    # Step 3: Randomly try combinations until ooh < bud_ooh (Eq. 10)
    for (try in 1:N_TRIES) {
      p_idx      <- sample(length(valid_durs), 1)
      dur_chosen <- sum(valid_durs[[p_idx]])
      ooh        <- tt_total + dur_chosen   # Eq. 10

      if (ooh < bud$bud_ooh) {
        plan$durations  <- valid_durs[[p_idx]]
        plan$total_dur  <- dur_chosen
        plan$ooh        <- ooh
        plan$iteration  <- iter
        plan$plan_valid <- TRUE
        valid_plan_found <- TRUE
        break
      }
    }
    if (valid_plan_found) break
  }

  # Step 4: Fallback if no valid plan found after 4 iterations
  # Paper reports <0.1 per mille of agents reach this point
  if (!valid_plan_found) {
    min_durs        <- dur_alts[[which.min(sapply(dur_alts, sum))]]
    plan$durations  <- min_durs
    plan$total_dur  <- sum(min_durs)
    plan$ooh        <- tt_total + sum(min_durs)
    plan$iteration  <- 99    # iteration = 99 flags as unresolved fallback
    plan$plan_valid <- FALSE
  }

  plan
}

# =============================================================================
# TESTING SECTION — Part 4
# Run this block to step through the plan-building algorithm for a single
# agent, showing exactly which budget iteration resolves the plan and why.
# Produces budget_test: one row per agent across the first N_TEST agents,
# showing travel time, chosen duration, ooh, and which iteration resolved it.
# =============================================================================

N_TEST    <- 10
agent_idx <- 1   # agent to inspect in detail

plan_single <- agent_plans_p3[[agent_idx]]
acts_single <- unlist(plan_single$activities)

cat(sprintf("=== Step-through plan-building for Agent %d ===\n", agent_idx))
cat(sprintf("Activities    : %s\n", paste(acts_single, collapse=" | ")))
cat(sprintf("Total travel  : %.2f h\n", plan_single$total_travel))

# Show all duration alternatives and their totals
dur_totals <- sapply(plan_single$dur_alts, sum)
cat(sprintf("\n%d duration alternatives — total durations:\n", length(plan_single$dur_alts)))
print(round(dur_totals, 2))

# Step through each budget iteration manually
cat("\nChecking against time budgets (Table 3):\n")
for (iter in 1:4) {
  bud <- TIME_BUDGETS[iter, ]
  tt  <- plan_single$total_travel

  travel_ok <- tt < bud$bud_travel
  valid_durs <- plan_single$dur_alts[dur_totals < bud$bud_perf]
  n_valid_d  <- length(valid_durs)

  if (n_valid_d > 0) {
    min_ooh <- tt + min(sapply(valid_durs, sum))
    max_ooh <- tt + max(sapply(valid_durs, sum))
  } else {
    min_ooh <- max_ooh <- NA
  }

  cat(sprintf(
    "  Iter %d | bud_travel=%.1f tt=%.2f [%s] | bud_perf=%.1f valid_durs=%d/%d | bud_ooh=%.1f ooh_range=[%.2f,%.2f] [%s]\n",
    iter,
    bud$bud_travel, tt, if (travel_ok) "OK" else "FAIL",
    bud$bud_perf, n_valid_d, length(plan_single$dur_alts),
    bud$bud_ooh, min_ooh, max_ooh,
    if (!is.na(min_ooh) && min_ooh < bud$bud_ooh) "OK" else "FAIL"
  ))
}

# Apply the function and show the result
result_single <- build_plan_with_budgets(plan_single)
cat(sprintf("\nResult: resolved at iteration %d | ooh=%.2f h | valid=%s\n",
    result_single$iteration,
    result_single$ooh,
    result_single$plan_valid))

# --- Budget test across N_TEST agents ---
budget_test <- do.call(rbind, lapply(1:N_TEST, function(i) {
  plan   <- agent_plans_p3[[i]]
  result <- build_plan_with_budgets(plan)
  data.frame(
    agent_id     = i,
    employment   = pop$employment[i],
    n_activities = length(unlist(plan$activities)),
    travel_h     = round(plan$total_travel, 2),
    chosen_dur_h = round(result$total_dur,  2),
    ooh_h        = round(result$ooh,        2),
    iteration    = result$iteration,
    plan_valid   = result$plan_valid,
    stringsAsFactors = FALSE
  )
}))

cat("\n=== budget_test: plan-building results across first", N_TEST, "agents ===\n")
print(budget_test)
cat(sprintf("\nMean travel time [h]      : %.2f  [Paper: 1.52]\n",
    mean(budget_test$travel_h)))
cat(sprintf("Mean activity duration [h]: %.2f  [Paper: 5.92]\n",
    mean(budget_test$chosen_dur_h)))
cat(sprintf("Mean out-of-home time [h] : %.2f  [Paper: 7.44]\n",
    mean(budget_test$ooh_h)))
cat(sprintf("Plans valid               : %d / %d\n",
    sum(budget_test$plan_valid), N_TEST))
cat("\n")

# =============================================================================
# RUN PART 4: Apply plan-building for each agent
# =============================================================================

cat("Running plan-building with time budgets for all agents...\n")

agent_plans_p4 <- lapply(agent_plans_p3, build_plan_with_budgets)

cat("Done.\n\n")

# =============================================================================
# VALIDATION SUMMARY (Table 4 analog — partial, without scheduling yet)
# =============================================================================

cat("=== Part 4: Plan-building summary ===\n")

n_valid    <- sum(sapply(agent_plans_p4, function(p) isTRUE(p$plan_valid)))
n_fallback <- sum(sapply(agent_plans_p4, function(p) p$iteration == 99))
iter_dist  <- table(sapply(agent_plans_p4, function(p) p$iteration))

cat(sprintf("Valid plans found      : %d / %d (%.1f%%)\n",
    n_valid, N_AGENTS, 100 * n_valid / N_AGENTS))
cat(sprintf("Fallback plans (iter99): %d  [Paper: <0.1 per mille]\n", n_fallback))
cat("Plans resolved by iteration:\n")
for (it in names(iter_dist))
  cat(sprintf("  Iteration %s: %d agents\n", it, iter_dist[it]))

mean_tt  <- mean(sapply(agent_plans_p4, function(p) p$total_travel))
mean_dur <- mean(sapply(agent_plans_p4, function(p) p$total_dur))
mean_ooh <- mean(sapply(agent_plans_p4, function(p) p$ooh))

cat(sprintf("\nMean travel time [h]       : %.2f  [Paper: 1.52]\n", mean_tt))
cat(sprintf("Mean activity duration [h]  : %.2f  [Paper: 5.92]\n", mean_dur))
cat(sprintf("Mean out-of-home time [h]   : %.2f  [Paper: 7.44]\n", mean_ooh))
cat("\n")

# =============================================================================
# SAVE OUTPUT for Part 5
# =============================================================================

save(pop, agent_plans_p4, N_AGENTS, TIME_BUDGETS,
     file = "part4_output.RData")

cat("Part 4 complete. Output saved to part4_output.RData\n")
cat("Run part5_scheduling.R next.\n")
