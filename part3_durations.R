# =============================================================================
# SIMBA MOBi Simulation — PART 3 of 5
# Scherr et al. (2020), EJTIR 20(4), pp. 152-172
#
# Covers:
#   7. Activity duration sampling (Figure 2, Section 2.4)
#      Distributions segmented by activity type and employment status.
#      Derived from national travel diary survey (MZ 2015) in the real model.
#      Here approximated with normal distributions.
#
# INPUTS:  part2_output.RData
# OUTPUTS: part3_output.RData
#   - agent_plans_p3 : agent_plans_p2 enriched with duration alternatives
#
# Run next: part4_plan_building.R
# =============================================================================

set.seed(42)
library(ggplot2)

# Load Part 2 outputs
load("part2_output.RData")
cat("Part 2 data loaded.\n\n")

# =============================================================================
# ACTIVITY DURATION SAMPLING (Figure 2, Section 2.4)
#
# The paper uses empirical distributions from MZ 2015 travel diary survey,
# segmented by activity type, employment type, and number of work tours.
# Here we approximate with normal distributions with plausible parameters.
#
# Key insight from Figure 2:
#   - Full-time employees with 1 work tour: longest durations (~8-9h)
#   - Full-time employees with 2 work tours: shorter (~5-6h each)
#   - Part-time employees: proportionally shorter
# =============================================================================

N_DUR_ALTS <- 10   # number of duration alternatives per agent (Section 2.5)

sample_duration <- function(activity_type, agent) {
  if (activity_type == "Work") {
    # Work duration segmented by employment type (Figure 2)
    params <- switch(agent$employment,
      full_time    = c(mu = 8.5, sd = 1.5),
      part_time_hi = c(mu = 6.0, sd = 1.2),
      part_time_lo = c(mu = 4.0, sd = 1.0),
                     c(mu = 4.0, sd = 1.0)   # fallback
    )
    return(max(0.5, rnorm(1, params["mu"], params["sd"])))
  }

  params <- switch(activity_type,
    Education = c(mu = 5.0, sd = 1.5),
    Leisure   = c(mu = 2.0, sd = 1.2),
    Shopping  = c(mu = 1.0, sd = 0.5),
    Business  = c(mu = 2.5, sd = 1.0),
    Accompany = c(mu = 0.5, sd = 0.3),
    Other     = c(mu = 1.5, sd = 0.8),
               c(mu = 1.5, sd = 0.8)   # fallback
  )
  max(0.25, rnorm(1, params["mu"], params["sd"]))
}

# =============================================================================
# TESTING SECTION — Part 3
# Run this block to test sample_duration() on individual activity types
# and inspect the full set of duration alternatives for a single agent.
# Produces dur_test: one row per activity type showing sampled durations
# across N_SAMPLES draws, so you can see the distribution shape.
# =============================================================================

N_TEST    <- 10
N_SAMPLES <- 50   # how many duration samples to draw per activity type
agent_idx <- 1    # agent used for employment-specific tests

agent_single <- pop[agent_idx, ]
cat(sprintf("=== Testing sample_duration() for Agent %d (employment: %s) ===\n",
    agent_idx, agent_single$employment))

# --- Test sample_duration() for each activity type ---
activity_types <- c("Work","Education","Leisure","Shopping",
                    "Business","Accompany","Other")

dur_test <- do.call(rbind, lapply(activity_types, function(a) {
  samples <- replicate(N_SAMPLES, sample_duration(a, agent_single))
  data.frame(
    activity_type = a,
    mean_h        = round(mean(samples), 2),
    sd_h          = round(sd(samples),   2),
    min_h         = round(min(samples),  2),
    max_h         = round(max(samples),  2),
    stringsAsFactors = FALSE
  )
}))

cat("\nDuration distribution summary by activity type:\n")
print(dur_test)

# --- Inspect all N_DUR_ALTS alternatives for one agent ---
agent_single_plan <- agent_plans_p2[[agent_idx]]
acts_single       <- unlist(agent_single_plan$activities)

if (length(acts_single) > 0) {
  dur_alts_test <- replicate(N_DUR_ALTS,
    sapply(acts_single, function(a) sample_duration(a, agent_single)),
    simplify = FALSE)

  cat(sprintf("\n=== %d duration alternatives for Agent %d ===\n",
      N_DUR_ALTS, agent_idx))
  cat(sprintf("Activities: %s\n", paste(acts_single, collapse=" | ")))

  dur_alts_df <- do.call(rbind, lapply(seq_along(dur_alts_test), function(k) {
    d <- dur_alts_test[[k]]
    data.frame(
      alternative  = k,
      total_dur_h  = round(sum(d), 2),
      durations    = paste(round(d, 2), collapse=" | "),
      stringsAsFactors = FALSE
    )
  }))
  print(dur_alts_df)

  cat(sprintf("\nTotal duration range across alternatives: %.2f h — %.2f h\n",
      min(dur_alts_df$total_dur_h), max(dur_alts_df$total_dur_h)))
  cat(sprintf("bud_perf (iter 1) = 14h — alternatives within budget: %d / %d\n",
      sum(dur_alts_df$total_dur_h < 14), N_DUR_ALTS))
}

# --- Compact test across N_TEST agents ---
dur_agent_test <- do.call(rbind, lapply(1:N_TEST, function(i) {
  agent <- pop[i, ]
  plan  <- agent_plans_p2[[i]]
  acts  <- unlist(plan$activities)
  if (length(acts) == 0) return(NULL)
  # Use first duration alternative
  durs <- sapply(acts, function(a) sample_duration(a, agent))
  data.frame(
    agent_id      = i,
    employment    = agent$employment,
    n_activities  = length(acts),
    total_dur_h   = round(sum(durs), 2),
    activities    = paste(acts,          collapse=" | "),
    durations     = paste(round(durs,2), collapse=" | "),
    stringsAsFactors = FALSE
  )
}))

cat("\n=== dur_agent_test: duration alt 1 across first", N_TEST, "agents ===\n")
print(dur_agent_test[, c("agent_id","employment","n_activities","total_dur_h")])
cat(sprintf("\nMean total duration [h]: %.2f  [Paper: 5.92]\n",
    mean(dur_agent_test$total_dur_h)))
cat("\n")

# =============================================================================
# RUN PART 3: Generate duration alternatives for each agent
# We generate N_DUR_ALTS alternatives here; plan-building (Part 4) will
# select among them based on time budget constraints (Table 3)
# =============================================================================

cat("Sampling activity duration alternatives for all agents...\n")

agent_plans_p3 <- vector("list", N_AGENTS)

for (i in 1:N_AGENTS) {
  agent  <- pop[i, ]
  plan   <- agent_plans_p2[[i]]
  acts   <- unlist(plan$activities)

  if (length(acts) == 0) {
    agent_plans_p3[[i]] <- plan
    agent_plans_p3[[i]]$dur_alts <- list(numeric(0))
    next
  }

  # Generate N_DUR_ALTS sets of durations (one per activity)
  dur_alts <- replicate(N_DUR_ALTS,
    sapply(acts, function(a) sample_duration(a, agent)),
    simplify = FALSE)

  # Attach to plan — plan-building will filter and select from these
  plan$dur_alts   <- dur_alts
  plan$N_DUR_ALTS <- N_DUR_ALTS
  agent_plans_p3[[i]] <- plan
}

cat("Done.\n\n")

# =============================================================================
# VALIDATION PLOT: Figure 2 analog
# Cumulative distribution of work durations by employment segment
# =============================================================================

cat("=== Part 3: Duration sampling summary ===\n")

work_dur_df <- do.call(rbind, lapply(1:N_AGENTS, function(i) {
  plan <- agent_plans_p3[[i]]
  acts <- unlist(plan$activities)
  idx  <- which(acts == "Work")
  if (length(idx) == 0 || length(plan$dur_alts) == 0) return(NULL)

  # Use first duration alternative for summary
  durs <- plan$dur_alts[[1]]
  data.frame(
    agent_id   = i,
    employment = pop$employment[i],
    n_work_act = length(idx),
    duration   = durs[idx[1]],
    stringsAsFactors = FALSE
  )
}))

if (!is.null(work_dur_df) && nrow(work_dur_df) > 0) {
  cat("Mean work durations by employment type:\n")
  agg <- aggregate(duration ~ employment, data = work_dur_df, FUN = mean)
  for (j in 1:nrow(agg))
    cat(sprintf("  %-15s: %.2f h\n", agg$employment[j], agg$duration[j]))

  # Figure 2 analog: CDF of work durations by segment
  p <- ggplot(work_dur_df,
              aes(x = duration,
                  colour = paste0(employment, " (", n_work_act, " W-act)"))) +
    stat_ecdf(linewidth = 0.9) +
    scale_x_continuous(limits = c(0, 14), breaks = 0:14) +
    labs(
      title    = "Figure 2 analog: Work duration cumulative distributions",
      subtitle = paste0("N = ", N_AGENTS, " agents, first duration alternative shown"),
      x        = "Activity duration [hours]",
      y        = "Cumulative probability",
      colour   = "Segment"
    ) +
    theme_minimal(base_size = 13)
  print(p)
}

# Summary of all activity types
all_acts <- unlist(lapply(agent_plans_p3, function(p) p$activities))
cat(sprintf("\nMean total activity duration (alt 1) [h]: %.2f  [Paper: 5.92]\n",
    mean(sapply(agent_plans_p3, function(p) {
      if (length(p$dur_alts) == 0) return(0)
      sum(p$dur_alts[[1]])
    }))))
cat("\nActivity type distribution across all agents:\n")
act_tab <- round(100 * prop.table(table(all_acts)), 1)
for (a in names(act_tab)) cat(sprintf("  %-12s: %.1f%%\n", a, act_tab[a]))
cat("\n")

# =============================================================================
# SAVE OUTPUT for Part 4
# =============================================================================

save(pop, agent_plans_p3, N_AGENTS, N_ZONES, tt, zone_attr,
     sample_duration, get_travel_time,
     file = "part3_output.RData")

cat("Part 3 complete. Output saved to part3_output.RData\n")
cat("Run part4_plan_building.R next.\n")
