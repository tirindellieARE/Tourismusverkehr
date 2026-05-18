# =============================================================================
# SIMBA MOBi Simulation — PART 5 of 5
# Scherr et al. (2020), EJTIR 20(4), pp. 152-172
#
# Covers:
#   9.  Activity start-time scheduling (Section 2.5, Figure 3)
#   10. Validation summary (Table 4 analog)
#   11. Plots
#
#      "Outward" approach: primary activity (Work/Education) is scheduled
#      first based on start-time preference distributions (Figure 3).
#      Secondary activities and stops are then fitted around it.
#      Tours that do not fit within 24h are discarded (last resort,
#      reported as 0.06 per mille of all trips in the paper).
#
# INPUTS:  part4_output.RData
# OUTPUTS: 6 plots printed to screen
#          final_plans.RData (complete simulation results)
#
# =============================================================================

set.seed(42)
library(ggplot2)
library(dplyr)
library(tidyr)

# =============================================================================
# ASSUMPTIONS AND DEVIATIONS FROM PAPER — emitted as messages at runtime
# =============================================================================
message("[FIX DIV-9 – Part 5] Travel time per trip (Section 2.5): schedule_plan()",
        " now looks up travel time for EACH individual trip from the network",
        " matrix (tt) using the per-activity destinations and modes stored from",
        " Part 2. The previous even-distribution approximation is removed.")
message("[ASSUMPTION 13 – Part 5] Iterative scoring for secondary start times",
        " (Section 2.5): The paper implements an iterative algorithm that scores",
        " secondary activity start times against preference curves (Figure 3) and",
        " allows agents to re-sample primary start times to improve the score.",
        " Here, a single-pass layout is used with no iterative scoring.")
message("[NOTE – Part 5] MOBi.sim (Section 2.6) is NOT simulated. The MATSim",
        " feedback loop (mode/route/time adaptation and capacity-constrained",
        " travel times fed back to MOBi.plans) is absent from this R implementation.")

# Load Part 4 outputs
load("part4_output.RData")
cat("Part 4 data loaded.\n\n")

# =============================================================================
# START-TIME PREFERENCE DISTRIBUTIONS (Figure 3, Section 2.5)
#
# The paper uses empirical distributions from MZ 2015 travel diary survey.
# Work and education show strong morning peaks; leisure and shopping start
# later without significant peaks.
# Here approximated with normal distributions clamped to plausible ranges.
# =============================================================================

clamp <- function(x, lo, hi) max(lo, min(hi, x))

start_time_dist <- function(activity_type) {
  switch(activity_type,
    Work      = clamp(rnorm(1,  8.0, 0.8),  5, 10),
    Education = clamp(rnorm(1,  8.2, 0.7),  6, 10),
    Leisure   = clamp(rnorm(1, 14.0, 3.0),  9, 22),
    Shopping  = clamp(rnorm(1, 11.0, 2.0),  8, 20),
    Business  = clamp(rnorm(1, 10.0, 2.0),  7, 18),
    Accompany = clamp(rnorm(1,  8.5, 1.5),  6, 18),
    Other     = clamp(rnorm(1, 12.0, 3.0),  8, 20),
                clamp(rnorm(1, 10.0, 2.0),  6, 22)   # fallback
  )
}

# =============================================================================
# START-TIME SCHEDULING (Section 2.5)
#
# "Outward" approach (Castiglione et al., 2015):
#   1. Find primary activity (Work/Education) in the plan
#   2. Sample its start time from the preference distribution
#   3. Work backwards to set the start of the day
#   4. Work forwards to place all subsequent activities
#   5. Discard any activities that overflow 24h (last resort)
# =============================================================================

schedule_plan <- function(plan) {
  acts <- unlist(plan$activities)

  if (length(acts) == 0) {
    plan$schedule <- list()
    return(plan)
  }

  durs <- if (length(plan$durations) <= 1)
            rep(plan$durations, length(acts))
          else
            plan$durations

  # [FIX DIV-9] Look up travel time for each individual trip from the network
  # matrix given its origin, destination, and mode — as stated in Section 2.5.
  # origin of trip k: home_zone for k=1, dest of activity k-1 for k>1.
  # tt and get_travel_time are loaded from part4_output.RData.
  if (length(plan$dests) == length(acts) && length(plan$modes) == length(acts)) {
    origins_vec <- c(plan$home_zone, plan$dests[-length(plan$dests)])
    tt_per <- mapply(function(o, d, m) get_travel_time(o, d, m, tt),
                     origins_vec, plan$dests, plan$modes)
  } else {
    # Fallback: distribute evenly when dest/mode vectors are misaligned
    tt_per <- rep(plan$total_travel / max(length(acts), 1), length(acts))
  }

  # Find primary activity — Work or Education takes scheduling priority
  prim_idx <- which(acts %in% c("Work","Education"))
  if (length(prim_idx) == 0) prim_idx <- 1

  # Sample primary activity start time from preference distribution
  prim_start <- start_time_dist(acts[prim_idx[1]])

  # Work backwards to find start of activity 1.
  # Trips to sum: tt_per[2], tt_per[3], ..., tt_per[prim_idx] (trips arriving at
  # activities 2..prim_idx). tt_per[1] is home→act1 and is implicit in current_time.
  tt_backward  <- if (prim_idx[1] > 1L) sum(tt_per[seq(2L, prim_idx[1])]) else 0
  current_time <- prim_start - tt_backward -
                  sum(durs[seq_len(max(0L, prim_idx[1] - 1L))])
  current_time <- max(current_time, 5.0)   # no earlier than 5am

  # Build schedule sequentially
  schedule <- vector("list", length(acts))
  for (k in seq_along(acts)) {
    start_k <- current_time + if (k == 1) 0 else tt_per[k]
    end_k   <- start_k + durs[k]
    schedule[[k]] <- list(
      activity   = acts[k],
      start_h    = round(start_k, 2),
      end_h      = round(end_k,   2),
      duration_h = round(durs[k], 2)
    )
    current_time <- end_k
  }

  # Discard activities overflowing 24h (Section 2.5 last resort: 0.06‰ of trips)
  plan$schedule <- Filter(function(s) s$start_h < 24, schedule)
  plan
}

# =============================================================================
# TESTING SECTION — Part 5
# Run this block to test start_time_dist() and schedule_plan() on a small
# subset of agents before running the full simulation.
# Produces schedule_test: one row per scheduled activity across N_TEST agents,
# showing start time, end time, duration and mode for each activity.
# =============================================================================

N_TEST    <- 10
agent_idx <- 1   # agent to inspect in detail

# --- Test start_time_dist() for each activity type ---
activity_types <- c("Work","Education","Leisure","Shopping",
                    "Business","Accompany","Other")

cat("=== start_time_dist() — 20 samples per activity type ===\n")
start_time_test <- do.call(rbind, lapply(activity_types, function(a) {
  samples <- replicate(20, start_time_dist(a))
  data.frame(
    activity = a,
    mean_h   = round(mean(samples), 2),
    min_h    = round(min(samples),  2),
    max_h    = round(max(samples),  2),
    stringsAsFactors = FALSE
  )
}))
print(start_time_test)

# --- Step through schedule_plan() for one agent ---
plan_single   <- agent_plans_p4[[agent_idx]]
acts_single   <- unlist(plan_single$activities)
sched_single  <- schedule_plan(plan_single)

cat(sprintf("\n=== Scheduled day plan for Agent %d ===\n", agent_idx))
cat(sprintf("Employment : %s\n", pop$employment[agent_idx]))
cat(sprintf("Activities : %s\n", paste(acts_single, collapse=" | ")))
cat(sprintf("Tour mode  : %s\n\n", paste(plan_single$modes, collapse=" | ")))

if (length(sched_single$schedule) > 0) {
  sched_df_single <- do.call(rbind, lapply(sched_single$schedule, function(s) {
    data.frame(
      activity   = s$activity,
      start_h    = s$start_h,
      end_h      = s$end_h,
      duration_h = s$duration_h,
      stringsAsFactors = FALSE
    )
  }))
  print(sched_df_single)
  cat(sprintf("\nDay starts at: %.2fh  Day ends at: %.2fh\n",
      min(sched_df_single$start_h),
      max(sched_df_single$end_h)))
} else {
  cat("No activities scheduled.\n")
}

# --- Build schedule_test: one row per activity across N_TEST agents ---
schedule_test <- do.call(rbind, lapply(1:N_TEST, function(i) {
  plan   <- agent_plans_p4[[i]]
  sched  <- schedule_plan(plan)
  if (length(sched$schedule) == 0) return(NULL)

  do.call(rbind, lapply(seq_along(sched$schedule), function(k) {
    s <- sched$schedule[[k]]
    data.frame(
      agent_id   = i,
      employment = pop$employment[i],
      pt_sub     = pop$pt_sub[i],
      act_num    = k,
      activity   = s$activity,
      start_h    = s$start_h,
      end_h      = s$end_h,
      duration_h = s$duration_h,
      # Mode for this activity (tour-consistent from Part 2)
      mode       = if (length(plan$modes) >= k) plan$modes[k] else "unknown",
      stringsAsFactors = FALSE
    )
  }))
}))

cat("\n=== schedule_test: scheduled activities across first", N_TEST, "agents ===\n")
print(head(schedule_test, 15))
cat(sprintf("\nTotal activities scheduled : %d\n", nrow(schedule_test)))
cat(sprintf("Activities starting before 6h : %d\n",
    sum(schedule_test$start_h < 6)))
cat(sprintf("Activities starting after 20h : %d\n",
    sum(schedule_test$start_h > 20)))
cat(sprintf("Activities overflowing 24h    : %d\n",
    sum(schedule_test$end_h > 24)))
cat("\n")

# =============================================================================
# RUN PART 5: Schedule all agent plans
# =============================================================================

cat("Scheduling activity start times for all agents...\n")

final_plans <- lapply(agent_plans_p4, schedule_plan)

cat("Done.\n\n")

# Count discarded activities (overflow 24h)
n_sched  <- sum(sapply(final_plans, function(p) length(p$schedule)))
n_total  <- sum(sapply(final_plans, function(p) length(unlist(p$activities))))
n_discard <- n_total - n_sched
cat(sprintf("Activities scheduled  : %d\n", n_sched))
cat(sprintf("Activities discarded  : %d  (%.3f per mille)  [Paper: 0.06 per mille]\n",
    n_discard, 1000 * n_discard / max(n_total, 1)))
cat("\n")

# =============================================================================
# VALIDATION SUMMARY — Table 4 analog
# =============================================================================

cat("=== SYSTEM-WIDE VALIDATION SUMMARY (Table 4 analog) ===\n")
cat(sprintf("N agents simulated          : %d\n",   N_AGENTS))
cat(sprintf("Mean number of tours        : %.2f  [Paper: 1.49]\n",
    mean(sapply(final_plans, function(p) p$n_tours))))
cat(sprintf("Mean number of trips        : %.2f  [Paper: 3.76]\n",
    mean(sapply(final_plans, function(p) p$n_trips))))
cat(sprintf("Mean work trips             : %.2f  [Paper: 0.51]\n",
    mean(sapply(final_plans, function(p) p$n_work * 2))))
cat(sprintf("Mean leisure trips          : %.2f  [Paper: 0.72]\n",
    mean(sapply(final_plans, function(p)
      sum(unlist(p$activities) == "Leisure") * 2))))
cat(sprintf("Mean travel time [h]        : %.2f  [Paper: 1.52]\n",
    mean(sapply(final_plans, function(p) p$total_travel))))
cat(sprintf("Mean activity duration [h]  : %.2f  [Paper: 5.92]\n",
    mean(sapply(final_plans, function(p) p$total_dur))))
cat(sprintf("Mean out-of-home time [h]   : %.2f  [Paper: 7.44]\n",
    mean(sapply(final_plans, function(p) p$ooh))))
cat("========================================================\n\n")

# =============================================================================
# EXTRACT SCHEDULE DATA FRAME FOR PLOTS
# =============================================================================

extract_schedule_df <- function(plans, pop) {
  rows <- list()
  for (i in seq_along(plans)) {
    plan <- plans[[i]]
    if (is.null(plan$schedule) || length(plan$schedule) == 0) next
    # [FIX UA-8] Use per-activity mode (indexed by activity position k_act)
    # instead of always taking plan$modes[1] for every activity in the schedule.
    k_act <- 0L
    for (s in plan$schedule) {
      k_act <- k_act + 1L
      rows[[length(rows)+1]] <- data.frame(
        agent_id   = i,
        activity   = s$activity,
        start_h    = s$start_h,
        end_h      = min(s$end_h, 24),
        duration_h = s$duration_h,
        employment = pop$employment[i],
        pt_sub     = pop$pt_sub[i],
        mode       = if (length(plan$modes) >= k_act) plan$modes[k_act] else "unknown",
        stringsAsFactors = FALSE
      )
    }
  }
  bind_rows(rows)
}

sched_df <- extract_schedule_df(final_plans, pop)

# =============================================================================
# PLOTS
# =============================================================================

# ---- Plot 1: Activity start-time distributions (Figure 3 analog) ----
p1 <- sched_df %>%
  filter(activity %in% c("Work","Education","Leisure","Shopping")) %>%
  ggplot(aes(x = start_h, fill = activity, colour = activity)) +
  geom_density(alpha = 0.3, linewidth = 0.8) +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 24)) +
  labs(
    title    = "Figure 3 analog: Activity start-time distributions",
    subtitle = paste0("N = ", N_AGENTS, " agents"),
    x = "Hour of the day", y = "Density",
    fill = "Activity", colour = "Activity"
  ) +
  theme_minimal(base_size = 13)
print(p1)

# ---- Plot 2: Work duration CDF by employment type (Figure 2 analog) ----
work_durs <- lapply(1:N_AGENTS, function(i) {
  plan <- final_plans[[i]]
  acts <- unlist(plan$activities)
  durs <- if (length(plan$durations) <= 1)
            rep(plan$durations, length(acts))
          else plan$durations
  idx  <- which(acts == "Work")
  if (length(idx) == 0) return(NULL)
  data.frame(employment = pop$employment[i],
             n_work     = length(idx),
             duration   = durs[idx[1]])
}) %>% bind_rows()

if (!is.null(work_durs) && nrow(work_durs) > 0) {
  p2 <- work_durs %>%
    mutate(segment = paste0(employment, " (", n_work, " W-act)")) %>%
    ggplot(aes(x = duration, colour = segment)) +
    stat_ecdf(linewidth = 0.9) +
    scale_x_continuous(limits = c(0, 14), breaks = 0:14) +
    labs(
      title  = "Figure 2 analog: Work duration cumulative distributions",
      x = "Activity duration [hours]", y = "Cumulative probability",
      colour = "Segment"
    ) +
    theme_minimal(base_size = 13)
  print(p2)
}

# ---- Plot 3: Mode shares by PT subscription type (Figure 4 analog) ----
mode_df <- lapply(1:N_AGENTS, function(i) {
  plan <- final_plans[[i]]
  data.frame(
    mode   = if (length(plan$modes) > 0) plan$modes else "walk",
    pt_sub = pop$pt_sub[i],
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

if (nrow(mode_df) > 0) {
  p3 <- mode_df %>%
    count(pt_sub, mode) %>%
    group_by(pt_sub) %>%
    mutate(share = n / sum(n)) %>%
    ungroup() %>%
    mutate(pt_sub = factor(pt_sub,
             levels = c("none","halbtax","verbund","general"),
             labels = c("No Sub","Halbtax-Abo","Verbund-Abo","General-Abo"))) %>%
    ggplot(aes(x = pt_sub, y = share, fill = mode)) +
    geom_col(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title    = "Figure 4 analog: Mode shares by PT subscription type",
      subtitle = "Share of trips by mode (tour-level mode consistency applied)",
      x = "PT Subscription", y = "Share", fill = "Mode"
    ) +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
  print(p3)
}

# ---- Plot 4: Gantt chart — first 10 agents ----
act_colors <- c(Work      = "steelblue",
                Education = "royalblue",
                Leisure   = "forestgreen",
                Shopping  = "orange",
                Business  = "purple",
                Accompany = "pink",
                Other     = "grey60")

p4 <- sched_df %>%
  filter(agent_id <= 10) %>%
  mutate(agent_label = factor(paste("Agent", agent_id))) %>%
  ggplot(aes(xmin = start_h, xmax = end_h,
             ymin = as.numeric(agent_label) - 0.4,
             ymax = as.numeric(agent_label) + 0.4,
             fill = activity)) +
  geom_rect(colour = "white", linewidth = 0.3) +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 24)) +
  scale_y_continuous(breaks = 1:10, labels = paste("Agent", 1:10)) +
  scale_fill_manual(values = act_colors, na.value = "grey80") +
  labs(title = "Agent day plans — first 10 agents (Gantt chart)",
       x = "Hour of day", y = "", fill = "Activity") +
  theme_minimal(base_size = 13)
print(p4)

# ---- Plot 5: Out-of-home time distribution vs paper benchmark ----
ooh_vals <- sapply(final_plans, function(p) p$ooh)

p5 <- ggplot(data.frame(ooh = ooh_vals), aes(x = ooh)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20,
                 fill = "steelblue", alpha = 0.7, colour = "white") +
  geom_density(colour = "steelblue", linewidth = 1) +
  geom_vline(xintercept = 7.44, linetype = "dashed",
             colour = "red",  linewidth = 1) +
  geom_vline(xintercept = mean(ooh_vals), linetype = "solid",
             colour = "navy", linewidth = 1) +
  annotate("text", x = 7.44 + 0.15, y = Inf, vjust = 1.5, hjust = 0,
           label = "Paper: 7.44h", colour = "red",  size = 3.5) +
  annotate("text", x = mean(ooh_vals) + 0.15, y = Inf, vjust = 3.5, hjust = 0,
           label = sprintf("Simulated: %.2fh", mean(ooh_vals)),
           colour = "navy", size = 3.5) +
  labs(title = "Out-of-home time distribution",
       x = "Out-of-home time [hours]", y = "Density") +
  theme_minimal(base_size = 13)
print(p5)

# ---- Plot 6: Tour composition by employment type ----
tour_comp <- lapply(1:N_AGENTS, function(i) {
  plan <- final_plans[[i]]
  data.frame(employment = pop$employment[i],
             n_work  = plan$n_work,
             n_edu   = plan$n_edu,
             n_bus   = plan$n_bus,
             n_other = plan$n_other)
}) %>% bind_rows() %>%
  group_by(employment) %>%
  summarise(across(starts_with("n_"), mean), .groups = "drop") %>%
  pivot_longer(cols = starts_with("n_"),
               names_to = "tour_type", values_to = "mean_tours") %>%
  mutate(tour_type = recode(tour_type,
    n_work  = "Work",
    n_edu   = "Education",
    n_bus   = "Business",
    n_other = "Other"
  ))

p6 <- ggplot(tour_comp, aes(x = employment, y = mean_tours, fill = tour_type)) +
  geom_col(position = "stack") +
  labs(
    title = "Mean number of tours by type and employment status",
    x = "Employment", y = "Mean number of tours", fill = "Tour type"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
print(p6)

# =============================================================================
# SAVE FINAL OUTPUT
# =============================================================================

save(pop, final_plans, sched_df, N_AGENTS,
     file = "final_plans.RData")

cat("\nPart 5 complete. Six plots produced.\n")
cat("Final results saved to final_plans.RData\n")
