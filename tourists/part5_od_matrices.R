# =============================================================================
# Tourist Mode Choice — PART 5 of 5
# Danalet et al. (2023), STRC Conference Paper
#
# Covers:
#   Aggregation of synthetic tourist trips to OD demand matrices by mode.
#   Each synthetic agent represents 1/SCALE_FACTOR real tourists; trip counts
#   are weighted accordingly.
#
#   Trip extraction:
#     - For each agent × excursion, one round-trip is extracted:
#         origin  = excursion_zone (hotel zone)
#         dest    = excursion_zone (same, pending destination model)
#         mode    = chosen_mode from Part 3
#         weight  = 1 / SCALE_FACTOR
#     - Tour bus (group_tour = TRUE) agents: counted as one trip in mode "7"
#       at their hotel zone, with dest = hotel_zone (self-loop, group itinerary).
#
#   Output matrices:
#     - One matrix per mode: zone × zone, entries = weighted trip count
#     - One summary matrix: all modes combined
#     - Long-format CSV for GIS/external analysis
#
# INPUTS:  part3_output.RData, part4_output.RData
# OUTPUTS: part5_output.RData, plots/, od_matrices.csv
#
# =============================================================================

# [ASSUMPTION] Since Part 4 does not implement a tourist destination choice model,
# all excursion trips are self-loops (origin = dest = hotel zone). To produce
# meaningful OD matrices, a destination model must be added to Part 4 or a
# post-hoc assignment can distribute trips to surrounding zones using attraction weights.

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

set.seed(42)

# =============================================================================
# RUNTIME FLAGS
# =============================================================================

message("[ASSUMPTION – Part 5] All excursion trips are self-loops (origin = dest =",
        " hotel zone) because Part 4 does not implement tourist destination choice.",
        " Add destination model to Part 4 to produce meaningful spatial OD patterns.")
message("[NOTE – Part 5] Tour bus agents (mode 7) are included as a single trip",
        " per agent per excursion, with origin = dest = hotel zone.")

# =============================================================================
# 1. LOAD INPUTS
# =============================================================================

load("part3_output.RData")  # agent_modes, tourist_pop, SCALE_FACTOR
load("part4_output.RData")  # tourist_plans, pop_ind, N_AGENTS

cat(sprintf("Loaded %d individual agents (Part 4).\n", N_AGENTS))
cat(sprintf("Scale factor: %.4f  (1 agent = %.0f real tourists)\n\n",
    SCALE_FACTOR, 1 / SCALE_FACTOR))

# =============================================================================
# 2. EXTRACT TRIP LIST FROM INDIVIDUAL AGENTS
# =============================================================================

cat("Extracting trip list from individual agent plans...\n")

trip_rows <- list()

for (tp in tourist_plans) {
  for (dp in tp$day_plans) {
    for (ex in dp$excursions) {
      if (is.null(ex)) next
      trip_rows[[length(trip_rows) + 1]] <- data.frame(
        agent_id       = tp$agent_id,
        day            = dp$day,
        nationality_group = tp$nationality_group,
        mode           = ex$mode,
        origin_zone    = as.character(ex$excursion_zone),
        dest_zone      = as.character(ex$excursion_zone),  # self-loop until dest model added
        ooh_h          = ex$ooh,
        n_activities   = length(ex$activities),
        weight         = 1 / SCALE_FACTOR,
        stringsAsFactors = FALSE
      )
    }
  }
}

# =============================================================================
# 3. ADD GROUP TOUR (mode 7) TRIPS
# =============================================================================

group_agents <- dplyr::left_join(
  agent_modes[agent_modes$group_tour, c("agent_id", "chosen_mode")],
  tourist_pop[, c("agent_id", "excursion_zone", "nationality_group", "n_nights")],
  by = "agent_id"
)

for (i in seq_len(nrow(group_agents))) {
  ga <- group_agents[i, ]
  for (d in seq_len(ga$n_nights)) {
    trip_rows[[length(trip_rows) + 1]] <- data.frame(
      agent_id       = ga$agent_id,
      day            = d,
      nationality_group = ga$nationality_group,
      mode           = ga$chosen_mode,
      origin_zone    = as.character(ga$excursion_zone),
      dest_zone      = as.character(ga$excursion_zone),
      ooh_h          = NA_real_,
      n_activities   = NA_integer_,
      weight         = 1 / SCALE_FACTOR,
      stringsAsFactors = FALSE
    )
  }
}

trip_list <- dplyr::bind_rows(trip_rows)
cat(sprintf("Trip list: %d synthetic trips (%.0f weighted trips).\n\n",
    nrow(trip_list), sum(trip_list$weight)))

# =============================================================================
# 4. BUILD OD MATRICES PER MODE
# =============================================================================

MODE_LABELS <- c(
  "1" = "Train", "2" = "Bus", "3" = "Car",
  "6" = "Long-dist bus", "7" = "Tour bus", "8" = "Bicycle", "9" = "Other"
)

all_zones <- sort(unique(c(trip_list$origin_zone, trip_list$dest_zone)))
n_zones   <- length(all_zones)
cat(sprintf("OD matrix dimensions: %d zones × %d zones.\n\n", n_zones, n_zones))

# Build one matrix per mode + one combined
od_matrices <- list()

for (m in c(names(MODE_LABELS), "all")) {
  if (m == "all") {
    trips_m <- trip_list
  } else {
    trips_m <- trip_list[trip_list$mode == m, ]
  }

  agg <- trips_m %>%
    dplyr::group_by(origin_zone, dest_zone) %>%
    dplyr::summarise(demand = sum(weight), .groups = "drop")

  mat <- matrix(0, nrow = n_zones, ncol = n_zones,
                dimnames = list(all_zones, all_zones))

  for (k in seq_len(nrow(agg))) {
    o <- agg$origin_zone[k]
    d <- agg$dest_zone[k]
    if (o %in% all_zones && d %in% all_zones) {
      mat[o, d] <- mat[o, d] + agg$demand[k]
    }
  }

  od_matrices[[m]] <- mat
}

cat("OD matrix totals by mode:\n")
for (m in names(MODE_LABELS)) {
  total <- sum(od_matrices[[m]])
  label <- MODE_LABELS[m]
  cat(sprintf("  Mode %s (%s): %.0f trips (%.1f%%)\n",
      m, label, total, 100 * total / sum(od_matrices[["all"]])))
}
cat(sprintf("  ALL modes: %.0f trips\n\n", sum(od_matrices[["all"]])))

# =============================================================================
# 5. EXPORT LONG-FORMAT CSV
# =============================================================================

od_long <- dplyr::bind_rows(lapply(names(MODE_LABELS), function(m) {
  mat <- od_matrices[[m]]
  expand.grid(origin_zone = rownames(mat), dest_zone = colnames(mat),
              stringsAsFactors = FALSE) %>%
    dplyr::mutate(mode = m, mode_label = MODE_LABELS[m],
                  demand = as.vector(mat)) %>%
    dplyr::filter(demand > 0)
}))

write.csv(od_long, "od_matrices.csv", row.names = FALSE)
cat(sprintf("Exported %d non-zero OD pairs to od_matrices.csv.\n\n",
    nrow(od_long)))

# =============================================================================
# 6. PLOTS
# =============================================================================

dir.create("plots", showWarnings = FALSE)

# --- 6a. Mode share bar chart ---
mode_shares <- trip_list %>%
  dplyr::group_by(mode) %>%
  dplyr::summarise(weighted_trips = sum(weight), .groups = "drop") %>%
  dplyr::mutate(
    mode_label = MODE_LABELS[mode],
    share      = weighted_trips / sum(weighted_trips)
  )

p_mode_share <- ggplot(mode_shares, aes(x = reorder(mode_label, -share), y = share)) +
  geom_col(fill = "#2C7BB6") +
  geom_text(aes(label = scales::percent(share, accuracy = 0.1)), vjust = -0.3, size = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Tourist Excursion Mode Shares",
       subtitle = paste0("N = ", scales::comma(round(sum(mode_shares$weighted_trips))),
                         " weighted trips"),
       x = NULL, y = "Share") +
  theme_minimal()

ggsave("plots/mode_shares.pdf", p_mode_share, width = 8, height = 5)
cat("Saved plots/mode_shares.pdf\n")

# --- 6b. Mode share by nationality ---
mode_by_nat <- trip_list %>%
  dplyr::group_by(nationality_group, mode) %>%
  dplyr::summarise(weighted_trips = sum(weight), .groups = "drop") %>%
  dplyr::group_by(nationality_group) %>%
  dplyr::mutate(share = weighted_trips / sum(weighted_trips),
                mode_label = MODE_LABELS[mode])

p_nat <- ggplot(mode_by_nat, aes(x = mode_label, y = share, fill = mode_label)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~nationality_group, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Mode Shares by Nationality Group",
       x = NULL, y = "Share") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/mode_shares_by_nationality.pdf", p_nat, width = 14, height = 9)
cat("Saved plots/mode_shares_by_nationality.pdf\n")

# --- 6c. Top OD pairs (combined) ---
top_od <- od_long %>%
  dplyr::filter(origin_zone != dest_zone) %>%
  dplyr::arrange(desc(demand)) %>%
  head(20)

if (nrow(top_od) > 0) {
  top_od <- top_od %>%
    dplyr::mutate(od_label = paste0(origin_zone, " → ", dest_zone))

  p_top <- ggplot(top_od, aes(x = reorder(od_label, demand), y = demand,
                               fill = mode_label)) +
    geom_col() +
    coord_flip() +
    labs(title = "Top 20 OD Pairs (all modes)",
         x = NULL, y = "Weighted trips", fill = "Mode") +
    theme_minimal()

  ggsave("plots/top_od_pairs.pdf", p_top, width = 10, height = 7)
  cat("Saved plots/top_od_pairs.pdf\n")
} else {
  cat("No cross-zone OD pairs found (all trips are self-loops).",
      "Add destination model to Part 4 to generate cross-zone OD pairs.\n")
}

# --- 6d. OD heatmap for car (example) ---
car_mat <- od_matrices[["3"]]
if (nrow(car_mat) <= 30) {
  car_long <- as.data.frame(as.table(car_mat)) %>%
    rename(origin_zone = Var1, dest_zone = Var2, demand = Freq)

  p_heat <- ggplot(car_long, aes(x = dest_zone, y = origin_zone, fill = demand)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "#D7191C",
                        name = "Trips", labels = scales::comma) +
    labs(title = "Car OD Matrix", x = "Destination zone", y = "Origin zone") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

  ggsave("plots/car_od_heatmap.pdf", p_heat, width = 10, height = 9)
  cat("Saved plots/car_od_heatmap.pdf\n")
} else {
  cat(sprintf("Car OD heatmap skipped (matrix too large: %d zones).\n", nrow(car_mat)))
}

cat("\n")

# =============================================================================
# 7. SUMMARY STATISTICS
# =============================================================================

cat("=== Final Summary ===\n")
cat(sprintf("Total weighted trips         : %.0f\n", sum(trip_list$weight)))
cat(sprintf("Mean ooh per excursion [h]  : %.2f\n",
    mean(trip_list$ooh_h, na.rm = TRUE)))
cat(sprintf("OD pairs (non-zero, all modes): %d\n", nrow(od_long)))
cat(sprintf("Cross-zone pairs             : %d",
    nrow(od_long[od_long$origin_zone != od_long$dest_zone, ])))
cat("  [0 expected until destination model added to Part 4]\n")
cat("\nMode totals (weighted trips):\n")
for (m in names(MODE_LABELS)) {
  cat(sprintf("  %s: %.0f\n", MODE_LABELS[m], sum(od_matrices[[m]])))
}
cat(sprintf("  Verification: ALL = %.0f (should match total trips)\n\n",
    sum(od_matrices[["all"]])))

# =============================================================================
# 8. SAVE
# =============================================================================

save(od_matrices, od_long, trip_list,
     file = "part5_output.RData")

cat("Part 5 complete. Output saved to part5_output.RData\n")
cat("OD matrices exported to od_matrices.csv\n")
cat("Plots saved to plots/\n")
