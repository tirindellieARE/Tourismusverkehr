# =============================================================================
# Tourist Mode Choice — PART 3 of 5
# Danalet et al. (2023), STRC Conference Paper
#
# Covers:
#   Application of the estimated MNL (Part 1) to assign an excursion mode
#   to each synthetic tourist agent (Part 2).
#
#   Utilities are computed directly from coef_table rather than using
#   predict(mnl_model), which requires the original data structure.
#   This approach is more robust when applying the model to a new population.
#
#   [ASSUMPTION – A-4] The assigned mode is the mode used for the main
#   excursion leg (hotel → excursion area). The return leg uses the same mode
#   (symmetric round trip). Sub-excursion legs within the day plan are on foot
#   or bicycle [INVENTED PARAMETER — see Part 4 for within-excursion modes].
#
#   [ASSUMPTION – A-5] Tour bus (mode 7) agents are on a fixed group itinerary
#   and do not receive individual activity chains. They are flagged group_tour=TRUE,
#   excluded from Part 4 activity simulation, but counted in Part 5 OD matrices
#   at their hotel zone → excursion zone OD pair.
#
# INPUTS:  part1_output.RData, part2_output.RData
# OUTPUTS: part3_output.RData
#   - agent_modes : data.frame with agent_id, chosen_mode, mode_probs, group_tour
#
# Run next: part4_tourist_activity_simulation.R
# =============================================================================

library(dplyr)
library(tibble)

set.seed(42)

# =============================================================================
# RUNTIME FLAGS
# =============================================================================

message("[ASSUMPTION – Part 3] Mode assignment uses direct utility computation",
        " from coef_table (not predict()). LOS terms set to 0 if no skim available.")
message("[ASSUMPTION – Part 3] Excursion mode = main hotel→destination leg.",
        " Return trip uses the same mode. Within-day local legs are not modelled here.")
message("[ASSUMPTION – Part 3] Tour bus agents (mode 7) assigned group_tour=TRUE",
        " and excluded from individual activity simulation in Part 4.")

# =============================================================================
# 1. LOAD INPUTS
# =============================================================================

load("part1_output.RData")   # coef_table, ALT_CODES, REF_ALT, NATIONALITY_MAP, NAT_GROUPS
load("part2_output.RData")   # tourist_pop, SCALE_FACTOR, zone_attrs

cat(sprintf("Agents loaded: %d\n", nrow(tourist_pop)))
cat(sprintf("Alternatives : %s  (reference = %s)\n\n",
    paste(ALT_CODES, collapse=", "), REF_ALT))

# =============================================================================
# 2. BUILD UTILITY FUNCTION
#
# For each agent, compute V_a for each alternative a using coef_table.
# The coefficient naming convention from mlogit follows:
#   "<alternative>:(Intercept)"  → ASC for alternative a
#   "<alternative>:<variable>"   → coefficient for variable interacted with alt a
#   "<variable>"                 → generic coefficient (same across all alts)
# =============================================================================

softmax <- function(v) {
  v <- v - max(v)
  exp(v) / sum(exp(v))
}

compute_utilities <- function(agent, coef_table, alt_codes, ref_alt) {
  non_ref <- setdiff(alt_codes, ref_alt)

  V <- setNames(rep(0.0, length(alt_codes)), alt_codes)

  # mlogit names coefficients as "variable:alternative" (e.g. "nat_germany:1").
  # All lookups below use exact string matching against coef_table$term.
  for (a in non_ref) {
    # ASC: term is "(Intercept):<alt>"
    asc_row <- coef_table[coef_table$term == paste0("(Intercept):", a), ]
    if (nrow(asc_row) > 0) V[a] <- V[a] + asc_row$estimate[1]

    # Nationality: term is "nat_<group>:<alt>"
    nat_g   <- agent$nationality_group
    nat_row <- coef_table[coef_table$term == paste0("nat_", nat_g, ":", a), ]
    if (nrow(nat_row) > 0) V[a] <- V[a] + nat_row$estimate[1]

    # Urban: term is "urban:<alt>"
    urb_row <- coef_table[coef_table$term == paste0("urban:", a), ]
    if (nrow(urb_row) > 0) V[a] <- V[a] + urb_row$estimate[1] * agent$urban

    # Accommodation type dummies (reference = hotel; all three = 0 for hotel tourists).
    # Coefficients for camping/holiday_home/collective are invented placeholders —
    # see Part 1 section 7b. Replace when finer TMS sub-codes are available.
    for (accom_var in c("accom_camping", "accom_holiday_home", "accom_collective")) {
      accom_row <- coef_table[coef_table$term == paste0(accom_var, ":", a), ]
      if (nrow(accom_row) > 0)
        V[a] <- V[a] + accom_row$estimate[1] * agent[[accom_var]]
    }

    # [ASSUMPTION] No LOS skims: travel_time / travel_cost contribute 0.
    # When LOS data is added, look up "travel_time" and "travel_cost" (generic
    # coefficients, no alternative suffix) and multiply by agent$tt_a / agent$cost_a.
  }

  V
}

# =============================================================================
# 3. ASSIGN MODE TO EACH AGENT
# =============================================================================

cat("Assigning excursion modes...\n")

mode_results <- lapply(seq_len(nrow(tourist_pop)), function(i) {
  agent <- tourist_pop[i, ]
  V     <- compute_utilities(agent, coef_table, ALT_CODES, REF_ALT)
  probs <- softmax(V)
  chosen <- sample(ALT_CODES, 1, prob = probs)

  list(
    agent_id      = agent$agent_id,
    chosen_mode   = chosen,
    group_tour    = (chosen == "7"),
    prob_train    = probs["1"],
    prob_bus      = probs["2"],
    prob_car      = probs["3"],
    prob_ldbus    = probs["6"],
    prob_tourbus  = probs["7"],
    prob_bicycle  = probs["8"],
    prob_other    = probs["9"]
  )
})

agent_modes <- dplyr::bind_rows(lapply(mode_results, as.data.frame))

cat("Done.\n\n")

# =============================================================================
# 4. VALIDATION SUMMARY
# =============================================================================

mode_labels <- c(
  "1" = "Train", "2" = "Bus", "3" = "Car", "6" = "Long-dist bus",
  "7" = "Tour bus", "8" = "Bicycle", "9" = "Other"
)

n_total  <- nrow(agent_modes)
n_group  <- sum(agent_modes$group_tour)

cat("=== Mode Assignment Summary ===\n")
cat(sprintf("Total agents          : %d\n", n_total))
cat(sprintf("Group tour (mode 7)   : %d (%.1f%%) — excluded from activity simulation\n",
    n_group, 100 * n_group / n_total))
cat("\nMode shares (all agents):\n")
mode_counts <- table(agent_modes$chosen_mode)
for (a in ALT_CODES) {
  n_a <- ifelse(a %in% names(mode_counts), mode_counts[a], 0)
  cat(sprintf("  %s (%s): %d (%.1f%%)\n",
      a, mode_labels[a], n_a, 100 * n_a / n_total))
}

cat("\nMode shares by nationality group:\n")
mode_by_nat <- dplyr::left_join(agent_modes, tourist_pop[, c("agent_id", "nationality_group")],
                                 by = "agent_id") %>%
  dplyr::group_by(nationality_group, chosen_mode) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::group_by(nationality_group) %>%
  dplyr::mutate(share = n / sum(n))
print(as.data.frame(mode_by_nat))
cat("\n")

# =============================================================================
# 5. SAVE
# =============================================================================

save(agent_modes, tourist_pop, SCALE_FACTOR, NATIONALITY_MAP, NAT_GROUPS,
     ALT_CODES, REF_ALT, zone_attrs,
     file = "part3_output.RData")

cat("Part 3 complete. Output saved to part3_output.RData\n")
cat("Run part4_tourist_activity_simulation.R next.\n")
