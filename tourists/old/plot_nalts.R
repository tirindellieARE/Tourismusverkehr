suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

dt <- fread("results_output/nalts_n10000_results.csv")
dt <- unique(dt, by = c("n_alts", "param"))
dt <- dt[param != "beta_tt_other" & param != "beta_tt_FR"]

# nicer labels
dt[, label := fcase(
  param == "beta_tt_DE",  "Travel time: DE",
  param == "beta_tt_AT",  "Travel time: AT",
  param == "beta_tt_IT",  "Travel time: IT",
  param == "beta_topo2",  "Topology: hilly",
  param == "beta_topo3",  "Topology: mountain"
)]
dt[, group := ifelse(grepl("topo", param), "Topology", "Travel time")]

# --- Plot 1: raw coefficients ---
p1 <- ggplot(dt, aes(x = n_alts, y = estimate, colour = label, group = label)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  facet_wrap(~ group, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = seq(300, 1000, 100)) +
  labs(
    title    = "MNL coefficients vs. number of sampled alternatives",
    subtitle = "10,000 agents, seed = 42",
    x        = "Number of alternatives",
    y        = "Coefficient estimate",
    colour   = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

ggsave("results_output/plot_coefficients_vs_nalts.png", p1,
       width = 8, height = 7, dpi = 150)

# --- Plot 2: % change relative to N_ALTS = 300 ---
base <- dt[n_alts == 300, .(param, label, group, base = estimate)]
pct  <- merge(dt, base[, .(param, base)], by = "param")
pct[, delta_pct := 100 * (estimate - base) / abs(base)]

p2 <- ggplot(pct, aes(x = n_alts, y = delta_pct, colour = label, group = label)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  facet_wrap(~ group, ncol = 1) +
  scale_x_continuous(breaks = seq(300, 1000, 100)) +
  labs(
    title    = "% change in coefficients relative to 300 alternatives",
    subtitle = "10,000 agents, seed = 42",
    x        = "Number of alternatives",
    y        = "% change vs. 300 alts",
    colour   = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

ggsave("results_output/plot_pctchange_vs_nalts.png", p2,
       width = 8, height = 7, dpi = 150)

cat("Saved:\n  results_output/plot_coefficients_vs_nalts.png\n  results_output/plot_pctchange_vs_nalts.png\n")
