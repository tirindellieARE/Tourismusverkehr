# =============================================================================
# figures5_6.R
# Replication of Figures 5 and 6 from Danalet et al. (2023), STRC paper:
#
#   Figure 5: Shares of the different main transport modes for foreign tourists
#             in Switzerland (weighted, TMS 2017, foreign only)
#
#   Figure 6: Shares of the different main transport modes for foreign tourists
#             in Switzerland by countries of origin (top 9 by sample size)
#
# DATA: data/TMS 2017.sav
#   means_of_transport_in_CH: 1=Train, 2=Bus(light), 3=Car, 4=Camper,
#                              5=Motorcycle, 6=Long-dist bus, 7=Tour bus,
#                              8=Bicycle, 9=Other
#   origin_country: numeric BFS code; 212 = Switzerland (excluded)
#   weighting_factor: survey expansion weight
#
# NOTE: Paper groups motorcycle (5) with car (3) and camper (4) with other (9)
#   when describing mode shares. These recodings are applied here so totals
#   match the paper (car ≈ 48%, train ≈ 36%, light rail/bus ≈ 7%).
#
# OUTPUTS: plots/figure5_mode_shares.pdf
#          plots/figure6_mode_shares_by_country.pdf
# =============================================================================

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)

dir.create("plots", showWarnings = FALSE)

# =============================================================================
# 1. LOAD AND PREPARE TMS DATA
# =============================================================================

tms_raw <- read_sav("data/TMS 2017.sav")

# Exclude Swiss residents (origin_country == 212)
tms <- tms_raw %>%
  filter(origin_country != 212) %>%
  mutate(
    mode_raw  = as.integer(means_of_transport_in_CH),
    country   = as.integer(origin_country),
    weight    = as.numeric(weighting_factor)
  ) %>%
  filter(!is.na(mode_raw), !is.na(weight))

# Mode recoding: motorcycle → car; camper → other (as per paper model)
tms <- tms %>%
  mutate(
    mode = case_when(
      mode_raw == 5L ~ 3L,   # motorcycle → car
      mode_raw == 4L ~ 9L,   # camper/caravan → other
      TRUE           ~ mode_raw
    )
  )

# Mode labels (7 modelled alternatives after recoding)
MODE_LABELS <- c(
  "1" = "Train",
  "2" = "Light rail / public bus",
  "3" = "Car (incl. motorcycle)",
  "6" = "Long-distance bus",
  "7" = "Tour bus",
  "8" = "Bicycle",
  "9" = "Other (incl. camper)"
)

# Colour palette: one colour per mode; train = SBB red, car = grey, others distinct
MODE_COLOURS <- c(
  "Train"                    = "#CC0000",
  "Car (incl. motorcycle)"   = "#808080",
  "Light rail / public bus"  = "#2171B5",
  "Tour bus"                 = "#6BAED6",
  "Long-distance bus"        = "#08519C",
  "Bicycle"                  = "#41AB5D",
  "Other (incl. camper)"     = "#BDBDBD"
)

MODES_ORDERED <- c(
  "Car (incl. motorcycle)",
  "Train",
  "Light rail / public bus",
  "Tour bus",
  "Other (incl. camper)",
  "Bicycle",
  "Long-distance bus"
)

# =============================================================================
# 2. FIGURE 5 — overall weighted mode shares
# =============================================================================

fig5_data <- tms %>%
  group_by(mode) %>%
  summarise(w_sum = sum(weight), .groups = "drop") %>%
  mutate(
    share      = w_sum / sum(w_sum),
    mode_label = factor(MODE_LABELS[as.character(mode)],
                        levels = rev(MODES_ORDERED))
  ) %>%
  filter(!is.na(mode_label))

cat("Figure 5 — weighted mode shares:\n")
print(fig5_data %>%
  arrange(desc(share)) %>%
  select(mode_label, share) %>%
  mutate(share_pct = round(100 * share, 1)))

fig5 <- ggplot(fig5_data, aes(x = share, y = mode_label,
                               fill = as.character(mode_label))) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(100 * share, 1), "%")),
            hjust = -0.12, size = 3.5, colour = "grey30") +
  scale_fill_manual(values = MODE_COLOURS) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.58),
    expand = c(0, 0)
  ) +
  labs(
    title    = "Figure 5: Main transport mode shares",
    subtitle = "Foreign tourists in Switzerland (TMS 2017, weighted)",
    x        = "Share of tourists",
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(colour = "grey40", size = 9),
    axis.text.y        = element_text(size = 10)
  )

ggsave("plots/figure5_mode_shares.pdf", fig5,
       width = 7, height = 4, device = "pdf")
cat("Saved: plots/figure5_mode_shares.pdf\n\n")

# =============================================================================
# 3. FIGURE 6 — mode shares by country of origin (top 9 countries)
# =============================================================================

# Top 9 countries by unweighted observation count (foreign only)
top9_codes <- tms %>%
  count(country, sort = TRUE) %>%
  slice_head(n = 9) %>%
  pull(country)

# Country labels from SPSS value attributes
oc_labels <- attr(tms_raw$origin_country, "labels")
get_country_label <- function(code) {
  lab <- names(oc_labels)[oc_labels == code]
  if (length(lab) > 0) lab[1] else as.character(code)
}

country_names <- setNames(
  sapply(top9_codes, get_country_label),
  top9_codes
)

# Shorten labels for axis
country_names["3049"]  <- "India"          # already short
country_names["4029"]  <- "USA"
country_names["314"]   <- "United Kingdom"
country_names["3008"]  <- "China"
country_names["243"]   <- "Spain"
country_names["111"]   <- "Netherlands"
country_names["44"]    <- "France"
country_names["17"]    <- "Germany"
country_names["74"]    <- "Italy"

# Compute weighted mode share per country
fig6_data <- tms %>%
  filter(country %in% top9_codes) %>%
  mutate(country_label = country_names[as.character(country)]) %>%
  group_by(country_label, mode) %>%
  summarise(w_sum = sum(weight), .groups = "drop") %>%
  group_by(country_label) %>%
  mutate(share = w_sum / sum(w_sum)) %>%
  ungroup() %>%
  mutate(
    mode_label = factor(MODE_LABELS[as.character(mode)],
                        levels = MODES_ORDERED)
  ) %>%
  filter(!is.na(mode_label))

# Order countries by train share descending (shows the car vs train contrast)
country_train_share <- fig6_data %>%
  filter(mode_label == "Train") %>%
  arrange(desc(share)) %>%
  pull(country_label)

fig6_data <- fig6_data %>%
  mutate(country_label = factor(country_label, levels = country_train_share))

cat("Figure 6 — mode shares by country (top 9, train share ascending order):\n")
print(
  fig6_data %>%
    select(country_label, mode_label, share) %>%
    mutate(share_pct = round(100 * share, 1)) %>%
    pivot_wider(names_from = mode_label, values_from = share_pct) %>%
    arrange(country_label),
  n = 20
)

fig6 <- ggplot(fig6_data,
               aes(x = share, y = country_label, fill = mode_label)) +
  geom_col(width = 0.7, position = "stack") +
  geom_text(
    data = fig6_data %>% filter(share >= 0.04),
    aes(label = paste0(round(100 * share), "%")),
    position = position_stack(vjust = 0.5),
    size = 2.8, colour = "white", fontface = "bold"
  ) +
  scale_fill_manual(values = MODE_COLOURS, name = "Main transport mode",
                    breaks = MODES_ORDERED) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.01),
    expand = c(0, 0)
  ) +
  labs(
    title    = "Figure 6: Main transport mode shares by country of origin",
    subtitle = "Top 9 countries — foreign tourists in Switzerland (TMS 2017, weighted)",
    x        = "Share of tourists",
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "bottom",
    legend.key.size    = unit(0.45, "cm"),
    legend.text        = element_text(size = 8.5),
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(colour = "grey40", size = 9),
    axis.text.y        = element_text(size = 10)
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave("plots/figure6_mode_shares_by_country.pdf", fig6,
       width = 8, height = 5.5, device = "pdf")
cat("Saved: plots/figure6_mode_shares_by_country.pdf\n\n")

cat("Done. Both figures saved in plots/\n")
