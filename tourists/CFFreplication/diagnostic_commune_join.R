setwd("E:/ARE/ProjekteTIE/Turismusverkehr/RScript/tourists")

library(haven)
library(dplyr)

# =============================================================================
# Load data
# =============================================================================

raw         <- read_sav("data/TMS 2017.sav")
commune_ref <- read.csv("data/commune_type_region.csv", stringsAsFactors = FALSE,
                        encoding = "UTF-8")

# Rename CSV region to amr_region to avoid collision with TMS region column
commune_ref <- commune_ref %>% rename(amr_region = region)

cat("=== TMS file ===\n")
cat(sprintf("Rows: %d  |  Columns: %d\n", nrow(raw), ncol(raw)))

cat("\n=== Commune reference CSV ===\n")
cat(sprintf("Rows: %d\n", nrow(commune_ref)))
cat(sprintf("Unique amr_region codes: %d  (expected 16)\n", n_distinct(commune_ref$amr_region)))

# =============================================================================
# Verify commune_name uniqueness in the reference CSV
# =============================================================================

cat("\n=== Commune name uniqueness in CSV ===\n")
n_rows           <- nrow(commune_ref)
n_distinct_names <- n_distinct(commune_ref$commune_name)
cat(sprintf("Total rows: %d  |  Distinct commune_name values: %d\n", n_rows, n_distinct_names))
if (n_rows == n_distinct_names) {
  cat("OK: every commune_name appears exactly once.\n")
} else {
  cat(sprintf("WARNING: %d duplicate entries found.\n", n_rows - n_distinct_names))
  dup_names <- commune_ref %>% group_by(commune_name) %>% filter(n() > 1) %>% arrange(commune_name)
  print(head(dup_names, 20))
}

# =============================================================================
# Name lookup table: TMS destination label -> CSV commune_name
# Covers: (1) English->German/French city names, (2) tourist resort/hamlet ->
# official post-merger commune, (3) minor spelling differences
# =============================================================================

name_lookup <- tribble(
  ~tms_name,                         ~csv_name,

  # --- English -> German/French ---
  "Zurich",                          "Zürich",
  "Lucerne",                         "Luzern",
  "Geneva",                          "Genève",

  # --- Tourist resort / hamlet -> official commune (mergers pre-2017) ---
  # Valais - Val de Bagnes
  "Verbier",                         "Val de Bagnes",
  "Bagnes",                          "Val de Bagnes",
  "Fionnay",                         "Val de Bagnes",
  "Versegères",                      "Val de Bagnes",
  "Lourtier",                        "Val de Bagnes",
  "Champex-Lac",                     "Orsières",
  "La Tzoumaz",                      "Riddes",
  "Ovronnaz",                        "Leytron",
  "Anzère",                          "Ayent",
  "Montana",                         "Crans-Montana",
  "Randogne",                        "Crans-Montana",
  "Chermignon",                      "Crans-Montana",

  # Valais - Anniviers
  "St. Luc",                         "Anniviers",
  "Zinal",                           "Anniviers",
  "Grimentz",                        "Anniviers",
  "Chandolin",                       "Anniviers",
  "St. Jean",                        "Anniviers",
  "Vissoie",                         "Anniviers",
  "Vercorin",                        "Anniviers",

  # Valais - Obergoms
  "Münster-Geschinen",               "Obergoms",
  "Reckingen-Gluringen",             "Obergoms",
  "Grafschaft",                      "Obergoms",
  "Oberwald",                        "Obergoms",
  "Obergesteln",                     "Obergoms",
  "Ulrichen",                        "Obergoms",
  "Blitzingen",                      "Obergoms",
  "Niederwald",                      "Obergoms",

  # Valais - Hérémences merged into Vex (01.01.2017)
  "Hérémences",                      "Vex",
  "Mase",                            "Vex",
  "Nax",                             "Vex",

  # Valais - Miège merged into Salgesch (01.01.2018, CSV uses new name)
  "Miège",                           "Salgesch",

  # Graubünden - Scuol (absorbed Ardez, Ftan, Guarda, Lavin, Ramosch, Sent,
  #              Sur En, Tarasp, Tschlin, Vnà, Vulpera in 2015)
  "Sent",                            "Scuol",
  "Ftan",                            "Scuol",
  "Ardez",                           "Scuol",
  "Guarda",                          "Scuol",
  "Tarasp",                          "Scuol",
  "Vulpera",                         "Scuol",
  "Sur En",                          "Scuol",
  "Lavin",                           "Scuol",
  "Ramosch",                         "Scuol",
  "Tschlin",                         "Scuol",
  "Vnà",                             "Scuol",

  # Graubünden - Surses (absorbed Savognin, Bivio, Tinizong-Rona etc. in 2016)
  "Savognin",                        "Surses",
  "Bivio",                           "Surses",
  "Tinizong-Rona",                   "Surses",
  "Riom-Parsonz",                    "Surses",
  "Cinuos-chel",                     "Trun",

  # Graubünden - Bergün Filisur (merged Bergün/Bravuogn + Filisur in 2017)
  "Bergün/Bravuogn",                 "Bergün Filisur",
  "Filisur",                         "Bergün Filisur",

  # Graubünden - other
  "Maloja",                          "Bregaglia",
  "Valbella",                        "Vaz/Obervaz",
  "Lenzerheide",                     "Vaz/Obervaz",
  "Klosters-Serneus",                "Klosters",
  "La Punt",                         "La Punt Chamues-ch",
  "Splügen",                         "Rheinwald",
  "Alvaneu Bad",                     "Albula/Alvra",
  "Lantsch / Lenz",                  "Lantsch/Lenz",
  "Vella",                           "Lumnezia",
  "Vrin",                            "Lumnezia",
  "Ilanz / Glion",                   "Ilanz/Glion",
  "Susch",                           "Zernez",

  # Bern - Lauterbrunnen (Wengen, Mürren are car-free villages within)
  "Wengen",                          "Lauterbrunnen",
  "Mürren",                          "Lauterbrunnen",
  "Kiental",                         "Reichenbach im Kandertal",

  # Bern - Saanen (Gstaad is the resort village)
  "Gstaad",                          "Saanen",

  # Glarus - Glarus Süd (absorbed Braunwald, Elm, Linthal in 2011)
  "Braunwald",                       "Glarus Süd",
  "Elm",                             "Glarus Süd",
  "Linthal",                         "Glarus Süd",

  # St. Gallen - Wildhaus-Alt St. Johann (merged 2013)
  "Wildhaus",                        "Wildhaus-Alt St. Johann",
  "Unterwasser",                     "Wildhaus-Alt St. Johann",
  "Alt St. Johann",                  "Wildhaus-Alt St. Johann",
  "Säntis-Schwägalp",                "Nesslau",
  "Gossau SG",                       "Gossau (SG)",

  # Schwyz - Ingenbohl (Brunnen is the lakeside district)
  "Brunnen",                         "Ingenbohl",
  "Stoos",                           "Morschach",
  "Sattel-Hochstuckli",              "Sattel",

  # Nidwalden - Kerns (Melchsee-Frutt is a plateau resort above)
  "Melchsee-Frutt",                  "Kerns",

  # Uri - Seedorf (Bauen merged 01.01.2016)
  "Bauen",                           "Seedorf (UR)",

  # Vaud - Ollon (Villars-sur-Ollon is the ski resort)
  "Villars-sur-Ollon",               "Ollon",
  "Les Diablerets",                  "Ormont-Dessus",
  "Torgon",                          "Val-d'Illiez",
  "Morgins",                         "Val-d'Illiez",
  "Champoussin",                     "Val-d'Illiez",
  "Blonay",                          "Blonay - Saint-Légier",
  "Cully",                           "Bourg-en-Lavaux",
  "Estavayer-le-Lac",                "Estavayer",
  "Saint Croix",                     "Sainte-Croix",
  "Breuleux",                        "Les Breuleux",
  "St. Aubin",                       "Saint-Aubin (FR)",

  # Fribourg
  "Schwarzsee",                      "Plaffeien",
  "Schmitten",                       "Schmitten (FR)",

  # Aargau
  "Bad Zurzach",                     "Zurzach",

  # Ticino
  "Tenero",                          "Tenero-Contra",
  "Gordevio",                        "Avegno Gordevio",
  "Avegno",                          "Avegno Gordevio",
  "Cugnasco",                        "Cugnasco-Gerra",
  "Magadino",                        "Gambarogno",
  "Vira (Gambarogno)",               "Gambarogno",
  "San Nazzaro",                     "Gambarogno",
  "Piazzogna",                       "Gambarogno",
  "Intragna",                        "Gambarogno",

  # Neuchâtel
  "Mosen",                           "Roggliswil",

  # Montreux area
  "Chernex",                         "Montreux",

  # Jura
  "Châtel-St-Denis",                 "Châtel-Saint-Denis",

  # Silvaplana
  "Champfèr",                        "Silvaplana",

  # Leuk
  "Susten",                          "Leuk",

  # Oberdorf NW (spacing)
  "Oberdorf NW",                     "Oberdorf (NW)",

  # Payerne (TMS drops final 'e')
  "Payern",                          "Payerne",

  # Affoltern im Emmental abbreviation
  "Affoltern i. E.",                 "Affoltern im Emmental",

  # Brione (Ticino, distinct from Brione (GR))
  "Brione",                          "Brione sopra Minusio"
)

# =============================================================================
# Build the corrected TMS commune name column
# =============================================================================

tms_foreign <- raw %>%
  filter(!is.na(means_of_transport_in_CH)) %>%
  mutate(
    commune_name_raw = as.character(as_factor(destination)),
    commune_name_tms = ifelse(
      commune_name_raw %in% name_lookup$tms_name,
      name_lookup$csv_name[match(commune_name_raw, name_lookup$tms_name)],
      commune_name_raw
    )
  )

cat(sprintf("\nTMS foreign tourist observations: %d\n", nrow(tms_foreign)))

# =============================================================================
# Join
# =============================================================================

merged <- tms_foreign %>%
  left_join(commune_ref, by = c("commune_name_tms" = "commune_name"))

n_total     <- nrow(tms_foreign)
n_unmatched <- sum(is.na(merged$urban_rural_topology))
n_inflated  <- nrow(merged) - n_total

cat(sprintf("Rows after join: %d (inflation from duplicates: %d)\n", nrow(merged), n_inflated))
cat(sprintf("Matched:   %d / %d (%.1f%%)\n",
    n_total - n_unmatched, n_total, 100 * (n_total - n_unmatched) / n_total))
cat(sprintf("Unmatched: %d / %d (%.1f%%)\n",
    n_unmatched, n_total, 100 * n_unmatched / n_total))

# Remaining unmatched
unmatched_communes <- tms_foreign %>%
  filter(!commune_name_tms %in% commune_ref$commune_name) %>%
  count(commune_name_raw, commune_name_tms, sort = TRUE)

cat(sprintf("\nDistinct remaining unmatched names: %d\n", nrow(unmatched_communes)))
cat("All remaining unmatched:\n")
print(unmatched_communes, n = Inf)

# =============================================================================
# Distributions
# =============================================================================

cat("\n--- Urban/rural typology (matched obs) ---\n")
print(table(merged$urban_rural_topology, useNA = "always"))

cat("\n--- AMR region (matched obs) ---\n")
print(sort(table(merged$amr_region, useNA = "always"), decreasing = TRUE))

cat("\n=== Done ===\n")
