# =============================================================================
# Tourist Mode Choice — PART 2 of 5
# Danalet et al. (2023), STRC Conference Paper
#
# Covers:
#   Expansion of hotel data into a synthetic tourist agent population.
#   The paper notes lack of hotel-level destination data; the user provides it.
#
#   Hotel data format (one row per hotel):
#     hotel_id, x, y, accommodation_type, stars,
#     nights_<nationality_1>, nights_<nationality_2>, ...
#   (LV95 coordinates; one column per nationality group or one JSON column)
#
#   Each hotel × nationality combination produces:
#     round(n_nights × SCALE_FACTOR) synthetic agents
#   where SCALE_FACTOR = 0.01 [INVENTED PARAMETER] (1 agent per 100 nights).
#
# INPUTS:  part1_output.RData, data/hotels.csv, data/amr_zones.gpkg
# OUTPUTS: part2_output.RData
#   - tourist_pop  : data.frame of synthetic agents
#   - SCALE_FACTOR : numeric scalar used for OD matrix weighting in Part 5
#   - NATIONALITY_MAP, NAT_GROUPS : passed through for Part 3
#   - zone_attrs   : data.frame of zone attributes (urban/rural, region label)
#
# Run next: part3_mode_assignment.R
# =============================================================================

# SCALE_FACTOR: controls population thinning for computational convenience in Part 4's
# agent loop. Set to 0.01 (1 agent per 100 overnight stays) by default. The Python
# pipeline uses 1:1 (no thinning); Part 5 weights each agent by 1/SCALE_FACTOR to
# restore correct totals in OD matrices. Increase toward 1.0 for higher precision.

# [ASSUMPTION] Hotel zone is the tourist's excursion ORIGIN (replaces residential
# home zone from Scherr et al.). Tourists start each excursion from their hotel.

# [ASSUMPTION] Urban/rural typology comes from an AMR zone attributes table that
# must be user-supplied (data/amr_zones.gpkg or data/zone_attributes.csv).

library(dplyr)
library(tidyr)
library(tibble)
library(sf)

set.seed(42)

SCALE_FACTOR <- 0.01   # computational thinning: 1 agent per 100 overnight stays

# =============================================================================
# RUNTIME FLAGS
# =============================================================================

message("[NOTE – Part 2] SCALE_FACTOR = ", SCALE_FACTOR,
        " (1 agent per ", round(1 / SCALE_FACTOR), " overnight stays).",
        " Computational thinning for Part 4 loop. Part 5 reweights by 1/SCALE_FACTOR.")
message("[NOTE – Part 2] Age drawn from TMS distribution (17 bins, 18–98 in 5-year steps).",
        " Probabilities and age_cat mapping taken directly from the Python pipeline.")
message("[ASSUMPTION – Part 2] Hotel x/y (LV95) is joined to AMR regions via",
        " sf spatial join; nearest-centroid fallback used if join fails.")
message("[ASSUMPTION – Part 2] All hotel tourists are assigned purpose = 'leisure'.",
        " Business tourists in hotels are not distinguished from the hotel data.")

# =============================================================================
# 1. LOAD PART 1 OUTPUTS AND HOTEL DATA
# =============================================================================

load("part1_output.RData")
cat("Part 1 data loaded.\n")

HOTEL_FILE  <- "data/hotels_clean.csv"   # produced by prepare_hotel_data.R
ZONES_FILE  <- "data/amr_zones.gpkg"
COMMUNE_FILE  <- "data/communes.gpkg"

# Run prepare_hotel_data.R first if hotels_clean.csv does not exist yet.
if (!file.exists(HOTEL_FILE))
  stop("Hotel file not found: ", HOTEL_FILE,
       "\n  Run prepare_hotel_data.R first to generate hotels_clean.csv from PASTA_HESTA.xlsx.")

hotels_raw <- read.csv(HOTEL_FILE, stringsAsFactors = FALSE)
cat(sprintf("Loaded %d establishments from %s.\n", nrow(hotels_raw), HOTEL_FILE))
cat("Accommodation type breakdown:\n")
print(table(hotels_raw$accommodation_type, useNA = "ifany"))
cat("\n")

# =============================================================================
# 2. RESHAPE TO LONG FORMAT: hotel × nationality → n_nights
# =============================================================================

# Detect nights columns (prefix "nights_")
nights_cols <- grep("^nights_", names(hotels_raw), value = TRUE)

if (length(nights_cols) == 0)
  stop("No 'nights_*' columns found in hotels_clean.csv. ",
       "Expected: nights_germany, nights_france, nights_italy, nights_uk, ",
       "nights_netherlands, nights_usa, nights_switzerland, nights_rest_of_world. ",
       "Run prepare_hotel_data.R to generate this file.")

cat(sprintf("Found %d nights columns: %s\n\n",
    length(nights_cols), paste(nights_cols, collapse = ", ")))

hotels_long <- hotels_raw %>%
  select(hotel_id, x, y, accommodation_type, stars, all_of(nights_cols)) %>%
  pivot_longer(
    cols      = all_of(nights_cols),
    names_to  = "nationality_label",
    values_to = "n_nights_hotel"
  ) %>%
  mutate(
    # Strip "nights_" prefix to get nationality label
    nationality_label = sub("^nights_", "", nationality_label),
    # Map to 8-group coding used in Part 1 MNL
    nationality_group = case_when(
      nationality_label == "germany"      ~ "germany",
      nationality_label == "france"       ~ "france",
      nationality_label == "italy"        ~ "italy",
      nationality_label %in% c("uk","united_kingdom","britain") ~ "uk",
      nationality_label == "netherlands"  ~ "netherlands",
      nationality_label %in% c("usa","united_states") ~ "usa",
      nationality_label == "switzerland"  ~ "switzerland",
      TRUE                                ~ "rest_of_world"
    )
  ) %>%
  filter(n_nights_hotel > 0)

cat(sprintf("Long format: %d hotel × nationality combinations (non-zero nights).\n",
    nrow(hotels_long)))
cat(sprintf("Total hotel nights across all nationalities: %d\n\n",
    sum(hotels_long$n_nights_hotel)))

# =============================================================================
# 3. MAP HOTEL LV95 COORDINATES TO AMR ZONES
# =============================================================================

# Build sf point layer from hotel coordinates (LV95 = EPSG:2056)
hotels_sf <- hotels_long %>%
  distinct(hotel_id, x, y) %>%
  filter(!is.na(x) & !is.na(y)) %>%
  st_as_sf(coords = c("x", "y"), crs = 2056)

zone_join <- NULL

cat("Joining hotels to AMR zones via", ZONES_FILE, "...\n")
zones_sf <- st_read(ZONES_FILE, quiet = TRUE) %>%
  st_transform(2056)
names(zones_sf) = c("zone_name", "zone_id", "geometry")

joined <- st_join(hotels_sf, zones_sf, left = TRUE)

# Fallback: nearest zone centroid for hotels that missed the spatial join
n_miss <- sum(is.na(joined$zone_id))
if (n_miss > 0) {
  # [ASSUMPTION – A-1] Nearest-centroid fallback
  message("[ASSUMPTION – Part 2] ", n_miss,
          " hotels not covered by AMR polygons; assigning nearest zone centroid.")
  centroids <- st_centroid(zones_sf)
  miss_idx  <- which(is.na(joined$zone_id))
  nearest   <- st_nearest_feature(hotels_sf[miss_idx, ], centroids)
  joined$zone_id[miss_idx] <- zones_sf$zone_id[nearest]
}


communes_sf <- st_read(COMMUNES_FILE, quiet = TRUE) %>%
  st_transform(2056)
communes_sf = communes_sf[c("GDENAME", "STALAN2020", "STALAN2020_fr", "geometry")]
names(communes_sf) = c("name", "urban_rural_topology", "urban_rural_topology_char","geometry")

joined <- st_join(joined, communes_sf, left = TRUE)

zone_join <- as.data.frame(joined) %>%
  select(hotel_id, hotel_zone = zone_id, urban_rural_topology, zone_name)

cat(sprintf("Hotel → zone mapping complete. %d unique hotel zones.\n\n",
    length(unique(zone_join$hotel_zone))))

# =============================================================================
# 4. EXPAND TO SYNTHETIC AGENTS
# =============================================================================

# Accommodation type: maps HESTA betriebsart values to the 4-category factor
# used in Part 1's extended coef_table (reference = "hotel").
# [INVENTED PARAMETER] Category mapping for non-hotel types reflects HESTA
# betriebsart labels; holiday_home is absent from HESTA and assigned to any
# betriebsart not matching the other three categories.
# Replace mapping when finer TMS sub-codes become available.
ACCOM_TYPE_MAP <- c(
  "Hotel"       = "hotel",
  "Camping"     = "camping",
  "Kurbetriebe" = "collective"
)

map_accom_type <- function(betriebsart) {
  mapped <- ACCOM_TYPE_MAP[betriebsart]
  mapped[is.na(mapped)] <- "holiday_home"   # fallback for unknown betriebsart values
  unname(mapped)
}

# Age distribution from TMS (Tourismus Monitor Schweiz).
# Taken directly from the Python pipeline (distribute_tourists_in_tourist_accommodation.py).
# Ages are 5-year bin midpoints: 18, 23, ..., 98.
AGE_BINS <- seq(18L, 98L, by = 5L)

AGE_PROBS <- c(
  0.030929457494933546,   # 18
  0.097119341077536900,   # 23
  0.125549804783617380,   # 28
  0.113448342032605720,   # 33
  0.101594693765626050,   # 38
  0.103064934974812130,   # 43
  0.101212743841690250,   # 48
  0.101859678674628290,   # 53
  0.079836002464638620,   # 58
  0.068277081782321750,   # 63
  0.044943030334914605,   # 68
  0.021272053150429220,   # 73
  0.008042705403412684,   # 78
  0.002263171845485889,   # 83
  0.000170148085542497,   # 88
  0.000144955528486629,   # 93
  0.000271854759317744    # 98
)

# age_cat: 2 = 18–27, 3 = 28–47, 4 = 48–67, 5 = 68–77, 6 = 78+
AGE_CAT_MAP <- c(
  "18" = 2L, "23" = 2L,
  "28" = 3L, "33" = 3L, "38" = 3L, "43" = 3L,
  "48" = 4L, "53" = 4L, "58" = 4L, "63" = 4L,
  "68" = 5L, "73" = 5L,
  "78" = 6L, "83" = 6L, "88" = 6L, "93" = 6L, "98" = 6L
)

hotels_with_zones <- hotels_long %>%
  left_join(zone_join, by = "hotel_id") %>% 
  rename(hotel_zone = zone_id) %>%
 na.omit
cat(sprintf("Accommodation type mapping: %s\n",
    paste(names(ACCOM_TYPE_MAP), ACCOM_TYPE_MAP, sep = " → ", collapse = ", ")))
cat("Expanding to synthetic agents...\n")

tourist_pop <- hotels_with_zones %>%
  mutate(
    n_agents = pmax(1L, as.integer(round(n_nights_hotel * SCALE_FACTOR)))
  ) %>%
  # Repeat each row n_agents times
  tidyr::uncount(n_agents, .id = "agent_seq") %>%
  mutate(
    agent_id   = row_number(),
    # Each agent represents 1/SCALE_FACTOR overnight stays
    n_nights   = pmax(1L, rpois(n(), lambda = pmax(1, n_nights_hotel * SCALE_FACTOR))),
    # Accommodation type factor: hotel (reference), camping, holiday_home, collective
    # [INVENTED PARAMETER] mapping — replace when finer TMS sub-codes available
    accom_type         = map_accom_type(accommodation_type),
    accom_camping      = as.integer(accom_type == "camping"),
    accom_holiday_home = as.integer(accom_type == "holiday_home"),
    accom_collective   = as.integer(accom_type == "collective"),
    # Age: drawn from TMS distribution (same probabilities as Python pipeline)
    age     = sample(AGE_BINS, n(), replace = TRUE, prob = AGE_PROBS),
    age_cat = as.integer(AGE_CAT_MAP[as.character(age)]),
    # Urban binary: default 1 (urban) if zone info not available from shapefile
    urban      = as.integer(urban_rural_topology == 1),
    rural      = as.integer(urban_rural_topology == 2),
    # Purpose: leisure (all hotel tourists) [ASSUMPTION]
    purpose    = "leisure",
    # Excursion origin = hotel zone [ASSUMPTION – A-6]
    excursion_zone = hotel_zone
  ) %>%
  select(agent_id, hotel_id, excursion_zone, nationality_group,
         accom_type, accom_camping, accom_holiday_home, accom_collective,
         stars, n_nights, age, age_cat, urban, purpose)

cat(sprintf("Synthetic population: %d agents.\n", nrow(tourist_pop)))
cat(sprintf("Expected ≈ %d (= total nights × SCALE_FACTOR = %d × %.2f).\n",
    round(sum(hotels_long$n_nights_hotel) * SCALE_FACTOR),
    sum(hotels_long$n_nights_hotel), SCALE_FACTOR))
cat("Accommodation type distribution:\n")
print(table(tourist_pop$accom_type, useNA = "ifany"))
cat("\n")

# Summary
cat("=== Population Summary ===\n")
cat("Nationality group distribution:\n")
print(table(tourist_pop$nationality_group))
cat("\nAccommodation type (hotel = reference in MNL):\n")
print(table(tourist_pop$accom_type))
cat(sprintf("\nAge distribution (mean = %.1f):\n", mean(tourist_pop$age)))
print(table(tourist_pop$age_cat))
cat("\n")

# =============================================================================
# 5. SAVE
# =============================================================================

save(tourist_pop, SCALE_FACTOR, NATIONALITY_MAP, NAT_GROUPS, ALT_CODES, REF_ALT,
     zone_attrs,
     file = "part2_output.RData")

cat("Part 2 complete. Output saved to part2_output.RData\n")
cat("Run part3_mode_assignment.R next.\n")
