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

# [INVENTED PARAMETER] SCALE_FACTOR = 0.01: 1 synthetic agent per 100 hotel nights.
# Adjust upward (e.g. 0.1) for more agents or downward for faster runs.

# [ASSUMPTION] Hotel zone is the tourist's excursion ORIGIN (replaces residential
# home zone from Scherr et al.). Tourists start each excursion from their hotel.

# [ASSUMPTION] Urban/rural typology comes from an AMR zone attributes table that
# must be user-supplied (data/amr_zones.gpkg or data/zone_attributes.csv).

library(dplyr)
library(tidyr)
library(tibble)
library(sf)

set.seed(42)

SCALE_FACTOR <- 0.01   # [INVENTED PARAMETER]

# =============================================================================
# RUNTIME FLAGS
# =============================================================================

message("[INVENTED PARAMETER – Part 2] SCALE_FACTOR = ", SCALE_FACTOR,
        " (1 synthetic agent per ", round(1 / SCALE_FACTOR), " hotel nights).")
message("[ASSUMPTION – Part 2] Hotel x/y (LV95) is joined to AMR regions via",
        " sf spatial join; nearest-centroid fallback used if join fails.")
message("[ASSUMPTION – Part 2] All hotel tourists are assigned purpose = 'leisure'.",
        " Business tourists in hotels are not distinguished from the hotel data.")
message("[INVENTED PARAMETER – Part 2] n_nights per agent drawn from",
        " Poisson(mean = hotel_nights / n_agents_from_hotel). Replace with TMS",
        " marginals once available.")
message("[INVENTED PARAMETER – Part 2] party_size drawn from Poisson(lambda)",
        " segmented by nationality group. Replace with TMS marginals.")

# =============================================================================
# 1. LOAD PART 1 OUTPUTS AND HOTEL DATA
# =============================================================================

load("part1_output.RData")
cat("Part 1 data loaded.\n")

HOTEL_FILE  <- "data/hotels_clean.csv"   # produced by prepare_hotel_data.R
ZONES_FILE  <- "data/amr_zones.gpkg"    # or .shp; must have zone_id column

# Run prepare_hotel_data.R first if hotels_clean.csv does not exist yet.
if (!file.exists(HOTEL_FILE))
  stop("Hotel file not found: ", HOTEL_FILE,
       "\n  Run prepare_hotel_data.R first to generate hotels_clean.csv from PASTA_HESTA.xlsx.")

hotels_raw <- read.csv(HOTEL_FILE, stringsAsFactors = FALSE)
cat(sprintf("Loaded %d hotels from %s.\n\n", nrow(hotels_raw), HOTEL_FILE))

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
  st_as_sf(coords = c("x", "y"), crs = 2056)

zone_join <- NULL

if (file.exists(ZONES_FILE)) {
  cat("Joining hotels to AMR zones via", ZONES_FILE, "...\n")
  zones_sf <- st_read(ZONES_FILE, quiet = TRUE) %>%
    st_transform(2056)

  joined <- st_join(hotels_sf, zones_sf["zone_id"], left = TRUE)

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

  zone_join <- as.data.frame(joined) %>%
    select(hotel_id, hotel_zone = zone_id)

  # Zone attributes (urban/rural) from zone layer
  zone_attrs <- as.data.frame(zones_sf) %>%
    select(zone_id, any_of(c("urban_rural", "region_label", "amr_name")))
  if (!"urban_rural" %in% names(zone_attrs)) {
    message("[ASSUMPTION – Part 2] 'urban_rural' column not found in AMR shapefile.",
            " Defaulting all zones to urban = 1.")
    zone_attrs$urban_rural <- 1L
  }

} else {
  message("[ASSUMPTION – Part 2] AMR zone file not found at ", ZONES_FILE,
          ". Assigning hotel_zone = hotel_id as placeholder; urban = 1.")
  zone_join  <- data.frame(hotel_id = hotels_sf$hotel_id,
                            hotel_zone = hotels_sf$hotel_id)
  zone_attrs <- data.frame(zone_id = unique(hotels_sf$hotel_id),
                            urban_rural = 1L,
                            region_label = "unknown")
}

cat(sprintf("Hotel → zone mapping complete. %d unique hotel zones.\n\n",
    length(unique(zone_join$hotel_zone))))

# =============================================================================
# 4. EXPAND TO SYNTHETIC AGENTS
# =============================================================================

# [INVENTED PARAMETER] party_size lambda by nationality group
party_size_lambda <- c(
  germany      = 2.1,
  france       = 2.3,
  italy        = 2.5,
  uk           = 2.0,
  netherlands  = 2.2,
  usa          = 2.4,
  switzerland  = 1.9,
  rest_of_world = 2.2
)

# Accommodation binary: TMS 2017 has only two categories:
#   1 = hotel, 0 = supplementary accommodation.
# Map hotel CSV accommodation_type to this binary.
# [ASSUMPTION] Any accommodation_type labelled "hotel" maps to TMS hotel=1;
# all others (hostel, airbnb, camping, chalet, b&b, etc.) map to supplementary=0.
# Adjust the left-hand values below to match the actual strings in your hotels.csv.
HOTEL_TYPE_LABELS <- c("hotel", "Hotel", "HOTEL")   # strings that map to hotel=1

hotels_with_zones <- hotels_long %>%
  left_join(zone_join, by = "hotel_id") %>%
  left_join(
    zone_attrs %>% rename(hotel_zone = zone_id),
    by = "hotel_zone"
  ) %>%
  # Ensure urban_rural column always exists; NA → treated as urban in coalesce below
  { if (!"urban_rural" %in% names(.)) dplyr::mutate(., urban_rural = NA_integer_) else . }

cat("Expanding to synthetic agents...\n")

tourist_pop <- hotels_with_zones %>%
  rowwise() %>%
  mutate(
    n_agents = max(1L, round(n_nights_hotel * SCALE_FACTOR))
  ) %>%
  ungroup() %>%
  # Repeat each row n_agents times
  tidyr::uncount(n_agents, .id = "agent_seq") %>%
  mutate(
    agent_id = row_number(),
    # n_nights: Poisson draw per agent [INVENTED PARAMETER]
    n_nights = pmax(1L, rpois(n(), lambda = pmax(1, n_nights_hotel * SCALE_FACTOR))),
    # party_size: Poisson draw per nationality group [INVENTED PARAMETER]
    party_size = pmax(1L, rpois(n(), lambda = party_size_lambda[nationality_group])),
    # Accommodation binary: 1=hotel, 0=supplementary [ASSUMPTION]
    # Matches TMS 2017 accomodation coding used in Part 1 estimation.
    accom_hotel = as.integer(accommodation_type %in% HOTEL_TYPE_LABELS),
    # Urban binary: default 1 (urban) if zone info not available from shapefile
    urban = as.integer(coalesce(urban_rural, 1L) >= 1L),
    # Purpose: leisure (all hotel tourists) [ASSUMPTION]
    purpose = "leisure",
    # Excursion origin = hotel zone [ASSUMPTION – A-6]
    excursion_zone = hotel_zone
  ) %>%
  select(agent_id, hotel_id, excursion_zone, nationality_group,
         accom_hotel, stars, n_nights, party_size, urban, purpose)

cat(sprintf("Synthetic population: %d agents.\n", nrow(tourist_pop)))
cat(sprintf("Expected ≈ %d (= total nights × SCALE_FACTOR = %d × %.2f).\n\n",
    round(sum(hotels_long$n_nights_hotel) * SCALE_FACTOR),
    sum(hotels_long$n_nights_hotel), SCALE_FACTOR))

# Summary
cat("=== Population Summary ===\n")
cat("Nationality group distribution:\n")
print(table(tourist_pop$nationality_group))
cat("\nAccommodation (hotel=1, supplementary=0):\n")
print(table(tourist_pop$accom_hotel))
cat(sprintf("\nMean n_nights : %.1f  [replace with TMS marginals]\n",
    mean(tourist_pop$n_nights)))
cat(sprintf("Mean party_size: %.1f  [replace with TMS marginals]\n\n",
    mean(tourist_pop$party_size)))

# =============================================================================
# 5. SAVE
# =============================================================================

save(tourist_pop, SCALE_FACTOR, NATIONALITY_MAP, NAT_GROUPS, ALT_CODES, REF_ALT,
     zone_attrs,
     file = "part2_output.RData")

cat("Part 2 complete. Output saved to part2_output.RData\n")
cat("Run part3_mode_assignment.R next.\n")
