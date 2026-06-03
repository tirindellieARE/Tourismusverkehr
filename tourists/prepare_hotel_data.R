# =============================================================================
# prepare_hotel_data.R
# Parse PASTA_HESTA.xlsx → geocode missing coordinates → hotels_clean.csv
#
# INPUTS:  data/PASTA_HESTA.xlsx   — Swiss HESTA hotel statistics
#          data/hotels_geocoded.csv — cache; created on first run, read thereafter
# OUTPUTS: data/hotels_geocoded.csv — intermediate cache (hotel_id + coords)
#          data/hotels_clean.csv    — final hotel file for part2_tourist_population.R
#
# Column mapping (PASTA_HESTA.xlsx, skip=2, 155 cols):
#   d[,1]  = hotel_id (sequential 1–4764)
#   d[,2]  = name
#   d[,3]  = kategorie
#   d[,4]  = coord_x  (LV95 easting;  NA for 600 hotels)
#   d[,5]  = coord_y  (LV95 northing; NA for 600 hotels)
#   d[,6]  = adresse
#   d[,7]  = gemeinde_nr  (BFS municipality code)
#   d[,8]  = gemeinde     (municipality name)
#   d[,9]  = betriebsart  (Hotel / Camping / Kurbetriebe)
#   d[,10..155] = 73 nationality pairs (Ankünfte, Logiernächte)
#
# Nationality Logiernächte column positions (in 155-col d, after skip=2):
#   germany=33, france=39, italy=57, netherlands=87, switzerland=109,
#   uk=151, usa=153; rest_of_world = row-sum of all other 66 Logiernächte cols.
#
# [ASSUMPTION] All establishments with a non-NA betriebsart are retained
#   (Hotel, Camping, Kurbetriebe, etc.) — accommodation_type mirrors betriebsart.
# [ASSUMPTION] Geocoding via swisstopo SearchServer API (free, LV03→LV95
#   conversion: coord_x = y+2e6, coord_y = x+1e6). Establishments not resolvable
#   by API are kept with NA coords and dropped from part2 spatial join.
# [ASSUMPTION] Stars are parsed from kategorie string (e.g. "3-Stern" → 3);
#   non-hotel establishment types will have stars = NA.
# =============================================================================

library(readxl)
library(dplyr)
library(jsonlite)

HESTA_FILE  <- "data/PASTA_HESTA.xlsx"
CACHE_FILE  <- "data/hotels_geocoded.csv"
OUTPUT_FILE <- "data/hotels_clean.csv"

# Logiernächte col positions in d (156-col raw, first col dropped → 155 cols)
LOGIER_COLS <- list(
  germany     = 33L,
  france      = 39L,
  italy       = 57L,
  netherlands = 87L,
  switzerland = 109L,
  uk          = 151L,
  usa         = 153L
)
ALL_LOGIER_COLS <- seq(11L, 155L, by = 2L)   # all 73 Logiernächte cols
ROW_COLS <- c(33L, 39L, 57L, 87L, 109L, 151L, 153L)  # TMS group cols
ROW_COLS_REST <- setdiff(ALL_LOGIER_COLS, ROW_COLS)   # 66 rest-of-world cols

# =============================================================================
# 1. PARSE PASTA_HESTA.xlsx
# =============================================================================

cat("Parsing PASTA_HESTA.xlsx...\n")

raw <- as.data.frame(read_excel(HESTA_FILE, sheet = 1, col_names = FALSE, skip = 2))

names(raw)[1:9] <- c("hotel_id", "name", "kategorie",
                     "coord_x", "coord_y",
                     "adresse", "gemeinde_nr", "gemeinde", "betriebsart")

raw$hotel_id    <- as.integer(raw$hotel_id)
raw$coord_x     <- suppressWarnings(as.numeric(raw$coord_x))
raw$coord_y     <- suppressWarnings(as.numeric(raw$coord_y))
raw$gemeinde_nr <- suppressWarnings(as.integer(raw$gemeinde_nr))

cat(sprintf("Loaded %d establishments.\n", nrow(raw)))
cat("Betriebsart distribution:\n")
print(table(raw$betriebsart, useNA = "ifany"))

hotels <- raw[!is.na(raw$betriebsart), ]
cat(sprintf("\nAll establishments (non-NA betriebsart): %d\n", nrow(hotels)))
cat("Betriebsart breakdown:\n")
print(table(hotels$betriebsart))
cat(sprintf("Establishments with missing coords: %d\n\n",
    sum(is.na(hotels$coord_x))))

# =============================================================================
# 2. GEOCODING — load cache or run API
# =============================================================================

geocode_swisstopo <- function(name, adresse, gemeinde) {
  # Build the most informative query possible
  parts <- c(
    if (!is.na(adresse) && nchar(trimws(adresse)) > 0) trimws(adresse) else NULL,
    if (!is.na(gemeinde) && nchar(trimws(gemeinde)) > 0) trimws(gemeinde) else trimws(name)
  )
  query <- paste(parts, collapse = " ")
  if (nchar(query) == 0) return(c(NA_real_, NA_real_))

  url <- paste0(
    "https://api3.geo.admin.ch/rest/services/api/SearchServer?",
    "searchText=", URLencode(query, reserved = TRUE),
    "&lang=de&type=locations&limit=1"
  )

  result <- tryCatch({
    r <- fromJSON(url, simplifyVector = TRUE)
    if (!is.null(r$results) && length(r$results) > 0 &&
        !is.null(r$results$attrs) && nrow(r$results$attrs) > 0) {
      x_lv03 <- r$results$attrs$x[1]   # LV03 northing
      y_lv03 <- r$results$attrs$y[1]   # LV03 easting
      c(y_lv03 + 2000000, x_lv03 + 1000000)  # LV95: (coord_x, coord_y)
    } else {
      c(NA_real_, NA_real_)
    }
  }, error = function(e) {
    message("  API error for query '", query, "': ", conditionMessage(e))
    c(NA_real_, NA_real_)
  })

  Sys.sleep(0.3)
  result
}

if (file.exists(CACHE_FILE)) {
  cat("Loading cached geocoding results from", CACHE_FILE, "...\n")
  geocode_cache <- read.csv(CACHE_FILE, stringsAsFactors = FALSE)
  cat(sprintf("  Cache: %d establishments, %d with coords.\n\n",
      nrow(geocode_cache),
      sum(!is.na(geocode_cache$coord_x_geo))))

  # Geocode any establishments added since the cache was built (missing coords, not cached)
  uncached_miss_idx <- which(
    !hotels$hotel_id %in% geocode_cache$hotel_id & is.na(hotels$coord_x)
  )
  if (length(uncached_miss_idx) > 0) {
    cat(sprintf("Geocoding %d uncached establishments with missing coordinates...\n",
        length(uncached_miss_idx)))
    new_geo_x <- rep(NA_real_, length(uncached_miss_idx))
    new_geo_y <- rep(NA_real_, length(uncached_miss_idx))
    for (i in seq_along(uncached_miss_idx)) {
      row <- hotels[uncached_miss_idx[i], ]
      if (i %% 50 == 1)
        cat(sprintf("  Geocoding %d / %d ...\n", i, length(uncached_miss_idx)))
      coords <- geocode_swisstopo(row$name, row$adresse, row$gemeinde)
      if (!is.na(coords[1]) &&
          (coords[1] < 2480000 | coords[1] > 2830000 |
           coords[2] < 1070000 | coords[2] > 1300000)) {
        coords <- c(NA_real_, NA_real_)
      }
      new_geo_x[i] <- coords[1]
      new_geo_y[i] <- coords[2]
    }
    new_cache_rows <- data.frame(
      hotel_id    = hotels$hotel_id[uncached_miss_idx],
      coord_x_geo = new_geo_x,
      coord_y_geo = new_geo_y
    )
    geocode_cache <- rbind(geocode_cache, new_cache_rows)
    write.csv(geocode_cache, CACHE_FILE, row.names = FALSE)
    cat(sprintf("Cache updated: %d total entries.\n\n", nrow(geocode_cache)))
  }

} else {
  cat("Geocoding hotels with missing coordinates via swisstopo API...\n")
  cat("(Rate-limited to 0.3s per request. Will stop on HTTP error.)\n\n")

  miss_idx <- which(is.na(hotels$coord_x))
  cat(sprintf("Hotels to geocode: %d\n\n", length(miss_idx)))

  geo_x <- rep(NA_real_, nrow(hotels))
  geo_y <- rep(NA_real_, nrow(hotels))

  for (i in seq_along(miss_idx)) {
    row <- hotels[miss_idx[i], ]
    if (i %% 50 == 1)
      cat(sprintf("  Geocoding %d / %d ...\n", i, length(miss_idx)))

    coords <- geocode_swisstopo(row$name, row$adresse, row$gemeinde)

    # If both are implausible (outside LV95 Switzerland bbox), discard
    if (!is.na(coords[1]) &&
        (coords[1] < 2480000 | coords[1] > 2830000 |
         coords[2] < 1070000 | coords[2] > 1300000)) {
      coords <- c(NA_real_, NA_real_)
    }

    geo_x[miss_idx[i]] <- coords[1]
    geo_y[miss_idx[i]] <- coords[2]
  }

  geocode_cache <- data.frame(
    hotel_id  = hotels$hotel_id,
    coord_x_geo = geo_x,
    coord_y_geo = geo_y
  )

  write.csv(geocode_cache, CACHE_FILE, row.names = FALSE)
  n_resolved <- sum(!is.na(geocode_cache$coord_x_geo[miss_idx]))
  cat(sprintf("\nGeocoding complete. Resolved %d / %d missing hotels.\n",
      n_resolved, length(miss_idx)))
  cat(sprintf("Cache saved to %s\n\n", CACHE_FILE))
}

# =============================================================================
# 3. MERGE GEOCODED COORDS INTO HOTEL TABLE
# =============================================================================

# Apply geocoded coords where original coords are missing
hotels <- hotels %>%
  left_join(geocode_cache[, c("hotel_id", "coord_x_geo", "coord_y_geo")],
            by = "hotel_id") %>%
  mutate(
    coord_x = if_else(is.na(coord_x) & !is.na(coord_x_geo), coord_x_geo, coord_x),
    coord_y = if_else(is.na(coord_y) & !is.na(coord_y_geo), coord_y_geo, coord_y)
  ) %>%
  select(-coord_x_geo, -coord_y_geo)

cat(sprintf("Establishments with coords after geocoding: %d / %d\n",
    sum(!is.na(hotels$coord_x)), nrow(hotels)))
cat(sprintf("Establishments still without coords (will use nearest zone fallback in part2): %d\n\n",
    sum(is.na(hotels$coord_x))))

# =============================================================================
# 4. PARSE STARS FROM KATEGORIE
# =============================================================================

m <- regexpr("[1-5](?=-Stern)", hotels$kategorie, perl = TRUE)
hotels$stars <- as.integer(ifelse(m > 0L,
  regmatches(hotels$kategorie, m), NA_character_))
cat("Stars distribution (NA = keine Angaben):\n")
print(table(hotels$stars, useNA = "ifany"))
cat("\n")

# =============================================================================
# 5. AGGREGATE NATIONALITY LOGIERNÄCHTE → TMS GROUPS
# =============================================================================

cat("Aggregating Logiernächte by TMS nationality group...\n")

hotel_idx <- match(hotels$hotel_id, raw$hotel_id)   # row index into raw
to_nights <- function(col_idx) suppressWarnings(as.numeric(raw[hotel_idx, col_idx]))

hotels$nights_germany     <- to_nights(LOGIER_COLS$germany)
hotels$nights_france      <- to_nights(LOGIER_COLS$france)
hotels$nights_italy       <- to_nights(LOGIER_COLS$italy)
hotels$nights_netherlands <- to_nights(LOGIER_COLS$netherlands)
hotels$nights_switzerland <- to_nights(LOGIER_COLS$switzerland)
hotels$nights_uk          <- to_nights(LOGIER_COLS$uk)
hotels$nights_usa         <- to_nights(LOGIER_COLS$usa)

# rest_of_world = row-sum of all other 66 Logiernächte cols
rest_mat <- matrix(NA_real_, nrow = nrow(hotels), ncol = length(ROW_COLS_REST))
for (j in seq_along(ROW_COLS_REST)) {
  rest_mat[, j] <- suppressWarnings(as.numeric(raw[hotel_idx, ROW_COLS_REST[j]]))
}
hotels$nights_rest_of_world <- rowSums(rest_mat, na.rm = TRUE)

# Replace NA with 0 for nights columns (confidential/suppressed cells → 0)
nights_cols <- grep("^nights_", names(hotels), value = TRUE)
for (nc in nights_cols) hotels[[nc]][is.na(hotels[[nc]])] <- 0L

cat("Nationality nights summary (total across all hotels):\n")
for (nc in nights_cols) cat(sprintf("  %-30s %d\n", nc, sum(hotels[[nc]])))
cat("\n")

# =============================================================================
# 6. BUILD OUTPUT: hotels_clean.csv  (format expected by part2)
# =============================================================================

hotels_clean <- hotels %>%
  transmute(
    hotel_id          = hotel_id,
    name              = name,
    x                 = coord_x,
    y                 = coord_y,
    accommodation_type = betriebsart,
    stars             = stars,
    nights_germany    = nights_germany,
    nights_france     = nights_france,
    nights_italy      = nights_italy,
    nights_uk         = nights_uk,
    nights_netherlands = nights_netherlands,
    nights_usa        = nights_usa,
    nights_switzerland = nights_switzerland,
    nights_rest_of_world = nights_rest_of_world
  )

write.csv(hotels_clean, OUTPUT_FILE, row.names = FALSE)
cat(sprintf("hotels_clean.csv saved: %d rows × %d cols → %s\n",
    nrow(hotels_clean), ncol(hotels_clean), OUTPUT_FILE))
cat(sprintf("Total nights across all groups: %d\n",
    sum(hotels_clean[, nights_cols], na.rm = TRUE)))
cat(sprintf("Hotels with valid coordinates: %d / %d\n\n",
    sum(!is.na(hotels_clean$x)), nrow(hotels_clean)))

# =============================================================================
# 7. WHAT THE PAPER REQUIRES THAT IS MISSING FROM HESTA DATA
# =============================================================================

cat("=== Gaps between HESTA data and Danalet et al. (2023) requirements ===\n\n")
cat("[MISSING] Person-level attributes: HESTA is aggregate (hotel × nationality).\n")
cat("  Each row in persons.csv represents one overnight stay, not one individual.\n\n")
cat("[MISSING] Entry mode to Switzerland (means_of_transport_to_CH).\n")
cat("  Paper's mode variable refers to in-Switzerland transport, not entry.\n")
cat("  TMS 2017 has this; it feeds part1 estimation only, not hotel data.\n\n")
cat("[MISSING] Purpose segmentation (leisure/business): HESTA mixes both.\n")
cat("  [ASSUMPTION] All hotel tourists assigned purpose=leisure in part2.\n\n")
cat("[MISSING] Urban/rural at hotel level: must be derived from LV95 coords\n")
cat("  via AMR zone spatial join in part2 (data/amr_zones.gpkg required).\n\n")
cat("[MISSING] LOS skims (travel time/cost by mode per OD pair).\n")
cat("  Paper uses these in MNL; absent from both TMS and HESTA.\n")
cat("  Consequence: rho² in part1 will be lower than paper's 0.469.\n\n")
cat("[NOTE] HESTA nationality groups (73) map cleanly to 8 TMS groups:\n")
cat("  germany, france, italy, uk, netherlands, usa, switzerland, rest_of_world.\n\n")
