# ============================================================
# build_ch_fr_sample.R
# Standalone: samples near-CH French-origin tourists and joins each to
# their nearest rail stop. Split out of pt_r5r_rail_ch_fr.R so the sample
# can be (re)built without touching r5r/Java or the network at all -- this
# is pure GTFS/CSV/sf work, no JVM, no network build/rebuild risk.
#
# Reuses data/r5_rail_ch_fr/rail_stops.csv (the candidate rail-stop list
# already extracted from ch.gtfs.zip + fr.gtfs.zip by pt_r5r_rail_ch_fr.R)
# rather than re-parsing the GTFS feeds -- the frequency-fix applied to
# ch.gtfs.zip since then only expanded existing trips into explicit
# stop_times, it didn't add or remove any stops/routes, so the set of
# rail-serving stops is unchanged and this file is still valid. Delete it
# and re-run pt_r5r_rail_ch_fr.R's rail-stop extraction step first if you
# ever need to regenerate it from scratch.
#
# FIX vs. the original script: this version saves nearest_stop_lat/
# nearest_stop_lon directly in the output (the first version didn't,
# which broke a later follow-up script that needed them and had to
# recover them via an extra join -- see pt_r5r_rail_ch_fr_nowalkcap.R).
#
# OUTPUT: data/output/pt_rail_ch_fr_sample.csv -- tourist_id, origin_lat,
# origin_long, state, nearest_stop_id, nearest_stop_name, nearest_stop_feed,
# nearest_stop_lat, nearest_stop_lon, dist_to_station_km, dest_lat, dest_long
#
# Run:  source("Scripts/00_setup.R"); source("Scripts/02_travel_time/build_ch_fr_sample.R")
# ============================================================

source("Scripts/00_setup.R")

FR_NEAR_CH_REGIONS <- c("Auvergne-Rhône-Alpes", "Grand Est", "Bourgogne-Franche-Comté")
RAIL_DIR <- "data/r5_rail_ch_fr"
SAMPLE_N <- 100
SEED <- 42

# --- Load agqpv, identify near-CH French-origin tourists -------------------
geo_file <- "data/output/origins_fr_geocoded.csv"
if (!file.exists(geo_file)) {
  stop("Missing ", geo_file, " -- run the FR reverse-geocode step first (see pt_r5r_rail_ch_fr.R's header).")
}
fr_geo <- readr::read_csv(geo_file, show_col_types = FALSE)

agqpv <- readr::read_csv(cfg$input_csv, locale = readr::locale(encoding = cfg$input_encoding),
                          show_col_types = FALSE)
agqpv$commune_country <- substr(agqpv$origin_commune, 1, 2)
fr <- agqpv[!is.na(agqpv$commune_country) & agqpv$commune_country == "FR", ]
fr <- dplyr::left_join(fr, fr_geo, by = c("origin_lat", "origin_long"))
fr_near <- fr[!is.na(fr$state) & fr$state %in% FR_NEAR_CH_REGIONS, ]
log_msg(sprintf("Near-CH French-origin tourist rows (%s): %d / %d total FR-origin rows",
                paste(FR_NEAR_CH_REGIONS, collapse = ", "), nrow(fr_near), nrow(fr)))

# --- Sample ------------------------------------------------------------
set.seed(SEED)
sample_n <- min(SAMPLE_N, nrow(fr_near))
if (sample_n < SAMPLE_N) {
  log_msg(sprintf("WARNING: only %d near-CH French-origin rows available, sampling all of them.", sample_n))
}
samp <- fr_near[sample(nrow(fr_near), sample_n), ]
samp$tourist_id <- sprintf("T%03d", seq_len(nrow(samp)))
log_msg(sprintf("Sampled %d near-CH French-origin tourists (seed=%d).", nrow(samp), SEED))

# --- Nearest rail station per tourist (great-circle, via sf) ---------------
rail_stops_file <- file.path(RAIL_DIR, "rail_stops.csv")
if (!file.exists(rail_stops_file)) {
  stop("Missing ", rail_stops_file, " -- run pt_r5r_rail_ch_fr.R's rail-stop extraction step first.")
}
rail_stops <- readr::read_csv(rail_stops_file, show_col_types = FALSE)

stops_sf   <- sf::st_as_sf(rail_stops, coords = c("stop_lon", "stop_lat"), crs = 4326)
origins_sf <- sf::st_as_sf(samp, coords = c("origin_long", "origin_lat"), crs = 4326, remove = FALSE)
nearest_idx <- sf::st_nearest_feature(origins_sf, stops_sf)

samp$nearest_stop_id   <- rail_stops$stop_id[nearest_idx]
samp$nearest_stop_name <- rail_stops$stop_name[nearest_idx]
samp$nearest_stop_feed <- rail_stops$feed[nearest_idx]
samp$nearest_stop_lat  <- rail_stops$stop_lat[nearest_idx]
samp$nearest_stop_lon  <- rail_stops$stop_lon[nearest_idx]
samp$dist_to_station_km <- as.numeric(
  sf::st_distance(origins_sf, stops_sf[nearest_idx, ], by_element = TRUE)
) / 1000
log_msg(sprintf("Nearest-station distance: median %.1f km, max %.1f km",
                median(samp$dist_to_station_km), max(samp$dist_to_station_km)))

out <- samp[, c("tourist_id", "origin_lat", "origin_long", "state",
                "nearest_stop_id", "nearest_stop_name", "nearest_stop_feed",
                "nearest_stop_lat", "nearest_stop_lon", "dist_to_station_km",
                "dest_lat", "dest_long")]
readr::write_csv(out, "data/output/pt_rail_ch_fr_sample.csv")
log_msg(sprintf("Wrote data/output/pt_rail_ch_fr_sample.csv | %d rows", nrow(out)))
log_msg("build_ch_fr_sample.R done.")
