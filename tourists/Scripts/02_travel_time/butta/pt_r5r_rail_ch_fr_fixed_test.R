# ============================================================
# pt_r5r_rail_ch_fr_fixed_test.R   (one-off test, archived in butta/)
# Split out of the former pt_r5r_rail_ch_fr_fixed.R (now OEV/FR_CH/02_clean_CH_gtfs.R,
# which only produces the frequency-fixed Swiss GTFS feed).
#
# Before/after test of that fix: re-routes the 100-tourist sample of the earlier
# experiment on the CH+France rail network (now containing the fixed CH feed) with the
# standard 30-min walk cap, then runs detailed_itineraries() on whatever still fails,
# with an 8h window, to pinpoint where each case breaks down.
#
# Input : data/output/gtfs/ch.freqfixed.gtfs.zip, data/r5_rail_ch_fr/ (r5 network),
#         data/output/butta/pt_rail_ch_fr_sample.csv
# Output: data/output/csv/pt_rail_ch_fr_sample_fixed.csv, _fr_stuck_diagnosis.csv
#
# Run in a FRESH R session, from the project root:
#   source("Scripts/02_travel_time/butta/pt_r5r_rail_ch_fr_fixed_test.R")
# ============================================================

.cfg0 <- yaml::read_yaml("Scripts/config.yml")
options(java.parameters = paste0("-Xmx", .cfg0$java_max_mem))

source("Scripts/00_setup.R")
if (!requireNamespace("r5r", quietly = TRUE)) install.packages("r5r", repos = "https://cloud.r-project.org")
library(r5r)

RAIL_DIR       <- "data/r5_rail_ch_fr"
FIXED_CH_GTFS  <- "data/output/gtfs/ch.freqfixed.gtfs.zip"
OLD_SAMPLE_CSV <- "data/output/butta/pt_rail_ch_fr_sample.csv"   # earlier 100-tourist sample
rebuild_network = FALSE

# --- Step 2: swap the fixed feed into RAIL_DIR, force a network rebuild ----
if(rebuild_network == TRUE){
  file.copy(FIXED_CH_GTFS, file.path(RAIL_DIR, basename(FIXED_CH_GTFS)), overwrite = TRUE)
  for (f in c("network.dat", "network_settings.json", "gtfs_errors.csv")) {
    fp <- file.path(RAIL_DIR, f)
    if (file.exists(fp)) file.remove(fp)
  }
  log_msg("Swapped in the frequency-fixed CH feed and cleared the cached network.dat -- rebuilding.")
}

# --- Step 3: rebuild the r5 network -----------------------------------------
log_msg("Building r5 network with the fixed CH feed (reuses the existing OSM MapDB cache if r5 allows it)...")
t0 <- Sys.time()
r5r_core <- r5r::setup_r5(data_path = RAIL_DIR, verbose = FALSE)
log_msg(sprintf("Network ready in %.1f min.", as.numeric(Sys.time() - t0, units = "mins")))

# --- Step 4: re-route the same 100 tourists, standard 30-min walk cap ------
samp <- readr::read_csv(OLD_SAMPLE_CSV, show_col_types = FALSE)
# rail_stops <- readr::read_csv(file.path(RAIL_DIR, "rail_stops.csv"), show_col_types = FALSE)
# samp <- dplyr::left_join(
#   samp,
#   dplyr::rename(rail_stops, nearest_stop_lat = stop_lat, nearest_stop_lon = stop_lon),
#   by = c("nearest_stop_id" = "stop_id")
# )

departure <- as.POSIXct(paste(cfg$pt$departure_date, cfg$pt$departure_time),
                         format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Zurich")
log_msg(sprintf("Re-routing %d tourists with the STANDARD %d-min walk cap (matches config.yml, for a clean before/after vs. the original run)...",
                nrow(samp), cfg$pt$max_walk_time_min))

route_one <- function(o, d) {
  ttm <- tryCatch(
    r5r::travel_time_matrix(
      r5r_core, origins = o, destinations = d, mode = c("WALK", "RAIL"),
      departure_datetime = departure, time_window = cfg$pt$time_window_minutes,
      percentiles = cfg$pt$percentile, max_trip_duration = cfg$pt$max_trip_duration_min,
      walk_speed = cfg$pt$walk_speed_kmh, max_walk_time = cfg$pt$max_walk_time_min,
      verbose = FALSE, progress = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(ttm) || nrow(ttm) == 0) return(NA_real_)
  tt_col <- grep("travel_time", names(ttm), value = TRUE)[1]
  as.numeric(ttm[[tt_col]][1])
}

samp$rail_time_min_fixed <- NA_real_
for (i in seq_len(nrow(samp))) {
  o <- data.frame(id = samp$tourist_id[i], lat = samp$nearest_stop_lat[i], lon = samp$nearest_stop_lon[i])
  d <- data.frame(id = samp$tourist_id[i], lat = samp$dest_lat[i],         lon = samp$dest_long[i])
  samp$rail_time_min_fixed[i] <- route_one(o, d)
  if (i %% 20 == 0 || i == nrow(samp)) log_msg(sprintf("  routed %d / %d", i, nrow(samp)))
}

log_msg(sprintf("BEFORE (original, buggy CH feed, 30-min cap): %d / %d missing.",
                sum(is.na(samp$rail_time_min)), nrow(samp)))
log_msg(sprintf("AFTER  (frequency-fixed CH feed, 30-min cap): %d / %d missing.",
                sum(is.na(samp$rail_time_min_fixed)), nrow(samp)))
readr::write_csv(samp, file.path(CSV_DIR, "pt_rail_ch_fr_sample_fixed.csv"))

# --- Step 5: pinpoint exactly where r5r gets stuck on whatever still fails -
still_failing <- samp[is.na(samp$rail_time_min_fixed), ]
log_msg(sprintf("Investigating %d still-failing case(s) with detailed_itineraries() (now unblocked)...", nrow(still_failing)))

stuck_report <- list()
if (nrow(still_failing) > 0) {
  for (i in seq_len(nrow(still_failing))) {
    r <- still_failing[i, ]
    o <- data.frame(id = r$tourist_id, lat = r$nearest_stop_lat, lon = r$nearest_stop_lon)
    d <- data.frame(id = r$tourist_id, lat = r$dest_lat,         lon = r$dest_long)

    log_msg(sprintf("  [%s] origin=%s (%.2fkm from tourist) -> dest(%.4f,%.4f) [%s]",
                    r$tourist_id, r$nearest_stop_name, r$dist_to_station_km, r$dest_lat, r$dest_long, r$state))

    # Try progressively more permissive settings to isolate WHERE it breaks:
    # (a) standard cap, wide window; (b) walk cap removed too.
    it_std <- tryCatch(
      r5r::detailed_itineraries(
        r5r_core, origins = o, destinations = d, mode = c("WALK", "RAIL"),
        departure_datetime = departure, time_window = 480,
        max_trip_duration = cfg$pt$max_trip_duration_min,
        walk_speed = cfg$pt$walk_speed_kmh, max_walk_time = cfg$pt$max_walk_time_min,
        shortest_path = FALSE, drop_geometry = TRUE, verbose = FALSE, progress = FALSE
      ),
      error = function(e) { log_msg(sprintf("    [std cap] error: %s", conditionMessage(e))); NULL }
    )

    if (!is.null(it_std) && nrow(it_std) > 0) {
      log_msg(sprintf("    -> FOUND with 8h window + standard walk cap (%d leg rows). Original 120-min search window was too narrow.", nrow(it_std)))
      stuck_report[[i]] <- data.frame(tourist_id = r$tourist_id, stuck_at = "search_window_too_narrow", detail = NA_character_)
      next
    }

    it_nocap <- tryCatch(
      r5r::detailed_itineraries(
        r5r_core, origins = o, destinations = d, mode = c("WALK", "RAIL"),
        departure_datetime = departure, time_window = 480,
        max_trip_duration = cfg$pt$max_trip_duration_min,
        walk_speed = cfg$pt$walk_speed_kmh, max_walk_time = 240,
        shortest_path = FALSE, drop_geometry = TRUE, verbose = FALSE, progress = FALSE
      ),
      error = function(e) { log_msg(sprintf("    [no walk cap] error: %s", conditionMessage(e))); NULL }
    )

    if (!is.null(it_nocap) && nrow(it_nocap) > 0) {
      leg_cols <- intersect(c("mode","route","from_name","to_name"), names(it_nocap))
      legs <- as.data.frame(it_nocap)[, leg_cols, drop = FALSE]
      legs <- legs[!duplicated(legs), , drop = FALSE]
      log_msg(sprintf("    -> FOUND only with walk cap removed too (%d leg rows) -- access/egress walk exceeds %d min even with the fix.",
                      nrow(it_nocap), cfg$pt$max_walk_time_min))
      print(legs)
      stuck_report[[i]] <- data.frame(tourist_id = r$tourist_id, stuck_at = "walk_cap_even_after_fix", detail = NA_character_)
      next
    }

    log_msg("    -> NO itinerary found even with an 8h window AND no walk cap. Genuine network-connectivity gap (no RAIL path exists between these two points in this network).")
    stuck_report[[i]] <- data.frame(tourist_id = r$tourist_id, stuck_at = "no_connectivity", detail = NA_character_)
  }
  stuck_df <- dplyr::bind_rows(stuck_report)
  readr::write_csv(stuck_df, file.path(CSV_DIR, "_fr_stuck_diagnosis.csv"))
  log_msg("Stuck-reason breakdown:")
  print(table(stuck_df$stuck_at))
}

r5r::stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)
log_msg("pt_r5r_rail_ch_fr_fixed_test.R done.")
