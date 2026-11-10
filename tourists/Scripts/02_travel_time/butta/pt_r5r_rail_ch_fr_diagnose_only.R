# ============================================================
# pt_r5r_rail_ch_fr_diagnose_only.R
# Re-runs ONLY the stuck-diagnosis step from pt_r5r_rail_ch_fr_fixed.R,
# against the network it already built (data/r5_rail_ch_fr, now containing
# the frequency-fixed CH feed -- do NOT delete network.dat here, that
# would force another ~80-minute rebuild for nothing).
#
# Needed because the first diagnosis pass crashed partway through on a
# data.table bug in this script's own leg-printing code (detailed_
# itineraries() returns an sf geometry column by default; passing that to
# unique() crashes data.table's ordering) -- fixed here via
# drop_geometry = TRUE. The routing results themselves
# (data/output/pt_rail_ch_fr_sample_fixed.csv) already completed
# successfully before the crash and are reused as-is.
#
# Run in a FRESH R session:
#   source("Scripts/00_setup.R")
#   source("Scripts/02_travel_time/pt_r5r_rail_ch_fr_diagnose_only.R")
# ============================================================

.cfg0 <- yaml::read_yaml("Scripts/config.yml")
options(java.parameters = paste0("-Xmx", .cfg0$java_max_mem))

source("Scripts/00_setup.R")
if (!requireNamespace("r5r", quietly = TRUE)) install.packages("r5r", repos = "https://cloud.r-project.org")
library(r5r)

RAIL_DIR <- "data/r5_rail_ch_fr"

log_msg("Loading the already-built (frequency-fixed) r5 network -- should be fast, no rebuild...")
t0 <- Sys.time()
r5r_core <- r5r::setup_r5(data_path = RAIL_DIR, verbose = FALSE)
log_msg(sprintf("Network loaded in %.1f sec.", as.numeric(Sys.time() - t0, units = "secs")))

samp <- readr::read_csv("data/output/pt_rail_ch_fr_sample_fixed.csv", show_col_types = FALSE)
departure <- as.POSIXct(paste(cfg$pt$departure_date, cfg$pt$departure_time),
                         format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Zurich")

still_failing <- samp[is.na(samp$rail_time_min_fixed), ]
log_msg(sprintf("Investigating %d still-failing case(s) with detailed_itineraries() (drop_geometry fix applied)...", nrow(still_failing)))

stuck_report <- list()
for (i in seq_len(nrow(still_failing))) {
  r <- still_failing[i, ]
  o <- data.frame(id = r$tourist_id, lat = r$nearest_stop_lat, lon = r$nearest_stop_lon)
  d <- data.frame(id = r$tourist_id, lat = r$dest_lat,         lon = r$dest_long)

  log_msg(sprintf("  [%d/%d %s] origin=%s (%.2fkm) -> dest(%.4f,%.4f) [%s]",
                  i, nrow(still_failing), r$tourist_id, r$nearest_stop_name, r$dist_to_station_km,
                  r$dest_lat, r$dest_long, r$state))

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
    stuck_report[[i]] <- data.frame(tourist_id = r$tourist_id, stuck_at = "search_window_too_narrow")
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
    stuck_report[[i]] <- data.frame(tourist_id = r$tourist_id, stuck_at = "walk_cap_even_after_fix")
    next
  }

  log_msg("    -> NO itinerary found even with an 8h window AND no walk cap. Genuine network-connectivity gap.")
  stuck_report[[i]] <- data.frame(tourist_id = r$tourist_id, stuck_at = "no_connectivity")
}

stuck_df <- dplyr::bind_rows(stuck_report)
readr::write_csv(stuck_df, "data/output/_fr_stuck_diagnosis.csv")
log_msg("Stuck-reason breakdown:")
print(table(stuck_df$stuck_at))

r5r::stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)
log_msg("pt_r5r_rail_ch_fr_diagnose_only.R done.")
