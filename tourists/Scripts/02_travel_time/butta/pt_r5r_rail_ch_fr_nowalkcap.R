# ============================================================
# pt_r5r_rail_ch_fr_nowalkcap.R
# Follow-up to pt_r5r_rail_ch_fr.R: re-routes the SAME 100 sampled tourists
# (data/output/pt_rail_ch_fr_sample.csv) against the SAME already-built
# network (data/r5_rail_ch_fr, reused via its cached network.dat -- no
# rebuild needed), but with max_walk_time raised from 30 min to 240 min
# (effectively removing the access/egress walk cap as a binding constraint
# -- at walk_speed_kmh=4.0 that's a 16km walk radius, far more than any
# station distance seen in the sample, max was ~22km origin-side).
#
# WHY: the original run had 63/100 missing rail times. Checking each
# failure's straight-line distance from its nearest_stop_id / dest_lat-long
# to the nearest rail stop showed EVERY destination beyond 2km (the
# original 30-min/4km/h cap) failed, and every success was under 2km --
# a clean, near-total explanation for 48/63 failures on the destination
# side alone, plus several more on the origin side (checked separately).
# This script isolates that effect: with the cap removed, whatever still
# fails is NOT an access/egress-distance problem, and needs a different
# explanation (checked in the second half of this script via
# r5r::detailed_itineraries() for a few remaining cases).
#
# Run in a FRESH R session:
#   source("Scripts/00_setup.R")
#   source("Scripts/02_travel_time/pt_r5r_rail_ch_fr_nowalkcap.R")
# ============================================================
rJavaEnv::use_java(21)
rJavaEnv::java_check_version_rjava()   # should now say 21
library(r5r)

setwd("E:/ARE/ProjekteTIE/Turismusverkehr/RScript/tourists")

.cfg0 <- yaml::read_yaml("Scripts/config.yml")
options(java.parameters = paste0("-Xmx", .cfg0$java_max_mem))

source("Scripts/00_setup.R")
if (!requireNamespace("r5r", quietly = TRUE)) install.packages("r5r", repos = "https://cloud.r-project.org")
library(r5r)

RAIL_DIR <- "data/r5_rail_ch_fr"
NO_CAP_MAX_WALK_MIN <- 240   # 16km walk radius at 4km/h -- effectively "no cap" for this sample

samp <- readr::read_csv("data/output/pt_rail_ch_fr_sample.csv", show_col_types = FALSE)
# # pt_rail_ch_fr_sample.csv saved nearest_stop_id/name but not its lat/lon --
# # recover them by joining back against the saved rail-stop candidate list.
# rail_stops <- readr::read_csv(file.path(RAIL_DIR, "rail_stops.csv"), show_col_types = FALSE)
# rail_stops = dplyr::rename(rail_stops, nearest_stop_lat = stop_lat, nearest_stop_lon = stop_lon)
# samp <- dplyr::left_join(
#   samp,
#   rail_stops,
#   by = c("nearest_stop_id" = "stop_id")
# )
log_msg(sprintf("Re-routing %d tourists from the original sample, max_walk_time %d -> %d min.",
                nrow(samp), cfg$pt$max_walk_time_min, NO_CAP_MAX_WALK_MIN))

log_msg("Loading cached r5 network (should be fast -- network.dat already built)...")
t0 <- Sys.time()
r5r_core <- r5r::build_network(data_path = RAIL_DIR, verbose = FALSE)
log_msg(sprintf("Network loaded in %.1f sec.", as.numeric(Sys.time() - t0, units = "secs")))

departure <- as.POSIXct(paste(cfg$pt$departure_date, cfg$pt$departure_time),
                         format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Zurich")

route_one <- function(o, d, max_walk_min) {
  ttm <- tryCatch(
    r5r::travel_time_matrix(
      r5r_core, origins = o, destinations = d, mode = c("WALK", "TRANSIT"),
      departure_datetime = departure, time_window = cfg$pt$time_window_minutes,
      percentiles = cfg$pt$percentile, max_trip_duration = cfg$pt$max_trip_duration_min,
      walk_speed = cfg$pt$walk_speed_kmh, max_walk_time = max_walk_min,
      verbose = FALSE, progress = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(ttm) || nrow(ttm) == 0) return(NA_real_)
  tt_col <- grep("travel_time", names(ttm), value = TRUE)[1]
  as.numeric(ttm[[tt_col]][1])
}

samp$rail_time_min_nocap <- NA_real_
for (i in seq_len(nrow(samp))) {
  o <- data.frame(id = samp$tourist_id[i], lat = samp$nearest_stop_lat[i], lon = samp$nearest_stop_lon[i])
  d <- data.frame(id = samp$tourist_id[i], lat = samp$dest_lat[i],         lon = samp$dest_long[i])
  samp$rail_time_min_nocap[i] <- route_one(o, d, NO_CAP_MAX_WALK_MIN)
  # if (i %% 20 == 0 || i == nrow(samp)) log_msg(sprintf("  routed %d / %d", i, nrow(samp)))
}

n_still_missing <- sum(is.na(samp$rail_time_min_nocap))
log_msg(sprintf("Original missing: %d / %d. With walk cap removed: %d / %d missing.",
                sum(is.na(samp$rail_time_min)), nrow(samp), n_still_missing, nrow(samp)))

# readr::write_csv(samp, "data/output/pt_rail_ch_fr_sample_nowalkcap.csv")
# log_msg("Wrote data/output/pt_rail_ch_fr_sample_nowalkcap.csv")

# --- Investigate whatever still fails with r5r::detailed_itineraries() -----
still_failing <- samp[is.na(samp$rail_time_min_nocap), ]
log_msg(sprintf("Investigating %d still-failing case(s) with detailed_itineraries()...", nrow(still_failing)))

if (nrow(still_failing) > 0) {
  detail_rows <- list()
  for (i in seq_len(nrow(still_failing))) {
    r <- still_failing[i, ]
    o <- data.frame(id = r$tourist_id, lat = r$nearest_stop_lat, lon = r$nearest_stop_lon)
    d <- data.frame(id = r$tourist_id, lat = r$dest_lat,         lon = r$dest_long)

    log_msg(sprintf("  [%s] %s -> dest(%.4f,%.4f) | state=%s | origin_stop_dist=%.2fkm",
                    r$tourist_id, r$nearest_stop_name, r$dest_lat, r$dest_long, r$state, r$dist_to_station_km))

    it <- tryCatch(
      r5r::detailed_itineraries(
        r5r_core, origins = o, destinations = d, mode = c("WALK", "TRANSIT"),
        departure_datetime = departure, time_window = 480,   # widen search to 8h to rule out a scheduling-window miss
        max_trip_duration = cfg$pt$max_trip_duration_min,
        walk_speed = cfg$pt$walk_speed_kmh, max_walk_time = NO_CAP_MAX_WALK_MIN,
        shortest_path = FALSE, verbose = FALSE, progress = FALSE
      ),
      error = function(e) { log_msg(sprintf("    detailed_itineraries() error: %s", conditionMessage(e))); NULL }
    )
    if (is.null(it) || nrow(it) == 0) {
      log_msg("    -> NO itinerary found even with an 8h search window. Likely a genuine network-connectivity gap.")
    } else {
      log_msg(sprintf("    -> Found %d leg(s) across %d option(s) with the widened window:", nrow(it), length(unique(it$option))))
      print(it[, intersect(c("option","mode","route","from_name","to_name","departure_time","arrival_time"), names(it))])
    }
    detail_rows[[i]] <- data.frame(tourist_id = r$tourist_id, found_with_wide_window = !is.null(it) && nrow(it) > 0)
  }
  detail_summary <- dplyr::bind_rows(detail_rows)
  readr::write_csv(detail_summary, "data/output/_fr_nowalkcap_investigation.csv")
  log_msg(sprintf("Of the %d still-failing cases, %d were found with an 8h search window (search-window issue), %d genuinely have no route.",
                  nrow(detail_summary), sum(detail_summary$found_with_wide_window), sum(!detail_summary$found_with_wide_window)))
}

r5r::stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)
log_msg("pt_r5r_rail_ch_fr_nowalkcap.R done.")
