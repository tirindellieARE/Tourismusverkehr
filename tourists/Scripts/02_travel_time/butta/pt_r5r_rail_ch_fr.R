# ============================================================
# pt_r5r_rail_ch_fr.R
# France-only PT routing experiment: Switzerland + near-CH France, via r5r.
# Sibling to pt_r5r_rail_ch_it.R -- see that script's header for the shared
# design rationale (explicit nearest-station join, RAIL-only r5r `mode`
# rather than a filtered GTFS/OSM feed, etc.). This header covers only
# what's DIFFERENT for France.
#
# GEOGRAPHIC SCOPE -- near-CH France only, not all of France:
#   Checked directly against agqpv.csv (origin_commune's GISCO country-
#   prefix). ALL French-origin tourists (3,631 rows, 987 unique origins)
#   unioned with their CH destinations gives a bounding box of
#   1,227,072 km^2 -- over R5's 975,000 km^2 hard limit, for the same
#   reason as Italy: France spans from Brittany (-4.7 deg long) to near
#   Lille (50.95 deg lat), and the bbox has to stretch to cover the
#   farthest outlier regardless of how few tourists are actually there.
#
#   UNLIKE Italy, a single latitude cutoff doesn't cleanly separate a
#   "near CH" cluster from the rest -- France's outliers spread in both
#   latitude AND longitude (Paris, Brittany, the south coast), not just
#   north-south. So the split here is by actual REGION name, not a
#   coordinate threshold: all 987 unique origins were reverse-geocoded
#   (data/output/origins_fr_geocoded.csv, via the same Transitous
#   reverse-geocode endpoint used in geocode_origins.R) to get their real
#   French region. The three regions that actually border Switzerland --
#   Auvergne-Rhône-Alpes, Grand Est, Bourgogne-Franche-Comté -- account
#   for 3,019 / 3,631 rows (83.1%) and give a bbox of 400,747 km^2, safely
#   under the cap. Everything else (Paris, the south coast, Brittany,
#   Corsica, etc. -- 612 rows, 16.9%) is excluded from this network, same
#   as Italy's excluded south/central rows.
#
# GTFS FEEDS -- reuses data/gtfs/ch.gtfs.zip and fr.gtfs.zip as-is.
#   UNLIKE Italy's it.gtfs.zip (Trenord/Lombardia regional rail only,
#   which is why 43% of North-Italy origins had zero regional coverage
#   and needed additional per-region feeds hunted down), fr.gtfs.zip is
#   SNCF's NATIONAL feed: checked directly -- 8,943 stops spanning the
#   whole country (lat 41.4-52.5, long -4.5 to 13.4), 4,870 of which
#   (54.5%) already fall inside the near-CH bbox alone. So unlike the
#   Italy script, no additional regional GTFS hunting was needed here --
#   the existing feed should already have real coverage for this area.
#   route_type breakdown: 2 (Rail, 589 routes), 3 (Bus, 126), 0 (Tram, 4)
#   -- all R5-supported types, same RAIL_TYPES filter (route_type 2 or
#   the CH feed's extended 100-117 hierarchy) applies at query time.
#
# Run in a FRESH R session:
#   source("Scripts/00_setup.R")
#   source("Scripts/02_travel_time/pt_r5r_rail_ch_fr.R")
# ============================================================

# --- Set JVM heap BEFORE anything loads r5r ---------------------------------
.cfg0 <- yaml::read_yaml("Scripts/config.yml")
options(java.parameters = paste0("-Xmx", .cfg0$java_max_mem))

Sys.setenv(PATH = paste("C:/Users/Modellrechner.ARE/miniforge3/envs/r5/Library/bin",
                        Sys.getenv("PATH"), sep = ";"))
Sys.which("osmium")   # should print the full path to osmium.exe

source("Scripts/00_setup.R")
if (!requireNamespace("r5r", quietly = TRUE)) install.packages("r5r", repos = "https://cloud.r-project.org")
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table", repos = "https://cloud.r-project.org")
library(r5r)
library(data.table)

FR_NEAR_CH_REGIONS <- c("Auvergne-Rhône-Alpes", "Grand Est", "Bourgogne-Franche-Comté")
RAIL_DIR <- "data/r5_rail_ch_fr"
STAGING_DIR <- "data/r5_rail_ch_fr_staging"
ensure_dir(RAIL_DIR); ensure_dir(STAGING_DIR)
SAMPLE_N <- 100
SEED <- 42

area_km2 <- function(xmin, ymin, xmax, ymax) {
  midlat <- (ymin + ymax) / 2
  (xmax - xmin) * 111.32 * cos(midlat * pi / 180) * (ymax - ymin) * 111.32
}

# --- Load agqpv, identify near-CH French-origin tourists -------------------
geo_file <- "data/output/origins_fr_geocoded.csv"
if (!file.exists(geo_file)) {
  stop("Missing ", geo_file, " -- run the FR reverse-geocode step first (see this script's header).")
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

# --- Bounding box check -- report immediately, refuse to build if over -----
pad <- cfg$bbox_pad_deg
xmin <- min(fr_near$origin_long, fr_near$dest_long) - pad
xmax <- max(fr_near$origin_long, fr_near$dest_long) + pad
ymin <- min(fr_near$origin_lat,  fr_near$dest_lat)  - pad
ymax <- max(fr_near$origin_lat,  fr_near$dest_lat)  + pad

# Île-de-France extent (from the IDFM feed's stops), so Paris stations and
# metro/RER stops are linked to the street network
IDF_BBOX <- c(xmin = 1.15, ymin = 47.96, xmax = 3.56, ymax = 49.46)
xmin <- min(xmin, IDF_BBOX["xmin"] - pad)
ymin <- min(ymin, IDF_BBOX["ymin"] - pad)
xmax <- max(xmax, IDF_BBOX["xmax"] + pad)
ymax <- max(ymax, IDF_BBOX["ymax"] + pad)

bbox_area <- area_km2(xmin, ymin, xmax, ymax)
log_msg(sprintf("Network bbox: [%.3f,%.3f,%.3f,%.3f]  area=%.0f km^2", xmin, ymin, xmax, ymax, bbox_area))
if (bbox_area >= 975000) {
  stop(sprintf("Bounding box (%.0f km^2) exceeds R5's 975,000 km^2 hard limit -- cannot build as one network. See this script's header.", bbox_area))
}
log_msg("Bbox is under the 975,000 km^2 R5 limit -- proceeding with a single network.")

# --- Build (or reuse) the CH+near-CH-France OSM extract ---------------------
# Intermediate (unclipped, whole-country) merge goes to a SEPARATE staging
# directory, not RAIL_DIR -- r5r's setup_r5() globs every *.osm.pbf file in
# its data_path and imports all of them, so the clipped and unclipped files
# must never sit in the same folder (see pt_r5r_rail_ch_it.R's header for
# the concrete bug this caused there).
osmium_ok <- nzchar(Sys.which("osmium"))
if (!osmium_ok) stop("osmium not found on PATH (needed to merge/clip the OSM extract).")

ch_pbf <- "data/osm/switzerland-latest.osm.pbf"
fr_pbf <- "data/osm/france-latest.osm.pbf"
if (!file.exists(ch_pbf) || !file.exists(fr_pbf)) {
  stop("Missing data/osm/switzerland-latest.osm.pbf and/or france-latest.osm.pbf. Run 02_download.R first.")
}

merged_pbf <- file.path(STAGING_DIR, "ch_fr_merged.osm.pbf")
if (!file.exists(merged_pbf)) {
  log_msg("Merging CH+FR OSM extracts (full countries, clipped next)...")
  system2("osmium", c("merge", shQuote(ch_pbf), shQuote(fr_pbf), "-o", shQuote(merged_pbf), "--overwrite"))
}

clipped_pbf <- file.path(RAIL_DIR, "network.osm.pbf")
if (!file.exists(clipped_pbf)) {
  log_msg("Clipping merged extract to the near-CH-France+CH bbox...")
  bbox_str <- sprintf("%f,%f,%f,%f", xmin, ymin, xmax, ymax)
  system2("osmium", c("extract", "-b", bbox_str, shQuote(merged_pbf), "-o", shQuote(clipped_pbf), "--overwrite"))
}

# --- Copy GTFS feeds into the network folder --------------------------------
gtfs_feeds <- c("data/gtfs/ch.gtfs.zip",
                "data/gtfs/fr.gtfs.zip",
                "data/gtfs/fr.Paris.gtfs.clean.zip")
for (src in gtfs_feeds) {
  if (!file.exists(src)) stop(sprintf("Missing %s", src))
  file.copy(src, file.path(RAIL_DIR, basename(src)), overwrite = TRUE)
}

# --- Build the r5r network (r5r caches network.dat across re-runs) ---------
log_msg("Building/loading r5 network (first build can take a while -- see routing_readme.md for the Windows flush-time note)...")
t0 <- Sys.time()
r5r_core <- r5r::setup_r5(data_path = RAIL_DIR, verbose = FALSE)
log_msg(sprintf("Network ready in %.1f min.", as.numeric(Sys.time() - t0, units = "mins")))

dep_date <- as.Date(cfg$pt$departure_date)
if (dep_date < as.Date("2026-10-05") || dep_date > as.Date("2026-11-03")) {
  stop("departure_date ", dep_date, " is outside the IDFM feed's validity (2026-10-05 to 2026-11-03).")
}

cfg$pt$departure_date = "2026-10-07"

# --- Rail-stop candidate list (from GTFS, not from the R5 network object) --
RAIL_TYPES <- as.character(c(2, 100:117))

get_rail_stops <- function(gtfs_zip, feed_label) {
  tmp <- file.path(tempdir(), paste0("gtfs_rail_", feed_label))
  unlink(tmp, recursive = TRUE); dir.create(tmp, recursive = TRUE)
  unzip(gtfs_zip, files = c("routes.txt", "trips.txt", "stop_times.txt", "stops.txt"),
        exdir = tmp, junkpaths = TRUE)
  routes <- fread(file.path(tmp, "routes.txt"), colClasses = "character")
  trips  <- fread(file.path(tmp, "trips.txt"),  colClasses = "character")
  stimes <- fread(file.path(tmp, "stop_times.txt"), colClasses = "character",
                   select = c("trip_id", "stop_id"))
  stops  <- fread(file.path(tmp, "stops.txt"), colClasses = "character")

  rail_route_ids <- routes[route_type %in% RAIL_TYPES, route_id]
  rail_trip_ids  <- trips[route_id %in% rail_route_ids, trip_id]
  rail_stop_ids  <- unique(stimes[trip_id %in% rail_trip_ids, stop_id])

  out <- stops[stop_id %in% rail_stop_ids & !grepl("OCECar", stop_id, fixed = TRUE),
               .(stop_id, stop_name, stop_lat = as.numeric(stop_lat), stop_lon = as.numeric(stop_lon))]
  out[, feed := feed_label]
  out
}

rail_stops <- rbind(get_rail_stops("data/gtfs/ch.gtfs.zip", "ch"),
                     get_rail_stops("data/gtfs/fr.gtfs.zip", "fr"))
log_msg(sprintf("Rail stops found: %d (ch: %d, fr: %d)",
                nrow(rail_stops), sum(rail_stops$feed == "ch"), sum(rail_stops$feed == "fr")))
readr::write_csv(rail_stops, file.path(RAIL_DIR, "rail_stops.csv"))

# --- Sample 100 near-CH French-origin tourists ------------------------------
set.seed(SEED)
sample_n <- min(SAMPLE_N, nrow(fr_near))
if (sample_n < SAMPLE_N) {
  log_msg(sprintf("WARNING: only %d near-CH French-origin rows available, sampling all of them.", sample_n))
}
samp <- fr_near[sample(nrow(fr_near), sample_n), ]
samp$tourist_id <- sprintf("T%03d", seq_len(nrow(samp)))
log_msg(sprintf("Sampled %d near-CH French-origin tourists (seed=%d).", nrow(samp), SEED))

# --- Nearest rail station per tourist (great-circle, via sf) ---------------
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

# --- Route each tourist: station -> destination, RAIL only -----------------
departure <- as.POSIXct(paste(cfg$pt$departure_date, cfg$pt$departure_time),
                         format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Zurich")
log_msg(sprintf("PT departure (off-peak): %s, window %d min, percentile %d, mode=WALK+RAIL",
                format(departure), cfg$pt$time_window_minutes, cfg$pt$percentile))

NO_CAP_MAX_WALK_MIN = 240
MAX_TRIP_DURATION = 1000


route_one <- function(o, d) {
  ttm <- tryCatch(
    r5r::travel_time_matrix(
      r5r_core, origins = o, destinations = d, mode = c("WALK", "RAIL", "SUBWAY", "TRAM"),
      max_rides = 8,
      departure_datetime = departure, time_window = cfg$pt$time_window_minutes,
      percentiles = cfg$pt$percentile, max_trip_duration = cfg$pt$max_trip_duration_min,
      walk_speed = cfg$pt$walk_speed_kmh, max_walk_time = NO_CAP_MAX_WALK_MIN,
      verbose = FALSE, progress = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(ttm) || nrow(ttm) == 0) return(NA_real_)
  tt_col <- grep("travel_time", names(ttm), value = TRUE)[1]
  as.numeric(ttm[[tt_col]][1])
}

samp = samp[4,]
samp$rail_time_min <- NA_real_
for (i in seq_len(nrow(samp))) {
  o <- data.frame(id = samp$tourist_id[i], lat = samp$nearest_stop_lat[i], lon = samp$nearest_stop_lon[i])
  d <- data.frame(id = samp$tourist_id[i], lat = samp$dest_lat[i],         lon = samp$dest_long[i])
  samp$rail_time_min[i] <- route_one(o, d)
  # if (i %% 20 == 0 || i == nrow(samp)) log_msg(sprintf("  routed %d / %d", i, nrow(samp)))
}

still_failing <- samp[is.na(samp$rail_time_min), ]
log_msg(sprintf("Investigating %d still-failing case(s) with detailed_itineraries()...", nrow(still_failing)))

r <- samp[samp$tourist_id == "T004", ]
o <- data.frame(id = "T004", lat = r$nearest_stop_lat, lon = r$nearest_stop_lon)
d <- data.frame(id = "T004", lat = r$dest_lat,         lon = r$dest_long)

base <- list(r5r_core, origins = o, destinations = d, mode = c("WALK", "TRANSIT"),
             departure_datetime = departure, time_window = cfg$pt$time_window_minutes,
             percentiles = cfg$pt$percentile, walk_speed = cfg$pt$walk_speed_kmh,
             max_walk_time = cfg$pt$max_walk_time_min,
             max_trip_duration = cfg$pt$max_trip_duration_min,
             max_rides = 3, verbose = FALSE, progress = FALSE)

try_tt <- function(...) {
  res <- suppressWarnings(do.call(r5r::travel_time_matrix, modifyList(base, list(...))))
  if (nrow(res) == 0) NA else res[[grep("travel_time", names(res))[1]]][1]
}

cat("Current settings: max_trip_duration =", cfg$pt$max_trip_duration_min,
    "| max_walk_time =", cfg$pt$max_walk_time_min, "| max_rides = 3\n")
cat("As configured:          ", try_tt(), "\n")
cat("Everything relaxed:     ", try_tt(max_trip_duration = 720, max_rides = 8, max_walk_time = 60), "\n")
cat("Only longer duration:   ", try_tt(max_trip_duration = 720), "\n")
cat("Only more rides:        ", try_tt(max_rides = 8), "\n")
cat("Only longer walk:       ", try_tt(max_walk_time = 60), "\n")
cat("Duration + rides:       ", try_tt(max_trip_duration = 720, max_rides = 8), "\n")


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


out <- samp[, c("tourist_id", "origin_lat", "origin_long", "state",
                "nearest_stop_id", "nearest_stop_name", "nearest_stop_feed", "dist_to_station_km",
                "dest_lat", "dest_long", "rail_time_min")]
readr::write_csv(out, "data/output/pt_rail_ch_fr_sample.csv")

log_msg(sprintf("Wrote data/output/pt_rail_ch_fr_sample.csv | %d rows | missing rail_time_min: %d / %d",
                nrow(out), sum(is.na(out$rail_time_min)), nrow(out)))

r5r::stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)
log_msg("pt_r5r_rail_ch_fr.R done.")
