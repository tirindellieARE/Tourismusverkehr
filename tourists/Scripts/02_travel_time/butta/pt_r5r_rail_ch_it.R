# ============================================================
# pt_r5r_rail_ch_it.R
# Rail-only PT routing experiment: Switzerland + North Italy, via r5r.
#
# WHY A SEPARATE SCRIPT (not the main tiled 04_pt_r5r.R): that pipeline
# covers all 5 countries and was abandoned for taking ~60h (see
# routing_readme.md). This is a smaller, single-network experiment scoped
# to just CH+Italy, restricted to the RAIL mode only, to validate r5r
# routing quality on real agqpv Italian-origin trips before deciding
# whether to extend further.
#
# GEOGRAPHIC SCOPE -- North Italy only, not all of Italy:
#   Checked directly against agqpv.csv (using origin_commune's GISCO
#   country-prefix, not the survey's GRENZABSCHNITT corridor field, which
#   was found to be unreliable -- some "IT corridor" rows had origins in
#   Spain/Germany). ALL Italian-origin tourists (2,786 rows) unioned with
#   their CH destinations gives a bounding box of 1,236,324 km^2 -- over
#   R5's 975,000 km^2 hard limit (see 04_pt_r5r.R's header for the limit
#   itself).
#   A CH + North-Italy-only split (origin latitude >= NORTH_LAT_CUTOFF,
#   2,622 / 2,786 rows = 94.1%) brings this down to 296,927 km^2, safely
#   under the cap. The complementary "South/Central Italy" network does
#   NOT shrink proportionally: even after excluding the 4 genuine Sicily
#   origins (checked against the authoritative GISCO commune boundaries,
#   not the incomplete public geocoder -- Calanna, Calabria mainland,
#   sits at essentially the same latitude, 38.2 deg, as Messina, Sicily,
#   and Lecce/Brindisi in Puglia reach further east, 18.2 deg, than any
#   Sicilian point does), that network is STILL 1,212,831 km^2 -- the
#   constraint is mainland southern Italy (Calabria/Puglia), not the
#   island. Decision: build North Italy only for now; the 164 central/
#   south-Italy rows are left uncovered by this script.
#
# DESIGN -- explicit nearest-station join, not r5r's automatic walk access:
#   Rather than routing WALK+RAIL from the tourist's raw address (which
#   would let r5r silently choose the access walk), this script explicitly
#   finds the nearest actual rail stop (from GTFS stop_times -- i.e. a
#   point real trains call at, not just any GTFS "stop") to each origin
#   coordinate first, then routes RAIL(+WALK for the destination-end
#   egress) from THAT station's exact coordinate. This makes the
#   station used for each tourist visible and inspectable, rather than
#   implicit inside r5r's access-leg search.
#   "Rail" = GTFS route_type 2 (standard) or 100-117 (the extended rail
#   hierarchy CH's feed uses -- high speed/long distance/regional/etc.);
#   excludes bus/tram/metro/funicular/aerial lift.
#
# GTFS FEEDS: reuses data/gtfs/ch.gtfs.zip and it.gtfs.zip as-is -- both
# already cleaned of R5-unsupported route_types in earlier work (ch had
# route_type=1500 "Taxi" routes stripped; it only ever had 2/3, both
# supported). No route-type filtering is needed at NETWORK BUILD time --
# the "rail only" restriction is applied at QUERY time via r5r's `mode`
# argument instead, which is the standard, robust way to do this (R5 uses
# GTFS purely for vehicle movement between stops; OSM only supplies the
# walk/access legs, so filtering the OSM layer to "rail tags" would be
# meaningless and would break station access).
#
# Run in a FRESH R session:
#   source("Scripts/00_setup.R")
#   source("Scripts/02_travel_time/pt_r5r_rail_ch_it.R")
# ============================================================

# --- Set JVM heap BEFORE anything loads r5r ---------------------------------
.cfg0 <- yaml::read_yaml("Scripts/config.yml")
options(java.parameters = paste0("-Xmx", .cfg0$java_max_mem))

source("Scripts/00_setup.R")
if (!requireNamespace("r5r", quietly = TRUE)) install.packages("r5r", repos = "https://cloud.r-project.org")
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table", repos = "https://cloud.r-project.org")
library(r5r)
library(data.table)

NORTH_LAT_CUTOFF <- 44
RAIL_DIR <- "data/r5_rail_ch_it"
ensure_dir(RAIL_DIR)
SAMPLE_N <- 100
SEED <- 42

area_km2 <- function(xmin, ymin, xmax, ymax) {
  midlat <- (ymin + ymax) / 2
  (xmax - xmin) * 111.32 * cos(midlat * pi / 180) * (ymax - ymin) * 111.32
}

# --- Load agqpv, identify North-Italy-origin tourists ------------------
agqpv <- readr::read_csv(cfg$input_csv, locale = readr::locale(encoding = cfg$input_encoding),
                          show_col_types = FALSE)
agqpv$commune_country <- substr(agqpv$origin_commune, 1, 2)
it_north <- agqpv[!is.na(agqpv$commune_country) & agqpv$commune_country == "IT" &
                   agqpv$origin_lat >= NORTH_LAT_CUTOFF, ]
log_msg(sprintf("North-Italy-origin tourist rows (origin_lat >= %.0f): %d", NORTH_LAT_CUTOFF, nrow(it_north)))

# --- Bounding box check -- report immediately, refuse to build if over -----
pad <- cfg$bbox_pad_deg
xmin <- min(it_north$origin_long, it_north$dest_long) - pad
xmax <- max(it_north$origin_long, it_north$dest_long) + pad
ymin <- min(it_north$origin_lat,  it_north$dest_lat)  - pad
ymax <- max(it_north$origin_lat,  it_north$dest_lat)  + pad
bbox_area <- area_km2(xmin, ymin, xmax, ymax)
log_msg(sprintf("Network bbox: [%.3f,%.3f,%.3f,%.3f]  area=%.0f km^2", xmin, ymin, xmax, ymax, bbox_area))
if (bbox_area >= 975000) {
  stop(sprintf("Bounding box (%.0f km^2) exceeds R5's 975,000 km^2 hard limit -- cannot build as one network. See this script's header.", bbox_area))
}
log_msg("Bbox is under the 975,000 km^2 R5 limit -- proceeding with a single network.")

# --- Build (or reuse) the CH+North-Italy OSM extract ------------------------
# The intermediate (unclipped, whole-country) merge is written to a SEPARATE
# staging directory, not RAIL_DIR -- r5r's setup_r5() globs every *.osm.pbf
# file present in its data_path and imports all of them as one combined
# street layer, so leaving the unclipped merge alongside the clipped
# network.osm.pbf caused it to import BOTH (confirmed: the MapDB scratch
# file reached 5.48GB, far larger than this region's clipped extent alone
# would produce, before the run was killed and this fixed). RAIL_DIR must
# contain ONLY network.osm.pbf (the clipped one) plus the GTFS zips.
osmium_ok <- nzchar(Sys.which("osmium"))
if (!osmium_ok) stop("osmium not found on PATH (needed to merge/clip the OSM extract).")

ch_pbf <- "data/osm/switzerland-latest.osm.pbf"
it_pbf <- "data/osm/italy-latest.osm.pbf"
if (!file.exists(ch_pbf) || !file.exists(it_pbf)) {
  stop("Missing data/osm/switzerland-latest.osm.pbf and/or italy-latest.osm.pbf. Run 02_download.R first.")
}

STAGING_DIR <- "data/r5_rail_ch_it_staging"
ensure_dir(STAGING_DIR)
merged_pbf <- file.path(STAGING_DIR, "ch_it_merged.osm.pbf")
if (!file.exists(merged_pbf)) {
  log_msg("Merging CH+IT OSM extracts (full countries, clipped next)...")
  system2("osmium", c("merge", shQuote(ch_pbf), shQuote(it_pbf), "-o", shQuote(merged_pbf), "--overwrite"))
}

clipped_pbf <- file.path(RAIL_DIR, "network.osm.pbf")
if (!file.exists(clipped_pbf)) {
  log_msg("Clipping merged extract to the North-Italy+CH bbox...")
  bbox_str <- sprintf("%f,%f,%f,%f", xmin, ymin, xmax, ymax)
  system2("osmium", c("extract", "-b", bbox_str, shQuote(merged_pbf), "-o", shQuote(clipped_pbf), "--overwrite"))
}

# --- Copy GTFS feeds into the network folder --------------------------------
for (feed in c("ch", "it")) {
  src <- sprintf("data/gtfs/%s.gtfs.zip", feed)
  if (!file.exists(src)) stop(sprintf("Missing %s", src))
  file.copy(src, file.path(RAIL_DIR, basename(src)), overwrite = TRUE)
}

# --- Build the r5r network (r5r caches network.dat across re-runs) ---------
log_msg("Building/loading r5 network (first build can take a while -- see routing_readme.md for the Windows flush-time note)...")
t0 <- Sys.time()
r5r_core <- r5r::setup_r5(data_path = RAIL_DIR, verbose = FALSE)
log_msg(sprintf("Network ready in %.1f min.", as.numeric(Sys.time() - t0, units = "mins")))

# --- Rail-stop candidate list (from GTFS, not from the R5 network object) --
# route_type 2 = standard GTFS "Rail"; 100-117 = the extended rail hierarchy
# (high speed / long distance / regional / suburban / etc.) that CH's feed
# uses. Excludes bus/tram/metro/funicular/aerial lift.
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

  out <- stops[stop_id %in% rail_stop_ids,
               .(stop_id, stop_name, stop_lat = as.numeric(stop_lat), stop_lon = as.numeric(stop_lon))]
  out[, feed := feed_label]
  out
}

rail_stops <- rbind(get_rail_stops("data/gtfs/ch.gtfs.zip", "ch"),
                     get_rail_stops("data/gtfs/it.gtfs.zip", "it"))
log_msg(sprintf("Rail stops found: %d (ch: %d, it: %d)",
                nrow(rail_stops), sum(rail_stops$feed == "ch"), sum(rail_stops$feed == "it")))
readr::write_csv(rail_stops, file.path(RAIL_DIR, "rail_stops.csv"))

# --- Sample 100 North-Italy-origin tourists ---------------------------------
set.seed(SEED)
sample_n <- min(SAMPLE_N, nrow(it_north))
if (sample_n < SAMPLE_N) {
  log_msg(sprintf("WARNING: only %d North-Italy-origin rows available, sampling all of them.", sample_n))
}
samp <- it_north[sample(nrow(it_north), sample_n), ]
samp$tourist_id <- sprintf("T%03d", seq_len(nrow(samp)))
log_msg(sprintf("Sampled %d North-Italy-origin tourists (seed=%d).", nrow(samp), SEED))

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

samp$rail_time_min <- NA_real_
for (i in seq_len(nrow(samp))) {
  o <- data.frame(id = samp$tourist_id[i], lat = samp$nearest_stop_lat[i], lon = samp$nearest_stop_lon[i])
  d <- data.frame(id = samp$tourist_id[i], lat = samp$dest_lat[i],         lon = samp$dest_long[i])
  samp$rail_time_min[i] <- route_one(o, d)
  if (i %% 20 == 0 || i == nrow(samp)) log_msg(sprintf("  routed %d / %d", i, nrow(samp)))
}

out <- samp[, c("tourist_id", "origin_lat", "origin_long",
                "nearest_stop_id", "nearest_stop_name", "nearest_stop_feed", "dist_to_station_km",
                "dest_lat", "dest_long", "rail_time_min")]
readr::write_csv(out, "data/output/pt_rail_ch_it_sample.csv")

log_msg(sprintf("Wrote data/output/pt_rail_ch_it_sample.csv | %d rows | missing rail_time_min: %d / %d",
                nrow(out), sum(is.na(out$rail_time_min)), nrow(out)))

r5r::stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)
log_msg("pt_r5r_rail_ch_it.R done.")
