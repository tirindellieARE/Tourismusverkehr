# ============================================================
# 04_pt_r5r_ch_fr_route_all.R
# Routes ALL near-CH French-origin trips of agqpv.csv (not a sample) on the
# network built by 03_pt_r5r_ch_fr_build_network.R: nearest rail station of the
# origin -> destination. Same method as test/04_pt_r5r_ch_fr_route_sample.R.
#
# 1. Rows with a French origin commune in the regions bordering Switzerland
#    (cfg$oev_ch_fr$near_ch_depts) are kept -- the area covered by the network.
# 2. Only UNIQUE origin-destination pairs (origin lat/long + destination lat/long)
#    are kept. A pair occurring in several rows is routed once; n_obs and the sum
#    of weight say how many observations it stands for.
# 3. Pairs are split in two groups, routed separately:
#      day       n_nights == 0   (day trips)
#      overnight n_nights  > 0
#    A pair observed in both groups appears in both (and is routed in both).
# 4. Each origin gets its nearest rail station (SNCF coach stops excluded).
#
# Routing (always): travel_time_matrix() per group -> pt_time_min (percentile
#   cfg$pt$percentile) and pt_time_fastest_min (1st percentile) over the departure
#   window. r5r searches once per ORIGIN STATION, so the cost depends on the number
#   of distinct stations, not of pairs; origins are processed in chunks.
# Trip legs (LEGS_MODE "flagged" or "all"; "flagged" = pairs that depend on a long walk, see
#   WALK_CAP_MIN): per pair, the fastest departure minute
#   is found (travel_time_matrix every DETAIL_MINUTE_STEP minutes, window = 1 min),
#   then detailed_itineraries() gives its legs. Legs are written in batches, and
#   both the minute scan and the legs can be resumed after an interruption.
#
# Input : data/output/csv/agqpv.csv, the r5r network in data/r5_rail_ch_fr/, the GTFS
#         feeds in it (the rail station list data/r5_rail_ch_fr/rail_stops.csv is
#         built from data/output/gtfs/ch.freqfixed and data/input/gtfs/fr if missing)
# Output (data/output/csv, one set per group <g> = day / overnight):
#         pt_ch_fr_pairs.csv               unique pairs, station, n_obs per group
#         pt_ch_fr_times_<g>.csv           pairs of the group + PT travel times
#         pt_ch_fr_legs_<g>.csv            (LEGS_MODE) legs of the fastest trip per pair
#         pt_ch_fr_trips_<g>.csv           (LEGS_MODE) departure/arrival, routes, waits
#
# Departure date: pt$departure_date; start time and window per group: oev_ch_fr$route_all (config.yml). Not changed
# automatically; the script stops if the date is outside the validity of the feeds.
#
# Run in a FRESH R session, after the build script:
#   source("Scripts/02_travel_time/OEV/FR_CH/04_pt_r5r_ch_fr_route_all.R")
# ============================================================

# Run from the project root.

# --- Java heap must be set before anything loads r5r ------------------------
.cfg0 <- yaml::read_yaml("Scripts/config.yml")
options(java.parameters = paste0("-Xmx", .cfg0$java_max_mem))

source("Scripts/00_setup.R")   # provides cfg, log_msg, ensure_dir, CSV_DIR (loads sf, data.table)
library(r5r)

# --- Settings ----------------------------------------------------------------
NET_DIR  <- "data/r5_rail_ch_fr"                  # built by 03_pt_r5r_ch_fr_build_network.R
OUT_DIR  <- CSV_DIR                               # data/output/csv
ensure_dir(OUT_DIR)
GROUPS_TO_RUN <- c("day", "overnight")            # which groups to route

LEGS_MODE          <- "flagged"   # "none": travel times only | "flagged": legs only for pairs flagged as walk-dependent | "all": legs for every pair (slow)
WALK_CAP_MIN       <- 15      # flagging: the matrix is rerun with walking capped at this many minutes ...
FLAG_EXTRA_MIN     <- 10      # ... and a pair is flagged if it then has no route or is slower by more than this
DETAIL_MINUTE_STEP <- 5       # minutes between the departures checked when looking for the fastest trip
DIAGNOSE_FAILS     <- TRUE    # re-test pairs without a time with each limit relaxed (fail_reason)

FR_NEAR_CH_DEPTS   <- cfg$oev_ch_fr$near_ch_depts # region -> INSEE departement codes (config.yml)
RAIL_STOP_FEEDS <- c(ch = "data/output/gtfs/ch.freqfixed.gtfs.zip",
                     fr = "data/input/gtfs/fr.gtfs.zip")
RAIL_TYPES <- as.character(c(2, 100:117))         # GTFS route_type: 2 = rail, 100-117 = extended rail types

MODES         <- c("WALK", "TRANSIT")
MAX_RIDES     <- 8                                # r5r's default of 3 is too low (see test/04_...)
MAX_WALK_MIN  <- 240
MAX_TRIP_MIN  <- 1000
RELAXED <- list(max_rides = 10, max_trip_duration = 720, max_walk_time = 60)   # diagnosis only
FASTEST_PCT   <- 1                                # r5r accepts percentiles 1-99; the 1st stands in for the minimum
SNAP_MIN_M    <- 200                              # destinations further than this from the street network are moved onto it
SNAP_RADIUS_M <- 20000                            # search radius of the snapping
CHUNK_ORIGINS <- 200                              # origin stations per travel_time_matrix() call
N_THREADS     <- Inf                              # r5r threads for the minute scan / itineraries
LEGS_BATCH    <- 200                              # pairs per batch of itineraries written to disk
STOP_MATCH_M  <- 150                              # max distance to name a leg's start/end stop

# --- Departure time and feed validity ----------------------------------------
departure <- as.POSIXct(paste(cfg$pt$departure_date, cfg$pt$departure_time),
                        format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Zurich")
if (is.na(departure)) {
  stop("Could not parse departure from config -- pt$departure_date must be 'YYYY-MM-DD' ",
       "and pt$departure_time 'HH:MM:SS'.")
}

# Period covered by a GTFS feed: feed_info.txt if present, otherwise calendar/calendar_dates
feed_validity <- function(zip) {
  files <- unzip(zip, list = TRUE)$Name
  tmp <- file.path(tempdir(), paste0("validity_", tools::file_path_sans_ext(basename(zip))))
  unlink(tmp, recursive = TRUE); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  rd <- function(f, cols) {
    unzip(zip, files = f, exdir = tmp, junkpaths = TRUE)
    fread(file.path(tmp, f), select = cols, colClasses = "character")
  }
  if ("feed_info.txt" %in% files) {
    fi <- rd("feed_info.txt", c("feed_start_date", "feed_end_date"))
    if (nrow(fi) && all(nzchar(unlist(fi[1])))) return(as.Date(unlist(fi[1]), "%Y%m%d"))
  }
  d <- character()
  if ("calendar.txt" %in% files)       { cal <- rd("calendar.txt", c("start_date", "end_date")); d <- c(d, cal$start_date, cal$end_date) }
  if ("calendar_dates.txt" %in% files) { d <- c(d, rd("calendar_dates.txt", "date")$date) }
  if (!length(d)) stop("No validity information in ", zip)
  range(as.Date(d, "%Y%m%d"))
}
feeds <- list.files(NET_DIR, pattern = "\\.zip$", full.names = TRUE)
validity <- t(sapply(feeds, function(z) as.character(feed_validity(z))))
rownames(validity) <- basename(feeds)
valid_from <- as.Date(max(validity[, 1])); valid_to <- as.Date(min(validity[, 2]))
log_msg(sprintf("Feed validity: %s", paste(sprintf("%s %s..%s", rownames(validity), validity[, 1], validity[, 2]), collapse = " | ")))
if (as.Date(departure) < valid_from || as.Date(departure) > valid_to) {
  stop("pt$departure_date ", as.Date(departure), " in config.yml is outside the period covered by all feeds (",
       valid_from, " to ", valid_to, "). Choose a date inside it.")
}
if (!file.exists(file.path(NET_DIR, "network.dat"))) {
  stop("No network.dat in ", NET_DIR, " -- run 03_pt_r5r_ch_fr_build_network.R first.")
}

# --- Candidate origin stations (rail only, no SNCF coach stops) --------------
RAIL_STOPS_CSV <- file.path(NET_DIR, "rail_stops.csv")
get_rail_stops <- function(gtfs_zip, feed_label) {
  tmp <- file.path(tempdir(), paste0("gtfs_rail_", feed_label))
  unlink(tmp, recursive = TRUE); dir.create(tmp, recursive = TRUE)
  unzip(gtfs_zip, files = c("routes.txt", "trips.txt", "stop_times.txt", "stops.txt"), exdir = tmp, junkpaths = TRUE)
  routes <- fread(file.path(tmp, "routes.txt"), colClasses = "character")
  trips  <- fread(file.path(tmp, "trips.txt"),  colClasses = "character")
  stimes <- fread(file.path(tmp, "stop_times.txt"), colClasses = "character", select = c("trip_id", "stop_id"))
  stops  <- fread(file.path(tmp, "stops.txt"), colClasses = "character")
  unlink(tmp, recursive = TRUE)
  rail_trip_ids <- trips[route_id %in% routes[route_type %in% RAIL_TYPES, route_id], trip_id]
  rail_stop_ids <- unique(stimes[trip_id %in% rail_trip_ids, stop_id])
  out <- stops[stop_id %in% rail_stop_ids & !grepl("OCECar", stop_id, fixed = TRUE),
               .(stop_id, stop_name, stop_lat = as.numeric(stop_lat), stop_lon = as.numeric(stop_lon))]
  out[, feed := feed_label]
  out
}
if (file.exists(RAIL_STOPS_CSV)) {
  rail_stops <- fread(RAIL_STOPS_CSV, colClasses = list(character = c("stop_id", "stop_name", "feed")))
} else {
  rail_stops <- rbindlist(Map(get_rail_stops, RAIL_STOP_FEEDS, names(RAIL_STOP_FEEDS)))
  fwrite(rail_stops, RAIL_STOPS_CSV)
}
log_msg(sprintf("Rail stops: %d", nrow(rail_stops)))

# --- Unique origin-destination pairs, split by n_nights ----------------------
agqpv <- as.data.table(readr::read_csv(cfg$input_csv, locale = readr::locale(encoding = cfg$input_encoding),
                                       show_col_types = FALSE))
fr <- agqpv[substr(origin_commune, 1, 2) %in% "FR"]
fr[, dept := substr(origin_commune, 4, 5)]
fr[, state := NA_character_]
for (reg in names(FR_NEAR_CH_DEPTS)) fr[dept %in% FR_NEAR_CH_DEPTS[[reg]], state := reg]
fr_near <- fr[!is.na(state)]
log_msg(sprintf("French-origin rows: %d | near-CH (network area): %d", nrow(fr), nrow(fr_near)))
stopifnot(!anyNA(fr_near$n_nights))
fr_near[, group := fifelse(n_nights == 0, "day", "overnight")]
fr_near[, pair_key := sprintf("%.6f|%.6f|%.6f|%.6f", origin_lat, origin_long, dest_lat, dest_long)]

pairs <- fr_near[, .(n_obs = .N, weight = sum(weight), origin_commune = origin_commune[1], state = state[1],
                     origin_lat = origin_lat[1], origin_long = origin_long[1],
                     dest_lat = dest_lat[1], dest_long = dest_long[1]), by = .(group, pair_key)]
pairs[, pair_id := sprintf("P%05d", .GRP), by = pair_key]    # same id for a pair in both groups

# Nearest rail station, computed once per distinct origin point
orig <- unique(pairs[, .(origin_lat, origin_long)])
orig_sf  <- sf::st_as_sf(orig, coords = c("origin_long", "origin_lat"), crs = 4326, remove = FALSE)
stops_sf <- sf::st_as_sf(rail_stops, coords = c("stop_lon", "stop_lat"), crs = 4326)
nn <- sf::st_nearest_feature(orig_sf, stops_sf)
orig[, `:=`(nearest_stop_id = rail_stops$stop_id[nn], nearest_stop_name = rail_stops$stop_name[nn],
            nearest_stop_feed = rail_stops$feed[nn], nearest_stop_lat = rail_stops$stop_lat[nn],
            nearest_stop_lon = rail_stops$stop_lon[nn],
            dist_to_station_km = as.numeric(sf::st_distance(orig_sf, stops_sf[nn, ], by_element = TRUE)) / 1000)]
pairs <- merge(pairs, orig, by = c("origin_lat", "origin_long"), sort = FALSE)
# Destination key: distinct destination points
pairs[, d_key := sprintf("%.6f|%.6f", dest_lat, dest_long)]
setcolorder(pairs, c("pair_id", "group", "origin_commune", "state", "origin_lat", "origin_long", "dest_lat", "dest_long",
                     "nearest_stop_id", "nearest_stop_name", "nearest_stop_feed", "nearest_stop_lat", "nearest_stop_lon",
                     "dist_to_station_km", "n_obs", "weight"))
setorder(pairs, group, pair_id)

for (g in c("day", "overnight")) {
  p <- pairs[group == g]
  log_msg(sprintf("Group %-9s: %5d rows -> %5d unique pairs | %4d origin stations | %4d destinations",
                  g, fr_near[group == g, .N], nrow(p), uniqueN(p$nearest_stop_id), uniqueN(p$d_key)))
}
log_msg(sprintf("Nearest-station distance: median %.1f km, max %.1f km",
                median(pairs$dist_to_station_km), max(pairs$dist_to_station_km)))
stopifnot(!anyNA(pairs$nearest_stop_lat), !anyDuplicated(pairs[, .(pair_id, group)]))

# --- Network -----------------------------------------------------------------
t0 <- Sys.time()
r5r_core <- r5r::build_network(data_path = NET_DIR, verbose = FALSE)   # loads the cached network
log_msg(sprintf("Network loaded in %.1f sec.", as.numeric(Sys.time() - t0, units = "secs")))

# --- Snap destinations to the street network ------------------------------------
# A destination off the walkable network (e.g. a commune centroid in a lake) is reached differently by
# the two r5r functions: travel_time_matrix() adds the walk from the street to the point, detailed_itineraries()
# stops at the street. Destinations further than SNAP_MIN_M from the network are moved onto it, so both
# describe the same trip. The reported coordinates of the destination (dest_*_orig) are kept in the pairs file.
dd <- unique(pairs[, .(d_key, lat = dest_lat, lon = dest_long)], by = "d_key")
sn <- as.data.table(r5r::find_snap(r5r_core, points = as.data.frame(dd[, .(id = d_key, lat, lon)]),
                                   radius = SNAP_RADIUS_M, mode = "WALK"))
dd <- merge(dd, sn[, .(d_key = point_id, snap_lat, snap_lon, dest_snap_m = distance, found)], by = "d_key")
dd[, moved := found & dest_snap_m > SNAP_MIN_M]
log_msg(sprintf("Destinations: %d | off the street network by > %d m (moved onto it): %d | not found within %d m: %d",
                nrow(dd), SNAP_MIN_M, sum(dd$moved), SNAP_RADIUS_M, sum(!dd$found)))
pairs[, `:=`(dest_lat_orig = dest_lat, dest_long_orig = dest_long)]
pairs[dd[moved == TRUE], on = "d_key", `:=`(dest_lat = i.snap_lat, dest_long = i.snap_lon)]
pairs[dd, on = "d_key", dest_snap_m := i.dest_snap_m]
pairs[, d_key := sprintf("%.6f|%.6f", dest_lat, dest_long)]
fwrite(pairs[, !c("pair_key", "d_key")], file.path(OUT_DIR, "pt_ch_fr_pairs.csv"))

# --- Routing helper ----------------------------------------------------------
# p: pairs (columns nearest_stop_id/lat/lon, d_key, dest_lat/long). Origin stations are processed in chunks
# of CHUNK_ORIGINS; each call gets the chunk's stations and the destinations of
# their pairs, and only the requested station-destination combinations are kept.
# Returns one row per input row with a travel time column per percentile (t_<pct>).
# Errors are NOT swallowed, so a real problem stops the script instead of turning into NA.
route_pairs <- function(p, percentiles, dep = departure, window = cfg$pt$time_window_minutes,
                        max_rides = MAX_RIDES, max_walk_time = MAX_WALK_MIN, max_trip_duration = MAX_TRIP_MIN,
                        n_threads = Inf, label = NULL) {
  percentiles <- sort(unique(percentiles))
  need <- unique(p[, .(from_id = nearest_stop_id, to_id = d_key)])
  st   <- unique(p[, .(id = nearest_stop_id, lat = nearest_stop_lat, lon = nearest_stop_lon)], by = "id")
  de   <- unique(p[, .(id = d_key, lat = dest_lat, lon = dest_long)], by = "id")   # one row per id (coordinates may differ beyond the 6th decimal)
  chunks <- split(st$id, ceiling(seq_len(nrow(st)) / CHUNK_ORIGINS))
  tt_names <- paste0("t_", percentiles)
  out <- vector("list", length(chunks)); tc <- Sys.time()
  for (i in seq_along(chunks)) {
    nd <- need[from_id %in% chunks[[i]]]
    ttm <- suppressWarnings(suppressMessages(   # hides the "<20% of services" note (expected for these feeds)
      r5r::travel_time_matrix(
        r5r_core, origins = as.data.frame(st[id %in% chunks[[i]]]), destinations = as.data.frame(de[id %in% nd$to_id]),
        mode = MODES, departure_datetime = dep, time_window = window, percentiles = percentiles,
        walk_speed = cfg$pt$walk_speed_kmh, max_rides = max_rides, max_walk_time = max_walk_time,
        max_trip_duration = max_trip_duration, n_threads = n_threads, verbose = FALSE, progress = FALSE)))
    ttm <- as.data.table(ttm)
    if (nrow(ttm)) {
      setnames(ttm, grep("travel_time", names(ttm), value = TRUE), tt_names)   # same order as percentiles
      res <- merge(nd, ttm[, c("from_id", "to_id", tt_names), with = FALSE], by = c("from_id", "to_id"), all.x = TRUE)
    } else {
      res <- copy(nd); res[, (tt_names) := NA_real_]
    }
    out[[i]] <- res
    if (!is.null(label)) log_msg(sprintf("  %s: chunk %d / %d (%d stations) | %.1f min elapsed",
                                         label, i, length(chunks), length(chunks[[i]]), as.numeric(Sys.time() - tc, units = "mins")))
  }
  res <- rbindlist(out)
  m <- res[p[, .(from_id = nearest_stop_id, to_id = d_key)], on = c("from_id", "to_id")]
  m[, c("from_id", "to_id") := NULL]
  stopifnot(nrow(m) == nrow(p))   # one result row per input row, same order
  m
}

# ============================================================
# Main loop over the groups
# ============================================================
route_names <- NULL; stops_all <- NULL; stops_all_sf <- NULL     # loaded once, only if legs are saved
pcts <- sort(unique(c(cfg$pt$percentile, FASTEST_PCT)))
cn_med <- paste0("t_", cfg$pt$percentile); cn_fast <- paste0("t_", FASTEST_PCT)
stopifnot(LEGS_MODE %in% c("none", "flagged", "all"))

for (g in GROUPS_TO_RUN) {
  p <- pairs[group == g]
  # Departure window of this group (config.yml: oev_ch_fr$route_all; falls back to pt:)
  ra    <- cfg$oev_ch_fr$route_all[[g]]
  dep_g <- as.POSIXct(paste(cfg$pt$departure_date, if (is.null(ra$departure_time)) cfg$pt$departure_time else ra$departure_time),
                      format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Zurich")
  win_g <- if (is.null(ra$time_window_minutes)) cfg$pt$time_window_minutes else ra$time_window_minutes
  stopifnot(!is.na(dep_g), as.Date(dep_g) >= valid_from, as.Date(dep_g) <= valid_to)
  log_msg(sprintf("=== Group %s: %d unique pairs, %d origin stations | departures %s to %s (window %d min) ===",
                  g, nrow(p), uniqueN(p$nearest_stop_id), format(dep_g, "%H:%M"), format(dep_g + win_g * 60, "%H:%M"), win_g))

  # --- Travel times ----------------------------------------------------------
  t0 <- Sys.time()
  tt <- route_pairs(p, pcts, dep = dep_g, window = win_g, label = g)
  p[, pt_time_min := tt[[cn_med]]]
  p[, pt_time_fastest_min := tt[[cn_fast]]]
  log_msg(sprintf("Routed in %.1f min. Missing: %d / %d.", as.numeric(Sys.time() - t0, units = "mins"),
                  sum(is.na(p$pt_time_min)), nrow(p)))

  # --- Diagnose pairs that still fail (which single limit blocks them) -------
  p[, fail_reason := NA_character_]
  fl <- p[is.na(pt_time_min)]
  if (DIAGNOSE_FAILS && nrow(fl) > 0) {
    log_msg(sprintf("Diagnosing %d pair(s) without a travel time...", nrow(fl)))
    run <- function(...) route_pairs(fl, cfg$pt$percentile, dep = dep_g, window = win_g, ...)[[1]]
    t_all   <- do.call(run, RELAXED)
    t_rides <- run(max_rides = RELAXED$max_rides)
    t_dur   <- run(max_trip_duration = RELAXED$max_trip_duration)
    t_walk  <- run(max_walk_time = RELAXED$max_walk_time)
    reason <- ifelse(is.na(t_all), "no route even with all limits relaxed",
              ifelse(!is.na(t_rides), "max_rides", ifelse(!is.na(t_dur), "max_trip_duration",
              ifelse(!is.na(t_walk), "max_walk_time", "combination of limits"))))
    p[match(fl$pair_id, pair_id), fail_reason := reason]
    print(table(reason))
  }
  # --- Flag pairs that depend on a long walk ----------------------------------
  # The matrix is rerun with walking capped at WALK_CAP_MIN. A pair with no route, or slower by
  # more than FLAG_EXTRA_MIN, relies on a long walk (typically the first leg, to another station;
  # the cap also applies to the final walk and transfers, so the legs show where). Legs are
  # then needed only for these pairs.
  p[, `:=`(pt_time_walkcap_min = NA_real_, flag_long_walk = NA)]
  if (LEGS_MODE != "none") {
    t0 <- Sys.time()
    okp <- which(!is.na(p$pt_time_min))
    p[okp, pt_time_walkcap_min := route_pairs(p[okp], cfg$pt$percentile, dep = dep_g, window = win_g,
                                              max_walk_time = WALK_CAP_MIN)[[1]]]
    p[okp, flag_long_walk := is.na(pt_time_walkcap_min) | (pt_time_walkcap_min - pt_time_min) > FLAG_EXTRA_MIN]
    log_msg(sprintf("Walk-cap check (%d min) in %.1f min: %d of %d routed pairs flagged (%.1f%%).", WALK_CAP_MIN,
                    as.numeric(Sys.time() - t0, units = "mins"), sum(p$flag_long_walk, na.rm = TRUE), length(okp),
                    100 * mean(p$flag_long_walk[okp])))
  }
  p_out <- p[, !c("pair_key", "d_key")]
  fwrite(p_out, file.path(OUT_DIR, sprintf("pt_ch_fr_times_%s.csv", g)))
  log_msg(sprintf("Wrote pt_ch_fr_times_%s.csv", g))
  if (LEGS_MODE == "none") next

  # --- Fastest departure minute per pair -------------------------------------
  # travel_time_matrix() once per departure minute (time_window = 1): the minute with the
  # lowest time is the trip described by the legs (ties -> earliest).
  ok <- if (LEGS_MODE == "flagged") p[flag_long_walk %in% TRUE] else p[!is.na(pt_time_min)]
  log_msg(sprintf("Legs for %d pairs (LEGS_MODE = %s).", nrow(ok), LEGS_MODE))
  if (!nrow(ok)) next
  scan_rds <- file.path(OUT_DIR, sprintf("pt_ch_fr_scan_%s_%s_%dmin.rds", g, LEGS_MODE, win_g))
  minutes <- seq(0, win_g - 1, by = DETAIL_MINUTE_STEP)
  scan <- if (file.exists(scan_rds)) readRDS(scan_rds) else list()
  log_msg(sprintf("Minute scan: %d departure minutes (step %d), %d already done.", length(minutes), DETAIL_MINUTE_STEP, length(scan)))
  for (k in setdiff(seq_along(minutes), as.integer(names(scan)))) {
    dep_k <- dep_g + minutes[k] * 60
    r <- route_pairs(ok, 50, dep = dep_k, window = 1, n_threads = N_THREADS)
    scan[[as.character(k)]] <- data.table(pair_id = ok$pair_id, dep_dt = dep_k, total_time = r[[1]])
    saveRDS(scan, scan_rds)
    log_msg(sprintf("  minute %d / %d", k, length(minutes)))
  }
  sc <- rbindlist(scan)[!is.na(total_time)]
  setorder(sc, pair_id, total_time, dep_dt)
  chosen <- sc[!duplicated(pair_id)]
  m <- match(chosen$pair_id, ok$pair_id)
  trips <- data.table(pair_id = chosen$pair_id, from_station = ok$nearest_stop_name[m],
                      origin_lat = ok$nearest_stop_lat[m], origin_lon = ok$nearest_stop_lon[m],
                      dest_lat = ok$dest_lat[m], dest_lon = ok$dest_long[m],
                      depart_at = format(chosen$dep_dt, "%Y-%m-%d %H:%M"),
                      arrive_at = format(chosen$dep_dt + chosen$total_time * 60, "%Y-%m-%d %H:%M"),
                      pt_time_fastest_min = ok$pt_time_fastest_min[m], trip_time_min = chosen$total_time)
  log_msg(sprintf("Fastest departure found for %d / %d pairs.", nrow(trips), nrow(ok)))

  # --- Reference tables (once) -----------------------------------------------
  if (is.null(route_names)) {
    zips <- list.files(NET_DIR, pattern = "\\.zip$", full.names = TRUE)
    route_names <- do.call(rbind, lapply(zips, function(z) {
      r <- read.csv(unz(z, "routes.txt"), colClasses = "character", encoding = "UTF-8")
      names(r) <- sub("^﻿|^X\\.U\\.FEFF\\.", "", names(r))
      data.frame(route_id = r$route_id, route_name = trimws(paste(r$route_short_name,
                                                                  if ("route_long_name" %in% names(r)) r$route_long_name else "")))
    }))
    route_names <- route_names[!duplicated(route_names$route_id), ]
    stops_all <- do.call(rbind, lapply(zips, function(z) {
      s <- read.csv(unz(z, "stops.txt"), colClasses = "character", encoding = "UTF-8")
      names(s) <- sub("^﻿|^X\\.U\\.FEFF\\.", "", names(s))
      if (!"location_type" %in% names(s)) s$location_type <- ""
      s <- s[s$location_type %in% c("", "0"), ]
      data.frame(stop_name = s$stop_name, lat = as.numeric(s$stop_lat), lon = as.numeric(s$stop_lon))
    }))
    stops_all <- stops_all[!is.na(stops_all$lat) & !is.na(stops_all$lon), ]
    stops_all_sf <- sf::st_as_sf(stops_all, coords = c("lon", "lat"), crs = 4326)
  }
  name_of <- function(x) { nm <- route_names$route_name[match(x, route_names$route_id)]; ifelse(is.na(nm), x, nm) }
  stop_name_at <- function(lon, lat) {   # nearest feed stop within STOP_MATCH_M, else NA
    out <- rep(NA_character_, length(lon)); ok_pt <- !is.na(lon) & !is.na(lat)
    if (!any(ok_pt)) return(out)
    pts <- sf::st_as_sf(data.frame(lon = lon[ok_pt], lat = lat[ok_pt]), coords = c("lon", "lat"), crs = 4326)
    idx <- sf::st_nearest_feature(pts, stops_all_sf)
    dist <- as.numeric(sf::st_distance(pts, stops_all_sf[idx, ], by_element = TRUE))
    out[ok_pt] <- ifelse(dist <= STOP_MATCH_M, stops_all$stop_name[idx], NA_character_)
    out
  }

  # --- Legs, in batches, resumable -------------------------------------------
  LEGS_CSV <- file.path(OUT_DIR, sprintf("pt_ch_fr_legs_%s.csv", g))
  done <- if (file.exists(LEGS_CSV)) unique(fread(LEGS_CSV, select = "pair_id", colClasses = "character")$pair_id) else character()
  todo <- trips[!pair_id %in% done]
  log_msg(sprintf("Legs: %d pairs already in %s, %d to do.", length(done), basename(LEGS_CSV), nrow(todo)))
  no_it <- character()
  for (b in split(seq_len(nrow(todo)), ceiling(seq_len(nrow(todo)) / LEGS_BATCH))) {
    legs_list <- list()
    for (i in b) {
      id <- todo$pair_id[i]; dep_i <- chosen$dep_dt[match(id, chosen$pair_id)]
      it <- tryCatch(suppressWarnings(suppressMessages(r5r::detailed_itineraries(
        r5r_core,
        origins = data.frame(id = id, lat = todo$origin_lat[i], lon = todo$origin_lon[i]),
        destinations = data.frame(id = id, lat = todo$dest_lat[i], lon = todo$dest_lon[i]),
        mode = MODES, departure_datetime = dep_i, time_window = 1, walk_speed = cfg$pt$walk_speed_kmh,
        max_rides = MAX_RIDES, max_walk_time = MAX_WALK_MIN, max_trip_duration = MAX_TRIP_MIN,
        shortest_path = TRUE, drop_geometry = FALSE, n_threads = N_THREADS, verbose = FALSE, progress = FALSE))),
        error = function(e) e)
      if (inherits(it, "error")) {
        if (grepl("frequencies", conditionMessage(it))) stop("detailed_itineraries() refuses to run: a feed in the network has a frequencies table. Run 02_clean_CH_gtfs.R and rebuild the network (03_).")
        log_msg(sprintf("  [%s] detailed_itineraries() error: %s", id, conditionMessage(it))); no_it <- c(no_it, id); next
      }
      if (nrow(it) == 0) { no_it <- c(no_it, id); next }
      xy <- sf::st_coordinates(it); grp <- xy[, ncol(xy)]      # first / last point of each leg's line
      first <- !duplicated(grp); last <- !duplicated(grp, fromLast = TRUE)
      it$leg_from_lon <- xy[first, "X"][match(seq_len(nrow(it)), grp[first])]
      it$leg_from_lat <- xy[first, "Y"][match(seq_len(nrow(it)), grp[first])]
      it$leg_to_lon   <- xy[last,  "X"][match(seq_len(nrow(it)), grp[last])]
      it$leg_to_lat   <- xy[last,  "Y"][match(seq_len(nrow(it)), grp[last])]
      it <- as.data.frame(sf::st_drop_geometry(it)); it$pair_id <- id; it$dep_dt <- dep_i
      legs_list[[id]] <- it
    }
    if (!length(legs_list)) next
    legs <- dplyr::bind_rows(legs_list)
    legs <- legs[order(legs$pair_id, legs$segment), ]
    if ("route" %in% names(legs)) legs$route_name <- name_of(as.character(legs$route))
    trip_start <- legs$dep_dt          # r5r's own departure time (with seconds) when available
    if ("departure_time" %in% names(legs)) {
      r5_start <- as.POSIXct(paste(format(legs$dep_dt, "%Y-%m-%d"), legs$departure_time), format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Zurich")
      trip_start[!is.na(r5_start)] <- r5_start[!is.na(r5_start)]
    }
    w <- ifelse(is.na(legs$wait), 0, legs$wait)
    end_offset <- ave(w + legs$segment_duration, legs$pair_id, FUN = cumsum)
    legs$leg_start <- format(trip_start + (end_offset - legs$segment_duration) * 60, "%H:%M")
    legs$leg_end   <- format(trip_start + end_offset * 60, "%H:%M")
    legs$from_stop <- stop_name_at(legs$leg_from_lon, legs$leg_from_lat)
    legs$to_stop   <- stop_name_at(legs$leg_to_lon,   legs$leg_to_lat)
    keep <- intersect(c("pair_id", "segment", "mode", "route_name", "from_stop", "to_stop", "leg_start", "leg_end", "wait",
                        "segment_duration", "distance", "route", "total_duration", "total_distance",
                        "leg_from_lat", "leg_from_lon", "leg_to_lat", "leg_to_lon"), names(legs))
    readr::write_csv(legs[, keep], LEGS_CSV, append = file.exists(LEGS_CSV))
    log_msg(sprintf("  legs written for %d pairs (%d rows)", length(legs_list), nrow(legs)))
  }
  if (length(no_it)) log_msg(sprintf("No itinerary for %d pair(s): %s", length(no_it), paste(head(no_it, 20), collapse = ", ")))

  # --- Trips table: routes, rides, waits, check against the matrix -----------
  legs <- fread(LEGS_CSV)
  transit <- legs[!mode %in% c("WALK", "BICYCLE", "CAR")]
  lab <- transit[, .(routes = paste(fifelse(is.na(route_name) | route_name == "", route, route_name), collapse = " -> "),
                     n_rides = .N, first_vehicle_at = leg_start[1], initial_wait_min = round(fifelse(is.na(wait[1]), 0, wait[1]), 1)), by = pair_id]
  tot <- legs[, .(legs_total_min = max(total_duration, na.rm = TRUE)), by = pair_id]
  trips <- merge(merge(trips, lab, by = "pair_id", all.x = TRUE), tot, by = "pair_id", all.x = TRUE)
  trips[, gap_legs_vs_matrix_min := round(legs_total_min - trip_time_min, 1)]   # |gap| > 10: matrix and itinerary differ
  fwrite(trips, file.path(OUT_DIR, sprintf("pt_ch_fr_trips_%s.csv", g)))
  log_msg(sprintf("Wrote pt_ch_fr_trips_%s.csv (%d trips; %d with |legs - matrix| > 10 min)", g, nrow(trips),
                  sum(abs(trips$gap_legs_vs_matrix_min) > 10, na.rm = TRUE)))
}

r5r::stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)
log_msg("04_pt_r5r_ch_fr_route_all.R done.")
