# ============================================================
# 04_pt_r5r_ch_fr_route_sample.R
# STEP 2 of 2: draws the tourist sample used to inspect the network and routes it
# on the network built by 03_pt_r5r_ch_fr_build_network.R (nearest station ->
# destination).
#
# SAMPLE: SAMPLE_N French-origin trips from the three regions bordering
# Switzerland are drawn at random (SEED) from agqpv.csv. Each gets the nearest
# rail station (GTFS stops served by trains, SNCF coach stops excluded) as its
# origin. The station list is written to NET_DIR/rail_stops.csv and the sample to
# pt_ch_fr_sample_tourists.csv.
#
# Input : data/output/csv/agqpv.csv, the r5r network in data/r5_rail_ch_fr/,
#         the GTFS feeds ch.freqfixed (data/output/gtfs) and fr (data/input/gtfs)
# Output (in data/test/CH_FR, config.yml: oev_ch_fr$test_dir):
#         pt_ch_fr_sample_tourists.csv  the sample + nearest station
#         pt_ch_fr_sample_times.csv     sample + PT travel times
#         pt_ch_fr_sample_trips.csv, pt_ch_fr_sample_legs.csv   trip details
#         plus data/r5_rail_ch_fr/rail_stops.csv   candidate origin stations
#
# max_rides: r5r's default allows only 3 vehicles per trip. Trips like
# Épernay -> TER -> Paris metro/RER -> TGV -> Léman Express -> south of
# Geneva need 4-5, so they came back as NA (tested on T004: NA with 3 rides,
# 437 min with more rides; duration and walk limits made no difference).
#
# Any tourist still without a time is re-tested with each limit relaxed in
# turn, and the blocking constraint is written to the fail_reason column.
#
# Besides the configured percentile (pt_time_min, e.g. the median), the
# fastest trip in the time window can be reported (pt_time_fastest_min).
# The detail files (trips, legs) describe either the median or the fastest
# trip per tourist (DETAIL_TRIP).
# (detailed_itineraries() gives the legs; it refuses to run while a feed in the
# network contains a frequencies table, which is why the network uses the Swiss
# feed fixed by 02_clean_CH_gtfs.R.)
#
# Run in a FRESH R session, after the build script:
#   source("Scripts/02_travel_time/OEV/FR_CH/test/04_pt_r5r_ch_fr_route_sample.R")
# ============================================================

# Run from the project root.

# --- Java heap must be set before anything loads r5r ------------------------
.cfg0 <- yaml::read_yaml("Scripts/config.yml")
options(java.parameters = paste0("-Xmx", .cfg0$java_max_mem))

source("Scripts/00_setup.R")   # provides cfg, log_msg, ensure_dir (loads sf, data.table)
library(r5r)

# --- Settings ----------------------------------------------------------------
NET_DIR    <- "data/r5_rail_ch_fr"                         # built by 03_pt_r5r_ch_fr_build_network.R
TEST_DIR   <- cfg$oev_ch_fr$test_dir                       # data/test/CH_FR: outputs of this test
ensure_dir(TEST_DIR)
SAMPLE_CSV <- file.path(TEST_DIR, "pt_ch_fr_sample_tourists.csv")   # written by this script
OUT_CSV    <- file.path(TEST_DIR, "pt_ch_fr_sample_times.csv")

# Sample
SAMPLE_N <- 100
SEED     <- 42
FR_NEAR_CH_DEPTS   <- cfg$oev_ch_fr$near_ch_depts          # region -> INSEE département codes (config.yml)
FR_NEAR_CH_REGIONS <- names(FR_NEAR_CH_DEPTS)
RAIL_STOP_FEEDS <- c(ch = "data/output/gtfs/ch.freqfixed.gtfs.zip",   # feeds searched for origin stations
                     fr = "data/input/gtfs/fr.gtfs.zip")   # (origins are never in Paris)
RAIL_TYPES <- as.character(c(2, 100:117))   # GTFS route_type: 2 = rail, 100-117 = extended rail types

# Rail incl. Paris metro and trams. Note SNCF also runs some coaches on rail
# routes, and r5r counts those as RAIL. Use c("WALK", "TRANSIT") for all modes.
MODES         <- c("WALK", "TRANSIT")
MAX_RIDES     <- 8
MAX_WALK_MIN  <- 240
MAX_TRIP_MIN  <- 1000
N_THREADS <- 2

TRIPS_CSV  <- file.path(TEST_DIR, "pt_ch_fr_sample_trips.csv")    # reported trip per tourist (for Google Maps checks)
LEGS_CSV   <- file.path(TEST_DIR, "pt_ch_fr_sample_legs.csv")     # its legs, one row each
WRITE_TRIP_DETAILS <- TRUE                                 # FALSE skips the (slower) detail step

DETAIL_MINUTE_STEP = 5
DETAIL_N    <- Inf           # trip details for this many randomly drawn tourists (Inf = all with a travel time)
DETAIL_SEED <- 1             # change to draw a different subset

STOP_MATCH_M <- 150

# Which travel times to report, and which trip the detail files describe
SHOW_FASTEST <- TRUE         # also report the fastest trip in the time window as pt_time_fastest_min
FASTEST_PCT  <- 1            # r5r only accepts percentiles 1-99, so the 1st percentile stands in for the minimum
DETAIL_TRIP  <- "fastest"     # detail files describe: "median" (= cfg$pt$percentile, pt_time_min) or "fastest"

# Generous limits used only to diagnose tourists that still fail
RELAXED <- list(max_rides = 10, max_trip_duration = 720, max_walk_time = 60)

# --- Departure time ----------------------------------------------------------
# Set pt$departure_date / pt$departure_time in config.yml. They are never changed
# automatically: if the date is outside the period covered by the feeds, the
# script stops and you choose a new date.
departure <- as.POSIXct(paste(cfg$pt$departure_date, cfg$pt$departure_time),
                        format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Zurich")
if (is.na(departure)) {
  stop("Could not parse departure from config -- pt$departure_date must be 'YYYY-MM-DD' ",
       "and pt$departure_time 'HH:MM:SS'.")
}

# Period covered by each GTFS feed in the network, read from the feeds themselves:
# feed_info.txt (feed_start_date / feed_end_date) if present, otherwise the range
# of calendar.txt and calendar_dates.txt. Only feed_info/calendar files are read.
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
    if (nrow(fi) && all(nzchar(unlist(fi[1])))) {
      return(as.Date(unlist(fi[1]), "%Y%m%d"))
    }
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
valid_from <- as.Date(max(validity[, 1]))   # period covered by ALL feeds
valid_to   <- as.Date(min(validity[, 2]))
log_msg(sprintf("Feed validity: %s", paste(sprintf("%s %s..%s", rownames(validity), validity[, 1], validity[, 2]), collapse = " | ")))
if (as.Date(departure) < valid_from || as.Date(departure) > valid_to) {
  stop("pt$departure_date ", as.Date(departure), " in config.yml is outside the period covered by all feeds (",
       valid_from, " to ", valid_to, "). Choose a date inside it.")
}

if (!file.exists(file.path(NET_DIR, "network.dat"))) {
  stop("No network.dat in ", NET_DIR, " -- run 03_pt_r5r_ch_fr_build_network.R first.")
}

# --- Candidate origin stations (rail only, no SNCF coach stops) --------------
# SNCF puts many coach services on rail routes (route_type 2); their stops
# have "OCECar" in the ID and are excluded so origins get a real train station.
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
  unlink(tmp, recursive = TRUE)

  rail_trip_ids <- trips[route_id %in% routes[route_type %in% RAIL_TYPES, route_id], trip_id]
  rail_stop_ids <- unique(stimes[trip_id %in% rail_trip_ids, stop_id])

  out <- stops[stop_id %in% rail_stop_ids & !grepl("OCECar", stop_id, fixed = TRUE),
               .(stop_id, stop_name, stop_lat = as.numeric(stop_lat), stop_lon = as.numeric(stop_lon))]
  out[, feed := feed_label]
  out
}

rail_stops <- rbindlist(Map(get_rail_stops, RAIL_STOP_FEEDS, names(RAIL_STOP_FEEDS)))
log_msg(sprintf("Rail stops: %d (%s)", nrow(rail_stops),
                paste(names(table(rail_stops$feed)), table(rail_stops$feed), sep = ": ", collapse = ", ")))
readr::write_csv(rail_stops, file.path(NET_DIR, "rail_stops.csv"))

# --- Sample: near-CH French-origin trips + their nearest station -------------
agqpv <- readr::read_csv(cfg$input_csv, locale = readr::locale(encoding = cfg$input_encoding),
                         show_col_types = FALSE)
fr <- agqpv[substr(agqpv$origin_commune, 1, 2) %in% "FR", ]

# Region of each French origin from the département part of its commune id
fr_dept <- substr(fr$origin_commune, 4, 5)
fr$state <- NA_character_
for (reg in FR_NEAR_CH_REGIONS) fr$state[fr_dept %in% FR_NEAR_CH_DEPTS[[reg]]] <- reg
fr_near <- fr[!is.na(fr$state), ]
log_msg(sprintf("Near-CH French-origin rows: %d / %d FR-origin rows", nrow(fr_near), nrow(fr)))

set.seed(SEED)
sample_n <- min(SAMPLE_N, nrow(fr_near))
if (sample_n < SAMPLE_N) log_msg(sprintf("WARNING: only %d rows available, sampling all.", sample_n))
samp <- fr_near[sample(nrow(fr_near), sample_n), ]
samp$tourist_id <- sprintf("T%03d", seq_len(nrow(samp)))

stops_sf    <- sf::st_as_sf(rail_stops, coords = c("stop_lon", "stop_lat"), crs = 4326)
origins_sf  <- sf::st_as_sf(samp, coords = c("origin_long", "origin_lat"), crs = 4326, remove = FALSE)
nearest_idx <- sf::st_nearest_feature(origins_sf, stops_sf)

samp$nearest_stop_id    <- rail_stops$stop_id[nearest_idx]
samp$nearest_stop_name  <- rail_stops$stop_name[nearest_idx]
samp$nearest_stop_feed  <- rail_stops$feed[nearest_idx]
samp$nearest_stop_lat   <- rail_stops$stop_lat[nearest_idx]
samp$nearest_stop_lon   <- rail_stops$stop_lon[nearest_idx]
samp$dist_to_station_km <- as.numeric(
  sf::st_distance(origins_sf, stops_sf[nearest_idx, ], by_element = TRUE)) / 1000
log_msg(sprintf("Nearest-station distance: median %.1f km, max %.1f km",
                median(samp$dist_to_station_km), max(samp$dist_to_station_km)))

samp <- samp[, c("tourist_id", "origin_lat", "origin_long", "state",
                 "nearest_stop_id", "nearest_stop_name", "nearest_stop_feed",
                 "nearest_stop_lat", "nearest_stop_lon", "dist_to_station_km",
                 "dest_lat", "dest_long")]
readr::write_csv(samp, SAMPLE_CSV)
log_msg(sprintf("Wrote %s (%d tourists, seed %d).", SAMPLE_CSV, nrow(samp), SEED))
stopifnot(!anyDuplicated(samp$tourist_id), !anyNA(samp$nearest_stop_lat))

log_msg(sprintf("Routing %d tourists | departure %s | modes %s | max_rides %d | max_walk %d min | max_trip %d min",
                nrow(samp), format(departure), paste(MODES, collapse = "+"),
                MAX_RIDES, MAX_WALK_MIN, MAX_TRIP_MIN))

# --- Network -----------------------------------------------------------------
t0 <- Sys.time()
if (!file.exists(file.path(NET_DIR, "network.dat"))) {
  stop("No network.dat in ", NET_DIR, " -- run 03_pt_r5r_ch_fr_build_network.R first.")
}
r5r_core <- r5r::build_network(data_path = NET_DIR, verbose = FALSE)   # loads the cached network
log_msg(sprintf("Network loaded in %.1f sec.", as.numeric(Sys.time() - t0, units = "secs")))

# --- Routing helper ----------------------------------------------------------
# Routes each origin to its OWN destination (pairs matched by id). One r5r
# call for all pairs; errors are NOT swallowed, so a real problem stops the
# script instead of turning into NA travel times.
# With several percentiles it returns a matrix (one column per percentile, in
# the order given); with one percentile, a plain vector.
route_pairs <- function(ids, o_lat, o_lon, d_lat, d_lon,
                        max_rides = MAX_RIDES, max_walk_time = MAX_WALK_MIN,
                        max_trip_duration = MAX_TRIP_MIN,
                        percentiles = cfg$pt$percentile) {
  o <- data.frame(id = ids, lat = o_lat, lon = o_lon)
  d <- data.frame(id = ids, lat = d_lat, lon = d_lon)
  ttm <- suppressWarnings(suppressMessages(   # hides the "<20% of services" note, see below
    r5r::travel_time_matrix(
      r5r_core, origins = o, destinations = d, mode = MODES,
      departure_datetime = departure, time_window = cfg$pt$time_window_minutes,
      percentiles = percentiles, walk_speed = cfg$pt$walk_speed_kmh,
      max_rides = max_rides, max_walk_time = max_walk_time,
      max_trip_duration = max_trip_duration,
      verbose = FALSE, progress = FALSE
    )
  ))
  # The "<20% of services running" warning is expected: the SNCF and IDFM
  # feeds use many short-lived service_ids (~26% active on any weekday),
  # while trip counts per day are normal.
  if (nrow(ttm) == 0) {
    out <- matrix(NA_real_, length(ids), length(percentiles))
  } else {
    tt_cols <- grep("travel_time", names(ttm), value = TRUE)   # one per percentile, same order
    own <- ttm[ttm$from_id == ttm$to_id, ]
    out <- sapply(tt_cols, function(cl) as.numeric(own[[cl]][match(ids, own$from_id)]))
    out <- matrix(out, nrow = length(ids))
  }
  if (length(percentiles) == 1) out[, 1] else out
}

# --- Route all tourists ------------------------------------------------------
t0 <- Sys.time()
# One call gives both the configured percentile and (optionally) the fastest
# (r5r requires the percentiles in ascending order, e.g. c(1, 50))
pcts <- sort(unique(if (SHOW_FASTEST) c(cfg$pt$percentile, FASTEST_PCT) else cfg$pt$percentile))
res  <- route_pairs(samp$tourist_id, samp$nearest_stop_lat, samp$nearest_stop_lon,
                    samp$dest_lat, samp$dest_long, percentiles = pcts)
res  <- matrix(res, nrow = nrow(samp))
samp$pt_time_min <- res[, match(cfg$pt$percentile, pcts)]                       # configured percentile (e.g. median)
if (SHOW_FASTEST) samp$pt_time_fastest_min <- res[, match(FASTEST_PCT, pcts)]   # fastest trip in the window
log_msg(sprintf("Routed in %.1f min. Missing: %d / %d.",
                as.numeric(Sys.time() - t0, units = "mins"),
                sum(is.na(samp$pt_time_min)), nrow(samp)))

# --- Diagnose tourists that still fail ---------------------------------------
# For each failing tourist: does it work with ALL limits relaxed, and if so,
# which single relaxation is enough?
failing <- samp[is.na(samp$pt_time_min), ]

samp$fail_reason <- NA_character_

if (nrow(failing) > 0) {
  log_msg(sprintf("Diagnosing %d tourist(s) without a travel time...", nrow(failing)))
  args <- list(failing$tourist_id, failing$nearest_stop_lat, failing$nearest_stop_lon,
               failing$dest_lat, failing$dest_long)
  run <- function(...) do.call(route_pairs, c(args, list(...)))
  
  t_all   <- do.call(run, RELAXED)
  t_rides <- run(max_rides = RELAXED$max_rides)
  t_dur   <- run(max_trip_duration = RELAXED$max_trip_duration)
  t_walk  <- run(max_walk_time = RELAXED$max_walk_time)
  
  reason <- ifelse(is.na(t_all), "no route even with all limits relaxed",
                   ifelse(!is.na(t_rides), "max_rides",
                          ifelse(!is.na(t_dur),   "max_trip_duration",
                                 ifelse(!is.na(t_walk),  "max_walk_time",
                                        "combination of limits"))))
  samp$fail_reason[match(failing$tourist_id, samp$tourist_id)] <- reason
  
  for (i in seq_len(nrow(failing))) {
    log_msg(sprintf("  [%s] %s -> dest(%.4f, %.4f) | %s | time with all limits relaxed: %s",
                    failing$tourist_id[i], failing$nearest_stop_name[i],
                    failing$dest_lat[i], failing$dest_long[i], reason[i],
                    ifelse(is.na(t_all[i]), "none", sprintf("%.0f min", t_all[i]))))
  }
}

# --- Save --------------------------------------------------------------------
readr::write_csv(samp, OUT_CSV)
log_msg(sprintf("Wrote %s", OUT_CSV))


# --- Trip details for plausibility checks (e.g. against Google Maps) ---------
# pt_time_min is a percentile (cfg$pt$percentile) of the travel times for every
# departure minute in the time window. Here we find, per tourist, the departure
# minute whose travel time equals that reported value (DETAIL_TRIP = "median")
# or the fastest departure minute (DETAIL_TRIP = "fastest"), and write:
#   TRIPS_CSV  one row per tourist: departure minute, arrival, routes used,
#              number of vehicles, a Google Maps link (set the departure time there
#              by hand), the time to compare with Google (comparable_to_google_min)
#              and empty google_* columns to fill in
#   LEGS_CSV   one row per leg of that trip (mode, route, times)
# Note: travel times count from the departure minute, so they include the wait
# at the origin station before the first vehicle leaves.
if (WRITE_TRIP_DETAILS && any(!is.na(samp$pt_time_min))) {
  DETAIL_TRIP <- match.arg(DETAIL_TRIP, c("median", "fastest"))
  ok <- samp[!is.na(samp$pt_time_min), ]
  if (nrow(ok) > DETAIL_N) {   # random subset: the detail step's run time grows with the number of tourists
    set.seed(DETAIL_SEED)
    ok <- ok[sort(sample(nrow(ok), DETAIL_N)), ]
  }
  log_msg(sprintf("Trip details for %d tourists: %s", nrow(ok), paste(ok$tourist_id, collapse = ", ")))
  o  <- data.frame(id = ok$tourist_id, lat = ok$nearest_stop_lat, lon = ok$nearest_stop_lon)
  d  <- data.frame(id = ok$tourist_id, lat = ok$dest_lat,         lon = ok$dest_long)
  
  # Route names from all feeds in the network, to make route IDs readable
  route_names <- do.call(rbind, lapply(list.files(NET_DIR, pattern = "\\.zip$", full.names = TRUE), function(z) {
    r <- read.csv(unz(z, "routes.txt"), colClasses = "character", encoding = "UTF-8")
    names(r) <- sub("^\ufeff|^X\\.U\\.FEFF\\.", "", names(r))
    data.frame(route_id = r$route_id,
               route_name = trimws(paste(r$route_short_name,
                                         if ("route_long_name" %in% names(r)) r$route_long_name else "")))
  }))
  route_names <- route_names[!duplicated(route_names$route_id), ]
  name_of <- function(x) {   # route ID -> "short long" name; values that aren't IDs are kept
    nm <- route_names$route_name[match(x, route_names$route_id)]
    ifelse(is.na(nm), x, nm)
  }
  
  # 1. Travel time per departure minute. expanded_travel_time_matrix() can't be
  #    used: it keeps a path to every reached stop for every minute and runs out
  #    of memory on this network even for one tourist. Instead, the ordinary
  #    travel_time_matrix() is run once per departure minute (time_window = 1),
  #    for all tourists at once -- the same memory use as the main run.
  #    DETAIL_MINUTE_STEP > 1 checks fewer minutes (faster, picks the closest).
  win     <- cfg$pt$time_window_minutes
  minutes <- seq(0, win - 1, by = DETAIL_MINUTE_STEP)
  log_msg(sprintf("Computing travel times for %d departure minutes (step %d)...",
                  length(minutes), DETAIL_MINUTE_STEP))
  per_min <- list()
  for (k in seq_along(minutes)) {
    dep_k <- departure + minutes[k] * 60
    ttm <- suppressWarnings(suppressMessages(
      r5r::travel_time_matrix(
        r5r_core, origins = o, destinations = d, mode = MODES,
        departure_datetime = dep_k, time_window = 1, percentiles = 50,
        walk_speed = cfg$pt$walk_speed_kmh, max_rides = MAX_RIDES,
        max_walk_time = MAX_WALK_MIN, max_trip_duration = MAX_TRIP_MIN,
        n_threads = N_THREADS, verbose = FALSE, progress = FALSE
      )
    ))
    ttm <- as.data.frame(ttm)
    if (nrow(ttm)) {
      tt_col <- grep("travel_time", names(ttm), value = TRUE)[1]
      ttm <- ttm[ttm$from_id == ttm$to_id & !is.na(ttm[[tt_col]]), ]
      if (nrow(ttm)) per_min[[k]] <- data.frame(tourist_id = ttm$from_id, dep_dt = dep_k,
                                                total_time = as.numeric(ttm[[tt_col]]))
    }
    if (k %% 10 == 0 || k == length(minutes)) log_msg(sprintf("  minute %d / %d", k, length(minutes)))
  }
  ett <- dplyr::bind_rows(per_min)
  if (!nrow(ett)) stop("No per-minute travel times found -- check the settings.")
  
  # 2. Per tourist, pick the departure minute to describe:
  #    "median"  -> the minute whose time is closest to pt_time_min
  #    "fastest" -> the minute with the shortest time (the true minimum over the
  #                 minutes checked; with DETAIL_MINUTE_STEP > 1 it may miss it)
  #    Ties -> earliest departure.
  ett$target <- ok$pt_time_min[match(ett$tourist_id, ok$tourist_id)]
  if (DETAIL_TRIP == "median") {
    ett <- ett[order(ett$tourist_id, abs(ett$total_time - ett$target), ett$dep_dt), ]
  } else {
    ett <- ett[order(ett$tourist_id, ett$total_time, ett$dep_dt), ]
  }
  chosen <- ett[!duplicated(ett$tourist_id), ]
  log_msg(sprintf("Detail files describe the %s trip per tourist.", DETAIL_TRIP))
  m <- match(chosen$tourist_id, ok$tourist_id)
  
  trips <- data.frame(
    tourist_id    = chosen$tourist_id,
    from_station  = ok$nearest_stop_name[m],
    origin_lat    = ok$nearest_stop_lat[m],
    origin_lon    = ok$nearest_stop_lon[m],
    dest_lat      = ok$dest_lat[m],
    dest_lon      = ok$dest_long[m],
    depart_at     = format(chosen$dep_dt, "%Y-%m-%d %H:%M"),
    arrive_at     = format(chosen$dep_dt + chosen$total_time * 60, "%Y-%m-%d %H:%M"),
    trip_type     = DETAIL_TRIP,
    pt_time_min   = chosen$target,
    trip_time_min = chosen$total_time
  )
  if ("pt_time_fastest_min" %in% names(ok)) {   # put it next to pt_time_min
    trips$pt_time_fastest_min <- ok$pt_time_fastest_min[m]
    trips <- trips[, c(setdiff(names(trips), "trip_time_min"), "trip_time_min")]
  }
  trips$google_maps <- sprintf(
    "https://www.google.com/maps/dir/?api=1&origin=%.6f,%.6f&destination=%.6f,%.6f&travelmode=transit",
    trips$origin_lat, trips$origin_lon, trips$dest_lat, trips$dest_lon)
  readr::write_csv(trips, TRIPS_CSV)   # written now, and again with routes once legs are known
  log_msg(sprintf("Wrote %s (%d trips).", TRIPS_CSV, nrow(trips)))
  
  # 3. Leg-by-leg itinerary for the chosen departure minute
  log_msg("Computing leg-by-leg itineraries for the chosen departures...")
  legs_list <- list()
  for (i in seq_len(nrow(trips))) {
    id <- trips$tourist_id[i]
    it <- tryCatch(
      suppressWarnings(suppressMessages(
        r5r::detailed_itineraries(
          r5r_core,
          origins      = o[o$id == id, ], destinations = d[d$id == id, ],
          mode = MODES, departure_datetime = chosen$dep_dt[i], time_window = 1,
          walk_speed = cfg$pt$walk_speed_kmh, max_rides = MAX_RIDES,
          max_walk_time = MAX_WALK_MIN, max_trip_duration = MAX_TRIP_MIN,
          shortest_path = TRUE, drop_geometry = FALSE,   # geometry gives each leg's start/end point
          n_threads = N_THREADS, verbose = FALSE, progress = FALSE
        )
      )),
      error = function(e) e
    )
    if (inherits(it, "error")) {
      msg <- conditionMessage(it)
      if (grepl("frequencies", msg)) {
        log_msg("  detailed_itineraries() refuses to run because a feed in the network has a frequencies table -- legs skipped.")
        log_msg("  Run 02_clean_CH_gtfs.R and rebuild the network with the fixed feed (BUILD_NETWORK <- TRUE in 03_...) to get legs; the trips file is still complete.")
        break
      }
      log_msg(sprintf("  [%s] detailed_itineraries() error: %s", id, msg))
      next
    }
    if (nrow(it) == 0) { log_msg(sprintf("  [%s] no itinerary found for the chosen departure.", id)); next }
    # First and last point of each leg's line = where the leg starts and ends
    xy <- sf::st_coordinates(it)
    grp <- xy[, ncol(xy)]                       # last column = leg index
    first <- !duplicated(grp); last <- !duplicated(grp, fromLast = TRUE)
    it$leg_from_lon <- xy[first, "X"][match(seq_len(nrow(it)), grp[first])]
    it$leg_from_lat <- xy[first, "Y"][match(seq_len(nrow(it)), grp[first])]
    it$leg_to_lon   <- xy[last,  "X"][match(seq_len(nrow(it)), grp[last])]
    it$leg_to_lat   <- xy[last,  "Y"][match(seq_len(nrow(it)), grp[last])]
    it <- as.data.frame(sf::st_drop_geometry(it))
    it$tourist_id <- id
    legs_list[[id]] <- it
  }
  
  if (length(legs_list)) {
    legs <- dplyr::bind_rows(legs_list)
    legs <- legs[order(legs$tourist_id, legs$segment), ]
    if ("route" %in% names(legs)) legs$route_name <- name_of(as.character(legs$route))
    
    # Clock time of each leg. r5r's departure_time column is the trip's start
    # time (same on every row); a leg starts after the previous leg ends plus
    # its own wait at the stop.
    # Start from r5r's own departure time (it has seconds, e.g. 11:55:55);
    # using the whole minute would put every leg ~1 minute too early.
    trip_start <- chosen$dep_dt[match(legs$tourist_id, chosen$tourist_id)]
    if ("departure_time" %in% names(legs)) {
      r5_start <- as.POSIXct(paste(format(trip_start, "%Y-%m-%d"), legs$departure_time),
                             format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Zurich")
      trip_start[!is.na(r5_start)] <- r5_start[!is.na(r5_start)]
    }
    w <- ifelse(is.na(legs$wait), 0, legs$wait)
    end_offset   <- ave(w + legs$segment_duration, legs$tourist_id, FUN = cumsum)
    start_offset <- end_offset - legs$segment_duration
    legs$leg_start <- format(trip_start + start_offset * 60, "%H:%M")
    legs$leg_end   <- format(trip_start + end_offset   * 60, "%H:%M")
    
    # Stop names: match each leg's start and end point to the nearest stop in
    # the feeds (within STOP_MATCH_M). Walking legs that start or end at a
    # home/destination point get no name.
    stops_all <- do.call(rbind, lapply(list.files(NET_DIR, pattern = "\\.zip$", full.names = TRUE), function(z) {
      st <- read.csv(unz(z, "stops.txt"), colClasses = "character", encoding = "UTF-8")
      names(st) <- sub("^\ufeff|^X\\.U\\.FEFF\\.", "", names(st))
      if (!"location_type" %in% names(st)) st$location_type <- ""
      st <- st[st$location_type %in% c("", "0"), ]           # platforms / stop points only
      data.frame(stop_name = st$stop_name, lat = as.numeric(st$stop_lat), lon = as.numeric(st$stop_lon))
    }))
    stops_all <- stops_all[!is.na(stops_all$lat) & !is.na(stops_all$lon), ]
    stops_sf  <- sf::st_as_sf(stops_all, coords = c("lon", "lat"), crs = 4326)
    stop_name_at <- function(lon, lat) {
      out <- rep(NA_character_, length(lon))
      ok_pt <- !is.na(lon) & !is.na(lat)
      if (!any(ok_pt)) return(out)
      pts <- sf::st_as_sf(data.frame(lon = lon[ok_pt], lat = lat[ok_pt]), coords = c("lon", "lat"), crs = 4326)
      idx  <- sf::st_nearest_feature(pts, stops_sf)
      dist <- as.numeric(sf::st_distance(pts, stops_sf[idx, ], by_element = TRUE))
      out[ok_pt] <- ifelse(dist <= STOP_MATCH_M, stops_all$stop_name[idx], NA_character_)
      out
    }
    legs$from_stop <- stop_name_at(legs$leg_from_lon, legs$leg_from_lat)
    legs$to_stop   <- stop_name_at(legs$leg_to_lon,   legs$leg_to_lat)
    
    keep <- intersect(c("tourist_id", "segment", "mode", "route_name", "from_stop", "to_stop",
                        "leg_start", "leg_end", "wait", "segment_duration", "distance",
                        "route", "total_duration", "total_distance",
                        "leg_from_lat", "leg_from_lon", "leg_to_lat", "leg_to_lon"), names(legs))
    legs <- legs[, keep]
    readr::write_csv(legs, LEGS_CSV)
    log_msg(sprintf("Wrote %s (%d legs for %d tourists).", LEGS_CSV, nrow(legs), length(legs_list)))
    
    # Add routes used and number of vehicles to the trips file
    if (all(c("mode", "route") %in% names(legs))) {
      transit <- legs[!legs$mode %in% c("WALK", "BICYCLE", "CAR"), ]
      lab <- ifelse(is.na(transit$route_name) | transit$route_name == "",
                    as.character(transit$route), transit$route_name)
      trips$routes  <- vapply(trips$tourist_id, function(id)
        paste(lab[transit$tourist_id == id], collapse = " -> "), character(1))
      trips$n_rides <- vapply(trips$tourist_id, function(id) sum(transit$tourist_id == id), numeric(1))
      
      # Columns for the Google Maps check. r5r's time counts from the departure
      # minute, so it includes the wait for the first vehicle; Google's doesn't.
      #   first_vehicle_at         when the first train/bus leaves
      #   initial_wait_min         wait at the start station before it leaves
      #   comparable_to_google_min trip_time_min minus that wait -> compare with Google
      first_row <- transit[!duplicated(transit$tourist_id), ]
      fm <- match(trips$tourist_id, first_row$tourist_id)
      trips$first_vehicle_at <- first_row$leg_start[fm]
      trips$initial_wait_min <- round(ifelse(is.na(first_row$wait[fm]), 0, first_row$wait[fm]), 1)
      trips$comparable_to_google_min <- round(trips$trip_time_min - trips$initial_wait_min, 1)
    }
    
    # Empty columns to fill in by hand from Google Maps (set "Depart at" to the
    # same date and to depart_at). google_diff_min = google - comparable is
    # left for Excel, e.g. =google_duration_min - comparable_to_google_min.
    for (cl in c("google_duration_min", "google_depart", "google_arrive",
                 "google_n_transfers", "google_routes", "google_notes")) {
      trips[[cl]] <- NA_character_
    }
    readr::write_csv(trips, TRIPS_CSV, na = "")
    log_msg(sprintf("Updated %s with routes per trip and Google check columns.", TRIPS_CSV))
    # detailed_itineraries() uses a different search, so its total can differ
    # slightly from the travel-time matrix; flag large gaps for checking.
    if ("total_duration" %in% names(legs)) {
      tot <- tapply(legs$total_duration, legs$tourist_id, max)
      gap <- abs(tot - trips$trip_time_min[match(names(tot), trips$tourist_id)])
      if (any(gap > 10, na.rm = TRUE)) {
        log_msg(sprintf("  Note: legs total differs by >10 min from trip_time_min for: %s",
                        paste(names(gap)[gap > 10], collapse = ", ")))
      }
    }
  }
}

r5r::stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)
log_msg("04_pt_r5r_ch_fr_route_sample.R done.")