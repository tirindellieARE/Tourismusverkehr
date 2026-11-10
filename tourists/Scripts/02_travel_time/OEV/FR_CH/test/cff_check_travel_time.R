# ============================================================
# cff_check_travel_time.R
# Plausibility check of the r5r trips against the official Swiss journey
# planner (OJP 2.0, opentransportdata.swiss).
#
# For every sampled tourist (start station -> destination), this asks OJP for
# the connections departing from the START of the r5r time window on CHECK_DATE
# (pt$departure_time, config.yml) and writes:
#   OUT_TRIPS  one row per OJP connection (several per tourist): times,
#              duration, transfers, lines, in_window -- plus the r5r values
#   OUT_LEGS   one row per leg of those connections: mode, line, from/to
#              stop, departure/arrival time (same idea as the r5r legs file)
#   OUT_COMPARE one row per sampled tourist (all of them, also those without an
#              r5r trip): r5r vs OJP, difference, % and a deviation class
#              (close / moderate / major)
#
# HOW THE TWO ARE COMPARED. r5r reports the fastest trip over all departures in
# its time window (pt$time_window_minutes) minus the wait for the first vehicle.
# The equivalent in OJP is the fastest connection that DEPARTS INSIDE THE SAME
# WINDOW (ojp_fastest_min). Connections leaving after the window are ignored,
# otherwise a fast train hours later would make OJP look faster than r5r.
# Arrival times are NOT differenced: r5r reports the departure with the shortest travel
# time, which can be late in the window, so its arrival is not comparable with the
# earliest OJP arrival. Both are listed (r5r_arrive_at, ojp_earliest_arrival) for reference.
#
# Before running:
#   1. Get a free API key: register at https://api-manager.opentransportdata.swiss/,
#      create an application and subscribe it to "OJP 2.0".
#      Free quota: 50 requests/minute, 20,000/day.
#   2. Put the key in your ~/.Renviron (then restart R):
#        OJP_API_KEY=your-key-here
#   3. CHECK_DATE is the departure date of the r5r routing (pt$departure_date in
#      config.yml). Timetables for past dates may not be available, so set that
#      date to today or later and rerun 04_pt_r5r_ch_fr_route_sample.R before
#      this script, so both use the same timetable.
#
# Coverage: OJP Switzerland uses the Swiss national timetable, which includes
# international and cross-border trains but not most purely French services.
# Trips starting deep in France (e.g. Châlons, Voiron) may come back empty or
# with a long walk at the start -- check the ojp_status column.
#
# Run from the project root: source("Scripts/02_travel_time/OEV/FR_CH/test/cff_check_travel_time.R")
# ============================================================

source("Scripts/00_setup.R")   # sets the project root; provides cfg

for (p in c("httr2", "xml2", "readr", "dplyr")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
}
library(httr2)
library(xml2)

# --- Settings ----------------------------------------------------------------
TEST_DIR   <- cfg$oev_ch_fr$test_dir                              # data/test/CH_FR (config.yml)
TRIPS_CSV  <- file.path(TEST_DIR, "pt_ch_fr_sample_trips.csv")   # from 04_pt_r5r_ch_fr_route_sample.R
TIMES_CSV  <- file.path(TEST_DIR, "pt_ch_fr_sample_times.csv")   # all sampled tourists (also those without an r5r time)
OUT_TRIPS  <- file.path(TEST_DIR, "ojp_trips.csv")
OUT_LEGS   <- file.path(TEST_DIR, "ojp_legs.csv")
OUT_COMPARE <- file.path(TEST_DIR, "comparison_r5r_vs_ojp.csv")   # one row per sampled tourist

CHECK_DATE  <- cfg$pt$departure_date   # same date as the r5r routing (config.yml), so both use the same
                                       # timetable; must be today or later
TZ          <- "Europe/Zurich"
# Same time window as the r5r routing: [departure_time, departure_time + time_window_minutes)
WIN_START   <- as.POSIXct(paste(CHECK_DATE, cfg$pt$departure_time), format = "%Y-%m-%d %H:%M:%S", tz = TZ)
WIN_END     <- WIN_START + cfg$pt$time_window_minutes * 60
N_RESULTS   <- 10             # connections requested per tourist, enough to cover the window for most trips
OJP_URL     <- "https://api.opentransportdata.swiss/ojp20"
OJP_KEY     <- Sys.getenv("OJP_API_KEY")
PAUSE_SEC   <- 1.3            # keeps you under 50 requests/minute

if (!nzchar(OJP_KEY)) stop("No API key: add OJP_API_KEY=... to ~/.Renviron and restart R.")
msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), sprintf(...), "\n", sep = "")

# --- Helpers -----------------------------------------------------------------
# XPath that ignores namespaces: "TimedLeg/LegBoard" -> "*[local-name()='TimedLeg']/..."
ln <- function(path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  paste(vapply(parts, function(p) if (p %in% c("", ".", "..")) p else
    sprintf("*[local-name()='%s']", p), character(1)), collapse = "/")
}
txt <- function(node, ...) {   # first non-missing text among several paths
  for (path in c(...)) {
    n <- xml_find_first(node, ln(path))
    if (!inherits(n, "xml_missing")) return(xml_text(n))
  }
  NA_character_
}
# ISO 8601 time ("2026-10-20T08:30:00Z" or "...+02:00") -> local POSIXct
parse_time <- function(x) {
  out <- rep(as.POSIXct(NA, tz = TZ), length(x))
  ok  <- !is.na(x)
  if (!any(ok)) return(out)
  s   <- x[ok]
  off <- rep(0, length(s))
  m   <- regmatches(s, regexpr("[+-]\\d{2}:\\d{2}$", s))
  has <- grepl("[+-]\\d{2}:\\d{2}$", s)
  off[has] <- (as.numeric(substr(m, 2, 3)) * 60 + as.numeric(substr(m, 5, 6))) *
    ifelse(substr(m, 1, 1) == "-", -1, 1)
  s <- sub("(Z|[+-]\\d{2}:\\d{2})$", "", s)
  t <- as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC") - off * 60
  out[ok] <- t
  attr(out, "tzone") <- TZ
  out
}
# ISO 8601 duration ("PT1H23M", "P1DT2H", "PT45S") -> minutes
parse_dur <- function(x) {
  vapply(x, function(d) {
    if (is.na(d)) return(NA_real_)
    days  <- if (grepl("P[0-9]+D", d)) as.numeric(sub("P([0-9]+)D.*", "\\1", d)) else 0
    tpart <- if (grepl("T", d)) sub("^[^T]*T", "", d) else ""   # part after "T": hours/minutes/seconds
    g <- function(u) { m <- regmatches(tpart, regexpr(paste0("[0-9.]+", u), tpart))
    if (length(m)) as.numeric(sub(u, "", m)) else 0 }
    days * 1440 + g("H") * 60 + g("M") + g("S") / 60
  }, numeric(1), USE.NAMES = FALSE)
}

build_request <- function(o_lat, o_lon, d_lat, d_lon, dep_time) {
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  dep <- format(dep_time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  sprintf('<?xml version="1.0" encoding="UTF-8"?>
<OJP xmlns="http://www.vdv.de/ojp" xmlns:siri="http://www.siri.org.uk/siri" version="2.0">
  <OJPRequest>
    <siri:ServiceRequest>
      <siri:RequestTimestamp>%s</siri:RequestTimestamp>
      <siri:RequestorRef>pt_r5r_plausibility_check</siri:RequestorRef>
      <OJPTripRequest>
        <siri:RequestTimestamp>%s</siri:RequestTimestamp>
        <Origin>
          <PlaceRef>
            <GeoPosition><siri:Longitude>%.6f</siri:Longitude><siri:Latitude>%.6f</siri:Latitude></GeoPosition>
            <Name><Text>Origin</Text></Name>
          </PlaceRef>
          <DepArrTime>%s</DepArrTime>
        </Origin>
        <Destination>
          <PlaceRef>
            <GeoPosition><siri:Longitude>%.6f</siri:Longitude><siri:Latitude>%.6f</siri:Latitude></GeoPosition>
            <Name><Text>Destination</Text></Name>
          </PlaceRef>
        </Destination>
        <Params>
          <NumberOfResults>%d</NumberOfResults>
          <IncludeIntermediateStops>false</IncludeIntermediateStops>
          <IncludeLegProjection>false</IncludeLegProjection>
        </Params>
      </OJPTripRequest>
    </siri:ServiceRequest>
  </OJPRequest>
</OJP>', now, now, o_lon, o_lat, dep, d_lon, d_lat, N_RESULTS)
}

# Returns list(trips = data.frame, legs = data.frame, status = text)
ojp_trip <- function(id, o_lat, o_lon, d_lat, d_lon, dep_time) {
  resp <- tryCatch(
    request(OJP_URL) |>
      req_headers(Authorization = paste("Bearer", OJP_KEY),
                  `Content-Type` = "application/xml",
                  `User-Agent` = "pt_r5r_plausibility_check") |>
      req_body_raw(build_request(o_lat, o_lon, d_lat, d_lon, dep_time), type = "application/xml") |>
      req_retry(max_tries = 3, is_transient = function(r) resp_status(r) %in% c(429, 503)) |>
      req_error(is_error = function(r) FALSE) |>
      req_perform(),
    error = function(e) e)
  if (inherits(resp, "error")) return(list(status = paste("request failed:", conditionMessage(resp))))
  if (resp_status(resp) != 200) return(list(status = sprintf("HTTP %d: %s", resp_status(resp),
                                                             substr(resp_body_string(resp), 1, 200))))
  doc <- read_xml(resp_body_string(resp))
  err <- txt(doc, "//ErrorCondition//Text", "//ErrorCondition//Description")
  results <- xml_find_all(doc, ln("//TripResult"))
  if (!length(results)) return(list(status = if (is.na(err)) "no connection found" else paste("OJP:", err)))
  
  trips <- list(); legs <- list()
  for (k in seq_along(results)) {
    trip <- xml_find_first(results[[k]], ln("./Trip"))
    trips[[k]] <- data.frame(
      tourist_id    = id, option = k,
      ojp_depart    = parse_time(txt(trip, "./StartTime")),
      ojp_arrive    = parse_time(txt(trip, "./EndTime")),
      ojp_duration_min = round(parse_dur(txt(trip, "./Duration")), 1),
      ojp_transfers = suppressWarnings(as.integer(txt(trip, "./Transfers"))))
    for (leg in xml_find_all(trip, ln("./Leg"))) {
      if (!inherits(xml_find_first(leg, ln("./TimedLeg")), "xml_missing")) {
        mode <- txt(leg, "./TimedLeg/Service/Mode/PtMode")
        line <- txt(leg, "./TimedLeg/Service/PublishedServiceName/Text",
                    "./TimedLeg/Service/PublishedLineName/Text")
        legs[[length(legs) + 1]] <- data.frame(
          tourist_id = id, option = k, leg = length(legs) + 1, type = "ride",
          mode = mode, line = line,
          train_number = txt(leg, "./TimedLeg/Service/TrainNumber"),
          direction    = txt(leg, "./TimedLeg/Service/DestinationText/Text"),
          from_stop    = txt(leg, "./TimedLeg/LegBoard/StopPointName/Text"),
          to_stop      = txt(leg, "./TimedLeg/LegAlight/StopPointName/Text"),
          dep = parse_time(txt(leg, "./TimedLeg/LegBoard/ServiceDeparture/TimetabledTime")),
          arr = parse_time(txt(leg, "./TimedLeg/LegAlight/ServiceArrival/TimetabledTime")),
          duration_min = NA_real_)
      } else {   # walking: TransferLeg (between stops) or ContinuousLeg (start/end)
        kind <- if (!inherits(xml_find_first(leg, ln("./TransferLeg")), "xml_missing")) "TransferLeg" else "ContinuousLeg"
        legs[[length(legs) + 1]] <- data.frame(
          tourist_id = id, option = k, leg = length(legs) + 1, type = "walk",
          mode = "walk", line = NA_character_, train_number = NA_character_, direction = NA_character_,
          from_stop = txt(leg, paste0("./", kind, "/LegStart/Name/Text")),
          to_stop   = txt(leg, paste0("./", kind, "/LegEnd/Name/Text")),
          dep = as.POSIXct(NA, tz = TZ), arr = as.POSIXct(NA, tz = TZ),
          duration_min = round(parse_dur(txt(leg, paste0("./", kind, "/Duration"), "./Duration")), 1))
      }
    }
  }
  legs <- dplyr::bind_rows(legs)
  legs$leg <- ave(legs$leg, legs$option, FUN = seq_along)   # number legs within each connection
  list(trips = dplyr::bind_rows(trips), legs = legs, status = "ok")
}

# --- Run ---------------------------------------------------------------------
# depart_at/arrive_at read as text: readr would otherwise treat them as UTC
# and shift the clock time by two hours
r5 <- readr::read_csv(TRIPS_CSV, show_col_types = FALSE,
                      col_types = readr::cols(depart_at = "c", arrive_at = "c", .default = readr::col_guess()))

# Every sampled tourist is checked, also those for which r5r found no trip (no row in
# the trips file): OJP is asked from their nearest station at the configured departure
# time, to see whether a connection exists where r5r found none.
samp <- readr::read_csv(TIMES_CSV, show_col_types = FALSE)
no_trip <- samp[!samp$tourist_id %in% r5$tourist_id, ]
if (nrow(no_trip)) {
  r5 <- dplyr::bind_rows(r5, data.frame(
    tourist_id = no_trip$tourist_id, from_station = no_trip$nearest_stop_name,
    origin_lat = no_trip$nearest_stop_lat, origin_lon = no_trip$nearest_stop_lon,
    dest_lat = no_trip$dest_lat, dest_lon = no_trip$dest_long,
    depart_at = paste(CHECK_DATE, substr(cfg$pt$departure_time, 1, 5)), stringsAsFactors = FALSE))
}
r5 <- r5[order(r5$tourist_id), ]
msg("Asking OJP for %d trips on %s (%d connections each; %d without an r5r trip)...",
    nrow(r5), CHECK_DATE, N_RESULTS, nrow(no_trip))

all_trips <- list(); all_legs <- list(); status <- character(nrow(r5))
for (i in seq_len(nrow(r5))) {
  dep   <- WIN_START                                            # connections from the start of the r5r window
  res <- ojp_trip(r5$tourist_id[i], r5$origin_lat[i], r5$origin_lon[i],
                  r5$dest_lat[i], r5$dest_lon[i], dep)
  status[i] <- res$status
  if (identical(res$status, "ok")) { all_trips[[i]] <- res$trips; all_legs[[i]] <- res$legs }
  msg("  [%s] %s -> %s", r5$tourist_id[i], r5$from_station[i], res$status)
  Sys.sleep(PAUSE_SEC)
}

ojp_trips <- dplyr::bind_rows(all_trips)
ojp_legs  <- dplyr::bind_rows(all_legs)
if (nrow(ojp_trips)) ojp_trips$in_window <- ojp_trips$ojp_depart >= WIN_START & ojp_trips$ojp_depart < WIN_END

if (nrow(ojp_trips)) {
  # First departing vehicle per connection (OJP's duration counts from the
  # first walk, so it starts close to the first train -- like Google)
  first_ride <- ojp_legs[ojp_legs$type == "ride", ]
  first_ride <- first_ride[!duplicated(paste(first_ride$tourist_id, first_ride$option)), ]
  key <- paste(ojp_trips$tourist_id, ojp_trips$option)
  ojp_trips$ojp_first_vehicle_at <- format(first_ride$dep[match(key, paste(first_ride$tourist_id, first_ride$option))], "%H:%M")
  ojp_trips$ojp_lines <- vapply(key, function(k) {
    l <- ojp_legs[ojp_legs$type == "ride" & paste(ojp_legs$tourist_id, ojp_legs$option) == k, ]
    paste(ifelse(is.na(l$line), l$mode, l$line), collapse = " -> ")
  }, character(1), USE.NAMES = FALSE)
  # TRUE for the fastest connection departing inside the r5r window (per tourist)
  dur_w <- ifelse(ojp_trips$in_window, ojp_trips$ojp_duration_min, NA_real_)
  ojp_trips$ojp_fastest <- as.logical(ave(dur_w, ojp_trips$tourist_id,
                                          FUN = function(d) !is.na(d) & d == suppressWarnings(min(d, na.rm = TRUE))))
  
  # r5r values to compare with (comparable_to_google_min = r5r time without the initial wait)
  keep_r5 <- intersect(c("tourist_id", "from_station", "depart_at", "trip_type", "pt_time_min",
                         "trip_time_min", "comparable_to_google_min", "n_rides", "routes"), names(r5))
  ojp_trips <- merge(ojp_trips, r5[, keep_r5], by = "tourist_id", all.x = TRUE, sort = FALSE)
  names(ojp_trips)[names(ojp_trips) %in% setdiff(keep_r5, "tourist_id")] <-
    paste0("r5r_", names(ojp_trips)[names(ojp_trips) %in% setdiff(keep_r5, "tourist_id")])
  if ("r5r_comparable_to_google_min" %in% names(ojp_trips)) {
    ojp_trips$diff_ojp_minus_r5r_min <- round(ojp_trips$ojp_duration_min - ojp_trips$r5r_comparable_to_google_min, 1)
  }
  ojp_trips <- ojp_trips[order(ojp_trips$tourist_id, ojp_trips$option), ]
  ojp_trips$ojp_depart <- format(ojp_trips$ojp_depart, "%Y-%m-%d %H:%M")
  ojp_trips$ojp_arrive <- format(ojp_trips$ojp_arrive, "%Y-%m-%d %H:%M")
  ojp_legs$dep <- format(ojp_legs$dep, "%H:%M"); ojp_legs$arr <- format(ojp_legs$arr, "%H:%M")
}

# Tourists without an OJP answer still get a row, with the reason
missing <- setdiff(r5$tourist_id, ojp_trips$tourist_id)
if (length(missing)) {
  ojp_trips <- dplyr::bind_rows(ojp_trips, data.frame(tourist_id = missing))
}
ojp_trips$ojp_status <- status[match(ojp_trips$tourist_id, r5$tourist_id)]

readr::write_csv(ojp_trips, OUT_TRIPS, na = "")
readr::write_csv(ojp_legs,  OUT_LEGS,  na = "")
msg("Wrote %s (%d connections) and %s (%d legs). %d of %d tourists answered.",
    OUT_TRIPS, sum(!is.na(ojp_trips$option)), OUT_LEGS, nrow(ojp_legs),
    sum(status == "ok"), nrow(r5))

# --- Comparison table: one row per sampled tourist ---------------------------
# r5r value = fastest trip over the departures in the time window minus the wait for
# the first vehicle (r5r_comparable_min). OJP value = fastest connection that departs
# INSIDE the same window (ojp_fastest_min). diff = OJP - r5r.
# Arrival times are listed for reference only (see the header: not differenced).
# ojp_window_covered: FALSE if the 10 connections returned all depart before the end
# of the window, i.e. OJP may have had faster ones inside the window we did not see.
# deviation: "close"    = within 10 min or 10%
#            "major"    = more than 30 min AND more than 25% apart
#            "moderate" = in between
to_time <- function(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M", tz = TZ)
first_or_na <- function(x) if (length(x)) x[1] else NA
pick_fastest <- function(x, dur, w) { i <- which(w %in% TRUE); if (length(i)) x[i][which.min(dur[i])] else NA }

has_trip <- if ("comparable_to_google_min" %in% names(r5)) !is.na(r5$comparable_to_google_min) else rep(FALSE, nrow(r5))
col_or_na <- function(nm, na) if (nm %in% names(r5)) r5[[nm]] else rep(na, nrow(r5))
r5_vals <- data.frame(
  tourist_id         = r5$tourist_id,
  r5r_comparable_min = col_or_na("comparable_to_google_min", NA_real_),
  r5r_trip_time_min  = col_or_na("trip_time_min", NA_real_),
  r5r_n_rides        = col_or_na("n_rides", NA_real_),
  r5r_depart_at      = ifelse(has_trip, col_or_na("depart_at", NA_character_), NA_character_),
  r5r_arrive_at      = ifelse(has_trip, col_or_na("arrive_at", NA_character_), NA_character_),
  r5r_routes         = col_or_na("routes", NA_character_),
  stringsAsFactors = FALSE)

cmp <- ojp_trips |>
  dplyr::group_by(tourist_id) |>
  dplyr::summarise(
    ojp_status            = dplyr::first(ojp_status),
    ojp_n_options         = sum(!is.na(option)),
    ojp_n_in_window       = sum(in_window %in% TRUE),
    ojp_last_departure    = if (all(is.na(ojp_depart))) NA_character_ else max(ojp_depart, na.rm = TRUE),
    ojp_fastest_min       = if (any(in_window %in% TRUE)) min(ojp_duration_min[in_window %in% TRUE], na.rm = TRUE) else NA_real_,
    ojp_fastest_depart    = pick_fastest(ojp_depart, ojp_duration_min, in_window),
    ojp_transfers_fastest = pick_fastest(ojp_transfers, ojp_duration_min, in_window),
    ojp_lines_fastest     = pick_fastest(ojp_lines, ojp_duration_min, in_window),
    ojp_earliest_arrival  = if (all(is.na(ojp_arrive))) NA_character_ else min(ojp_arrive, na.rm = TRUE),
    .groups = "drop") |>
  dplyr::left_join(r5_vals, by = "tourist_id") |>
  dplyr::left_join(
    samp[, intersect(c("tourist_id", "state", "nearest_stop_name", "dist_to_station_km", "dest_lat", "dest_long",
                       "pt_time_min", "pt_time_fastest_min", "fail_reason"), names(samp))],
    by = "tourist_id") |>
  dplyr::mutate(
    ojp_window_covered = !(ojp_n_options > 0 & !is.na(ojp_last_departure) & to_time(ojp_last_departure) < WIN_END),
    diff_ojp_minus_r5r_min = round(ojp_fastest_min - r5r_comparable_min, 1),
    pct_diff = round(100 * diff_ojp_minus_r5r_min / r5r_comparable_min, 1),
    deviation = dplyr::case_when(
      is.na(r5r_comparable_min) & ojp_n_options == 0 ~ "no connection in r5r and OJP",
      is.na(r5r_comparable_min)                      ~ "no r5r trip (OJP found one)",
      ojp_n_options == 0                             ~ "no OJP connection (r5r found one)",
      is.na(ojp_fastest_min)                         ~ "no OJP departure in the window (r5r found one)",
      abs(diff_ojp_minus_r5r_min) <= 10 | abs(pct_diff) <= 10 ~ "close",
      abs(diff_ojp_minus_r5r_min) > 30 & abs(pct_diff) > 25   ~ "major",
      TRUE ~ "moderate")) |>
  dplyr::relocate(tourist_id, state, nearest_stop_name, r5r_comparable_min, ojp_fastest_min,
                  diff_ojp_minus_r5r_min, pct_diff, deviation) |>
  dplyr::arrange(tourist_id)
readr::write_csv(cmp, OUT_COMPARE, na = "")
msg("Wrote %s (%d tourists).", OUT_COMPARE, nrow(cmp))
print(table(cmp$deviation))
