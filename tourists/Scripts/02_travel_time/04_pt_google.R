# ============================================================
# 04_pt_google.R
# Public-transport travel time origin -> destination for every unique pair,
# via the Google Routes API (Compute Routes, travelMode=TRANSIT). Written as
# an ALTERNATIVE to 04_pt_r5r.R, not a replacement -- pick one.
#
# WHY THIS EXISTS: the r5r/tiled approach (04_pt_r5r.R) is correct but slow
# on this machine. R5 hard-rejects any network over 975,000 km^2 (our full
# extent is ~3.6M km^2), so PT routing has to be split into ~40 tiles, and
# each tile's build hits a Windows-specific bottleneck (MappedByteBuffer's
# flush-to-disk is far slower on Windows than Linux for the multi-GB memory-
# mapped OSM database R5 builds per tile) -- confirmed via a `jstack` thread
# dump showing genuine, non-deadlocked CPU work, not a bug, just slow. At
# ~90 min/tile that's roughly 60 hours total. This script trades that for a
# per-request cost instead of a time cost: no OSM download, no GTFS feeds,
# no tiling, no Java -- just one HTTP call per pair.
#
# COST (verified against Google's own SKU-trigger docs, not assumed): plain
# travelMode=TRANSIT isn't listed as a Pro or Enterprise trigger (those are
# traffic-aware modifiers, waypoint optimization, two-wheel routing, tolls),
# so it bills as Essentials: $5.00 / 1,000 requests, with the first 10,000
# requests/month free. This dataset is 7,726 unique pairs -- a single run
# fits entirely inside the free monthly allowance (until you touch other
# Google Maps usage on the same billing project in the same month, or rerun
# this more than once in a month; overage is the same $5.00/1,000).
#
# PREREQUISITES:
#   1. A Google Cloud project with billing enabled and the "Routes API"
#      enabled (console.cloud.google.com -> APIs & Services).
#   2. An API key with the Routes API permitted, set as an environment
#      variable BEFORE starting R (never hardcode it, never put it in
#      config.yml -- that file is checked into git):
#        Windows (PowerShell):  $env:GOOGLE_MAPS_API_KEY = "your-key-here"
#        Windows (this repo's Bash tool): export GOOGLE_MAPS_API_KEY="..."
#   3. install.packages("httr2") if not already installed.
#
# SAFETY: config.yml's google.dry_run_limit defaults to 5 -- this script
# will ONLY query the first 5 pairs until you deliberately change that to 0
# (process everything) or a specific larger number. This is deliberate:
# unlike the other scripts in this pipeline, every run of this one can cost
# real money. It's also RESUMABLE -- if data/output/pt_times_google.csv
# already exists, pairs that already have a non-NA time are skipped, so a
# rerun after an interruption (or after raising dry_run_limit) does not
# re-pay for work already done.
#
# Run:  source("Scripts/02_travel_time/00_setup.R"); source("Scripts/02_travel_time/04_pt_google.R")
# ============================================================

source("Scripts/02_travel_time/00_setup.R")

if (!requireNamespace("httr2", quietly = TRUE)) {
  install.packages("httr2", repos = "https://cloud.r-project.org")
}
library(httr2)

# --- API key -----------------------------------------------------------
key_env <- cfg$google$api_key_env
api_key <- Sys.getenv(key_env)
if (!nzchar(api_key)) {
  stop(sprintf(
    "Environment variable '%s' is not set. Set it to a Google Maps API key\n",
    key_env), "with the Routes API enabled before running this script.\n",
    "See the header of this file for how to set it.")
}

# --- Inputs --------------------------------------------------------------
if (!file.exists(cfg$out$pairs)) stop("Run 01_filter.R first.")
pairs <- readr::read_csv(cfg$out$pairs, show_col_types = FALSE)
n_total <- nrow(pairs)

out_file <- "data/output/pt_times_google.csv"   # separate from pt_times.csv (the r5r output)

# --- Resume from a previous partial run -----------------------------------
pt_min <- rep(NA_real_, n_total)
if (file.exists(out_file)) {
  prev <- readr::read_csv(out_file, show_col_types = FALSE)
  if (all(c("pair_id", "pt_time_min") %in% names(prev)) && nrow(prev) == n_total) {
    pt_min <- prev$pt_time_min[match(pairs$pair_id, prev$pair_id)]
    log_msg(sprintf("Resuming: %d / %d pairs already have a time from a previous run.",
                    sum(!is.na(pt_min)), n_total))
  }
}

# --- Which pairs to query this run ----------------------------------------
todo_idx <- which(is.na(pt_min))
dry_run_limit <- cfg$google$dry_run_limit
if (isTRUE(dry_run_limit > 0)) {
  todo_idx <- head(todo_idx, dry_run_limit)
  log_msg(sprintf("DRY RUN: google.dry_run_limit = %d in config.yml -- only querying %d pair(s).",
                  dry_run_limit, length(todo_idx)))
  log_msg("Set google.dry_run_limit to 0 in config.yml to process all remaining pairs.")
}

n_todo <- length(todo_idx)
est_cost <- max(0, (sum(!is.na(pt_min)) + n_todo) - 10000) / 1000 * 5.00
log_msg(sprintf(
  "%d pair(s) to query this run (%d already done, %d total). Essentials tier: $5.00/1000 after the first 10,000/month free.",
  n_todo, sum(!is.na(pt_min)), n_total))
log_msg(sprintf("Estimated cost if this is your only Google Maps usage this month: ~$%.2f", est_cost))

if (n_todo == 0) {
  log_msg("Nothing to do -- all pairs already have a time (or dry_run_limit is 0 with none pending).")
} else {

# --- Departure time (RFC3339 UTC, converted from the configured local time) ---
departure_local <- as.POSIXct(
  paste(cfg$pt$departure_date, cfg$pt$departure_time),
  format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Zurich"
)
departure_utc <- format(departure_local, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
log_msg(sprintf("PT departure (off-peak): %s local (%s)", format(departure_local), departure_utc))

# --- One Compute Routes call, TRANSIT mode --------------------------------
call_google_transit <- function(o_lat, o_lon, d_lat, d_lon) {
  body <- list(
    origin      = list(location = list(latLng = list(latitude = o_lat, longitude = o_lon))),
    destination = list(location = list(latLng = list(latitude = d_lat, longitude = d_lon))),
    travelMode  = "TRANSIT",
    departureTime = departure_utc,
    computeAlternativeRoutes = FALSE
  )

  req <- request("https://routes.googleapis.com/directions/v2:computeRoutes") |>
    req_headers(
      "Content-Type"     = "application/json",
      "X-Goog-Api-Key"   = api_key,
      "X-Goog-FieldMask" = "routes.duration"
    ) |>
    req_body_json(body) |>
    req_error(is_error = function(resp) FALSE) |>   # handle non-200 ourselves, don't throw
    req_retry(max_tries = 3, is_transient = \(resp) resp_status(resp) %in% c(429, 500, 503))

  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(resp)) return(list(min = NA_real_, note = "request_failed"))

  if (resp_status(resp) != 200) {
    return(list(min = NA_real_, note = sprintf("http_%d", resp_status(resp))))
  }

  parsed <- tryCatch(resp_body_json(resp), error = function(e) NULL)
  routes <- parsed$routes
  if (is.null(routes) || length(routes) == 0) {
    return(list(min = NA_real_, note = "no_route"))
  }

  dur_str <- routes[[1]]$duration   # e.g. "5455s"
  secs <- suppressWarnings(as.numeric(sub("s$", "", dur_str)))
  if (is.na(secs)) return(list(min = NA_real_, note = "unparseable_duration"))
  list(min = secs / 60, note = "ok")
}

# --- Loop, one pair at a time, throttled ----------------------------------
delay_s <- 1 / cfg$google$requests_per_second
n_ok <- 0L
n_fail <- 0L

for (k in seq_along(todo_idx)) {
  i <- todo_idx[k]
  t0 <- Sys.time()
  r <- call_google_transit(pairs$origin_lat[i], pairs$origin_long[i],
                            pairs$dest_lat[i],   pairs$dest_long[i])
  pt_min[i] <- r$min
  if (r$note == "ok") n_ok <- n_ok + 1L else n_fail <- n_fail + 1L

  if (k %% 100 == 0 || k == n_todo) {
    log_msg(sprintf("  google transit routed %d / %d this run (ok: %d, failed/no-route: %d)",
                    k, n_todo, n_ok, n_fail))
    # periodic checkpoint save -- resumable if interrupted, and avoids
    # re-paying for pairs already fetched in this run
    readr::write_csv(
      pairs |> dplyr::select(pair_id, origin_lat, origin_long, dest_lat, dest_long) |>
        dplyr::mutate(pt_time_min = pt_min),
      out_file
    )
  }

  elapsed <- as.numeric(Sys.time() - t0, units = "secs")
  if (elapsed < delay_s) Sys.sleep(delay_s - elapsed)
}

log_msg(sprintf("This run: %d ok, %d failed/no-route out of %d queried.", n_ok, n_fail, n_todo))

}  # end if (n_todo == 0) ... else ...

# --- Final write -------------------------------------------------------
pt_times <- pairs |>
  dplyr::select(pair_id, origin_lat, origin_long, dest_lat, dest_long) |>
  dplyr::mutate(pt_time_min = pt_min)
readr::write_csv(pt_times, out_file)
log_msg(sprintf("Wrote %s | have a time: %d / %d",
                out_file, sum(!is.na(pt_min)), n_total))
log_msg(sprintf(
  "NOTE: this wrote to %s, not %s (the r5r output) -- rename/copy it before 05_join.R if you want to use these times instead.",
  out_file, cfg$out$pt_times))
log_msg("04_pt_google.R done.")
