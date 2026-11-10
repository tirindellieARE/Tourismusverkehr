# ============================================================
# geocode_origins.R   (archived in butta/ -- no longer used; the French region is now derived from the commune id in 03_pt_r5r_ch_fr_build_network.R)
# Reverse-geocodes every unique origin of the trips in agqpv.csv to its
# country / state (region) / commune, using Transitous's public reverse-geocode
# endpoint (/api/v1/reverse-geocode). It returns the OSM administrative boundary
# hierarchy of a coordinate; adminLevel is standardized across Western Europe as:
# 2 = country, 4 = state/region (Land/Region/Regione), 6 = county/province,
# 8 = commune/municipality (Gemeinde/Commune/Comune).
#
# WHAT IT IS USED FOR: the region of each origin, e.g. to pick the French
# origins in the three regions bordering Switzerland (see
# OEV/FR_CH/03_pt_r5r_ch_fr_build_network.R).
#
# NO NEED TO RERUN: the output (data/output/butta/origins_geocoded_current.csv) already
# covers all origins of the current agqpv.csv. Rerun only if agqpv.csv changes
# and contains new origins -- the script is resumable and only looks up origins
# that are not in the output yet (or that previously came back without a
# country), so a rerun is cheap.
#
# Input : data/output/csv/agqpv.csv          (cfg$input_csv; unique origin_lat/origin_long)
# Output: data/output/butta/origins_geocoded_current.csv
#         columns: origin_lat, origin_long, country, state, commune, place_name
#
# This is a lightweight address lookup, not a routing query -- Transitous's
# usage policy flags "routing or isochrone calculations" as needing prior
# contact for heavy use; geocoding a few thousand points, throttled, is a much
# lighter one-off research lookup. Throttled (4/sec) and sent with a
# descriptive User-Agent per their policy.
#
# Not every origin will resolve (Transitous's OSM coverage is best in Europe;
# a handful of far-flung or address-sparse points may come back empty) -- those
# are left as NA rather than guessed.
#
# Run from the project root:  source("Scripts/02_travel_time/geocode_origins.R")
# ============================================================

source("Scripts/00_setup.R")
if (!requireNamespace("httr2", quietly = TRUE)) install.packages("httr2", repos = "https://cloud.r-project.org")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite", repos = "https://cloud.r-project.org")
library(httr2)
library(jsonlite)

REVERSE_GEOCODE_URL <- "https://api.transitous.org/api/v1/reverse-geocode"
USER_AGENT <- "Tourismusverkehr-ARE-research/0.1 (one-off reverse-geocode lookup; contact: elisamaria.tirindelli@gmail.com)"
OUT_FILE <- "data/output/butta/origins_geocoded_current.csv"

# --- Unique origins of the current trips -------------------------------------
agqpv <- readr::read_csv(cfg$input_csv, locale = readr::locale(encoding = cfg$input_encoding),
                         show_col_types = FALSE)
origins <- dplyr::distinct(agqpv, origin_lat, origin_long)
origins$key <- sprintf("%.6f_%.6f", origins$origin_lat, origins$origin_long)
log_msg(sprintf("Reverse-geocoding %d unique origins from %s...", nrow(origins), basename(cfg$input_csv)))

# --- Resume from a previous run (matched by coordinates) ---------------------
result <- if (file.exists(OUT_FILE)) readr::read_csv(OUT_FILE, show_col_types = FALSE) else NULL
if (!is.null(result)) result$key <- sprintf("%.6f_%.6f", result$origin_lat, result$origin_long)
done_keys <- if (!is.null(result)) result$key[!is.na(result$country)] else character()
todo <- origins[!origins$key %in% done_keys, ]
log_msg(sprintf("%d already geocoded; %d to do.", sum(origins$key %in% done_keys), nrow(todo)))

reverse_geocode <- function(lat, lon) {
  empty <- list(country = NA_character_, state = NA_character_, commune = NA_character_, place_name = NA_character_)
  req <- request(REVERSE_GEOCODE_URL) |>
    req_url_query(place = sprintf("%.6f,%.6f", lat, lon), numResults = 1L) |>
    req_headers(`User-Agent` = USER_AGENT) |>
    req_error(is_error = function(resp) FALSE) |>
    req_retry(max_tries = 3, is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503))

  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) != 200) return(empty)
  matches <- tryCatch(fromJSON(resp_body_string(resp), simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(matches) || length(matches) == 0) return(empty)

  m <- matches[[1]]
  areas <- m$areas
  by_level <- function(lvl) {
    for (a in areas) if (isTRUE(a$adminLevel == lvl)) return(a$name)
    NA_character_
  }
  # adminLevel 8 is usually the commune, but a few countries use different
  # levels for the finest unit -- prefer the area explicitly marked as the
  # default/unique match (the commune actually containing the point) if
  # adminLevel 8 itself isn't present.
  commune <- by_level(8)
  if (is.na(commune)) {
    for (a in areas) if (isTRUE(a$default) && isTRUE(a$unique)) { commune <- a$name; break }
  }
  list(country = by_level(2), state = by_level(4), commune = commune,
       place_name = if (is.null(m$name)) NA_character_ else m$name)
}

# Keep only the columns of the output; previous rows for origins that are redone are replaced.
out_cols <- c("origin_lat", "origin_long", "country", "state", "commune", "place_name")
save_progress <- function(new_rows) {
  new_df <- dplyr::bind_rows(new_rows)
  new_df$key <- sprintf("%.6f_%.6f", new_df$origin_lat, new_df$origin_long)
  old <- if (!is.null(result)) result[!result$key %in% new_df$key, ] else NULL
  readr::write_csv(dplyr::bind_rows(old, new_df)[, out_cols], OUT_FILE)
}

new_rows <- vector("list", nrow(todo))
for (i in seq_len(nrow(todo))) {
  o <- todo[i, ]
  g <- reverse_geocode(o$origin_lat, o$origin_long)
  new_rows[[i]] <- data.frame(
    origin_lat = o$origin_lat, origin_long = o$origin_long,
    country = g$country, state = g$state, commune = g$commune, place_name = g$place_name,
    stringsAsFactors = FALSE
  )
  if (i %% 25 == 0 || i == nrow(todo)) {
    log_msg(sprintf("  geocoded %d / %d this run", i, nrow(todo)))
    save_progress(new_rows[seq_len(i)])
  }
  Sys.sleep(0.25)   # throttle -- ~4/sec, a lightweight lookup not a routing query
}

if (!file.exists(OUT_FILE)) stop("Nothing was geocoded and no previous ", OUT_FILE, " exists.")
final <- readr::read_csv(OUT_FILE, show_col_types = FALSE)[, out_cols] |> dplyr::arrange(origin_lat, origin_long)
readr::write_csv(final, OUT_FILE)
log_msg(sprintf("Wrote %s | %d rows | %d with a resolved country, %d with a resolved commune",
                OUT_FILE, nrow(final), sum(!is.na(final$country)), sum(!is.na(final$commune))))
missing_now <- sum(!origins$key %in% sprintf("%.6f_%.6f", final$origin_lat, final$origin_long))
if (missing_now > 0) warning(missing_now, " origin(s) of agqpv.csv are not in ", OUT_FILE)
log_msg("geocode_origins.R done.")
