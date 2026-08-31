# ============================================================
# geocode_origins.R
# Reverse-geocodes every unique ausland origin (data/output/origins_ausland.csv,
# 3,037 points) to its country / state (region) / commune, using Transitous's
# public reverse-geocode endpoint (/api/v1/reverse-geocode) -- the same
# service used during the Transitous/MOTIS routing validation (see
# transitous_test.R, routing_readme.md). It returns the OSM administrative
# boundary hierarchy for a coordinate; adminLevel is standardized across
# Western Europe as: 2 = country, 4 = state/region (Land/Region/Regione),
# 6 = county/province, 8 = commune/municipality (Gemeinde/Commune/Comune).
#
# This is a lightweight address lookup, not a routing query -- Transitous's
# usage policy specifically flags "routing or isochrone calculations" as
# needing prior contact for heavy use; geocoding ~3,037 points, throttled,
# is a much lighter, one-off research lookup. Still throttled (4/sec) and
# sent with a descriptive User-Agent per their policy.
#
# Not every origin will resolve (Transitous's OSM coverage is best in
# Europe; a handful of far-flung or address-sparse points may come back
# empty) -- those are left as NA rather than guessed.
#
# Run:  source("Scripts/02_travel_time/00_setup.R"); source("Scripts/02_travel_time/geocode_origins.R")
# ============================================================

user = "MR"
if(user == "MR"){setwd("E:/ARE/ProjekteTIE/Turismusverkehr/RScript/tourists")}
if(user == "CP"){setwd("P:/Verkehrsmodellierung/06_Jobs/188_touristische_Verkehr/Model_trafic_touristique/GitRepo/tourists")}

source("Scripts/02_travel_time/00_setup.R")
if (!requireNamespace("httr2", quietly = TRUE)) install.packages("httr2", repos = "https://cloud.r-project.org")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite", repos = "https://cloud.r-project.org")
library(httr2)
library(jsonlite)

REVERSE_GEOCODE_URL <- "https://api.transitous.org/api/v1/reverse-geocode"
USER_AGENT <- "Tourismusverkehr-ARE-research/0.1 (one-off reverse-geocode lookup; contact: elisamaria.tirindelli@gmail.com)"
OUT_FILE <- "data/output/origins_geocoded.csv"

origins <- readr::read_csv(cfg$out$origins_ausland, show_col_types = FALSE)
log_msg(sprintf("Reverse-geocoding %d unique ausland origins...", nrow(origins)))

# --- Resume from a previous partial run -------------------------------------
result <- if (file.exists(OUT_FILE)) readr::read_csv(OUT_FILE, show_col_types = FALSE) else NULL
done_ids <- if (!is.null(result)) result$origin_id[!is.na(result$country)] else character()
todo <- origins[!origins$origin_id %in% done_ids, ]
log_msg(sprintf("%d already geocoded (resumed); %d to do.", length(done_ids), nrow(todo)))

reverse_geocode <- function(lat, lon) {
  req <- request(REVERSE_GEOCODE_URL) |>
    req_url_query(place = sprintf("%.6f,%.6f", lat, lon), numResults = 1L) |>
    req_headers(`User-Agent` = USER_AGENT) |>
    req_error(is_error = function(resp) FALSE) |>
    req_retry(max_tries = 3, is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503))

  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) != 200) {
    return(list(country = NA_character_, state = NA_character_, commune = NA_character_, place_name = NA_character_))
  }
  matches <- tryCatch(fromJSON(resp_body_string(resp), simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(matches) || length(matches) == 0) {
    return(list(country = NA_character_, state = NA_character_, commune = NA_character_, place_name = NA_character_))
  }
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
  list(country = by_level(2), state = by_level(4), commune = commune, place_name = if (is.null(m$name)) NA_character_ else m$name)
}

new_rows <- vector("list", nrow(todo))
for (i in seq_len(nrow(todo))) {
  o <- todo[i, ]
  g <- reverse_geocode(o$origin_lat, o$origin_long)
  new_rows[[i]] <- data.frame(
    origin_id = o$origin_id, origin_lat = o$origin_lat, origin_long = o$origin_long,
    country = g$country, state = g$state, commune = g$commune, place_name = g$place_name,
    stringsAsFactors = FALSE
  )
  if (i %% 100 == 0 || i == nrow(todo)) {
    log_msg(sprintf("  geocoded %d / %d this run", i, nrow(todo)))
    partial <- dplyr::bind_rows(new_rows[seq_len(i)])
    combined <- if (!is.null(result)) dplyr::bind_rows(result[!result$origin_id %in% partial$origin_id, ], partial) else partial
    readr::write_csv(combined, OUT_FILE)
  }
  Sys.sleep(0.25)   # throttle -- ~4/sec, a lightweight lookup not a routing query
}

final <- readr::read_csv(OUT_FILE, show_col_types = FALSE) |> dplyr::arrange(origin_id)
readr::write_csv(final, OUT_FILE)
log_msg(sprintf("Wrote %s | %d rows | %d with a resolved country, %d with a resolved commune",
                OUT_FILE, nrow(final), sum(!is.na(final$country)), sum(!is.na(final$commune))))
log_msg("geocode_origins.R done.")
