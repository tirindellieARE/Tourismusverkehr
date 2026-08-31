# ============================================================
# transitous_test.R
# VALIDATION STEP -- not part of the numbered pipeline yet.
#
# Before committing to self-hosting Transitous + MOTIS for PT routing (the
# alternative to the r5r tiled approach, which was abandoned for taking
# ~60h -- see routing_readme.md), sanity-check that the routing itself
# looks right: for a spread of real DE/FR/IT/AT -> CH origin/destination
# pairs from agqpv.csv, does Transitous's public API return itineraries
# that use sensible real-world services (Trenitalia/EC/SBB etc.) with
# plausible transfer times, or nonsense?
#
# Hits the PUBLIC api.transitous.org instance (not self-hosted) -- per
# https://transitous.org/api/: best-effort, limited hosting resources,
# requests must carry a descriptive User-Agent, and non-trivial routing
# load should not be sent without contacting the maintainers first. This
# script makes ~20 throttled requests (1/sec), which is a one-off manual
# validation run, not a load: acceptable per that policy, but do not loop
# this over the full ~7,726-pair agqpv dataset against the public instance
# -- that's exactly the "many requests ... especially routing" case they
# ask to be contacted about first. Full-dataset runs belong on a
# self-hosted instance (the next step, once this validates).
#
# Extracts FULL itineraries (every leg: mode, service name, agency, stop
# names, scheduled times), not just a travel time -- so a human can look at
# e.g. an Italy -> Zurich result and judge whether it's a real, sensible
# route.
#
# Run:  source("Scripts/02_travel_time/00_setup.R"); source("Scripts/02_travel_time/transitous_test.R")
# ============================================================

user = "MR"
if(user == "MR"){setwd("E:/ARE/ProjekteTIE/Turismusverkehr/RScript/tourists")}
if(user == "CP"){setwd("P:/Verkehrsmodellierung/06_Jobs/188_touristische_Verkehr/Model_trafic_touristique/GitRepo/tourists")}

source("Scripts/02_travel_time/00_setup.R")
if (!requireNamespace("httr2", quietly = TRUE)) install.packages("httr2", repos = "https://cloud.r-project.org")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite", repos = "https://cloud.r-project.org")
library(httr2)
library(jsonlite)

TRANSITOUS_BASE <- "https://api.transitous.org/api/v6/plan"
USER_AGENT <- "Tourismusverkehr-ARE-research/0.1 (one-off routing validation test; contact: elisamaria.tirindelli@gmail.com)"

# --- Sample ~20 real OD pairs, spread across border corridors --------------
pairs    <- readr::read_csv(cfg$out$pairs, show_col_types = FALSE)
corridor <- readr::read_csv("data/output/pair_corridor.csv", show_col_types = FALSE)
d <- dplyr::inner_join(pairs, corridor, by = "pair_id")

set.seed(42)
sample_pairs <- d |>
  dplyr::filter(!is.na(corridor), corridor != "") |>
  dplyr::group_by(corridor) |>
  dplyr::slice_sample(n = 4) |>
  dplyr::ungroup()
log_msg(sprintf("Sampled %d pairs across corridors: %s",
                nrow(sample_pairs), paste(table(sample_pairs$corridor), collapse = ",")))

# --- Departure time (same representative off-peak slot as the rest of the
# pipeline's PT parameters -- see config.yml pt: section) -------------------
departure_local <- as.POSIXct(
  paste(cfg$pt$departure_date, cfg$pt$departure_time),
  format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Zurich"
)
departure_utc <- format(departure_local, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
log_msg(sprintf("Test departure: %s local (%s)", format(departure_local), departure_utc))

# --- One /plan call, parsed into a leg-level data.table ---------------------
call_transitous_plan <- function(o_lat, o_lon, d_lat, d_lon, pair_id, corridor) {
  req <- request(TRANSITOUS_BASE) |>
    req_url_query(
      fromPlace     = sprintf("%.6f,%.6f", o_lat, o_lon),
      toPlace       = sprintf("%.6f,%.6f", d_lat, d_lon),
      time          = departure_utc,
      searchWindow  = 240L,     # minutes -- widen beyond the default 15 to actually find a departure
      numItineraries = 3L,
      detailedLegs  = "false"
    ) |>
    req_headers(`User-Agent` = USER_AGENT) |>
    req_error(is_error = function(resp) FALSE) |>
    req_retry(max_tries = 3, is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503))

  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) != 200) {
    return(list(
      summary = data.table::data.table(pair_id = pair_id, corridor = corridor, itinerary_rank = NA_integer_,
                                        note = sprintf("HTTP request failed (status %s)",
                                                       if (is.null(resp)) "NA" else resp_status(resp))),
      legs = data.table::data.table()
    ))
  }

  j <- tryCatch(fromJSON(resp_body_string(resp), simplifyVector = FALSE), error = function(e) NULL)
  n_it <- if (is.null(j)) 0L else length(j$itineraries)
  if (n_it == 0L) {
    return(list(
      summary = data.table::data.table(pair_id = pair_id, corridor = corridor, itinerary_rank = NA_integer_,
                                        note = "No itineraries found (0 results)"),
      legs = data.table::data.table()
    ))
  }

  summ_rows <- list(); leg_rows <- list()
  for (r in seq_len(n_it)) {
    it <- j$itineraries[[r]]
    summ_rows[[r]] <- data.table::data.table(
      pair_id = pair_id, corridor = corridor, itinerary_rank = r,
      duration_min = round(it$duration / 60, 1),
      transfers = it$transfers,
      start_time = it$startTime, end_time = it$endTime, note = NA_character_
    )
    for (lg in seq_along(it$legs)) {
      leg <- it$legs[[lg]]
      g <- function(x) if (is.null(x)) NA_character_ else as.character(x)
      leg_rows[[length(leg_rows) + 1]] <- data.table::data.table(
        pair_id = pair_id, corridor = corridor, itinerary_rank = r, leg_seq = lg,
        mode = g(leg$mode), route_short_name = g(leg$routeShortName),
        display_name = g(leg$displayName), headsign = g(leg$headsign),
        agency_name = g(leg$agencyName),
        from_name = g(leg$from$name), to_name = g(leg$to$name),
        start_time = g(leg$startTime), end_time = g(leg$endTime),
        duration_min = round(leg$duration / 60, 1)
      )
    }
  }
  list(summary = data.table::rbindlist(summ_rows), legs = data.table::rbindlist(leg_rows, fill = TRUE))
}

# --- Run the test batch, throttled ------------------------------------------
all_summary <- list(); all_legs <- list()
for (i in seq_len(nrow(sample_pairs))) {
  p <- sample_pairs[i, ]
  log_msg(sprintf("  [%d/%d] %s (%s): (%.4f,%.4f) -> (%.4f,%.4f)",
                  i, nrow(sample_pairs), p$pair_id, p$corridor,
                  p$origin_lat, p$origin_long, p$dest_lat, p$dest_long))
  res <- call_transitous_plan(p$origin_lat, p$origin_long, p$dest_lat, p$dest_long, p$pair_id, p$corridor)
  all_summary[[i]] <- res$summary
  all_legs[[i]] <- res$legs
  Sys.sleep(1)   # throttle -- be a polite guest on the shared public instance
}
summary_dt <- data.table::rbindlist(all_summary, fill = TRUE)
legs_dt    <- data.table::rbindlist(all_legs, fill = TRUE)

readr::write_csv(summary_dt, "data/output/_transitous_test_summary.csv")
readr::write_csv(legs_dt,    "data/output/_transitous_test_legs.csv")

n_found <- sum(!is.na(summary_dt$duration_min))
log_msg(sprintf("Done: %d / %d pairs returned at least one itinerary.",
                length(unique(summary_dt$pair_id[!is.na(summary_dt$duration_min)])), nrow(sample_pairs)))
log_msg("Wrote data/output/_transitous_test_summary.csv and _transitous_test_legs.csv")
