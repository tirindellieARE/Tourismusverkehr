# ============================================================
# 06_trip_diagnostics.R
# Quality checks of one run of 04_pt_r5r_ch_fr_route_sample.R (trips + legs).
#
# 04_ finds the departure minute with the lowest travel time using
# travel_time_matrix() (a total time only) and then asks detailed_itineraries()
# for the legs of that minute. Both should describe the same trip. This script
# flags the trips where they do not, and lists long first walks.
#
#   gap_min             = itinerary total (legs) - matrix time. |gap| > 10 min means the
#                         two functions describe different trips; the initial wait and the
#                         legs of such a trip cannot be trusted (and "comparable to OJP" =
#                         matrix time - initial wait mixes two trips).
#   long_wait_midwindow = initial wait before the first vehicle > 10 min although the chosen
#                         departure is not the last minute of the window. With departures
#                         scanned every 5 min the wait should be short; a long wait at the
#                         last minute is legitimate (no train leaves soon enough).
#   first_walk_min      = duration of the first leg when it is a walk (long walks between
#                         two stations of one city or to a distant station).
#
# INPUT   <test_dir>/<RUN>/pt_ch_fr_sample_trips.csv, pt_ch_fr_sample_legs.csv
#         (RUN = "" uses <test_dir> itself, i.e. the latest run of 04_)
# OUTPUT  <test_dir>/<RUN>/trip_diagnostics.csv (one row per routed tourist) + printed summary
#
# Run from the project root:
#   source("Scripts/02_travel_time/OEV/FR_CH/test/06_trip_diagnostics.R")
# ============================================================

source("Scripts/00_setup.R")   # provides cfg, log_msg

# --- Settings ----------------------------------------------------------------
TEST_DIR   <- cfg$oev_ch_fr$test_dir
RUN        <- ""                                        # e.g. "run_window_8h"; "" = latest run in TEST_DIR
WINDOW_MIN <- cfg$pt$time_window_minutes                # set to the window of the run examined (120 / 480)
STEP_MIN   <- 5                                         # departure step used by 04_ (last scanned minute = window - STEP_MIN)
GAP_MIN    <- 10                                        # |matrix - itinerary| flagged above this
WAIT_MIN   <- 10                                        # initial wait flagged above this
WALK_MIN   <- 5                                         # first walks listed above this
RUN_DIR    <- file.path(TEST_DIR, RUN)
OUT_CSV    <- file.path(RUN_DIR, "trip_diagnostics.csv")

# --- Read ----------------------------------------------------------------------
tr <- readr::read_csv(file.path(RUN_DIR, "pt_ch_fr_sample_trips.csv"), show_col_types = FALSE,
                      col_types = readr::cols(.default = readr::col_guess(), depart_at = "c",
                                              arrive_at = "c", first_vehicle_at = "c"))
lg <- readr::read_csv(file.path(RUN_DIR, "pt_ch_fr_sample_legs.csv"), show_col_types = FALSE)

legs_total <- lg |> dplyr::group_by(tourist_id) |>
  dplyr::summarise(legs_total_min = max(total_duration, na.rm = TRUE), .groups = "drop")
first_leg <- lg |> dplyr::group_by(tourist_id) |> dplyr::arrange(segment, .by_group = TRUE) |>
  dplyr::slice(1) |> dplyr::ungroup() |>
  dplyr::transmute(tourist_id, first_leg_mode = mode, first_walk_min = ifelse(mode == "WALK", segment_duration, NA_real_),
                   first_walk_from = from_stop, first_walk_to = to_stop)

win_start <- as.POSIXct(paste(cfg$pt$departure_date, cfg$pt$departure_time), format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Zurich")
last_dep  <- format(win_start + (WINDOW_MIN - STEP_MIN) * 60, "%Y-%m-%d %H:%M")

d <- tr |>
  dplyr::select(tourist_id, from_station, depart_at, trip_time_min, initial_wait_min, n_rides, first_vehicle_at, routes) |>
  dplyr::left_join(legs_total, by = "tourist_id") |>
  dplyr::left_join(first_leg, by = "tourist_id") |>
  dplyr::mutate(gap_min = round(legs_total_min - trip_time_min, 1),
                flag_inconsistent = !is.na(gap_min) & abs(gap_min) > GAP_MIN,
                at_window_end = depart_at == last_dep,
                long_wait_midwindow = !is.na(initial_wait_min) & initial_wait_min > WAIT_MIN & !at_window_end)

# --- Summary ---------------------------------------------------------------------
cat(sprintf("\nTrips with legs: %d (last scanned departure of the window: %s)\n", sum(!is.na(d$legs_total_min)), last_dep))
cat("\nInitial wait before the first vehicle (min):\n"); print(summary(d$initial_wait_min))
cat(sprintf("<= 5: %d | 5-10: %d | > 10: %d (of which at the window end: %d)\n",
            sum(d$initial_wait_min <= 5, na.rm = TRUE), sum(d$initial_wait_min > 5 & d$initial_wait_min <= 10, na.rm = TRUE),
            sum(d$initial_wait_min > WAIT_MIN, na.rm = TRUE), sum(d$initial_wait_min > WAIT_MIN & d$at_window_end, na.rm = TRUE)))
cat(sprintf("\nMatrix vs itinerary differ by > %d min: %d trips\n", GAP_MIN, sum(d$flag_inconsistent, na.rm = TRUE)))
print(as.data.frame(d[d$flag_inconsistent %in% TRUE, c("tourist_id", "from_station", "depart_at", "trip_time_min", "legs_total_min", "gap_min", "initial_wait_min")]))
cat(sprintf("\nFirst leg is a walk longer than %d min:\n", WALK_MIN))
print(as.data.frame(d[d$first_walk_min > WALK_MIN & !is.na(d$first_walk_min), ] |>
                      dplyr::arrange(dplyr::desc(first_walk_min)) |>
                      dplyr::select(tourist_id, from_station, first_walk_from, first_walk_to, first_walk_min)))

readr::write_csv(d, OUT_CSV)
log_msg("Wrote ", OUT_CSV)
log_msg("06_trip_diagnostics.R done.")
