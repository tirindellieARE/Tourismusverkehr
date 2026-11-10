# ============================================================
# 05_compare_windows.R
# Compares two runs of 04_pt_r5r_ch_fr_route_sample.R made with different
# departure windows (e.g. 120 vs 480 minutes) for the same 100-tourist sample.
#
# HOW TO MAKE THE TWO RUNS (04_ always writes into data/test/CH_FR):
#   1. set pt$time_window_minutes in config.yml (e.g. 120), run 04_...R, then copy
#      the pt_ch_fr_sample_*.csv files into data/test/CH_FR/run_window_2h/
#   2. set the window to 480, run 04_...R again, copy the files into
#      data/test/CH_FR/run_window_8h/
#   3. set the window in config.yml back to its normal value.
#
# INPUT   <test_dir>/<RUN_A>/pt_ch_fr_sample_times.csv, _trips.csv
#         <test_dir>/<RUN_B>/pt_ch_fr_sample_times.csv, _trips.csv
# OUTPUT  <test_dir>/compare_windows_<RUN_A>_vs_<RUN_B>.csv  (one row per tourist)
#         and a printed summary (mean/median, number of tourists that change,
#         the largest changes)
#
# A longer window can only add departure options, so the fastest time of the
# longer run should never be larger. Tourists whose best departure in the short
# run was its last minute are upper bounds there (window-edge effect).
#
# Run from the project root:
#   source("Scripts/02_travel_time/OEV/FR_CH/test/05_compare_windows.R")
# ============================================================

source("Scripts/00_setup.R")   # provides cfg, log_msg

# --- Settings ----------------------------------------------------------------
TEST_DIR  <- cfg$oev_ch_fr$test_dir
RUN_A     <- "run_window_2h"      # short window
RUN_B     <- "run_window_8h"      # long window
THRESHOLD <- 5                    # minutes: a change larger than this counts as "different"
OUT_CSV   <- file.path(TEST_DIR, sprintf("compare_windows_%s_vs_%s.csv", RUN_A, RUN_B))

# --- Read ----------------------------------------------------------------------
read_run <- function(run) {
  d <- file.path(TEST_DIR, run)
  tm <- readr::read_csv(file.path(d, "pt_ch_fr_sample_times.csv"), show_col_types = FALSE)
  tr <- readr::read_csv(file.path(d, "pt_ch_fr_sample_trips.csv"), show_col_types = FALSE,
                        col_types = readr::cols(.default = readr::col_guess(), depart_at = "c",
                                                arrive_at = "c", first_vehicle_at = "c"))
  tm |>
    dplyr::select(tourist_id, nearest_stop_name, median_min = pt_time_min, fastest_min = pt_time_fastest_min) |>
    dplyr::left_join(tr |> dplyr::select(tourist_id, trip_time_min, depart_at, initial_wait_min,
                                         comparable_min = comparable_to_google_min), by = "tourist_id")
}
a <- read_run(RUN_A)
b <- read_run(RUN_B)
cmp <- dplyr::inner_join(a, b, by = c("tourist_id", "nearest_stop_name"), suffix = c("_A", "_B")) |>
  dplyr::mutate(change_fastest_min = fastest_min_B - fastest_min_A)

# --- Summary ---------------------------------------------------------------------
st <- function(v) c(n = sum(!is.na(v)), mean = round(mean(v, na.rm = TRUE), 1), median = median(v, na.rm = TRUE))
log_msg("Routed in A: ", sum(!is.na(a$fastest_min)), " | in B: ", sum(!is.na(b$fastest_min)),
        " | only in A: ", paste(setdiff(a$tourist_id[!is.na(a$fastest_min)], b$tourist_id[!is.na(b$fastest_min)]), collapse = ","),
        " | only in B: ", paste(setdiff(b$tourist_id[!is.na(b$fastest_min)], a$tourist_id[!is.na(a$fastest_min)]), collapse = ","))
both <- cmp[!is.na(cmp$fastest_min_A) & !is.na(cmp$fastest_min_B), ]
cat("\nFastest time (1st percentile), tourists routed in both runs:\n")
print(rbind(A = st(both$fastest_min_A), B = st(both$fastest_min_B)))
cat("\nMedian time (50th percentile):\n")
print(rbind(A = st(both$median_min_A), B = st(both$median_min_B)))
cat(sprintf("\nChange B - A (min): shorter by > %d: %d | within +-%d: %d | longer by > %d: %d\n",
            THRESHOLD, sum(both$change_fastest_min < -THRESHOLD), THRESHOLD, sum(abs(both$change_fastest_min) <= THRESHOLD),
            THRESHOLD, sum(both$change_fastest_min > THRESHOLD)))
cat("\nTourists with a change larger than the threshold:\n")
print(as.data.frame(both[abs(both$change_fastest_min) > THRESHOLD, ] |>
                      dplyr::arrange(change_fastest_min) |>
                      dplyr::select(tourist_id, nearest_stop_name, fastest_min_A, fastest_min_B,
                                    change_fastest_min, depart_at_A, depart_at_B)))

readr::write_csv(cmp, OUT_CSV)
log_msg("Wrote ", OUT_CSV)
log_msg("05_compare_windows.R done.")
