# OEV/FR_CH/test: test of the CH-FR rail routing on a 100-tourist sample

Scripts to check the r5r network built by `../03_pt_r5r_ch_fr_build_network.R`.
Run each from the project root with `source("Scripts/02_travel_time/OEV/FR_CH/test/<script>.R")`.
All outputs go to `data/test/CH_FR` (`oev_ch_fr.test_dir` in `Scripts/config.yml`).

Departure date, time and window come from `pt:` in `config.yml`. They are never
changed automatically; the date must lie inside the validity period of all GTFS feeds
(`04_` checks this).

| Order | Script | What it does | Main output |
|---|---|---|---|
| 1 | `04_pt_r5r_ch_fr_route_sample.R` | draws 100 French tourists (SEED 42) with their nearest rail station, routes them to the destination with r5r, adds trip details (legs, waits) | `pt_ch_fr_sample_tourists/times/trips/legs.csv` |
| 2 | `cff_check_travel_time.R` | asks the Swiss journey planner (OJP, API key `OJP_API_KEY` in `~/.Renviron`) for the same trips and compares | `ojp_trips.csv`, `ojp_legs.csv`, `comparison_r5r_vs_ojp.csv` |
| 3 | `05_compare_windows.R` | compares two runs of `04_` made with different windows (e.g. 2 h vs 8 h) | `compare_windows_<A>_vs_<B>.csv` |
| 4 | `06_trip_diagnostics.R` | flags trips where matrix time and itinerary disagree, long initial waits and long first walks | `trip_diagnostics.csv` |
| 5 | `07_gtfs_service_check.R` | lists direct trains between two stations on the routing date (e.g. to check a suspicious walk) | `gtfs_service_check.csv` |

## Replicating the 2 h vs 8 h comparison

1. `pt.time_window_minutes: 120` in `config.yml`; run `04_`; copy `pt_ch_fr_sample_*.csv` to `data/test/CH_FR/run_window_2h/`.
2. `pt.time_window_minutes: 480`; run `04_` (about 30 min); copy the files to `data/test/CH_FR/run_window_8h/`.
3. Set the window back to its normal value and run `05_compare_windows.R`.

`cff_check_travel_time.R` uses the window of `config.yml` for the OJP query too
(10 connections requested, enough for about 2 h). A longer window needs a larger
`N_RESULTS` or paged queries.

## Things to know when reading the results

* `comparable_to_google_min` in the trips file = matrix time - initial wait from the
  itinerary. When matrix and itinerary describe different trips (flagged by `06_`),
  this value is wrong.
* r5r has no local transit in cities like Lyon: the walk between two stations of a
  city can be very long, and OJP may not list some French domestic trains.
* A short window truncates the best option at its last minute (window-edge effect).
