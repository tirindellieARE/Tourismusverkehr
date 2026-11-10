# ============================================================
# 02_clean_CH_gtfs.R
# Fixes the frequencies in the Swiss GTFS feed so that it can be used in r5r:
# frequency-based trips are expanded into explicit stop_times, leaving the feed
# without a 'frequencies' table.
#
# Input : data/input/gtfs/ch.gtfs.zip              (raw Swiss national feed)
# Output: data/output/gtfs/ch.freqfixed.gtfs.zip   (used by 03_pt_r5r_ch_fr_build_network.R)
#
# Skipped if the output already exists -- delete it to force a rebuild.
# After changing the feed, rebuild the r5 network with BUILD_NETWORK <- TRUE in
# 03_pt_r5r_ch_fr_build_network.R.
#
# The before/after routing test of this fix is archived in
# Scripts/02_travel_time/butta/pt_r5r_rail_ch_fr_fixed_test.R.
#
# Run from the project root:
#   source("Scripts/02_travel_time/OEV/FR_CH/02_clean_CH_gtfs.R")
# ============================================================

source("Scripts/00_setup.R")   # sets the project root; provides cfg, log_msg, ensure_dir
if (!requireNamespace("gtfstools", quietly = TRUE)) install.packages("gtfstools", repos = "https://cloud.r-project.org")

RAW_CH_GTFS   <- "data/input/gtfs/ch.gtfs.zip"
FIXED_CH_GTFS <- "data/output/gtfs/ch.freqfixed.gtfs.zip"
ensure_dir(dirname(FIXED_CH_GTFS))

if (!file.exists(RAW_CH_GTFS)) stop("Missing ", RAW_CH_GTFS)

if (file.exists(FIXED_CH_GTFS)) {
  log_msg(sprintf("%s already exists -- nothing to do (delete it to rebuild).", FIXED_CH_GTFS))
} else {
  log_msg(sprintf("Reading %s via gtfstools...", RAW_CH_GTFS))
  gtfs <- gtfstools::read_gtfs(RAW_CH_GTFS)
  n_freq <- if (!is.null(gtfs$frequencies)) nrow(gtfs$frequencies) else 0
  log_msg(sprintf("frequencies.txt rows: %d (%d unique trips)", n_freq,
                  if (n_freq > 0) length(unique(gtfs$frequencies$trip_id)) else 0))

  log_msg("Expanding frequency-based trips into explicit stop_times (gtfstools::frequencies_to_stop_times)...")
  t0 <- Sys.time()
  gtfs_fixed <- gtfstools::frequencies_to_stop_times(gtfs)
  log_msg(sprintf("Expansion done in %.1f sec. frequencies table now: %s",
                  as.numeric(Sys.time() - t0, units = "secs"),
                  if (is.null(gtfs_fixed$frequencies) || nrow(gtfs_fixed$frequencies) == 0) "empty/absent"
                  else sprintf("%d rows (!)", nrow(gtfs_fixed$frequencies))))

  log_msg(sprintf("Writing fixed feed to %s...", FIXED_CH_GTFS))
  gtfstools::write_gtfs(gtfs_fixed, FIXED_CH_GTFS)
}

log_msg("02_clean_CH_gtfs.R done.")
