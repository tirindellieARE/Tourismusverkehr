# ============================================================
# 02_download.R
# 1. Download Geofabrik country extracts listed in config.
# 2. Merge them and clip to a bbox around all routing points.
# 3. Download the auto-fetchable GTFS feeds.
#
# Requires the osmium command-line tool on PATH.
#   In your conda env:  conda install -c conda-forge osmium-tool
#
# Run:  source("Scripts/02_travel_time/00_setup.R"); source("Scripts/02_travel_time/02_download.R")
# ============================================================
user = "MR"
if(user == "MR"){setwd("E:/ARE/ProjekteTIE/Turismusverkehr/RScript/tourists")}
if(user == "CP"){setwd("P:/Verkehrsmodellierung/06_Jobs/188_touristische_Verkehr/Model_trafic_touristique/GitRepo/tourists")}

source("Scripts/02_travel_time/00_setup.R")

# --- Check osmium is available ----------------------------------------------
osmium_ok <- nzchar(Sys.which("osmium"))
if (!osmium_ok) {
  stop("osmium not found on PATH. Install with:\n",
       "  conda install -c conda-forge osmium-tool\n",
       "and make sure you launched R from the activated conda env.")
}

# --- Download country extracts (skip if present) -----------------------------
# Large extracts (Germany ~4.8GB) need a generous timeout -- R's default (60s)
# fails long before a multi-GB file finishes even on a fast connection.
options(timeout = max(1800, getOption("timeout")))

osm_dir <- "data/osm"
country_files <- character(0)
for (c in cfg$osm_countries) {
  dest <- file.path(osm_dir, paste0(c$name, "-latest.osm.pbf"))
  if (!file.exists(dest)) {
    log_msg(sprintf("Downloading OSM extract: %s", c$name))
    tryCatch(
      utils::download.file(c$url, dest, mode = "wb", quiet = TRUE),
      error = function(e) {
        if (file.exists(dest)) file.remove(dest)  # don't leave a corrupt partial file behind
        stop(sprintf("Failed to download %s: %s", c$name, conditionMessage(e)))
      }
    )
  } else {
    log_msg(sprintf("OSM extract already present: %s", c$name))
  }
  country_files <- c(country_files, dest)
}

# --- Compute bbox from the routing points (pairs) ----------------------------
if (!file.exists(cfg$out$pairs)) {
  stop("pairs_to_route.csv not found. Run 01_filter.R first.")
}
pairs <- readr::read_csv(cfg$out$pairs, show_col_types = FALSE)
pad <- cfg$bbox_pad_deg
xmin <- min(pairs$origin_long, pairs$dest_long) - pad
xmax <- max(pairs$origin_long, pairs$dest_long) + pad
ymin <- min(pairs$origin_lat,  pairs$dest_lat)  - pad
ymax <- max(pairs$origin_lat,  pairs$dest_lat)  + pad
log_msg(sprintf("Clip bbox: %.4f,%.4f,%.4f,%.4f (W,S,E,N)", xmin, ymin, xmax, ymax))

# --- Merge country extracts --------------------------------------------------
merged <- file.path(osm_dir, "merged.osm.pbf")
if (!file.exists(merged)) {
  log_msg("Merging country extracts (this can take several minutes) ...")
  system2("osmium", c("merge", shQuote(country_files), "-o", shQuote(merged), "--overwrite"))
}

# --- Clip to bbox ------------------------------------------------------------
clipped <- file.path(osm_dir, "network.osm.pbf")
if (!file.exists(clipped)) {
  log_msg("Clipping merged extract to bbox ...")
  bbox_str <- sprintf("%f,%f,%f,%f", xmin, ymin, xmax, ymax)
  system2("osmium", c("extract", "-b", bbox_str,
                      shQuote(merged), "-o", shQuote(clipped), "--overwrite"))
}
log_msg(sprintf("Network OSM ready: %s", clipped))

# --- Download auto-fetchable GTFS feeds --------------------------------------
gtfs_dir <- "data/gtfs"
for (f in cfg$gtfs_feeds) {
  if (isTRUE(f$auto) && nzchar(f$url)) {
    dest <- file.path(gtfs_dir, paste0(f$name, ".gtfs.zip"))
    if (!file.exists(dest)) {
      log_msg(sprintf("Downloading GTFS feed: %s", f$name))
      tryCatch(
        utils::download.file(f$url, dest, mode = "wb", quiet = TRUE),
        error = function(e)
          log_msg(sprintf("  WARNING: failed to download %s (%s). Fetch manually.",
                          f$name, conditionMessage(e)))
      )
    } else {
      log_msg(sprintf("GTFS feed already present: %s", f$name))
    }
  } else {
    log_msg(sprintf("GTFS feed '%s' needs MANUAL download (set url/auto in config).",
                    f$name))
  }
}

log_msg("02_download.R done. Place any manual GTFS zips in data/gtfs/ before 04_pt_r5r.R.")
