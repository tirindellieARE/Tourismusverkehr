# ============================================================
# 04_pt_r5r.R
# Public-transport travel time origin -> destination for every unique pair,
# via r5r (the R5 engine). Off-peak departure, median over a time window.
#
# TILED: R5 enforces a hard limit of 975,000 km^2 on the geographic extent
# of the street layer -- it's built for city/regional-scale analysis, not
# continental. A first attempt using ONE network for the whole bbox (all of
# Western/Central Europe) spent ~2 hours genuinely parsing OSM+GTFS data,
# then failed at the very last step with:
#   "Geographic extent of street layer exceeds limit of 975,000 km^2."
# No amount of extra JVM heap fixes this -- it's a deterministic rejection
# based on area, not a resource limit.
#
# Fix: split the routing pairs into tiles by recursively bisecting the
# ORIGIN points (median split on whichever axis -- lon or lat -- has more
# spread) until each tile's bounding box -- origin extent UNION the fixed
# CH-side destination extent, since every pair routes there -- is safely
# under the cap (900,000 km^2, leaving margin under the 975,000 hard limit).
# Each tile gets its own clipped .osm.pbf (via osmium, from the already-built
# merged.osm.pbf) and its own R5 network.
#
# GTFS feeds are filtered PER TILE, not blindly included in every one: CH is
# always included (every pair's destination is in/near Switzerland), and
# DE/FR/IT/AT are included only if that tile's bbox actually overlaps that
# country's (generous, approximate) bounding box. This matters a lot in
# practice -- the CH feed alone is 235MB, and a first attempt that included
# all 5 feeds in every tile took well over an hour on a SINGLE tile before
# failing (on an unrelated bad GTFS zip -- see below), which would not have
# scaled across 15 tiles. Filtering keeps each tile's GTFS parsing cost
# roughly proportional to what that tile actually needs.
#
# One feed (the manually-downloaded ÖBB/`at.gtfs.zip`) originally shipped
# with its files nested inside a `GTFS_Fahrplan_2026/` subfolder instead of
# at the zip root, which R5's strict GTFS parser rejects outright
# ("TableInSubdirectoryError"). Fixed by re-zipping with files at root (and
# dropping the unused 666MB shapes.txt, not needed for travel-time routing).
#
# THE ACTUAL DOMINANT COST PER TILE, discovered via a `jstack` thread dump
# after a tile that "looked hung" for 90+ minutes with flat memory and no
# log output: the main thread was RUNNABLE the entire time (not deadlocked),
# stuck in `MappedByteBuffer.force()` -> `StoreDirect.close()` ->
# `com.conveyal.osmlib.OSM.close()` -- i.e. flushing R5's MapDB-backed OSM
# store (a *memory-mapped file*, 12GB+ for a ~870,000 km^2 tile) to disk when
# the network build finishes. This is a well-known Windows pathology:
# MappedByteBuffer.force() on very large mapped files is dramatically slower
# on Windows than Linux. It is NOT proportional to GTFS feed size (dropping
# a feed changed nothing) -- it's proportional to the clipped .osm.pbf size.
# AREA_CAP_KM2 below is now tuned for THIS (smaller tiles -> smaller MapDB
# file -> tractable flush time), not primarily for the 975,000 km^2 R5 limit.
# If re-tuning: use `jstack <pid>` on a slow-looking tile before assuming a
# hang and killing it -- RUNNABLE + climbing cpu= time means it's working.
#
# Build -> route -> teardown, one tile at a time, so only one tile's network
# is ever in memory.
#
# Tiles with fewer than MIN_TILE_PAIRS pairs are skipped entirely (PT time
# left NA) rather than given a dedicated network build -- verified this
# covers only extreme geographic outliers (isolated origins in far southern
# Italy): at the current cap, ~10 such tiles account for ~16 of 11,729 trips
# (0.14%). Car times (03_car_osrm.R) already cover these; only PT is
# affected. Exact counts are computed and logged at runtime, not hardcoded.
#
# IMPORTANT: JVM memory MUST be set BEFORE library(r5r). This script does
# that via options(java.parameters=...). Do not load r5r earlier.
#
# Run in a FRESH R session:
#   source("Scripts/02_travel_time/00_setup.R")  # for cfg + helpers (does NOT load r5r)
#   source("Scripts/02_travel_time/04_pt_r5r.R")
# ============================================================

# --- Set JVM heap BEFORE anything loads r5r ---------------------------------
# Read config minimally first (00_setup also reads it, but we need memory now).
.cfg0 <- yaml::read_yaml("Scripts/02_travel_time/config.yml")
options(java.parameters = paste0("-Xmx", .cfg0$java_max_mem))

source("Scripts/02_travel_time/00_setup.R")   # loads cfg, helpers (r5r intentionally not in .pkgs)

# --- Load r5r now that heap is set ------------------------------------------
if (!requireNamespace("r5r", quietly = TRUE)) {
  install.packages("r5r", repos = "https://cloud.r-project.org")
}
library(r5r)

# --- Inputs -------------------------------------------------------------
if (!file.exists(cfg$out$pairs)) stop("Run 01_filter.R first.")
pairs <- readr::read_csv(cfg$out$pairs, show_col_types = FALSE)
n <- nrow(pairs)

merged_pbf <- "data/osm/merged.osm.pbf"
if (!file.exists(merged_pbf)) stop("data/osm/merged.osm.pbf missing. Run 02_download.R first.")

osmium_ok <- nzchar(Sys.which("osmium"))
if (!osmium_ok) stop("osmium not found on PATH (needed to clip per-tile networks).")

gtfs_zips <- list.files("data/gtfs", pattern = "\\.zip$", full.names = TRUE)
if (length(gtfs_zips) == 0) {
  stop("No GTFS zips found in data/gtfs/. Add feeds (see 02_download.R) first.")
}
log_msg(sprintf("Found %d GTFS feed(s): %s", length(gtfs_zips), paste(basename(gtfs_zips), collapse = ", ")))

# --- Which country corridor did each pair's origin actually use? -----------
# A first attempt selected feeds by testing tile-bbox overlap against rough
# country bounding boxes -- geometrically weak here, because every tile's
# bbox always includes the CH destination area, and Switzerland sits tightly
# wedged against DE/IT/AT, so nearly every tile's bbox grazed all of their
# corners regardless of where the origins actually were. Using the survey's
# own border-crossing corridor (GRENZABSCHNITT, joined via grenz_id already
# in agqpv.csv) is precise instead of geometric guessing.
agqpv_min <- readr::read_csv(cfg$input_csv, locale = readr::locale(encoding = cfg$input_encoding),
                              show_col_types = FALSE) |>
  dplyr::select(grenz_id, origin_lat, origin_long, dest_lat, dest_long)

raw_corridor <- readr::read_delim(
  "data/input/Finale_Auswertungsdatenbank_AGQPV2015_V2.csv", delim = ",",
  locale = readr::locale(encoding = "latin1"), show_col_types = FALSE,
  col_select = c(BEFRAGUNGSORTID, GRENZABSCHNITT)
) |>
  dplyr::distinct(BEFRAGUNGSORTID, .keep_all = TRUE) |>
  dplyr::rename(grenz_id = BEFRAGUNGSORTID, corridor = GRENZABSCHNITT)

agqpv_min <- agqpv_min |>
  dplyr::left_join(raw_corridor, by = "grenz_id") |>
  dplyr::mutate(pkey = make_pair_key(origin_lat, origin_long, dest_lat, dest_long))

# majority corridor per pair-key (a handful of pkeys have >1 distinct
# corridor across the trips sharing that exact origin/destination; rare,
# verified earlier at 112 / 7726)
pair_corridor <- agqpv_min |>
  dplyr::count(pkey, corridor, name = "n_") |>
  dplyr::arrange(dplyr::desc(n_)) |>
  dplyr::distinct(pkey, .keep_all = TRUE) |>
  dplyr::select(pkey, corridor)

pairs$pkey <- make_pair_key(pairs$origin_lat, pairs$origin_long, pairs$dest_lat, pairs$dest_long)
pairs <- dplyr::left_join(pairs, pair_corridor, by = "pkey")
pairs$corridor[is.na(pairs$corridor) | pairs$corridor == ""] <- "OTHER"  # Alpine-pass survey sites with no country tag
log_msg(sprintf("Pair corridors: %s",
                paste(names(table(pairs$corridor)), table(pairs$corridor), sep = "=", collapse = ", ")))

CORRIDOR_TO_COUNTRY <- c(DE = "de", FR = "fr", IT = "it", AT = "at")
feeds_for_tile <- function(idx, gtfs_zips) {
  corridors <- unique(pairs$corridor[idx])
  if ("OTHER" %in% corridors) {
    # OTHER = internal Alpine-pass survey sites; origin country isn't
    # recoverable from the label alone, so stay conservative and include
    # every neighbour's feed for this tile rather than risk dropping real
    # transit coverage.
    keep_names <- c("ch", "de", "fr", "it", "at")
  } else {
    keep_names <- unique(c("ch", unname(CORRIDOR_TO_COUNTRY[corridors])))
  }
  gtfs_zips[tolower(tools::file_path_sans_ext(basename(gtfs_zips))) %in% paste0(keep_names, ".gtfs")]
}

# --- Recursive tiling ---------------------------------------------------
dest_xmin <- min(pairs$dest_long); dest_xmax <- max(pairs$dest_long)
dest_ymin <- min(pairs$dest_lat);  dest_ymax <- max(pairs$dest_lat)
BBOX_PAD <- cfg$bbox_pad_deg
AREA_CAP_KM2 <- 400000   # NOT primarily about the 975,000 km^2 hard limit anymore -- see
                          # header note on the Windows MappedByteBuffer.force() bottleneck.
                          # Smaller tiles -> smaller clipped .osm.pbf -> smaller MapDB file to
                          # flush on network close, which is the actual dominant cost per tile.
MIN_TILE_PAIRS <- 6      # tiles smaller than this are skipped (PT = NA) -- see header

area_km2 <- function(xmin, ymin, xmax, ymax) {
  midlat <- (ymin + ymax) / 2
  (xmax - xmin) * 111.32 * cos(midlat * pi / 180) * (ymax - ymin) * 111.32
}
tile_bbox <- function(idx) {
  c(xmin = min(pairs$origin_long[idx], dest_xmin) - BBOX_PAD,
    ymin = min(pairs$origin_lat[idx],  dest_ymin) - BBOX_PAD,
    xmax = max(pairs$origin_long[idx], dest_xmax) + BBOX_PAD,
    ymax = max(pairs$origin_lat[idx],  dest_ymax) + BBOX_PAD)
}
split_tile <- function(idx) {
  bb <- tile_bbox(idx)
  a <- area_km2(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
  if (a <= AREA_CAP_KM2 || length(idx) <= 1) return(list(list(idx = idx, bbox = bb, area = a)))
  lon_spread <- diff(range(pairs$origin_long[idx]))
  lat_spread <- diff(range(pairs$origin_lat[idx]))
  if (lon_spread >= lat_spread) {
    med <- median(pairs$origin_long[idx])
    left <- idx[pairs$origin_long[idx] <= med]; right <- idx[pairs$origin_long[idx] > med]
  } else {
    med <- median(pairs$origin_lat[idx])
    left <- idx[pairs$origin_lat[idx] <= med]; right <- idx[pairs$origin_lat[idx] > med]
  }
  if (length(left) == 0 || length(right) == 0) return(list(list(idx = idx, bbox = bb, area = a)))
  c(split_tile(left), split_tile(right))
}

all_tiles <- split_tile(seq_len(n))
tile_sizes <- sapply(all_tiles, function(t) length(t$idx))
real_tiles <- Filter(function(t) length(t$idx) >= MIN_TILE_PAIRS, all_tiles)
skipped_pairs <- n - sum(sapply(real_tiles, function(t) length(t$idx)))
skipped_trips <- sum(pairs$n_rows) - sum(pairs$n_rows[unlist(lapply(real_tiles, function(t) t$idx))])

log_msg(sprintf("Tiling: %d tile(s) total, %d kept (>= %d pairs), %d skipped (PT = NA: %d pairs / %d trips)",
                length(all_tiles), length(real_tiles), MIN_TILE_PAIRS,
                length(all_tiles) - length(real_tiles), skipped_pairs, skipped_trips))

# --- Departure datetime ------------------------------------------------------
departure <- as.POSIXct(
  paste(cfg$pt$departure_date, cfg$pt$departure_time),
  format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Zurich"
)
log_msg(sprintf("PT departure (off-peak): %s, window %d min, percentile %d",
                format(departure), cfg$pt$time_window_minutes, cfg$pt$percentile))

route_one <- function(r5r_core, o, d) {
  ttm <- tryCatch(
    r5r::travel_time_matrix(
      r5r_core,
      origins       = o,
      destinations  = d,
      mode          = cfg$pt$modes,
      departure_datetime = departure,
      time_window        = cfg$pt$time_window_minutes,
      percentiles        = cfg$pt$percentile,
      max_trip_duration  = cfg$pt$max_trip_duration_min,
      walk_speed         = cfg$pt$walk_speed_kmh,
      max_walk_time      = cfg$pt$max_walk_time_min,
      verbose = FALSE, progress = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(ttm) || nrow(ttm) == 0) return(NA_real_)
  tt_col <- grep("travel_time", names(ttm), value = TRUE)[1]
  as.numeric(ttm[[tt_col]][1])
}

# --- Build -> route -> teardown, one tile at a time --------------------------
pt_min <- rep(NA_real_, n)
tile_dir_base <- "data/r5_tiles"
ensure_dir(tile_dir_base)

for (ti in seq_along(real_tiles)) {
  t <- real_tiles[[ti]]; idx <- t$idx; bb <- t$bbox
  log_msg(sprintf("--- Tile %d/%d: %d pairs, bbox=[%.2f,%.2f,%.2f,%.2f], area=%.0f km^2 ---",
                  ti, length(real_tiles), length(idx), bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"], t$area))

  tile_dir <- file.path(tile_dir_base, sprintf("tile_%02d", ti))
  ensure_dir(tile_dir)

  tile_pbf <- file.path(tile_dir, "network.osm.pbf")
  if (!file.exists(tile_pbf)) {
    bbox_str <- sprintf("%f,%f,%f,%f", bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    system2("osmium", c("extract", "-b", bbox_str,
                        shQuote(merged_pbf), "-o", shQuote(tile_pbf), "--overwrite"))
  }

  tile_feeds <- feeds_for_tile(idx, gtfs_zips)
  log_msg(sprintf("  GTFS feeds for this tile: %s", paste(basename(tile_feeds), collapse = ", ")))
  # remove any stale feeds left over from a previous run of this tile dir
  file.remove(setdiff(list.files(tile_dir, pattern = "\\.zip$", full.names = TRUE), tile_feeds))
  for (z in tile_feeds) file.copy(z, file.path(tile_dir, basename(z)), overwrite = TRUE)

  r5r_core <- r5r::setup_r5(data_path = tile_dir, verbose = FALSE)

  o_all <- data.frame(id = pairs$pair_id[idx], lat = pairs$origin_lat[idx], lon = pairs$origin_long[idx])
  d_all <- data.frame(id = pairs$pair_id[idx], lat = pairs$dest_lat[idx],   lon = pairs$dest_long[idx])
  for (i in seq_along(idx)) {
    pt_min[idx[i]] <- route_one(r5r_core, o_all[i, , drop = FALSE], d_all[i, , drop = FALSE])
  }
  log_msg(sprintf("  tile %d done: %d / %d pairs got a PT time", ti, sum(!is.na(pt_min[idx])), length(idx)))

  r5r::stop_r5(r5r_core)
  rJava::.jgc(R.gc = TRUE)
}

pt_times <- pairs |>
  dplyr::select(pair_id, origin_lat, origin_long, dest_lat, dest_long) |>
  dplyr::mutate(pt_time_min = pt_min)

readr::write_csv(pt_times, cfg$out$pt_times)
log_msg(sprintf("Wrote %s | missing PT times: %d / %d (%d from skipped tiles, rest = no route found)",
                cfg$out$pt_times, sum(is.na(pt_min)), n, skipped_pairs))
log_msg("04_pt_r5r.R done.")
