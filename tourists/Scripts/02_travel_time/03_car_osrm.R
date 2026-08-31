# ============================================================
# 03_car_osrm.R
# Car travel time via a LOCAL OSRM server -- two related but distinct
# datasets, merged into one script because they share all their setup
# (server connection, package loading) even though they route different
# things. NOT one a subset of the other: dataset B's destinations are
# Swiss ZONE CENTROIDS (st_centroid() of the zone polygons), while dataset
# A's destinations are respondents' actual reported destination
# coordinates (real addresses, median ~63km from the border crossing --
# see 01_filter.R's header). Same origins, genuinely different
# destinations, so A's pairs are not literally contained in B's.
#
#   A. car_times_agqpv.csv        -- every unique origin -> destination
#      pair that actually occurs in agqpv.csv (real respondent trips).
#      Enough for a mode-choice logsum on observed trips.
#   B. car_times_ausland_CH.fst   -- every unique ausland origin (deduped
#      from agqpv.csv) -> every Swiss zone centroid. Needed for a
#      destination-choice model, which requires a time to every CANDIDATE
#      zone, not just the one each respondent visited. Mirrors
#      06_build_tt_lookups.R's tt_ausland_CH.fst (built from the OMX
#      demand-model matrices) but via real OSRM road routing.
#
# -------- Starting a local OSRM server (one-time, outside this script) ------
# OSRM runs best as a server. If Docker Desktop is available, the official
# image is the simplest reproducible route (see git history of this file for
# the docker run commands). Where Docker isn't available (e.g. this machine),
# use the R package **osrm.backend**, which downloads real OSRM binaries for
# Windows/macOS/Linux (no Docker, no compiler) and drives
# extract/partition/customize/serve for you:
#
#   install.packages("osrm.backend",
#     repos = c("https://e-kotov.r-universe.dev", "https://cloud.r-project.org"))
#   library(osrm.backend)
#   osrm_install()                                   # one-time binary download
#   osrm_start("data/osm/network.osm.pbf", algorithm = "mld")  # extract+partition+customize+serve
#
# IMPORTANT: osrm_start() launches osrm-routed.exe as a CHILD of the R
# process that called it -- on Windows that child dies when the parent R
# session exits. Run it in a session/terminal you leave open (or a detached
# background process), not in a one-off `Rscript -e ...` that returns
# immediately. Re-running osrm_start() after the graph is already built
# skips straight to starting the server (no re-extraction).
#
# The osrm R package then queries http://127.0.0.1:5001 (osrm.backend's
# default port; set in config.yml).
#
# --- Dataset B specifics -----------------------------------------------
# SCALE: ~3,037 unique ausland origins x 7,966 CH zone centroids =~ 24.2M
# pairs. Routed via OSRM's /table service (osrmTable, many-to-many in one
# call), in nested chunks of `osrm.table_origin_chunk` origins x
# `osrm.table_dest_chunk` zone centroids per call (see config.yml for why
# both axes are chunked, and why a roughly balanced chunk shape is ~4x
# faster per cell than a skewed one) -- NOT a one-by-one osrmRoute loop like
# dataset A uses, which would take far too long at this scale. At the
# measured ~0.15s/1,000 cells this is roughly an hour end to end.
#
# UNIQUENESS / RESUMABILITY for dataset B ("do not compute already-computed
# trips"):
#   - Origins are deduplicated to unique coordinates first (3,037, not
#     11,729 raw agqpv rows) -- the same de-dup pattern 01_filter.R uses for
#     pairs_to_route_agqpv.csv.
#   - Destinations are the 7,966 distinct CH zone centroids -- one per zone,
#     by construction (each row is already one physical zone).
#   - NOT cross-checked against car_times_agqpv.csv: as explained above,
#     that file's destinations are real reported points, not zone
#     centroids, so an exact-coordinate match there is not meaningful.
#   - IS resumable across re-runs: an origin is only routed if it doesn't
#     already have a full row of times (one per CH zone) in the existing
#     output file, checked (and the accumulated result re-written) after
#     every origin-chunk.
#
# OUTPUT (dataset B is normalized, not one wide row per origin -- see
# header of 06_build_tt_lookups.R for the same rationale):
#   data/output/origins_ausland.csv        origin_id, origin_lat, origin_long
#                                           (the 3,037 unique ausland origins)
#   data/output/car_times_ausland_CH.fst   origin_id, zone_no, car_time_min,
#                                           car_dist_km (zone_no joins back to
#                                           NO in data/input/zones_communes.gpkg)
#
# Run:  source("Scripts/02_travel_time/00_setup.R"); source("Scripts/02_travel_time/03_car_osrm.R")
# ============================================================

user = "MR"
if(user == "MR"){setwd("E:/ARE/ProjekteTIE/Turismusverkehr/RScript/tourists")}
if(user == "CP"){setwd("P:/Verkehrsmodellierung/06_Jobs/188_touristische_Verkehr/Model_trafic_touristique/GitRepo/tourists")}

source("Scripts/02_travel_time/00_setup.R")
library(osrm)
if (!requireNamespace("fst", quietly = TRUE)) install.packages("fst", repos = "https://cloud.r-project.org")
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table", repos = "https://cloud.r-project.org")
library(fst)
library(data.table)

options(osrm.server = paste0(cfg$osrm$server, "/"),
        osrm.profile = cfg$osrm$profile)

# ============================================================
# Dataset A: car_times_agqpv.csv -- every unique agqpv origin -> destination
# pair, routed one-by-one (small N: 7,726 pairs).
# ============================================================

if (!file.exists(cfg$out$pairs)) stop("Run 01_filter.R first.")
pairs <- readr::read_csv(cfg$out$pairs, show_col_types = FALSE)
log_msg(sprintf("[A] Routing %d agqpv pairs by car via OSRM at %s",
                nrow(pairs), cfg$osrm$server))

# --- Quick connectivity check (covers both datasets -- same server) --------
test <- tryCatch(
  osrmRoute(src = c(pairs$origin_long[1], pairs$origin_lat[1]),
            dst = c(pairs$dest_long[1],   pairs$dest_lat[1]),
            overview = FALSE),
  error = function(e) {
    stop("Could not reach the OSRM server at ", cfg$osrm$server,
         ".\nStart the server (see header of this script) and retry.\n",
         "Original error: ", conditionMessage(e))
  }
)
log_msg("OSRM server reachable. Beginning dataset A batch.")

# --- Route each pair one-to-one ---------------------------------------------
n <- nrow(pairs)
car_min <- rep(NA_real_, n)
car_km  <- rep(NA_real_, n)

for (i in seq_len(n)) {
  r <- tryCatch(
    osrmRoute(src = c(pairs$origin_long[i], pairs$origin_lat[i]),
              dst = c(pairs$dest_long[i],   pairs$dest_lat[i]),
              overview = FALSE),
    error = function(e) NULL
  )
  if (!is.null(r)) {
    # osrm >= 5.0.0: osrmRoute(overview=FALSE) returns a named numeric vector
    # c(duration=<min>, distance=<km>), not a data.frame/list -- no $ accessor.
    car_min[i] <- as.numeric(r["duration"])
    car_km[i]  <- as.numeric(r["distance"])
  }
  if (i %% 500 == 0) log_msg(sprintf("  [A] car routed %d / %d", i, n))
}

car_times <- pairs |>
  dplyr::select(pair_id, origin_lat, origin_long, dest_lat, dest_long) |>
  dplyr::mutate(car_time_min = car_min, car_dist_km = car_km)

readr::write_csv(car_times, cfg$out$car_times)
log_msg(sprintf("[A] Wrote %s | missing car times: %d / %d",
                cfg$out$car_times, sum(is.na(car_min)), n))

# ============================================================
# Dataset B: car_times_ausland_CH.fst -- every unique ausland origin ->
# every Swiss zone centroid, routed via OSRM's /table service in nested
# chunks (large N: ~24.2M pairs). See header for why this can't reuse
# dataset A's one-by-one loop or its pairs.
# ============================================================

# --- Unique ausland origins ---------------------------------------------
cn <- cfg$cols
raw <- readr::read_delim(cfg$input_csv, delim = ",",
                          locale = readr::locale(encoding = cfg$input_encoding),
                          show_col_types = FALSE)
n_non_ausland <- sum(!raw[[cn$origin_zone_country]] %in% c("Ausland", "LI"))
if (n_non_ausland > 0) {
  log_msg(sprintf("[B] NOTE: %d row(s) with origin_zone_country outside Ausland/LI found; excluded from origins.",
                  n_non_ausland))
}
raw <- raw[raw[[cn$origin_zone_country]] %in% c("Ausland", "LI"), ]

setDT(raw)
origins <- unique(raw[, .(origin_lat = get(cn$origin_lat), origin_long = get(cn$origin_long))])
setorder(origins, origin_lat, origin_long)
origins[, origin_id := sprintf("O%05d", .I - 1L)]
setcolorder(origins, c("origin_id", "origin_lat", "origin_long"))
log_msg(sprintf("[B] Unique ausland origins: %d", nrow(origins)))
readr::write_csv(origins, cfg$out$origins_ausland)

# --- Swiss zone centroids ------------------------------------------------
zones_sf <- sf::st_read("data/input/zones_communes.gpkg", quiet = TRUE)
ch_zones_sf <- zones_sf[zones_sf$MAKROBEZ_STAAT == "CH" & !is.na(zones_sf$MAKROBEZ_STAAT), ]
cen <- sf::st_transform(sf::st_centroid(sf::st_geometry(ch_zones_sf)), cfg$input_crs)
cen_xy <- sf::st_coordinates(cen)
zones <- data.table(zone_no = ch_zones_sf$NO, lon = cen_xy[, "X"], lat = cen_xy[, "Y"])
setkey(zones, zone_no)
n_zones <- nrow(zones)
log_msg(sprintf("[B] Swiss (CH) zone centroids: %d", n_zones))
log_msg("[B] Beginning table batch.")

# --- Resume from a previous partial run -------------------------------------
out_file <- cfg$out$car_times_ausland_ch
result <- if (file.exists(out_file)) read_fst(out_file, as.data.table = TRUE) else
  data.table(origin_id = character(), zone_no = integer(),
             car_time_min = double(), car_dist_km = double())

done_counts <- result[, .N, by = origin_id]
done_ids <- done_counts[N == n_zones, origin_id]   # only fully-routed origins count as done
todo <- origins[!origin_id %in% done_ids]
log_msg(sprintf("[B] %d / %d origins already fully routed (resumed); %d to do.",
                length(done_ids), nrow(origins), nrow(todo)))

# --- Route in nested chunks: table_origin_chunk origins x table_dest_chunk --
# zone centroids per call (see config.yml for why both axes are chunked --
# a single max-table-size limit alone would allow far bigger origin chunks).
o_chunk <- cfg$osrm$table_origin_chunk
d_chunk <- cfg$osrm$table_dest_chunk
n_todo <- nrow(todo)
if (n_todo == 0) {
  log_msg("[B] Nothing to do -- all origins already routed.")
} else {
  n_ochunks <- ceiling(n_todo / o_chunk)
  n_dchunks <- ceiling(n_zones / d_chunk)
  for (oc in seq_len(n_ochunks)) {
    o_idx <- ((oc - 1) * o_chunk + 1):min(oc * o_chunk, n_todo)
    o_batch <- todo[o_idx]
    src_df <- data.frame(lon = o_batch$origin_long, lat = o_batch$origin_lat)

    batch_parts <- vector("list", n_dchunks)
    ok <- TRUE
    for (dc in seq_len(n_dchunks)) {
      d_idx <- ((dc - 1) * d_chunk + 1):min(dc * d_chunk, n_zones)
      d_batch <- zones[d_idx]

      tbl <- tryCatch(
        osrmTable(src = src_df, dst = d_batch[, .(lon, lat)], measure = c("duration", "distance")),
        error = function(e) {
          log_msg(sprintf("  [B] origin-chunk %d/%d dest-chunk %d/%d FAILED: %s",
                          oc, n_ochunks, dc, n_dchunks, conditionMessage(e)))
          NULL
        }
      )
      if (is.null(tbl)) { ok <- FALSE; break }

      batch_parts[[dc]] <- data.table(
        origin_id    = rep(o_batch$origin_id, times = nrow(d_batch)),
        zone_no      = rep(d_batch$zone_no,   each  = nrow(o_batch)),
        car_time_min = as.vector(tbl$durations),
        car_dist_km  = as.vector(tbl$distances) / 1000
      )
    }
    if (!ok) next   # this origin-chunk stays undone; retried on the next run

    batch_res <- rbindlist(batch_parts)
    result <- rbind(result[!origin_id %in% o_batch$origin_id], batch_res)
    write_fst(result, out_file, compress = 100)

    log_msg(sprintf("  [B] origin-chunk %d/%d routed (%d origins x %d zones, %d total origins done / %d)",
                    oc, n_ochunks, nrow(o_batch), n_zones,
                    length(unique(result$origin_id)), nrow(origins)))
  }
}

log_msg(sprintf("[B] Wrote %s | %d rows | %d / %d origins fully routed | missing car_time_min: %d",
                out_file, nrow(result), length(unique(result$origin_id)), nrow(origins),
                sum(is.na(result$car_time_min))))
log_msg("03_car_osrm.R done.")
