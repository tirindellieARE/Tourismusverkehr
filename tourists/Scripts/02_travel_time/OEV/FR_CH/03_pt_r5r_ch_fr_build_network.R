# ============================================================
# 03_pt_r5r_ch_fr_build_network.R
# STEP 1 of 2: builds the CH + near-CH France + Île-de-France r5r network.
# No sample and no routing here -- test/04_pt_r5r_ch_fr_route_sample.R draws the
# tourist sample and routes it on this network.
#
# GEOGRAPHIC SCOPE
#   All French-origin tourists together give a bbox of ~1.23M km^2, over
#   R5's 975,000 km^2 limit. The region of each origin is taken from the
#   département part of its INSEE commune id (origin_commune, e.g. "FR_74010"),
#   and only the three regions that border Switzerland are kept:
#   Auvergne-Rhône-Alpes, Grand Est, Bourgogne-Franche-Comté
#   (3,208 / 3,963 rows, 81%, in the current agqpv.csv).
#   The bbox is then extended to cover Île-de-France, because many trips
#   from these regions to Switzerland change trains in Paris (e.g. Épernay ->
#   Paris Est -> metro/RER -> Paris Gare de Lyon -> Genève). Without Paris in
#   the street network, its stations can't be linked and those trips fail.
#
# GTFS FEEDS (copied into NET_DIR)
#   ch.gtfs.zip               Swiss national feed
#   fr.gtfs.zip               SNCF national feed (TER, Intercités, TGV)
#   fr.Paris.gtfs.clean.zip   IDFM feed (metro, RER, Transilien, tram),
#                             cleaned with 01_clean_fr_gtfs.R
#   Feed file names must stay distinct: R5 uses them as feed IDs.
#
# OUTPUTS
#   NET_DIR/network.osm.pbf, network.dat   the r5r network
#
# BUILD_NETWORK decides whether the network is (re)built:
#   TRUE  -> clip the OSM extract, copy the feeds, build network.dat from
#            scratch (slow). Do this after changing the bbox, regions or feeds.
#   FALSE -> skip all of that; the existing network is left untouched (the
#            script then only checks that the bbox still matches it).
#
# Run in a FRESH R session:
#   source("Scripts/02_travel_time/OEV/FR_CH/03_pt_r5r_ch_fr_build_network.R")
# ============================================================

# Run from the project root.

# --- Java heap must be set before anything loads r5r ------------------------
.cfg0 <- yaml::read_yaml("Scripts/config.yml")
options(java.parameters = paste0("-Xmx", .cfg0$java_max_mem))

source("Scripts/00_setup.R")   # sets the project root; provides cfg, CSV_DIR, log_msg, ensure_dir
if (!requireNamespace("r5r", quietly = TRUE)) install.packages("r5r", repos = "https://cloud.r-project.org")
library(r5r)

# --- Settings ----------------------------------------------------------------
NET_DIR     <- "data/r5_rail_ch_fr"           # network folder (shared with the routing script)
STAGING_DIR <- "data/r5_rail_ch_fr_staging"   # unclipped CH+FR merge -- must NOT be in NET_DIR

# The three French regions bordering Switzerland, as INSEE département codes (config.yml)
FR_NEAR_CH_DEPTS   <- cfg$oev_ch_fr$near_ch_depts
FR_NEAR_CH_REGIONS <- names(FR_NEAR_CH_DEPTS)
IDF_BBOX <- c(xmin = 1.15, ymin = 47.96, xmax = 3.56, ymax = 49.46)  # extent of IDFM stops

# Raw OSM extracts (data/input/osm)
CH_PBF <- "data/input/osm/switzerland-latest.osm.pbf"
FR_PBF <- "data/input/osm/france-latest.osm.pbf"
DE_PBF <- "data/input/osm/baden-wuerttemberg-latest.osm.pbf"

# GTFS: raw feeds in data/input/gtfs, feeds produced by our own scripts in data/output/gtfs
#   ch.freqfixed.gtfs.zip     <- frequencies expanded (02_clean_CH_gtfs.R)
#   fr.Paris.gtfs.clean.zip   <- 01_clean_fr_gtfs.R
GTFS_FEEDS <- c("data/output/gtfs/ch.freqfixed.gtfs.zip",
                "data/input/gtfs/fr.gtfs.zip",
                "data/output/gtfs/fr.Paris.gtfs.clean.zip")

OSMIUM_DIR    <- "C:/Users/Modellrechner.ARE/miniforge3/envs/r5/Library/bin"
BUILD_NETWORK <- FALSE   # TRUE = rebuild the network (slow), FALSE = keep the existing one
R5_MAX_AREA_KM2 <- 975000

ensure_dir(NET_DIR); ensure_dir(STAGING_DIR)

area_km2 <- function(xmin, ymin, xmax, ymax) {
  midlat <- (ymin + ymax) / 2
  (xmax - xmin) * 111.32 * cos(midlat * pi / 180) * (ymax - ymin) * 111.32
}

# --- Near-CH French-origin tourists ------------------------------------------
agqpv <- readr::read_csv(cfg$input_csv, locale = readr::locale(encoding = cfg$input_encoding),
                         show_col_types = FALSE)
agqpv$commune_country <- substr(agqpv$origin_commune, 1, 2)
fr <- agqpv[!is.na(agqpv$commune_country) & agqpv$commune_country == "FR", ]

# Region of each French origin from the département part of its commune id
fr_dept <- substr(fr$origin_commune, 4, 5)
fr$state <- NA_character_
for (reg in FR_NEAR_CH_REGIONS) fr$state[fr_dept %in% FR_NEAR_CH_DEPTS[[reg]]] <- reg
fr_near <- fr[!is.na(fr$state), ]
log_msg(sprintf("Near-CH French-origin rows: %d / %d FR-origin rows", nrow(fr_near), nrow(fr)))

# --- Bounding box: tourists + Île-de-France ----------------------------------
pad  <- cfg$bbox_pad_deg
xmin <- min(fr_near$origin_long, fr_near$dest_long, IDF_BBOX[["xmin"]]) - pad
xmax <- max(fr_near$origin_long, fr_near$dest_long, IDF_BBOX[["xmax"]]) + pad
ymin <- min(fr_near$origin_lat,  fr_near$dest_lat,  IDF_BBOX[["ymin"]]) - pad
ymax <- max(fr_near$origin_lat,  fr_near$dest_lat,  IDF_BBOX[["ymax"]]) + pad

bbox_area <- area_km2(xmin, ymin, xmax, ymax)
bbox_str  <- sprintf("%f,%f,%f,%f", xmin, ymin, xmax, ymax)
log_msg(sprintf("Network bbox: [%s]  area = %.0f km^2", bbox_str, bbox_area))
if (bbox_area >= R5_MAX_AREA_KM2) {
  stop(sprintf("Bbox (%.0f km^2) exceeds R5's %d km^2 limit.", bbox_area, R5_MAX_AREA_KM2))
}

# --- Network: build only if BUILD_NETWORK is TRUE ---------------------------
bbox_record <- file.path(NET_DIR, "network_bbox.txt")   # bbox the current network was clipped with

if (BUILD_NETWORK) {
  Sys.setenv(PATH = paste(OSMIUM_DIR, Sys.getenv("PATH"), sep = ";"))
  if (!nzchar(Sys.which("osmium"))) stop("osmium not found -- check OSMIUM_DIR.")
  if (!all(file.exists(c(CH_PBF, FR_PBF, DE_PBF)))) stop("Missing OSM extracts -- check data/input/osm/.")
  
  missing_feeds <- GTFS_FEEDS[!file.exists(GTFS_FEEDS)]
  if (length(missing_feeds)) stop("Missing feed(s): ", paste(missing_feeds, collapse = ", "))
  
  # Any other GTFS zip in NET_DIR would be loaded too (e.g. the uncleaned IDFM feed)
  extra <- setdiff(list.files(NET_DIR, pattern = "\\.zip$"), basename(GTFS_FEEDS))
  if (length(extra)) {
    log_msg(sprintf("Removing old feed copies from %s: %s", NET_DIR, paste(extra, collapse = ", ")))
    file.remove(file.path(NET_DIR, extra))
  }  
  # Merge the two countries + a bit of Germany once (unclipped, kept in STAGING_DIR for reuse)
  merged_pbf <- file.path(STAGING_DIR, "ch_fr_merged.osm.pbf")
  if (!file.exists(merged_pbf)) {
    log_msg("Merging CH + FR + Baden-Württemberg OSM extracts (slow, done once)...")
    system2("osmium", c("merge", shQuote(CH_PBF), shQuote(FR_PBF), shQuote(DE_PBF),
                        "-o", shQuote(merged_pbf), "--overwrite"))
  }
  
  # Remove the old network so nothing stale is reused
  stale <- list.files(NET_DIR, pattern = "^network\\.(osm\\.pbf|dat)|\\.mapdb|network_settings\\.json",
                      full.names = TRUE)
  if (length(stale)) file.remove(stale)
  
  log_msg("Clipping merged extract to the network bbox...")
  clipped_pbf <- file.path(NET_DIR, "network.osm.pbf")
  system2("osmium", c("extract", "-b", bbox_str, shQuote(merged_pbf), "-o", shQuote(clipped_pbf), "--overwrite"))
  if (!file.exists(clipped_pbf)) stop("osmium extract failed -- no ", clipped_pbf)
  
  file.copy(GTFS_FEEDS, NET_DIR, overwrite = TRUE)
  
  log_msg("Building r5r network (takes a while)...")
  t0 <- Sys.time()
  r5r_core <- r5r::build_network(data_path = NET_DIR, verbose = FALSE, overwrite = TRUE)
  log_msg(sprintf("Network built in %.1f min.", as.numeric(Sys.time() - t0, units = "mins")))
  r5r::stop_r5(r5r_core)
  rJava::.jgc(R.gc = TRUE)
  writeLines(bbox_str, bbox_record)
  
} else {
  if (!file.exists(file.path(NET_DIR, "network.dat"))) {
    stop("No network.dat in ", NET_DIR, " -- set BUILD_NETWORK <- TRUE to build it.")
  }
  log_msg("BUILD_NETWORK is FALSE -- keeping the existing network.")
  # Only a warning: tells you if the tourists' bbox no longer matches the network
  if (file.exists(bbox_record) && !identical(readLines(bbox_record), bbox_str)) {
    warning("The bbox differs from the one the existing network was built with -- ",
            "set BUILD_NETWORK <- TRUE if you changed the regions or the Paris extent.")
  }
}

log_msg("03_pt_r5r_ch_fr_build_network.R done -- next: test/04_pt_r5r_ch_fr_route_sample.R")
