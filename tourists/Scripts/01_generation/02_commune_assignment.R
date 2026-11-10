# ============================================================
# 02_commune_assignment.R
# Assigns every trip (point-in-polygon only, no nearest-polygon fallback)
#   - DESTINATION -> Swiss zone/commune     (zones_communes.gpkg)
#   - ORIGIN      -> foreign municipality   (Eurostat GISCO LAU, IT/FR/AT/DE)
# and filters:
#   - drops trips whose destination is not inside a Swiss zone
#   - drops Swiss residents (survey variable `nationality` == 1, i.e. WOHNORTLAND = CH)
#   - drops trips whose origin is not inside a foreign commune
# A final check stops the script if any remaining origin/destination is unassigned.
#
# Input  : <csv_out>/agqpv_clean.csv                (from 01_agqpv_clean.R)
#          <gpkg_in>/zones_communes.gpkg
#          <gpkg_in>/LAU_RG_01M_<year>_4326.gpkg    (downloaded if missing)
# Output : <csv_out>/agqpv.csv
#   new columns:
#     destination_zone, dest_zone_topology, dest_zone_country
#     origin_commune, origin_commune_name, origin_commune_country
#
# Run from the project root:  source("Scripts/01_generation/02_commune_assignment.R")
# ============================================================

source("Scripts/00_setup.R")
gen <- cfg$generation

# ------------------------------------------------------------
# Helper: id of the polygon (column `id_col` of `polys`) containing each point
# (WGS84 lon/lat). NA if the point is outside every polygon or has no coordinates.
# ------------------------------------------------------------
assign_points <- function(lon, lat, polys, id_col) {
  id     <- rep(NA_character_, length(lon))
  has_xy <- !is.na(lon) & !is.na(lat)
  if (!any(has_xy)) return(id)

  pts <- st_as_sf(data.frame(lon = lon[has_xy], lat = lat[has_xy], .row = which(has_xy)),
                  coords = c("lon", "lat"), crs = 4326)
  pts <- st_transform(pts, st_crs(polys))

  joined <- st_join(pts, polys[, id_col], left = TRUE)
  joined <- joined[!duplicated(joined$.row), ]   # a point on a border can match twice
  id[joined$.row] <- as.character(joined[[id_col]])
  id
}

# Log how many rows a filter removes (count and share of survey weight)
log_drop <- function(dt, drop, reason) {
  w <- 100 * sum(dt$weight[drop], na.rm = TRUE) / sum(dt$weight, na.rm = TRUE)
  log_msg(sprintf("Dropping %d trips (%.2f%% of weight): %s", sum(drop), w, reason))
}

# ------------------------------------------------------------
# 1. Read cleaned survey
# ------------------------------------------------------------
agqpv <- fread(file.path(CSV_DIR, gen$out$agqpv_clean))
log_msg(sprintf("Read %d trips from %s", nrow(agqpv), gen$out$agqpv_clean))

# ------------------------------------------------------------
# 2. Swiss zones: DESTINATION -> zone/commune, RESIDENCE -> country
# ------------------------------------------------------------
zones_sf <- st_read(file.path(GPKG_IN_DIR, gen$zones_gpkg), quiet = TRUE)

# One zone (NO 910000000) has an empty geometry and no attributes -- drop it.
n_before <- nrow(zones_sf)
zones_sf <- zones_sf[!is.na(zones_sf$MAKROBEZ_STAAT), ]
if (nrow(zones_sf) < n_before) {
  log_msg(sprintf("Dropped %d zone(s) with missing MAKROBEZ_STAAT", n_before - nrow(zones_sf)))
}
zone_attr <- unique(as.data.table(st_drop_geometry(zones_sf)[, c("NO", "STALAN2020", "MAKROBEZ_STAAT")]),
                    by = "NO")
zone_attr[, NO := as.character(NO)]   # assign_points() returns ids as character

# Destination
agqpv[, destination_zone := assign_points(dest_long, dest_lat, zones_sf, "NO")]
agqpv[zone_attr, on = c(destination_zone = "NO"),
      `:=`(dest_zone_topology = i.STALAN2020, dest_zone_country = i.MAKROBEZ_STAAT)]

# The survey has a few records with placeholder destination coordinates (e.g.
# Basel-Weil, Milan, Besancon; dest_commune is 0/NA) although ZIELORTLAND == 1.
bad_dest <- is.na(agqpv$destination_zone) | agqpv$dest_zone_country != "CH"
log_drop(agqpv, bad_dest, "destination not inside a Swiss zone")
print(agqpv[bad_dest, .N, by = grenz][order(-N)])
agqpv <- agqpv[!bad_dest]

# Keep foreign residents only. `nationality` is the survey's WOHNORTLAND
# (country of residence): 1 = CH, 2 = DE, 3 = AT, 4 = FR, 5 = IT, ...
# (see Beschreibung_Finale_Auswertungsdatenbank_AGQPV2015_V2.xls, "Werte & Labels").
swiss_res <- agqpv$nationality %in% 1
log_drop(agqpv, swiss_res, "Swiss residents (nationality == 1)")
agqpv <- agqpv[!swiss_res]

# ------------------------------------------------------------
# 3. ORIGIN -> foreign municipality (LAU)
# ------------------------------------------------------------
lau_file <- file.path(GPKG_IN_DIR, sprintf(gen$lau$file_template, gen$lau$year))

if (!file.exists(lau_file)) {
  lau_url <- sprintf(gen$lau$url_template, gen$lau$year)
  log_msg("LAU file not found, downloading: ", lau_url)
  old_timeout <- options(timeout = 3600)
  download.file(lau_url, lau_file, mode = "wb", quiet = FALSE)   # mode="wb": binary on Windows
  options(old_timeout)
}
if (!file.exists(lau_file) || file.info(lau_file)$size < 10000) {
  stop("LAU file missing or too small. Download it manually from:\n",
       sprintf(gen$lau$url_template, gen$lau$year), "\nand save it as:\n", lau_file)
}

# Read only the target countries (SQL filter); fall back to a full read if the
# layer has no CNTR_CODE column.
lau_layer <- st_layers(lau_file)$name[1]
cntr_list <- paste(sprintf("'%s'", gen$lau$countries), collapse = ", ")
lau <- tryCatch(
  st_read(lau_file, quiet = TRUE,
          query = sprintf('SELECT * FROM "%s" WHERE CNTR_CODE IN (%s)', lau_layer, cntr_list)),
  error = function(e) st_read(lau_file, quiet = TRUE)
)

need <- c("GISCO_ID", "LAU_NAME", "CNTR_CODE")
if (!all(need %in% names(lau))) {
  stop("Unexpected fields in LAU file. Need: ", paste(need, collapse = ", "),
       "\nAvailable: ", paste(names(lau), collapse = ", "))
}
communes_ausland <- lau |>
  dplyr::filter(CNTR_CODE %in% gen$lau$countries) |>
  st_transform(4326) |>
  st_make_valid() |>
  dplyr::transmute(zone_id = as.character(GISCO_ID),
                   name    = as.character(LAU_NAME),
                   country = as.character(CNTR_CODE))
rm(lau)
log_msg(sprintf("Foreign communes loaded: %d (%s)", nrow(communes_ausland),
                paste(sort(unique(communes_ausland$country)), collapse = ", ")))

agqpv[, origin_commune := assign_points(origin_long, origin_lat, communes_ausland, "zone_id")]

bad_origin <- is.na(agqpv$origin_commune)
log_drop(agqpv, bad_origin, "origin not inside a foreign commune (IT/FR/AT/DE)")
agqpv <- agqpv[!bad_origin]

commune_attr <- unique(as.data.table(st_drop_geometry(communes_ausland)), by = "zone_id")
agqpv[commune_attr, on = c(origin_commune = "zone_id"),
      `:=`(origin_commune_name = i.name, origin_commune_country = i.country)]

# ------------------------------------------------------------
# 4. FINAL CHECK
#    - every origin has a foreign commune
#    - every destination has a zone, and that zone is in Switzerland
# ------------------------------------------------------------
problems <- c(
  if (anyNA(agqpv$origin_commune))
    sprintf("%d origin(s) not matched to a foreign commune", sum(is.na(agqpv$origin_commune))),
  if (anyNA(agqpv$destination_zone))
    sprintf("%d destination(s) not matched to a zone", sum(is.na(agqpv$destination_zone))),
  if (any(agqpv$dest_zone_country != "CH", na.rm = TRUE))
    sprintf("%d destination(s) in a zone outside Switzerland", sum(agqpv$dest_zone_country != "CH", na.rm = TRUE)),
  if (any(agqpv$nationality %in% 1))
    sprintf("%d trip(s) with Swiss residence", sum(agqpv$nationality %in% 1))
)
if (length(problems)) stop("Assignment check failed:\n  - ", paste(problems, collapse = "\n  - "))
log_msg(sprintf("Assignment check passed: %d trips, all origins in a foreign commune, all destinations in a Swiss zone, no Swiss residents.",
                nrow(agqpv)))

# ------------------------------------------------------------
# 5. Write
# ------------------------------------------------------------
out_file <- file.path(CSV_DIR, gen$out$agqpv)
fwrite(agqpv, out_file)
log_msg(sprintf("Wrote %s (%d rows, %d columns)", out_file, nrow(agqpv), ncol(agqpv)))
