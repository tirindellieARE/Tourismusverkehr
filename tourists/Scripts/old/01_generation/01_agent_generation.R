library(data.table)
library(sf)

user = "MR"
if(user == "MR"){setwd("E:/ARE/ProjekteTIE/Turismusverkehr/RScript/tourists")}
if(user == "CP"){setwd("P:/Verkehrsmodellierung/06_Jobs/188_touristische_Verkehr/Model_trafic_touristique/GitRepo/tourists")}

CSV_DIR      <- "data/output/csv"      # all CSV outputs
GPKG_IN_DIR  <- "data/input/gpkg"      # input geopackages (zones_communes.gpkg)
CSV_IN_DIR   <- "data/input/csv"       # input CSVs (raw survey data)
dir.create(CSV_DIR, recursive = TRUE, showWarnings = FALSE)

raw = fread(file.path(CSV_IN_DIR, "Finale_Auswertungsdatenbank_AGQPV2015_V2.csv"), encoding = "Latin-1")
agqpv= dplyr::filter(raw, STARTORTLANDISO != "CH")
agqpv= dplyr::filter(agqpv, ZIELORTLAND == 1)
agqpv= dplyr::filter(agqpv, FAHRTZWECK == 5)

agqpv = dplyr::select(agqpv, c(BEFRAGUNGSORTID, BEFRAGUNGSORT, WOHNORTLAND, WOHNORTORTLATITUDE, WOHNORTORTLONGITUDE, STARTORTLAND,
                               STARTORTORTLATITUDE, STARTORTORTLONGITUDE, ZIELORTORTLATITUDE, ZIELORTORTLONGITUDE, ZIELORTGEONAMESID_BFSNUMMER,
                               ANZAHLUEBERNACHTUNGEN, VERKEHRSTRAEGER, GEWICHT))
new_names = c("grenz_id", "grenz", "nationality", "residence_lat", "residence_long", "origin_country", "origin_lat", "origin_long",
              "dest_lat", "dest_long", "dest_commune","n_nights", "border_mode", "weight")
names(agqpv) = new_names

# Origin and destination coordinates must be complete: drop rows missing any
# of the four (needed for zone assignment and routing).
no_od <- is.na(agqpv$origin_lat) | is.na(agqpv$origin_long) |
  is.na(agqpv$dest_lat)   | is.na(agqpv$dest_long)
cat(sprintf("Dropped %d of %d rows with missing origin/destination coordinates.\n",
            sum(no_od), nrow(agqpv)))
agqpv <- agqpv[!no_od]

# Other missing values (e.g. residence, weight) are KEPT (no na.omit). Report
# what is missing so it's visible; the steps below handle them.
na_counts <- colSums(is.na(agqpv))
if (any(na_counts > 0)) {
  cat(sprintf("Rows with missing values are kept (%d of %d rows have at least one NA). NA per column:\n",
              sum(!complete.cases(agqpv)), nrow(agqpv)))
  print(na_counts[na_counts > 0])
}


agqpv[, border_mode_label := dplyr::case_when(
  border_mode == 1 ~ "road",
  border_mode == 2 ~ "rail",
  TRUE             ~ "unknown"
)]

# =============================================================================
# ZONE ASSIGNMENT
# Map origin, destination and residence coordinates to transit zones (zones.gpkg)
# Zones CRS: CH1903+/LV95; coordinates in agents are WGS84 (degrees).
# =============================================================================

ZONES_FILE <- file.path(GPKG_IN_DIR, "zones_communes.gpkg")
zones_sf   <- st_read(ZONES_FILE, quiet = TRUE)

# One zone (NO 910000000) has an empty geometry and no attributes at all
# (including no MAKROBEZ_STAAT) -- drop it before any zone assignment.
n_zones_before <- nrow(zones_sf)
zones_sf <- zones_sf[!is.na(zones_sf$MAKROBEZ_STAAT), ]
if (nrow(zones_sf) < n_zones_before) {
  cat(sprintf("Dropped %d zone(s) with missing MAKROBEZ_STAAT\n", n_zones_before - nrow(zones_sf)))
}


# Rows without coordinates (possible now that na.omit is gone) get zone = NA
# and mismatch = NA; only rows with coordinates are assigned.
assign_zones <- function(lon, lat, zones) {
  has_xy   <- !is.na(lon) & !is.na(lat)
  zone     <- rep(NA, length(lon))
  mismatch <- rep(NA, length(lon))
  if (!any(has_xy)) return(list(zone = zone, mismatch = mismatch))
  
  pts <- st_as_sf(
    data.frame(lon = lon[has_xy], lat = lat[has_xy], .row = which(has_xy)),
    coords = c("lon", "lat"),
    crs    = 4326
  )
  
  pts <- st_transform(pts, st_crs(zones))
  
  # First: assign points that are actually inside a zone
  joined <- st_join(pts, zones[, "NO"], left = TRUE)
  joined <- joined[!duplicated(joined$.row), ]   # a point on a zone border can match twice
  
  # Flag for points that are not inside any zone
  joined$zone_mismatch <- FALSE
  
  na_idx <- which(is.na(joined$NO))
  
  if (length(na_idx) > 0) {
    
    # Find nearest zone for unmatched points
    nearest <- st_nearest_feature(pts[na_idx, ], zones)
    
    # Assign nearest zone
    joined$NO[na_idx] <- zones$NO[nearest]
    
    # These points are outside the zone they were assigned to
    joined$zone_mismatch[na_idx] <- TRUE
  }
  
  zone[joined$.row]     <- joined$NO
  mismatch[joined$.row] <- joined$zone_mismatch
  list(
    zone = zone,
    mismatch = mismatch
  )
}


# Origin
origin_result <- assign_zones(
  agqpv$origin_long,
  agqpv$origin_lat,
  zones_sf
)

agqpv[, origin_zone := origin_result$zone]
agqpv[, origin_zone_mismatch := origin_result$mismatch]


# Destination
dest_result <- assign_zones(
  agqpv$dest_long,
  agqpv$dest_lat,
  zones_sf
)

agqpv[, dest_zone := dest_result$zone]
agqpv[, dest_zone_mismatch := dest_result$mismatch]


# Residence
residence_result <- assign_zones(
  agqpv$residence_long,
  agqpv$residence_lat,
  zones_sf
)

agqpv[, residence_zone := residence_result$zone]
agqpv[, residence_zone_mismatch := residence_result$mismatch]



# Summary of unmatched points per coordinate type
cat(sprintf(
  "Zone match summary (out of %d obs):\n  origin_zone    unmatched: %d (%.1f%%)\n  dest_zone      unmatched: %d (%.1f%%)\n  residence_zone unmatched: %d (%.1f%%)\n",
  nrow(agqpv),
  sum(is.na(agqpv$origin_zone)),    100 * mean(is.na(agqpv$origin_zone)),
  sum(is.na(agqpv$dest_zone)),      100 * mean(is.na(agqpv$dest_zone)),
  sum(is.na(agqpv$residence_zone)), 100 * mean(is.na(agqpv$residence_zone))
))

if (any(is.na(agqpv$residence_zone))) {
  cat("Nationality breakdown of obs with unmatched residence zone:\n")
  print(table(agqpv[is.na(residence_zone), nationality]))
}

if (any(is.na(agqpv$origin_zone))) {
  cat("Nationality breakdown of obs with unmatched origin zone:\n")
  print(table(agqpv[is.na(origin_zone), nationality]))
}

# Check: every destination point must fall inside a transit zone
# (all rows have destination coordinates after the filter above)
n_dest_missing <- sum(is.na(agqpv$dest_zone))
if (n_dest_missing > 0) {
  stop(sprintf(
    paste0("%d destination point(s) could not be matched to any transit zone. ",
           "Inspect agqpv[is.na(dest_zone)] and verify coordinate coverage."),
    n_dest_missing
  ))
}
cat(sprintf(
  "Zone assignment complete: %d obs, all destination points matched a transit zone.\n",
  nrow(agqpv)
))

# Add destination zone urban/rural topology from zones_communes
zone_topology <- as.data.table(unique(st_drop_geometry(zones_sf)[, c("NO", "STALAN2020")]))
agqpv <- zone_topology[agqpv, on = c(NO = "dest_zone")]
setnames(agqpv, c("NO", "STALAN2020"), c("dest_zone", "dest_zone_topology"))

# Country (CH / LI / Ausland) of each zone type, from MAKROBEZ_STAAT -- lets us
# verify these are genuinely foreign tourists (origin/residence abroad) headed
# into Switzerland (destination CH).
zone_country <- as.data.table(unique(st_drop_geometry(zones_sf)[, c("NO", "MAKROBEZ_STAAT")]))

agqpv <- zone_country[agqpv, on = c(NO = "origin_zone")]
setnames(agqpv, c("NO", "MAKROBEZ_STAAT"), c("origin_zone", "origin_zone_country"))

agqpv <- zone_country[agqpv, on = c(NO = "dest_zone")]
setnames(agqpv, c("NO", "MAKROBEZ_STAAT"), c("dest_zone", "dest_zone_country"))

agqpv <- zone_country[agqpv, on = c(NO = "residence_zone")]
setnames(agqpv, c("NO", "MAKROBEZ_STAAT"), c("residence_zone", "residence_zone_country"))

cat("origin_zone_country breakdown:\n");    print(agqpv[, .N, by = origin_zone_country][order(-N)])
cat("dest_zone_country breakdown:\n");      print(agqpv[, .N, by = dest_zone_country][order(-N)])
cat("residence_zone_country breakdown:\n"); print(agqpv[, .N, by = residence_zone_country][order(-N)])

# NOTE: origin_zone_CH (the border-crossing zone, from geocoding `grenz` via
# Scripts/01_generation/00_geocode_grenz.R -> data/output/grenz_coordinates.csv)
# is intentionally NOT merged into agqpv here right now -- stepping back on
# that per request. The geocoding script itself is untouched and its output
# still exists on disk for whenever this is picked back up.

# destination_zone (the tourist's actual in-CH destination) -- rename from
# dest_zone for a name that reads the same way as origin_zone would.
setnames(agqpv, "dest_zone", "destination_zone")

# =============================================================================
# CONSISTENCY FILTER
# Drop observations that contradict "foreign tourist entering Switzerland":
#   - origin_zone_country == "CH"        : the reported border-entry point is
#     itself in Switzerland, not abroad
#   - dest_zone_country != "CH"          : the reported destination point is
#     not in Switzerland, despite the raw-data filter (ZIELORTLAND == 1)
#   - residence == 1 (Swiss) implies residence_zone_country should not be "CH", 
#     should be abroad ("Ausland"/"LI"), i.e. it should be tourist going to CH and not
#     Swiss coming back home
# =============================================================================

# NA-safe: a row with a missing zone country is NOT dropped by that criterion
drop_origin_ch    <- agqpv$origin_zone_country %in% "CH"
drop_dest_abroad  <- !is.na(agqpv$dest_zone_country) & agqpv$dest_zone_country != "CH"
drop_residence_ch <- agqpv$residence_zone_country %in% "CH"

cat(sprintf(
  "\nConsistency filter:\n  origin_zone_country == CH        : %d obs\n  dest_zone_country != CH           : %d obs\n  nationality/residence mismatch    : %d obs\n",
  sum(drop_origin_ch), sum(drop_dest_abroad), sum(drop_residence_ch)
))

to_drop <- drop_origin_ch | drop_dest_abroad | drop_residence_ch
n_overlap <- sum(drop_origin_ch) + sum(drop_dest_abroad) + sum(drop_residence_ch) - sum(to_drop)
cat(sprintf("  Total dropped (union): %d / %d obs (%d obs matched more than one criterion)\n", sum(to_drop), nrow(agqpv), n_overlap))

agqpv <- agqpv[!to_drop]
cat(sprintf("Remaining after consistency filter: %d obs\n", nrow(agqpv)))

fwrite(agqpv, file.path(CSV_DIR, "agqpv_CHcommune.csv"))

# set scaling factor for computational reasons
SCALING_FACTOR = 1000

set.seed(1)
w = agqpv$weight/SCALING_FACTOR
reps = floor(w) + (runif(length(w)) < (w - floor(w)))
# Rows without a weight can't be expanded into agents: they stay in
# agqpv_CHcommune.csv but get no agents
if (any(is.na(reps))) {
  cat(sprintf("%d row(s) have no weight -> no agents generated for them.\n", sum(is.na(reps))))
  reps[is.na(reps)] <- 0
}
agents = agqpv[rep(1:.N, reps)]

agents[, agent_id := .I]
fwrite(agents, file.path(CSV_DIR, "agents.csv"))
cat(sprintf("Agents saved to %s (%d rows, %d columns)\n", file.path(CSV_DIR, "agents.csv"), nrow(agents), ncol(agents)))