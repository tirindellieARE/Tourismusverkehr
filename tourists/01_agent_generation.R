library(data.table)
library(sf)

user = "MR"
if(user == "MR"){setwd("E:/ARE/ProjekteTIE/Turismusverkehr/RScript/tourists")}
if(user == "CP"){setwd("P:/Verkehrsmodellierung/06_Jobs/188_touristische_Verkehr/Model_trafic_touristique/")}

dir.create("data/output", recursive = TRUE, showWarnings = FALSE)

raw = fread("data/input/Finale_Auswertungsdatenbank_AGQPV2015_V2.csv")
agqpv= dplyr::filter(raw, STARTORTLANDISO != "CH")
agqpv= dplyr::filter(agqpv, ZIELORTLAND == 1)
agqpv= dplyr::filter(agqpv, FAHRTZWECK == 5)

agqpv = dplyr::select(agqpv, c(BEFRAGUNGSORT, WOHNORTLAND, WOHNORTORTLATITUDE, WOHNORTORTLONGITUDE, STARTORTLAND,
                               STARTORTORTLATITUDE, STARTORTORTLONGITUDE, ZIELORTORTLATITUDE, ZIELORTORTLONGITUDE,
                               ANZAHLUEBERNACHTUNGEN, VERKEHRSTRAEGER, GEWICHT))
new_names = c("grenz", "nationality", "residence_lat", "residence_long", "origin_country", "origin_lat", "origin_long",
              "dest_lat", "dest_long", "n_nights", "border_mode", "weight")
names(agqpv) = new_names
agqpv = na.omit(agqpv)


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

ZONES_FILE <- "data/input/zones_communes.gpkg"
zones_sf   <- st_read(ZONES_FILE, quiet = TRUE)

assign_zones <- function(lon, lat, zones) {
  pts <- st_as_sf(
    data.frame(lon = lon, lat = lat),
    coords = c("lon", "lat"),
    crs    = 4326
  )
  pts    <- st_transform(pts, st_crs(zones))
  joined <- st_join(pts, zones[, "NO"], left = TRUE)

  # Fallback: snap unmatched points to the nearest zone polygon
  na_idx <- which(is.na(joined$NO))
  if (length(na_idx) > 0) {
    nearest        <- st_nearest_feature(pts[na_idx, ], zones)
    joined$NO[na_idx] <- zones$NO[nearest]
  }

  joined$NO
}

agqpv[, origin_zone    := assign_zones(origin_long,    origin_lat,    zones_sf)]
agqpv[, dest_zone      := assign_zones(dest_long,      dest_lat,      zones_sf)]
agqpv[, residence_zone := assign_zones(residence_long, residence_lat, zones_sf)]

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
n_dest_missing <- sum(is.na(agqpv$dest_zone))
if (n_dest_missing > 0) {
  stop(sprintf(
    "%d destination point(s) could not be matched to any transit zone. ",
    "Inspect agents[is.na(dest_zone)] and verify coordinate coverage.",
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
fwrite(agqpv, "data/output/agqpv.csv")

# set scaling factor for computational reasons
SCALING_FACTOR = 1000

set.seed(1)
w = agqpv$weight/SCALING_FACTOR
reps = floor(w) + (runif(length(w)) < (w - floor(w)))
agents = agqpv[rep(1:.N, reps)]

agents[, agent_id := .I]
fwrite(agents, "data/output/agents.csv")
cat(sprintf("Agents saved to data/output/agents.csv (%d rows, %d columns)\n", nrow(agents), ncol(agents)))
