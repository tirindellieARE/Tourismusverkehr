# =============================================================================
# Aggregate zone-level EMU (logsum) and Erreichbarkeit (accessibility) up to
# the 101 AMR regions in data/amr101.gpkg, so the destination choice model
# can optionally use these regions as the choice set instead of individual
# NPVM zones.
#
# Aggregation is a population-weighted average over the zones within each
# region:
#   x_region = sum_i( pop_i * x_i ) / sum_i( pop_i ),   i = zones in region
# using each zone's resident population (v02_resident_population, Benzoni et
# al.) as the weight. Zones with no population in the Benzoni dataset (710 /
# 8,688 -- the same zones missing from every attractivity-index lookup used
# elsewhere in this project, e.g. lakes/uninhabited zones) get weight 0.
#
# Zone -> region assignment: restricted to genuine CH zones first, using
# zones_communes.gpkg's MAKROBEZ_STAAT column ("CH" / "LI" / "Ausland") --
# zones_communes.gpkg now also includes actual foreign (Ausland) and
# Liechtenstein (LI) zones, which the 101 CH-only AMR regions have no
# counterpart for. (Canton code, KTKZ, is NOT used for this -- it is NA for
# both the ~752 Ausland/LI zones AND 7 genuine CH zones, so it would wrongly
# exclude those 7.)
#
# Then each CH zone is assigned to the AMR region with which it shares the
# LARGEST OVERLAPPING AREA (st_join(..., largest = TRUE) -- an areal-majority
# join on the actual zone polygons, not just their centroid). ~525 / 8,688
# zones have no polygon overlap with any AMR region at all (lake/river slivers
# whose entire mapped area sits in water beyond every AMR boundary) and fall
# back to their nearest AMR polygon.
#
# This replaces an earlier centroid-in-polygon version (st_within on each
# zone's centroid, ~1,012 zones needing a nearest-fallback for centroids
# landing just outside every polygon). Comparing the two methods: 56 / 8,688
# zones (0.64%) end up in a different region -- almost all small communes that
# straddle two AMR regions (e.g. Rolle VD, Sennwald/Oberriet SG, Riehen/Möhlin
# on the BS/AG border, Hemishofen/Steckborn on the SH/TG border), where the
# centroid happens to fall on one side while most of the zone's area is on the
# other. The polygon-overlap method is used here as the more defensible of the
# two for population-weighted aggregation, since it accounts for the actual
# shape of each zone rather than a single point.
#
# Reads:
#   data/input/zones_communes.gpkg        -- NPVM zone geometries
#   data/amr101.gpkg                      -- 101 AMR region polygons
#   ../benzoni_thesis/output/attractivity_indexes.csv -- v02_resident_population
#   data/output/tt_ausland_CH_logsum.fst / _tagesreise.fst / _reisemitue.fst
#   data/erreichbarkeit_tt2023.gpkg
#
# Writes:
#   data/output/zone_to_amr101.csv              -- npvm_id -> BAE2018 region + population
#   data/output/amr101_population.csv           -- total population per region
#   data/output/tt_full_logsum_amr101.fst           (pooled EMU, origin_zone x dest_region)
#   data/output/tt_full_logsum_amr101_tagesreise.fst
#   data/output/tt_full_logsum_amr101_reisemitue.fst
#   data/output/erreichbarkeit_amr101.csv        -- region-level Erreichbarkeit (raw + z)
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(fst)
})

dir.create("data/output", recursive = TRUE, showWarnings = FALSE)

# --- 1. zone -> region mapping ----------------------------------------------

zones_sf <- st_read("data/input/zones_communes.gpkg", quiet = TRUE)
amr_sf   <- st_read("data/amr101.gpkg", quiet = TRUE)
zones_sf <- st_transform(zones_sf, st_crs(amr_sf))

# Restrict to genuine CH zones (see note above).
zones_ch <- zones_sf[zones_sf$MAKROBEZ_STAAT == "CH" & !is.na(zones_sf$MAKROBEZ_STAAT), ]
cat(sprintf("CH zones (MAKROBEZ_STAAT == CH): %d / %d\n", nrow(zones_ch), nrow(zones_sf)))

zones_valid <- st_make_valid(zones_ch)
amr_valid   <- st_make_valid(amr_sf)
join <- st_join(zones_valid, amr_valid, join = st_intersects, largest = TRUE)
zone_region <- as.data.table(st_drop_geometry(join))[, .(NO, BAE2018, BAE2018_fr)]

unmatched <- which(is.na(zone_region$BAE2018))
if (length(unmatched) > 0) {
  nn <- st_nearest_feature(zones_valid[unmatched, ], amr_valid)
  zone_region[unmatched, `:=`(BAE2018 = amr_valid$BAE2018[nn], BAE2018_fr = amr_valid$BAE2018_fr[nn])]
}
setnames(zone_region, "NO", "npvm_id")
cat(sprintf("Zone -> region mapping: %d zones, %d regions (%d assigned via nearest-fallback, no polygon overlap)\n",
    nrow(zone_region), uniqueN(zone_region$BAE2018), length(unmatched)))

# --- 2. population weights (raw resident population per zone) --------------

pop <- fread("../benzoni_thesis/output/attractivity_indexes.csv",
             select = c("npvm_id", "v02_resident_population"))
setnames(pop, "v02_resident_population", "population")

zone_region <- merge(zone_region, pop, by = "npvm_id", all.x = TRUE)
n_missing_pop <- sum(is.na(zone_region$population))
if (n_missing_pop > 0) {
  cat(sprintf("%d / %d zones missing population (not in Benzoni attractivity indexes) -- treated as weight 0\n",
      n_missing_pop, nrow(zone_region)))
  zone_region[is.na(population), population := 0]
}

region_pop <- zone_region[, .(region_population = sum(population)), by = .(BAE2018, BAE2018_fr)]
cat(sprintf("Region population totals: min %d, median %d, max %d\n",
    min(region_pop$region_population), as.integer(median(region_pop$region_population)), max(region_pop$region_population)))

fwrite(zone_region, "data/output/zone_to_amr101.csv")
fwrite(region_pop, "data/output/amr101_population.csv")
cat("Saved data/output/zone_to_amr101.csv and data/output/amr101_population.csv\n\n")

# --- 3. aggregate EMU (pop-weighted average over destination zones in each region) ---

aggregate_logsum <- function(logsum_file, out_file, label) {
  tt <- as.data.table(read_fst(logsum_file, columns = c("origin_zone", "dest_zone", "logsum")))
  tt <- merge(tt, zone_region[, .(npvm_id, BAE2018, population)],
              by.x = "dest_zone", by.y = "npvm_id", all.x = TRUE)
  n_missing <- sum(is.na(tt$BAE2018))
  if (n_missing > 0) cat(sprintf("  WARNING [%s]: %d rows with unmapped dest_zone dropped\n", label, n_missing))
  tt <- tt[!is.na(BAE2018)]

  agg <- tt[, .(
    logsum = if (sum(population) > 0) sum(logsum * population) / sum(population) else mean(logsum)
  ), by = .(origin_zone, dest_region = BAE2018)]

  write_fst(agg, out_file)
  cat(sprintf("[%s] %d origin zones x %d regions -> %s\n", label, uniqueN(agg$origin_zone), uniqueN(agg$dest_region), out_file))
}

aggregate_logsum("data/output/tt_ausland_CH_logsum.fst",            "data/output/tt_full_logsum_amr101.fst",            "pooled")
aggregate_logsum("data/output/tt_ausland_CH_logsum_tagesreise.fst", "data/output/tt_full_logsum_amr101_tagesreise.fst", "Tagesreise")
aggregate_logsum("data/output/tt_ausland_CH_logsum_reisemitue.fst", "data/output/tt_full_logsum_amr101_reisemitue.fst", "Reise mit Ue")

# --- 4. aggregate Erreichbarkeit (pop-weighted average over zones in each region) ---

err_sf <- st_read("data/erreichbarkeit_tt2023.gpkg", quiet = TRUE)
err_dt <- as.data.table(st_drop_geometry(err_sf))[, .(npvm_id = ID_Zone, erreichbarkeit_avg)]

err_dt <- merge(err_dt, zone_region[, .(npvm_id, BAE2018, population)], by = "npvm_id", all.x = TRUE)
n_missing_err <- sum(is.na(err_dt$BAE2018))
if (n_missing_err > 0) cat(sprintf("WARNING: %d / %d Erreichbarkeit zones unmapped, dropped\n", n_missing_err, nrow(err_dt)))
err_dt <- err_dt[!is.na(BAE2018)]

err_region <- err_dt[, .(
  erreichbarkeit_avg = if (sum(population) > 0) sum(erreichbarkeit_avg * population) / sum(population) else mean(erreichbarkeit_avg)
), by = .(BAE2018)]
err_region[, erreichbarkeit_avg_z := as.numeric(scale(erreichbarkeit_avg))]

fwrite(err_region, "data/output/erreichbarkeit_amr101.csv")
cat(sprintf("Aggregated Erreichbarkeit to %d regions -> data/output/erreichbarkeit_amr101.csv\n", nrow(err_region)))
