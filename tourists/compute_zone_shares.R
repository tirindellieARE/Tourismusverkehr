# =============================================================================
# Compute P(zone | region): each Swiss NPVM zone's share of its AMR region's
# resident population, used as a proxy for the probability of a zone being
# the destination given that its region was chosen -- so that a region-level
# destination choice probability (from 04_destination_choice_amr.R) can be
# disaggregated back down to individual zones:
#   P(zone) = P(region) * P(zone | region)
#   P(zone | region) = population_zone / sum(population_i), i = zones in region
#
# Reads:
#   data/output/zone_to_amr101.csv -- npvm_id -> BAE2018 region + population
#                                      (mapping and population both built by
#                                      aggregate_regions.R from Benzoni's
#                                      v02_resident_population)
#
# Writes:
#   data/output/zone_prob_given_region.csv -- npvm_id, BAE2018, population,
#                                              region_population, zone_share
# =============================================================================

suppressPackageStartupMessages(library(data.table))

zone_region <- fread("data/output/zone_to_amr101.csv", colClasses = c(BAE2018 = "character"))

zone_region[, region_population := sum(population), by = BAE2018]
zone_region[, zone_share := population / region_population]

# Sanity check: shares must sum to 1 within every region
check <- zone_region[, .(total_share = sum(zone_share)), by = BAE2018]
stopifnot(all(abs(check$total_share - 1) < 1e-9))

cat(sprintf("Computed P(zone | region) for %d zones across %d regions\n",
    nrow(zone_region), uniqueN(zone_region$BAE2018)))
cat(sprintf("Zones with zero population (zone_share = 0): %d / %d\n",
    sum(zone_region$population == 0), nrow(zone_region)))

out <- zone_region[, .(npvm_id, BAE2018, BAE2018_fr, population, region_population, zone_share)]
fwrite(out, "data/output/zone_prob_given_region.csv")
cat("Saved data/output/zone_prob_given_region.csv\n")
