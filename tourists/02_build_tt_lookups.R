# =============================================================================
# Build MIV/OeV travel-time lookup tables from the OMX matrices, keyed off the
# origin zones present in data/agqpv.csv.
#
# MIV (car)          = TTC
# OeV (public trans.) = RITA + EGT + ACT
# (kept separate here, unlike tt_avg_lookup.rds which pre-blends 0.9/0.1)
#
# Outputs (fst format - columnar, fast to read, small on disk):
#   data/tt_full.fst   origin_zone (all agqpv origins) x all reachable dest_zone
#   data/tt_light.fst  only the origin-destination pairs that actually occur
#                      in agqpv.csv
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(hdf5r)
  library(fst)
})

agqpv <- fread("data/agqpv.csv")
zones_sf <- st_read("data/zones_communes.gpkg", quiet = TRUE)
zone_attrs <- as.data.table(st_drop_geometry(zones_sf))[, .(NO)]
all_zones  <- zone_attrs$NO

orig_zones <- sort(unique(agqpv$origin_zone))
cat(sprintf("Unique origin zones in agqpv.csv: %d\n", length(orig_zones)))

pairs_needed <- unique(agqpv[, .(origin_zone, dest_zone)])
cat(sprintf("Unique origin-destination pairs in agqpv.csv: %d\n", nrow(pairs_needed)))

open_no <- function(path) {
  h  <- H5File$new(path, mode = "r")
  no <- h[["lookup/NO"]][]
  h$close_all()
  no
}
no_ttc  <- open_no("data/TTC.omx")
no_rita <- open_no("data/RITA.omx")
no_egt  <- open_no("data/EGT.omx")
no_act  <- open_no("data/ACT.omx")

dest_zones <- all_zones[all_zones %in% no_ttc &
                         all_zones %in% no_rita &
                         all_zones %in% no_egt  &
                         all_zones %in% no_act]
cat(sprintf("Origin zones (to process): %d\nDest zones (matrix coverage): %d\n",
    length(orig_zones), length(dest_zones)))

ci_ttc  <- match(dest_zones, no_ttc)
ci_rita <- match(dest_zones, no_rita)
ci_egt  <- match(dest_zones, no_egt)
ci_act  <- match(dest_zones, no_act)

h_ttc  <- H5File$new("data/TTC.omx",  mode = "r")
h_rita <- H5File$new("data/RITA.omx", mode = "r")
h_egt  <- H5File$new("data/EGT.omx",  mode = "r")
h_act  <- H5File$new("data/ACT.omx",  mode = "r")

missing_origin <- character(0)
tt_list <- vector("list", length(orig_zones))
for (i in seq_along(orig_zones)) {
  oz      <- orig_zones[i]
  ri_ttc  <- match(oz, no_ttc)
  ri_rita <- match(oz, no_rita)
  ri_egt  <- match(oz, no_egt)
  ri_act  <- match(oz, no_act)

  if (is.na(ri_ttc) || is.na(ri_rita) || is.na(ri_egt) || is.na(ri_act)) {
    missing_origin <- c(missing_origin, as.character(oz))
    next
  }

  ttc  <- h_ttc[["data/131"]][ri_ttc,  ][ci_ttc]
  rita <- h_rita[["data/141"]][ri_rita, ][ci_rita]
  egt  <- h_egt[["data/143"]][ri_egt,  ][ci_egt]
  act  <- h_act[["data/142"]][ri_act,  ][ci_act]

  tt_list[[i]] <- data.table(
    origin_zone = oz,
    dest_zone   = dest_zones,
    tt_miv      = ttc,
    tt_oev      = rita + egt + act
  )
  if (i %% 50 == 0) cat(sprintf("  %d / %d origin zones processed\n", i, length(orig_zones)))
}

h_ttc$close_all(); h_rita$close_all(); h_egt$close_all(); h_act$close_all()

if (length(missing_origin) > 0) {
  cat(sprintf("WARNING: %d origin zone(s) not found in OMX matrices, excluded: %s\n",
      length(missing_origin), paste(missing_origin, collapse = ",")))
}

tt_full <- rbindlist(tt_list)
setkey(tt_full, origin_zone, dest_zone)

tt_light <- tt_full[pairs_needed, on = c("origin_zone", "dest_zone")]
n_na <- sum(is.na(tt_light$tt_miv))
if (n_na > 0) cat(sprintf("WARNING: %d/%d agqpv OD pairs missing from tt_full\n", n_na, nrow(tt_light)))

write_fst(tt_full,  "data/tt_full.fst",  compress = 100)
write_fst(tt_light, "data/tt_light.fst", compress = 100)

cat(sprintf("\ntt_full.fst  : %d rows, %d origin zones -> %.2f MB\n",
    nrow(tt_full), length(unique(tt_full$origin_zone)), file.size("data/tt_full.fst") / 1e6))
cat(sprintf("tt_light.fst : %d rows                    -> %.2f MB\n",
    nrow(tt_light), file.size("data/tt_light.fst") / 1e6))
