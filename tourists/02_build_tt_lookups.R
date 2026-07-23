# =============================================================================
# Build MIV/OeV travel-time lookup tables from the OMX matrices.
#
# MIV (car)          = TTC
# OeV (public trans.) = RITA + EGT + ACT
# (kept separate here, unlike tt_avg_lookup.rds which pre-blends 0.9/0.1)
#
# All three outputs share the same destination side: Swiss zones only
# (MAKROBEZ_STAAT == "CH") -- none of the three ever needs an Ausland/LI zone
# as a destination. They differ only in which origin zones are included, so
# all three are derived from ONE travel-time computation (origin = every real
# zone, CH or abroad, x destination = CH zones only), filtered three ways --
# no redundant OMX reads.
#
# Outputs (fst format - columnar, fast to read, small on disk):
#   data/output/tt_agqpv.fst      only the origin-destination pairs that
#                                 actually occur in agqpv.csv (real
#                                 respondent trips)
#   data/output/tt_ausland_CH.fst every zone abroad (MAKROBEZ_STAAT != "CH",
#                                 i.e. "Ausland"/"LI") x every Swiss
#                                 destination zone -- the candidate universe
#                                 the destination choice model needs (foreign
#                                 entry point -> Swiss destination)
#   data/output/tt_CH_CH.fst      every Swiss zone x every other Swiss zone --
#                                 domestic zone-to-zone travel times
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(hdf5r)
  library(fst)
})

dir.create("data/output", recursive = TRUE, showWarnings = FALSE)

agqpv <- fread("data/output/agqpv.csv")
zones_sf <- st_read("data/input/zones_communes.gpkg", quiet = TRUE)
zone_country <- as.data.table(st_drop_geometry(zones_sf))[, .(NO, MAKROBEZ_STAAT)]

ch_zones     <- zone_country[MAKROBEZ_STAAT == "CH" & !is.na(MAKROBEZ_STAAT), NO]
abroad_zones <- zone_country[MAKROBEZ_STAAT != "CH" & !is.na(MAKROBEZ_STAAT), NO]  # "Ausland" + "LI"
orig_zones   <- sort(c(ch_zones, abroad_zones))  # every real zone -- origin universe for all three outputs

pairs_needed <- unique(agqpv[, .(origin_zone, dest_zone)])
cat(sprintf("Unique origin-destination pairs in agqpv.csv: %d\n", nrow(pairs_needed)))
cat(sprintf("CH zones: %d | Abroad zones (Ausland+LI): %d | Origin universe: %d\n",
    length(ch_zones), length(abroad_zones), length(orig_zones)))

open_no <- function(path) {
  h  <- H5File$new(path, mode = "r")
  no <- h[["lookup/NO"]][]
  h$close_all()
  no
}
no_ttc  <- open_no("data/input/TTC.omx")
no_rita <- open_no("data/input/RITA.omx")
no_egt  <- open_no("data/input/EGT.omx")
no_act  <- open_no("data/input/ACT.omx")

# Destination side is CH zones only, intersected with matrix coverage
dest_zones <- ch_zones[ch_zones %in% no_ttc &
                        ch_zones %in% no_rita &
                        ch_zones %in% no_egt  &
                        ch_zones %in% no_act]
cat(sprintf("Origin zones (to process): %d\nDest zones (CH, matrix coverage): %d\n",
    length(orig_zones), length(dest_zones)))

ci_ttc  <- match(dest_zones, no_ttc)
ci_rita <- match(dest_zones, no_rita)
ci_egt  <- match(dest_zones, no_egt)
ci_act  <- match(dest_zones, no_act)

# Drop any origin zone not covered by all four matrices BEFORE the bulk read
# (fancy-indexing an HDF5 dataset with an NA row index errors out).
ri_ttc_all  <- match(orig_zones, no_ttc)
ri_rita_all <- match(orig_zones, no_rita)
ri_egt_all  <- match(orig_zones, no_egt)
ri_act_all  <- match(orig_zones, no_act)
ok <- !is.na(ri_ttc_all) & !is.na(ri_rita_all) & !is.na(ri_egt_all) & !is.na(ri_act_all)
if (any(!ok)) {
  cat(sprintf("WARNING: %d origin zone(s) not found in OMX matrices, excluded: %s\n",
      sum(!ok), paste(orig_zones[!ok], collapse = ",")))
  orig_zones  <- orig_zones[ok]
  ri_ttc_all  <- ri_ttc_all[ok]
  ri_rita_all <- ri_rita_all[ok]
  ri_egt_all  <- ri_egt_all[ok]
  ri_act_all  <- ri_act_all[ok]
}

# One bulk read per matrix (origin rows x dest columns) instead of looping
# origin-by-origin -- a single HDF5 hyperslab read is vastly faster than
# thousands of individual row reads, which re-touch compressed chunks on
# every call and do not scale past a few hundred origins.
cat(sprintf("Reading %d x %d submatrices from each OMX file...\n", length(orig_zones), length(dest_zones)))
h_ttc  <- H5File$new("data/input/TTC.omx",  mode = "r")
mat_ttc <- h_ttc[["data/131"]][ri_ttc_all, ci_ttc]
h_ttc$close_all()

h_rita  <- H5File$new("data/input/RITA.omx", mode = "r")
mat_rita <- h_rita[["data/141"]][ri_rita_all, ci_rita]
h_rita$close_all()

h_egt  <- H5File$new("data/input/EGT.omx",  mode = "r")
mat_egt <- h_egt[["data/143"]][ri_egt_all, ci_egt]
h_egt$close_all()

h_act  <- H5File$new("data/input/ACT.omx",  mode = "r")
mat_act <- h_act[["data/142"]][ri_act_all, ci_act]
h_act$close_all()

# OMX encodes "no path found" / unreachable OD pairs with a sentinel value of
# 999999 (per leg -- so tt_oev for a fully-unreachable PT trip sums to
# 2999997). These are placeholders, not real travel times, and must not be
# treated as a genuine (huge) cost -- fed raw into the mode-choice utility
# they produced logsum values as extreme as -3,695 for the pooled model, vs.
# a normal range of roughly [-1, 1]. Converted to NA per matrix (per mode),
# not per combined tt_oev, since car and PT are not always unreachable
# together: 71,694 / 5.99M tt_ausland_CH rows have only car unreachable
# (PT still fine), 15 have only PT unreachable -- compute_logsum() in
# 03_estimate_logsum.R treats a single missing mode as "excluded from the
# logsum", not "whole OD pair missing".
SENTINEL_THRESHOLD <- 900000
mat_ttc[mat_ttc   >= SENTINEL_THRESHOLD] <- NA_real_
mat_rita[mat_rita >= SENTINEL_THRESHOLD] <- NA_real_
mat_egt[mat_egt   >= SENTINEL_THRESHOLD] <- NA_real_
mat_act[mat_act   >= SENTINEL_THRESHOLD] <- NA_real_

cat("Read complete, assembling long-format table...\n")
n_orig <- length(orig_zones)
n_dest <- length(dest_zones)
tt_all <- data.table(
  origin_zone = rep(orig_zones, times = n_dest),   # matches column-major as.vector(matrix)
  dest_zone   = rep(dest_zones, each  = n_orig),
  tt_miv      = as.vector(mat_ttc),
  tt_oev      = as.vector(mat_rita) + as.vector(mat_egt) + as.vector(mat_act)
)
cat(sprintf("Unreachable (sentinel) car legs: %d | PT legs (any leg): %d\n",
    sum(is.na(tt_all$tt_miv)), sum(is.na(tt_all$tt_oev))))
rm(mat_ttc, mat_rita, mat_egt, mat_act); invisible(gc(verbose = FALSE))

setkey(tt_all, origin_zone, dest_zone)
cat(sprintf("Master travel-time table: %d rows (%d origin zones x %d CH dest zones)\n\n",
    nrow(tt_all), length(unique(tt_all$origin_zone)), length(dest_zones)))

# --- derive the three outputs from tt_all -- no extra OMX reads needed -----

tt_agqpv <- tt_all[pairs_needed, on = c("origin_zone", "dest_zone")]
n_na <- sum(is.na(tt_agqpv$tt_miv))
if (n_na > 0) cat(sprintf("WARNING: %d/%d agqpv OD pairs missing from tt_all\n", n_na, nrow(tt_agqpv)))

tt_ausland_ch <- tt_all[origin_zone %in% abroad_zones]
tt_ch_ch      <- tt_all[origin_zone %in% ch_zones]

write_fst(tt_agqpv,      "data/output/tt_agqpv.fst",      compress = 100)
write_fst(tt_ausland_ch, "data/output/tt_ausland_CH.fst", compress = 100)
write_fst(tt_ch_ch,      "data/output/tt_CH_CH.fst",      compress = 100)

cat(sprintf("tt_agqpv.fst      : %d rows                                -> %.2f MB\n",
    nrow(tt_agqpv), file.size("data/output/tt_agqpv.fst") / 1e6))
cat(sprintf("tt_ausland_CH.fst : %d rows, %d origin zones x %d dest zones -> %.2f MB\n",
    nrow(tt_ausland_ch), length(unique(tt_ausland_ch$origin_zone)), length(unique(tt_ausland_ch$dest_zone)),
    file.size("data/output/tt_ausland_CH.fst") / 1e6))
cat(sprintf("tt_CH_CH.fst      : %d rows, %d origin zones x %d dest zones -> %.2f MB\n",
    nrow(tt_ch_ch), length(unique(tt_ch_ch$origin_zone)), length(unique(tt_ch_ch$dest_zone)),
    file.size("data/output/tt_CH_CH.fst") / 1e6))
