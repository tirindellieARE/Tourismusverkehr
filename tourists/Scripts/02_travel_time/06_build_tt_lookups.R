# =============================================================================
# Build MIV/OeV/foot/bike travel-time + car/PT cost + distance lookup tables
# from the OMX matrices.
#
# MIV (car)          = TTC
# OeV (public trans.) = RITA + EGT + ACT
# (kept separate here, unlike tt_avg_lookup.rds which pre-blends 0.9/0.1)
# Foot                = TT_FGV
# Bike                = TT0_VELO
# Car cost            = MIV_KOS
# PT cost             = OEV_KOS
# Distance (bike)     = DIS_VELO (km -- highly correlated with tt_fgv/tt_velo,
#                        r ~ 0.986, so NOT usable as a distance control for
#                        foot/bike; kept for reference)
# Distance (car)      = DIS_MIV (km -- correlated with tt_miv/tt_oev at
#                        r ~ 0.96/0.90, usable as a distance control for car)
# Distance (PT)       = DIS_OEV (km -- the actual PT route distance, r ~ 0.947
#                        with tt_oev; usable as a distance control for PT)
#
# All three outputs share the same destination side: Swiss zones only
# (MAKROBEZ_STAAT == "CH") -- none of the three ever needs an Ausland/LI zone
# as a destination. tt_agqpv/tt_ausland_CH/tt_CH_CH differ only in which
# origin zones are included, so those three are derived from ONE MIV/OeV
# travel-time computation (origin = every real zone, CH or abroad, x
# destination = CH zones only), filtered three ways -- no redundant OMX
# reads. Foot/bike travel time, car/PT cost and all three distance measures
# are added ONLY to tt_CH_CH: TT_FGV.omx/TT0_VELO.omx/OEV_KOS.omx/
# DIS_VELO.omx/DIS_OEV.omx cover all 7,966 CH zones but only 13/752 abroad
# zones (MIV_KOS.omx/DIS_MIV.omx cover all zones, but are kept CH-only too
# for consistency), too little coverage to be useful for
# tt_agqpv/tt_ausland_CH.
#
# Outputs (fst format - columnar, fast to read, small on disk):
#   data/output/tt_agqpv.fst      only the origin-destination pairs that
#                                 actually occur in agqpv.csv (real
#                                 respondent trips); columns tt_miv, tt_oev
#   data/output/tt_ausland_CH.fst every zone abroad (MAKROBEZ_STAAT != "CH",
#                                 i.e. "Ausland"/"LI") x every Swiss
#                                 destination zone -- the candidate universe
#                                 the destination choice model needs (foreign
#                                 entry point -> Swiss destination); columns
#                                 tt_miv, tt_oev
#   data/output/tt_CH_CH.fst      every Swiss zone x every other Swiss zone --
#                                 domestic zone-to-zone travel times/costs;
#                                 columns tt_miv, tt_oev, tt_fgv (foot),
#                                 tt_velo (bike), cost_car, cost_pt, dist
#                                 (bike km), dist_miv (car km), dist_pt (PT km)
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

# --- foot/bike travel times + car/PT cost, added ONLY to tt_CH_CH -----------
# TT_FGV.omx (foot) uses lookup/NO + data/110; TT0_VELO.omx (bike),
# MIV_KOS.omx (car cost) and OEV_KOS.omx (PT cost) use the OMX default
# naming, lookup/zones + data/matrix -- a different internal schema from
# TTC/RITA/EGT/ACT above, so read directly rather than via open_no(). No
# sentinel-value handling needed for any of these four: unlike car/PT travel
# TIME, most zone pairs have a real foot/bike route and a real, finite
# car/PT cost -- BUT a handful of zones (~2 destination zones, 15,930/63.5M
# tt_CH_CH rows) still carry the same 999999 sentinel in TT_FGV/TT0_VELO/
# DIS_VELO, and 170 rows in DIS_OEV -- an initial version of this script
# assumed (based on a 300-500 zone SAMPLE check) that these four matrices had
# no sentinel values at all, which was wrong; that sample just never hit the
# affected zones. Converted to NA below, same threshold/logic as tt_miv/
# tt_oev above. MIV_KOS covers all zones (CH + abroad) but OEV_KOS covers
# only 13/752 abroad zones -- like TT_FGV/TT0_VELO, too little abroad
# coverage to be useful outside tt_CH_CH, so both costs are added only here
# for now, matching foot/bike.
cat("Reading foot (TT_FGV), bike (TT0_VELO), car cost (MIV_KOS) and PT cost (OEV_KOS) for CH zones...\n")

h_fgv  <- H5File$new("data/input/TT_FGV.omx", mode = "r")
no_fgv <- h_fgv[["lookup/NO"]][]
ri_fgv <- match(ch_zones, no_fgv)
if (any(is.na(ri_fgv))) stop(sprintf("%d CH zone(s) not found in TT_FGV.omx", sum(is.na(ri_fgv))))
mat_fgv <- h_fgv[["data/110"]][ri_fgv, ri_fgv]
h_fgv$close_all()

h_velo  <- H5File$new("data/input/TT0_VELO.omx", mode = "r")
no_velo <- h_velo[["lookup/zones"]][]
ri_velo <- match(ch_zones, no_velo)
if (any(is.na(ri_velo))) stop(sprintf("%d CH zone(s) not found in TT0_VELO.omx", sum(is.na(ri_velo))))
mat_velo <- h_velo[["data/matrix"]][ri_velo, ri_velo]
h_velo$close_all()

h_miv_kos  <- H5File$new("data/input/MIV_KOS.omx", mode = "r")
no_miv_kos <- h_miv_kos[["lookup/zones"]][]
ri_miv_kos <- match(ch_zones, no_miv_kos)
if (any(is.na(ri_miv_kos))) stop(sprintf("%d CH zone(s) not found in MIV_KOS.omx", sum(is.na(ri_miv_kos))))
mat_cost_car <- h_miv_kos[["data/matrix"]][ri_miv_kos, ri_miv_kos]
h_miv_kos$close_all()

h_oev_kos  <- H5File$new("data/input/OEV_KOS.omx", mode = "r")
no_oev_kos <- h_oev_kos[["lookup/zones"]][]
ri_oev_kos <- match(ch_zones, no_oev_kos)
if (any(is.na(ri_oev_kos))) stop(sprintf("%d CH zone(s) not found in OEV_KOS.omx", sum(is.na(ri_oev_kos))))
mat_cost_pt <- h_oev_kos[["data/matrix"]][ri_oev_kos, ri_oev_kos]
h_oev_kos$close_all()

# Straight-network bike distance (km), used as a general zone-to-zone distance
# proxy (not mode-specific) -- e.g. to control for trip length when
# interpreting travel-time coefficients. Same schema/coverage as TT0_VELO.
# NOTE: highly correlated with tt_fgv/tt_velo (r ~ 0.986 on a 250k-pair
# sample) since foot/bike times are themselves ~constant-speed transforms of
# this same network distance -- adding both to the SAME alternative's utility
# is a near-exact collinearity and produced a singular (non-converging)
# model. Kept in the data for reference; not used as a distance control for
# foot/bike in 03_estimate_logsum_CH.R.
h_dis  <- H5File$new("data/input/DIS_VELO.omx", mode = "r")
no_dis <- h_dis[["lookup/zones"]][]
ri_dis <- match(ch_zones, no_dis)
if (any(is.na(ri_dis))) stop(sprintf("%d CH zone(s) not found in DIS_VELO.omx", sum(is.na(ri_dis))))
mat_dist <- h_dis[["data/matrix"]][ri_dis, ri_dis]
h_dis$close_all()

# Car network distance (km) -- less extreme correlation with car/PT time
# (r ~ 0.96 / 0.90 on the same sample) than DIS_VELO has with foot/bike, so
# usable as a distance control specifically for car and PT's utilities.
h_dis_miv  <- H5File$new("data/input/DIS_MIV.omx", mode = "r")
no_dis_miv <- h_dis_miv[["lookup/zones"]][]
ri_dis_miv <- match(ch_zones, no_dis_miv)
if (any(is.na(ri_dis_miv))) stop(sprintf("%d CH zone(s) not found in DIS_MIV.omx", sum(is.na(ri_dis_miv))))
mat_dist_miv <- h_dis_miv[["data/matrix"]][ri_dis_miv, ri_dis_miv]
h_dis_miv$close_all()

# PT network distance (km) -- the actual distance PT trips travel (rail/bus
# routes), as opposed to using the car-network distance as a proxy. r ~ 0.947
# with tt_oev on a 250k-pair sample -- still fairly high, so combined with
# tt_oev in the same utility it's a candidate for some collinearity, though
# less extreme than the foot/bike case.
h_dis_oev  <- H5File$new("data/input/DIS_OEV.omx", mode = "r")
no_dis_oev <- h_dis_oev[["lookup/zones"]][]
ri_dis_oev <- match(ch_zones, no_dis_oev)
if (any(is.na(ri_dis_oev))) stop(sprintf("%d CH zone(s) not found in DIS_OEV.omx", sum(is.na(ri_dis_oev))))
mat_dist_pt <- h_dis_oev[["data/matrix"]][ri_dis_oev, ri_dis_oev]
h_dis_oev$close_all()

mat_fgv[mat_fgv           >= SENTINEL_THRESHOLD] <- NA_real_
mat_velo[mat_velo         >= SENTINEL_THRESHOLD] <- NA_real_
mat_cost_car[mat_cost_car >= SENTINEL_THRESHOLD] <- NA_real_
mat_cost_pt[mat_cost_pt   >= SENTINEL_THRESHOLD] <- NA_real_
mat_dist[mat_dist         >= SENTINEL_THRESHOLD] <- NA_real_
mat_dist_miv[mat_dist_miv >= SENTINEL_THRESHOLD] <- NA_real_
mat_dist_pt[mat_dist_pt   >= SENTINEL_THRESHOLD] <- NA_real_
cat(sprintf("Sentinel values found: tt_fgv=%d tt_velo=%d cost_car=%d cost_pt=%d dist=%d dist_miv=%d dist_pt=%d\n",
    sum(is.na(mat_fgv)), sum(is.na(mat_velo)), sum(is.na(mat_cost_car)), sum(is.na(mat_cost_pt)),
    sum(is.na(mat_dist)), sum(is.na(mat_dist_miv)), sum(is.na(mat_dist_pt))))

n_ch <- length(ch_zones)
tt_ch_ch_extra <- data.table(
  origin_zone = rep(ch_zones, times = n_ch),   # matches column-major as.vector(matrix)
  dest_zone   = rep(ch_zones, each  = n_ch),
  tt_fgv      = as.vector(mat_fgv),
  tt_velo     = as.vector(mat_velo),
  cost_car    = as.vector(mat_cost_car),
  cost_pt     = as.vector(mat_cost_pt),
  dist        = as.vector(mat_dist),
  dist_miv    = as.vector(mat_dist_miv),
  dist_pt     = as.vector(mat_dist_pt)
)
rm(mat_fgv, mat_velo, mat_cost_car, mat_cost_pt, mat_dist, mat_dist_miv, mat_dist_pt); invisible(gc(verbose = FALSE))

tt_ch_ch[tt_ch_ch_extra, on = c("origin_zone", "dest_zone"),
         `:=`(tt_fgv = i.tt_fgv, tt_velo = i.tt_velo, cost_car = i.cost_car, cost_pt = i.cost_pt,
              dist = i.dist, dist_miv = i.dist_miv, dist_pt = i.dist_pt)]
# NA here is expected (and correct) for rows involving the handful of zones
# with a sentinel value above -- not a join failure. Reported as a count, not
# a WARNING.
n_na_extra <- sum(is.na(tt_ch_ch$tt_fgv) | is.na(tt_ch_ch$tt_velo) | is.na(tt_ch_ch$cost_car) |
                  is.na(tt_ch_ch$cost_pt) | is.na(tt_ch_ch$dist) | is.na(tt_ch_ch$dist_miv) | is.na(tt_ch_ch$dist_pt))
cat(sprintf("%d/%d tt_CH_CH rows have an NA (sentinel-derived) tt_fgv/tt_velo/cost_car/cost_pt/dist/dist_miv/dist_pt\n", n_na_extra, nrow(tt_ch_ch)))
cat(sprintf("Added tt_fgv/tt_velo/cost_car/cost_pt/dist/dist_miv/dist_pt to tt_CH_CH (%d rows)\n\n", nrow(tt_ch_ch)))

write_fst(tt_agqpv,      "data/output/tt_agqpv.fst",      compress = 100)
write_fst(tt_ausland_ch, "data/output/tt_ausland_CH.fst", compress = 100)
write_fst(tt_ch_ch,      "data/output/tt_CH_CH.fst",      compress = 100)

cat(sprintf("tt_agqpv.fst      : %d rows                                -> %.2f MB\n",
    nrow(tt_agqpv), file.size("data/output/tt_agqpv.fst") / 1e6))
cat(sprintf("tt_ausland_CH.fst : %d rows, %d origin zones x %d dest zones -> %.2f MB\n",
    nrow(tt_ausland_ch), length(unique(tt_ausland_ch$origin_zone)), length(unique(tt_ausland_ch$dest_zone)),
    file.size("data/output/tt_ausland_CH.fst") / 1e6))
cat(sprintf("tt_CH_CH.fst      : %d rows, %d origin zones x %d dest zones, cols tt_miv/tt_oev/tt_fgv/tt_velo/cost_car/cost_pt/dist/dist_miv/dist_pt -> %.2f MB\n",
    nrow(tt_ch_ch), length(unique(tt_ch_ch$origin_zone)), length(unique(tt_ch_ch$dest_zone)),
    file.size("data/output/tt_CH_CH.fst") / 1e6))
