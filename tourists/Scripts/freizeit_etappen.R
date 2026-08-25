setwd("P:/Verkehrsmodellierung/06_Jobs/188_touristische_Verkehr/Model_trafic_touristique")
etappen = fread("data/MRMT2015/CSV/etappen.csv")
zp = fread("data/MRMT2015/CSV/zielpersonen.csv")

zones_sf = st_read("GitRepo/tourists/data/input/zones_communes.gpkg", quiet = TRUE)
n_zones_before <- nrow(zones_sf)
zones_sf <- zones_sf[!is.na(zones_sf$MAKROBEZ_STAAT), ]

zp = zp[zp$ERWERB==4,]
zp = zp[zp$alter>65,]

freizeit_etappen = etappen[f52900==8 & HHNR %in% unique(zp$HHNR),]
assign_zones = function(lon, lat, zones) {
  pts = st_as_sf(
    data.frame(lon = lon, lat = lat),
    coords = c("lon", "lat"),
    crs    = 4326
  )
  pts    = st_transform(pts, st_crs(zones))
  joined = st_join(pts, zones[, "NO"], left = TRUE)
  
  # Fallback: snap unmatched points to the nearest zone polygon
  # na_idx <- which(is.na(joined$NO))
  # if (length(na_idx) > 0) {
  #   nearest        <- st_nearest_feature(pts[na_idx, ], zones)
  #   joined$NO[na_idx] <- zones$NO[nearest]
  # }
  
  joined$NO
}

freizeit_etappen[, origin_zone := assign_zones(S_X, S_Y, zones_sf)]
freizeit_etappen[, dest_zone      := assign_zones(Z_X, Z_Y, zones_sf)]
freizeit_etappen = freizeit_etappen[, c("HHNR", "f51300", "origin_zone", "dest_zone")]
freizeit_etappen = dplyr::left_join(freizeit_etappen, sf::st_drop_geometry(zones_sf[c("NO", "MAKROBEZ_STAAT", "GDENR", "GDENAME")]), 
                                    by = c("origin_zone" = "NO"))
setnames(freizeit_etappen, c("MAKROBEZ_STAAT", "GDENR", "GDENAME"), paste0("origin_", c("MAKROBEZ_STAAT", "GDENR", "GDENAME")))
freizeit_etappen = dplyr::left_join(freizeit_etappen, sf::st_drop_geometry(zones_sf[c("NO", "MAKROBEZ_STAAT", "GDENR", "GDENAME")]), 
                                    by = c("dest_zone" = "NO"))
setnames(freizeit_etappen, c("MAKROBEZ_STAAT", "GDENR", "GDENAME"), paste0("dest_", c("MAKROBEZ_STAAT", "GDENR", "GDENAME")))
freizeit_etappen = dplyr::filter(freizeit_etappen, origin_MAKROBEZ_STAAT =="CH" & dest_MAKROBEZ_STAAT =="CH")

N_etappen_distr = freizeit_etappen %>% dplyr::group_by(HHNR) %>% dplyr::summarise(n_etappen = dplyr::n())
N_etappen_distr = N_etappen_distr %>% dplyr::group_by(n_etappen) %>% dplyr::summarise(n = dplyr::n())

EMU_full <- as.data.table(read_fst("GitRepo/tourists/data/output/tt_CH_CH_logsum.fst"))
freizeit_etappen = merge.data.table(freizeit_etappen, EMU_full, by = c("origin_zone", "dest_zone"))
freizeit_etappen = freizeit_etappen[, mode := dplyr::case_when(
  f51300 %in% c(7,8) ~ "car", 
  f51300 %in% c(9:12) ~ "pt", 
  f51300 == 1 ~ "walk",
  f51300 == 2 ~ "bike",
  TRUE ~ "other"
)]


fwrite(freizeit_etappen, "GitRepo/tourists/data/output/freizeit_etappen.csv")

mc15 = fread("GitRepo/tourists/data/MC15.csv")
