# ============================================================
# DOWNLOAD FOREIGN MUNICIPALITY BOUNDARIES  (IT, FR, AT, DE)
#
# SOURCE: Eurostat GISCO LAU 2024, downloaded DIRECTLY as the
#   single pan-European GeoPackage, then filtered to the four
#   countries. No giscoR / httr2 / rlang needed — only base R
#   (download.file) + sf, which you already have.
#
#   https://gisco-services.ec.europa.eu/distribution/v2/lau/gpkg/LAU_RG_01M_2024_4326.gpkg
#
# NOTE: GISCO does NOT publish per-country LAU files; the whole
# EU comes in one file and we subset it ourselves via CNTR_CODE.
#
# GISCO_ID is unique across countries (country code + LAU code),
# e.g. "IT_015146", "FR_13055".
#
# Switzerland is intentionally excluded (you have your own layer).
#
# OUTPUT: zones_ausland_sf
# Columns: zone_id, NO, name, country, country_name, geometry
# ============================================================


# ============================================================
# 0. PACKAGES  (only sf + dplyr — no new dependencies)
# ============================================================

for (p in c("sf", "dplyr")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(sf)
library(dplyr)
library(data.table)


# ============================================================
# 1. SETTINGS
# ============================================================

user = "CP"
if(user == "MR"){setwd("E:/ARE/ProjekteTIE/Turismusverkehr/RScript/tourists")}
if(user == "CP"){setwd("P:/Verkehrsmodellierung/06_Jobs/188_touristische_Verkehr/Model_trafic_touristique/GitRepo/tourists")}

data_dir <- "data/output"
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

agqpv = fread("data/output/agqpv_CHcommune.csv")


lau_year         <- 2024
target_countries <- c("IT", "FR", "AT", "DE")

country_names <- c(
  IT = "Italy",
  FR = "France",
  AT = "Austria",
  DE = "Germany"
)

# Single pan-European LAU GeoPackage in EPSG:4326
lau_url  <- sprintf(
  "https://gisco-services.ec.europa.eu/distribution/v2/lau/gpkg/LAU_RG_01M_%d_4326.gpkg",
  lau_year
)
lau_file      <- file.path(data_dir, sprintf("LAU_RG_01M_%d_4326.gpkg", lau_year))
communes_file <- file.path(data_dir, "communes_ausland.gpkg")


# ============================================================
# 2. BUILD OR LOAD THE FOREIGN COMMUNE LAYER
# ============================================================
#
# If communes_ausland.gpkg already exists, we load it straight
# from disk and skip the download, read, filter and re-save.
# Delete that file (or set rebuild <- TRUE) to force a refresh.
# ============================================================

rebuild <- FALSE   # set TRUE to force re-download even if the file exists

if (!rebuild && file.exists(communes_file)) {
  
  message("Found existing ", communes_file, " — loading it (skipping download).")
  zones_ausland_sf <- st_read(communes_file, quiet = TRUE)
  
} else {
  
  # ----------------------------------------------------------
  # 2a. DOWNLOAD (once)
  #
  # mode="wb" is essential on Windows so the binary .gpkg isn't
  # corrupted. If the file is already on disk we skip the download.
  #
  # If download.file is blocked by your proxy, download the URL by
  # hand in a browser and drop the file at `lau_file` (path printed
  # below) — the script will then skip straight to reading it.
  # ----------------------------------------------------------
  
  message("Target file: ", lau_file)
  
  if (!file.exists(lau_file)) {
    message("Downloading pan-European LAU ", lau_year, " ...")
    message("  ", lau_url)
    download.file(lau_url, lau_file, mode = "wb", quiet = FALSE)
  }
  
  if (!file.exists(lau_file) || file.info(lau_file)$size < 10000) {
    stop("LAU file missing or too small. Download it manually from:\n",
         lau_url, "\nand save it as:\n", lau_file)
  }
  
  
  # ----------------------------------------------------------
  # 2b. READ + FILTER TO TARGET COUNTRIES
  # ----------------------------------------------------------
  
  lau_raw <- st_read(lau_file, quiet = TRUE)
  
  message("Raw LAU fields:")
  print(names(lau_raw))
  
  # Defensive field resolution (GISCO field names vary by release)
  nm <- names(lau_raw)
  id_field   <- intersect(c("GISCO_ID", "gisco_id"), nm)[1]
  name_field <- intersect(c("LAU_NAME", "LAU_LABEL", "lau_name", "NAME"), nm)[1]
  cntr_field <- intersect(c("CNTR_CODE", "cntr_code"), nm)[1]
  # LAU_ID is optional: the 2024 release drops it and embeds the
  # national code inside GISCO_ID (e.g. "IT_015146"). We derive NO
  # from GISCO_ID when no standalone code field is present.
  no_field   <- intersect(c("LAU_ID", "LAU_CODE", "lau_id"), nm)[1]
  
  if (any(is.na(c(id_field, name_field, cntr_field)))) {
    stop("Unexpected fields in LAU file.\nAvailable: ",
         paste(nm, collapse = ", "))
  }
  
  zones_ausland_sf <- lau_raw |>
    filter(.data[[cntr_field]] %in% target_countries) |>
    st_transform(4326) |>
    st_make_valid() |>
    transmute(
      zone_id      = as.character(.data[[id_field]]),
      NO           = if (!is.na(no_field)) as.character(.data[[no_field]])
      else sub("^[A-Z]{2}_", "", as.character(.data[[id_field]])),
      name         = as.character(.data[[name_field]]),
      country      = as.character(.data[[cntr_field]]),
      country_name = unname(country_names[as.character(.data[[cntr_field]])])
    ) |>
    select(zone_id, NO, name, country, country_name)
  
  
  # ----------------------------------------------------------
  # 2c. DUPLICATE CHECK
  # ----------------------------------------------------------
  
  duplicates <- zones_ausland_sf |>
    st_drop_geometry() |>
    count(zone_id) |>
    filter(n > 1)
  
  if (nrow(duplicates) > 0) {
    warning("Duplicate zone IDs found:")
    print(duplicates)
  } else {
    message("No duplicate zone IDs found.")
  }
  
  
  # ----------------------------------------------------------
  # 2d. SUMMARY + SAVE
  # ----------------------------------------------------------
  
  message("\nZones per country:")
  print(
    zones_ausland_sf |>
      st_drop_geometry() |>
      count(country, country_name, name = "n_zones")
  )
  
  message("\nTotal zones: ", nrow(zones_ausland_sf))
  message("CRS: ", st_crs(zones_ausland_sf)$input)
  
  st_write(
    zones_ausland_sf,
    communes_file,
    layer      = "municipalities",
    delete_dsn = TRUE,
    quiet      = TRUE
  )
}


# ============================================================
# 3. ASSIGN FOREIGN POINTS TO COMMUNES
# ============================================================
#
# Assigns origin and residence coordinates to the foreign
# municipality layer. Writes two new columns:
#
#   origin_commune     <- zone_id of the matched foreign commune
#   residence_commune  <- zone_id of the matched foreign commune
#
# Points inside a polygon get that commune; points not inside any
# polygon fall back to the nearest commune. A *_commune_nearest
# flag records which rows used the fallback.
#
# NOTE: assigns against the FOREIGN layer only. Destination points
# are the Swiss side and are intentionally not assigned here.
# ============================================================

assign_commune <- function(lon, lat, zones) {
  
  pts <- st_as_sf(
    data.frame(lon = lon, lat = lat, .row = seq_along(lon)),
    coords  = c("lon", "lat"),
    crs     = 4326,
    na.fail = FALSE
  )
  
  joined <- st_join(pts, zones[, "zone_id"], left = TRUE)
  
  # collapse border double-matches back to one row per input point
  joined <- joined[!duplicated(joined$.row), ]
  joined <- joined[order(joined$.row), ]
  
  nearest_flag <- is.na(joined$zone_id)
  
  na_idx <- which(nearest_flag)
  if (length(na_idx) > 0) {
    has_coord <- !st_is_empty(pts[na_idx, ])
    real_na   <- na_idx[has_coord]
    if (length(real_na) > 0) {
      nearest <- st_nearest_feature(pts[real_na, ], zones)
      joined$zone_id[real_na] <- zones$zone_id[nearest]
    }
    nearest_flag[na_idx] <- has_coord
  }
  
  list(commune = joined$zone_id, nearest = nearest_flag)
}


# ORIGIN
origin_res <- assign_commune(agqpv$origin_long, agqpv$origin_lat, zones_ausland_sf)
agqpv[, origin_commune         := origin_res$commune]
agqpv[, origin_commune_nearest := origin_res$nearest]

# RESIDENCE
residence_res <- assign_commune(agqpv$residence_long, agqpv$residence_lat, zones_ausland_sf)
agqpv[, residence_commune         := residence_res$commune]
agqpv[, residence_commune_nearest := residence_res$nearest]
fwrite(agqpv, "data/output/agqpv.csv")


# CHECK
message("\nForeign commune assignment:")
message("  origin    - assigned: ", sum(!is.na(agqpv$origin_commune)),
        " / ", nrow(agqpv),
        "  (nearest fallback: ", sum(agqpv$origin_commune_nearest, na.rm = TRUE), ")")
message("  residence - assigned: ", sum(!is.na(agqpv$residence_commune)),
        " / ", nrow(agqpv),
        "  (nearest fallback: ", sum(agqpv$residence_commune_nearest, na.rm = TRUE), ")")

message("\nDONE.")