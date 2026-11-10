# ============================================================
# TOURIST ORIGIN / RESIDENCE MAPS
# ============================================================

library(sf)
library(data.table)
library(ggplot2)
library(viridis)

user = "CP"
if(user == "MR"){setwd("E:/ARE/ProjekteTIE/Turismusverkehr/RScript/tourists")}
if(user == "CP"){setwd("P:/Verkehrsmodellierung/06_Jobs/188_touristische_Verkehr/Model_trafic_touristique/GitRepo/tourists")}

dir.create("data/output", recursive = TRUE, showWarnings = FALSE)

agqpv = fread("data/output/agqpv_communes.csv")

ZONES_FILE <- "data/output/communes_ausland.gpkg"
zones_ausland_sf   <- st_read(ZONES_FILE, quiet = TRUE)

# ============================================================
# 1. FUNCTION TO CREATE ZONE COUNTS
# ============================================================
#
# Uses the survey/expansion weight to estimate the number
# of tourists per zone.
#
# If you want raw observations instead, replace:
#     sum(weight, na.rm = TRUE)
# with:
#     .N
#
# ============================================================

# ============================================================
# 1. COUNTING HELPER
# ============================================================
#
# Counts tourists per foreign commune (zones_ausland_sf).
#
# zone_var    : the column in agqpv holding the commune id
#               (origin_commune / residence_commune). These hold
#               GISCO zone_id values like "IT_015146".
# nearest_var : optional flag column (origin_commune_nearest /
#               residence_commune_nearest). When supplied, rows
#               assigned via nearest-fallback (TRUE) are excluded.
#
# The grouping column is renamed to zone_id so it joins directly
# to zones_ausland_sf.
# ============================================================

zone_counts <- function(data, zone_var, nearest_var = NULL) {
  
  # Keep only observations with a valid commune
  d <- data[!is.na(get(zone_var))]
  
  # Optionally exclude observations assigned to nearest commune
  if (!is.null(nearest_var)) {
    d <- d[is.na(get(nearest_var)) | get(nearest_var) == FALSE]
  }
  
  # Number of tourists per commune
  counts <- d[
    ,
    .(n_tourists = .N),
    by = zone_var
  ]
  
  # Rename to zone_id so it can be joined to zones_ausland_sf
  setnames(counts, zone_var, "zone_id")
  
  return(counts)
}


# ============================================================
# 2. ALL TOURISTS
# ============================================================

# ----------------------------
# Origin
# ----------------------------

origin_all <- zone_counts(
  agqpv,
  "origin_commune"
)

zones_origin_all <- zones_ausland_sf[
  ,
  c("zone_id")
] |>
  left_join(origin_all, by = "zone_id")
zones_origin_all = na.omit(zones_origin_all)

# ----------------------------
# Residence
# ----------------------------

residence_all <- zone_counts(
  agqpv,
  "residence_commune"
)

zones_residence_all <- zones_ausland_sf[
  ,
  c("zone_id")
] |>
  left_join(residence_all, by = "zone_id")
zones_residence_all = na.omit(zones_residence_all)


# ============================================================
# 3. ONLY OBSERVATIONS ACTUALLY INSIDE THEIR ASSIGNED COMMUNE
# ============================================================

# ----------------------------
# Origin
# ----------------------------

origin_inside <- zone_counts(
  agqpv,
  "origin_commune",
  "origin_commune_nearest"
)

zones_origin_inside <- zones_ausland_sf[
  ,
  c("zone_id")
] |>
  left_join(origin_inside, by = "zone_id")


# ----------------------------
# Residence
# ----------------------------

residence_inside <- zone_counts(
  agqpv,
  "residence_commune",
  "residence_commune_nearest"
)

zones_residence_inside <- zones_ausland_sf[
  ,
  c("zone_id")
] |>
  left_join(residence_inside, by = "zone_id")


# ============================================================
# 4. COMMON SCALE FOR THE HEAT MAPS
# ============================================================
#
# This makes the colour scale comparable between:
#   - all tourists
#   - tourists actually inside their commune
#
# ============================================================

max_value <- max(
  c(
    zones_origin_all$n_tourists,
    zones_residence_all$n_tourists,
    zones_origin_inside$n_tourists,
    zones_residence_inside$n_tourists
  ),
  na.rm = TRUE
)


# ============================================================
# HEAT MAP FUNCTION
# ============================================================

make_heatmap <- function(
    zones,
    title,
    subtitle = NULL,
    max_value = NULL
) {
  
  p <- ggplot(zones) +
    
    geom_sf(
      aes(fill = n_tourists),
      color = "white",
      linewidth = 0.1
    ) +
    
    scale_fill_gradient(
      low = "white",
      high = "darkblue",
      na.value = "grey90",
      name = "Tourists",
      limits = if (!is.null(max_value)) c(0, max_value) else NULL
    ) +
    
    labs(
      title = title,
      subtitle = subtitle
    ) +
    
    theme_void() +
    
    theme(
      plot.title = element_text(
        size = 16,
        face = "bold"
      ),
      plot.subtitle = element_text(
        size = 11
      ),
      legend.position = "right"
    )
  
  return(p)
}


# ============================================================
# 6. MAP 1 — ALL TOURISTS: ORIGIN
# ============================================================

map_origin_all <- make_heatmap(
  zones_origin_all,
  title = "Tourists by Origin Commune (abroad)",
  subtitle = "All tourists",
  max_value = max_value
)

print(map_origin_all)


# ============================================================
# 7. MAP 2 — ALL TOURISTS: RESIDENCE
# ============================================================

map_residence_all <- make_heatmap(
  zones_residence_all,
  title = "Tourists by Residence Commune (abroad)",
  subtitle = "All tourists",
  max_value = max_value
)

print(map_residence_all)


# ============================================================
# 8. MAP 3 — ACTUALLY INSIDE COMMUNE: ORIGIN
# ============================================================

map_origin_inside <- make_heatmap(
  zones_origin_inside,
  title = "Tourists by Origin Commune (abroad)",
  subtitle = "Only tourists whose coordinates fall inside the assigned commune",
  max_value = max_value
)

print(map_origin_inside)


# ============================================================
# 9. MAP 4 — ACTUALLY INSIDE COMMUNE: RESIDENCE
# ============================================================

map_residence_inside <- make_heatmap(
  zones_residence_inside,
  title = "Tourists by Residence Commune (abroad)",
  subtitle = "Only tourists whose coordinates fall inside the assigned commune",
  max_value = max_value
)

print(map_residence_inside)


# ============================================================
# 10. POINT MAPS — NEAREST COMMUNE ASSIGNMENTS
# ============================================================
#
# These are the observations where:
#
#   *_commune_nearest == TRUE
#
# The points themselves are plotted using their original
# longitude/latitude coordinates.
#
# ============================================================


# ------------------------------------------------------------
# Origin points assigned to nearest commune
# ------------------------------------------------------------

origin_nearest <- agqpv[
  !is.na(origin_commune) &
    origin_commune_nearest == TRUE &
    !is.na(origin_long) &
    !is.na(origin_lat)
]

origin_nearest_sf <- st_as_sf(
  origin_nearest,
  coords = c("origin_long", "origin_lat"),
  crs = 4326,
  remove = FALSE
)

# Transform to same CRS as the foreign commune layer
origin_nearest_sf <- st_transform(
  origin_nearest_sf,
  st_crs(zones_ausland_sf)
)


# ------------------------------------------------------------
# Residence points assigned to nearest commune
# ------------------------------------------------------------

residence_nearest <- agqpv[
  !is.na(residence_commune) &
    residence_commune_nearest == TRUE &
    !is.na(residence_long) &
    !is.na(residence_lat)
]

residence_nearest_sf <- st_as_sf(
  residence_nearest,
  coords = c("residence_long", "residence_lat"),
  crs = 4326,
  remove = FALSE
)

# Transform to same CRS as the foreign commune layer
residence_nearest_sf <- st_transform(
  residence_nearest_sf,
  st_crs(zones_ausland_sf)
)


# ============================================================
# 11. MAP 5 — ORIGIN POINTS ASSIGNED TO NEAREST COMMUNE
# ============================================================

map_origin_nearest <- ggplot() +
  
  # Commune boundaries
  geom_sf(
    data = zones_ausland_sf,
    fill = NA,
    color = "grey70",
    linewidth = 0.2
  ) +
  
  # Original coordinate
  geom_sf(
    data = origin_nearest_sf,
    size = 1.5,
    alpha = 0.6
  ) +
  
  labs(
    title = "Origin Coordinates Assigned to Nearest Commune",
    subtitle = "Points where origin_commune_nearest == TRUE",
    caption = paste(
      "Number of observations:",
      nrow(origin_nearest_sf)
    )
  ) +
  
  theme_void() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 11
    ),
    plot.caption = element_text(
      size = 9
    )
  )

print(map_origin_nearest)


# ============================================================
# 12. MAP 6 — RESIDENCE POINTS ASSIGNED TO NEAREST COMMUNE
# ============================================================

map_residence_nearest <- ggplot() +
  
  # Commune boundaries
  geom_sf(
    data = zones_ausland_sf,
    fill = NA,
    color = "grey70",
    linewidth = 0.2
  ) +
  
  # Original coordinate
  geom_sf(
    data = residence_nearest_sf,
    size = 1.5,
    alpha = 0.6
  ) +
  
  labs(
    title = "Residence Coordinates Assigned to Nearest Commune",
    subtitle = "Points where residence_commune_nearest == TRUE",
    caption = paste(
      "Number of observations:",
      nrow(residence_nearest_sf)
    )
  ) +
  
  theme_void() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 11
    ),
    plot.caption = element_text(
      size = 9
    )
  )

print(map_residence_nearest)
# ============================================================
# 13. OPTIONAL: SAVE ALL SIX MAPS
# ============================================================

# Uncomment these if you want PNG files.

# ggsave(
#   "tourists_origin_all.png",
#   map_origin_all,
#   width = 10,
#   height = 8,
#   dpi = 300
# )

# ggsave(
#   "tourists_residence_all.png",
#   map_residence_all,
#   width = 10,
#   height = 8,
#   dpi = 300
# )

# ggsave(
#   "tourists_origin_inside.png",
#   map_origin_inside,
#   width = 10,
#   height = 8,
#   dpi = 300
# )

# ggsave(
#   "tourists_residence_inside.png",
#   map_residence_inside,
#   width = 10,
#   height = 8,
#   dpi = 300
# )

# ggsave(
#   "tourists_origin_nearest.png",
#   map_origin_nearest,
#   width = 10,
#   height = 8,
#   dpi = 300
# )

# ggsave(
#   "tourists_residence_nearest.png",
#   map_residence_nearest,
#   width = 10,
#   height = 8,
#   dpi = 300
# )

