library(readxl)
library(dplyr)
library(tidyr)
library(tidygeocoder)
library(sf)
setwd("P:/Verkehrsmodellierung/06_Jobs/188_touristische_Verkehr/Model_trafic_touristique/GitRepo/tourists")

sheet <- 1   # or the sheet name, e.g. "GEO Codes" (your tab name looks cut off)
path = "data/input/csv/PASTA_HESTA.xlsx"
# 1. Find where the header "ID" is
raw <- read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text")
pos <- which(raw == "ID", arr.ind = TRUE)[1, ]
hdr_row <- pos[1]; first_col <- pos[2]

measure <- unlist(raw[hdr_row,     first_col:ncol(raw)])   # ID, Name, ..., Ankünfte, Logiernächte
country <- unlist(raw[hdr_row - 1, first_col:ncol(raw)])   # Ägypten, NA, Argentinien, NA, ...

# 2. Fill each country across its pair of columns
is_meas <- measure %in% c("Ankünfte", "Logiernächte")
country[!is_meas] <- NA
country <- tidyr::fill(data.frame(x = country), x)$x

# 3. Clean names: Ä -> Ae, ü -> ue, é -> e, spaces -> _
clean <- function(x) {
  x <- stringi::stri_trans_general(x, "de-ASCII; Latin-ASCII")
  gsub("[^A-Za-z0-9]+", "_", x)
}
new_names <- ifelse(is_meas,
                    paste(clean(country), clean(measure), sep = "_"),
                    clean(measure))
new_names[new_names == "Koordinate"] <- c("E", "N")   # the two LV95 columns

# 4. Read the data (everything below the header) and apply the names
df <- read_excel(path, sheet = sheet, col_names = FALSE,
                 range = cellranger::cell_limits(
                   c(hdr_row + 1, first_col),
                   c(NA, first_col + length(new_names) - 1)))
names(df) <- new_names


# # --- 0. Proxy (federal network) -------------------------------------------
# # Uses your logged-in Windows account, no password needed
# httr::set_config(httr::use_proxy(
#   url = "http://vip-prx-clients-p.adb.intra.admin.ch",
#   port = 8080,
#   username = "", password = "",
#   auth = "any"
# ))
# 
# # Check the API key is loaded from .Renviron (should not be "")
# stopifnot(Sys.getenv("GOOGLEGEOCODE_API_KEY") != "")
# 
# places_search <- function(q) {
#   res <- POST(
#     "https://places.googleapis.com/v1/places:searchText",
#     add_headers(
#       `X-Goog-Api-Key`   = Sys.getenv("GOOGLEGEOCODE_API_KEY"),
#       `X-Goog-FieldMask` = "places.displayName,places.formattedAddress,places.location"
#     ),
#     body = list(textQuery = q, regionCode = "CH", languageCode = "de"),
#     encode = "json"
#   )
#   p <- if (status_code(res) == 200) content(res)$places else NULL
#   if (length(p) == 0) {
#     return(tibble(p_name = NA, p_address = NA, p_lat = NA_real_, p_lon = NA_real_))
#   }
#   tibble(p_name    = p[[1]]$displayName$text,
#          p_address = p[[1]]$formattedAddress,
#          p_lat     = p[[1]]$location$latitude,
#          p_lon     = p[[1]]$location$longitude)
# }
# 
# places_res <- missing |>
#   mutate(res = map(query, places_search)) |>
#   unnest(res) |>
#   mutate(name_sim = stringsim(tolower(Name), tolower(p_name), method = "jw"))
# 
# saveRDS(places_res, "data/output/csv/hesta_pasta_places_google.rds.rds")
# 
# this is two datasets are creating by quering goggle and then asking claude to check that what google 
# found makes sense so they are not reproducible (the google one is with the code below but should not be re-run 
# for API limits issues)
places_res = readRDS("data/output/csv/hesta_pasta_places_google.rds")
checked <- readr::read_csv("data/input/csv/hesta_places_checked.csv")

places_ok <- places_res |>
  left_join(checked, by = "ID") |>
  filter(!is.na(p_lat)) |>
  select(ID, p_lat, p_lon, ok)

# 2. Convert WGS84 (lat/lon) to LV95 (E/N)
places_lv95 <- places_ok |>
  st_as_sf(coords = c("p_lon", "p_lat"), crs = 4326) |>   # lon first!
  st_transform(2056) |>
  mutate(E_new = st_coordinates(geometry)[, 1],
         N_new = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry()

# 3. Fill the gaps in df
df <- df |>
  left_join(places_lv95, by = "ID") |>
  mutate(coord_source = case_when(!is.na(E) ~ "original",
                                  !is.na(E_new) ~ "google_places",
                                  TRUE ~ NA_character_),
         E = coalesce(E, E_new),
         N = coalesce(N, N_new)) |>
  select(-E_new, -N_new) |>
  filter(!(ok %in% c("wrong", "uncertain"))) |>
  filter(!(is.na(E) | is.na(N)))


# 5. Pivot longer: one row per hotel x country, with Ankuenfte and Logiernaechte as columns
library(tidyr)
library(dplyr)
long <- df |>
  pivot_longer(
    cols = matches("_(Ankuenfte|Logiernaechte)$"),
    names_to = c("Land", ".value"),
    names_pattern = "(.*)_(Ankuenfte|Logiernaechte)"
  ) |>
  filter(!(is.na(Ankuenfte) & is.na(Logiernaechte)))   # drop empty country rows

head(long)

long = long |>
  dplyr::filter(!is.na(E), !is.na(N)) |>  
  dplyr::filter(Land != "Schweiz") |>  
  dplyr::select(ID, Name, Gemeinde, E, N, Land,Ankuenfte, Logiernaechte ) %>% 
  dplyr::mutate(Land = ifelse(Land %in% c("Deutschland","Frankreich", "Italien", "Oesterreich"), Land, "Other")) %>% 
  dplyr::mutate(nationality = dplyr::case_when(
    Land == "Deutschland" ~ 2, 
    Land == "Frankreich" ~ 4,
    Land == "Italien" ~ 5,
    Land == "Oesterreich" ~ 3,
    TRUE ~ 99999999
  )) %>%
  dplyr::group_by(ID, Name, Gemeinde, nationality, E, N) %>% 
  dplyr::summarise(Ankuenfte = sum(Ankuenfte), Logiernaechte = sum(Logiernaechte))

commune = sf::read_sf("data/input/gpkg/communes.gpkg") |>
  dplyr::select(GDENAME, GDEHISTID) |>
  st_transform(2056)                    # make sure both layers use the same CRS

long_sf <- long |>
  dplyr::filter(!is.na(E), !is.na(N)) |>  
  st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE)

long_sf <- long_sf |>
  st_join(commune, join = st_within, left = TRUE)

long_prob = long_sf %>% st_drop_geometry() %>%
  dplyr::group_by(GDENAME, nationality) %>% 
  dplyr::mutate(n_land_commune = sum(Ankuenfte)) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(prop_land_hotel = Ankuenfte/n_land_commune)

long_max = long_sf %>%
  st_drop_geometry() %>%
  dplyr::slice_max(Ankuenfte, n = 1, by = c(GDENAME, nationality), with_ties = FALSE)

fwrite(long, "data/output/csv/hesta_pasta_clean.csv")
fwrite(long_max, "data/output/csv/hesta_pasta_long_max.csv")
fwrite(long_prob, "data/output/csv/hesta_pasta_long_prob.csv")

