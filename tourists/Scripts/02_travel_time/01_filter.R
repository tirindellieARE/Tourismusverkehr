# ============================================================
# 01_filter.R
# 1. Read the raw trip CSV (Latin-1).
# 2. Drop rows whose origin or residence is not abroad, using the
#    pre-computed origin_zone_country / residence_zone_country columns.
# 3. Extract unique origin -> destination pairs (the routing workload).
# 4. Write pairs_to_route.csv and row_to_pair.csv.
#
# NOTE ON NAMING: the routing target here is dest_lat/dest_long -- the
# tourist's real reported destination in Switzerland (verified: median ~63km
# from the actual border crossing, not the same point). `grenz` is kept as a
# separate label column (which border crossing this trip used) -- it's real,
# useful metadata, just not a coordinate, and must never be used as one.
#
# Run:  source("R/00_setup.R"); source("R/01_filter.R")
# ============================================================

user = "MR"
if(user == "MR"){setwd("E:/ARE/ProjekteTIE/Turismusverkehr/RScript/tourists")}
if(user == "CP"){setwd("P:/Verkehrsmodellierung/06_Jobs/188_touristische_Verkehr/Model_trafic_touristique/GitRepo/tourists")}

source("Scripts/02_travel_time/00_setup.R")

# --- Read the raw trip data --------------------------------------------------
cn <- cfg$cols
raw <- readr::read_delim(
  cfg$input_csv,
  delim = ",",
  locale = readr::locale(encoding = cfg$input_encoding),
  show_col_types = FALSE
)
raw$.orig_row_index <- seq_len(nrow(raw)) - 1L   # 0-based, matches earlier work
log_msg(sprintf("Read %d rows from %s", nrow(raw), cfg$input_csv))

ext = raw #this is just because before we were filtering out non swiss residents here while now it is done in the agent generation step

# --- Build unique origin -> destination pairs --------------------------------
ext <- ext |>
  dplyr::mutate(
    .pair_key = make_pair_key(
      .data[[cn$origin_lat]], .data[[cn$origin_long]],
      .data[[cn$dest_lat]],   .data[[cn$dest_long]]
    )
  )

pairs <- ext |>
  dplyr::group_by(.pair_key) |>
  dplyr::summarise(
    origin_lat  = dplyr::first(.data[[cn$origin_lat]]),
    origin_long = dplyr::first(.data[[cn$origin_long]]),
    dest_lat    = dplyr::first(.data[[cn$dest_lat]]),
    dest_long   = dplyr::first(.data[[cn$dest_long]]),
    n_rows      = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(origin_lat, origin_long, dest_lat, dest_long) |>
  dplyr::mutate(pair_id = sprintf("P%05d", dplyr::row_number() - 1L)) |>
  dplyr::select(pair_id, origin_lat, origin_long, dest_lat, dest_long,
                n_rows, .pair_key)

log_msg(sprintf("Unique origin -> destination pairs: %d", nrow(pairs)))

# --- Row-to-pair mapping (every external row -> its pair_id) -----------------
row_map <- ext |>
  dplyr::left_join(dplyr::select(pairs, pair_id, .pair_key), by = ".pair_key") |>
  dplyr::transmute(
    orig_row_index = .orig_row_index,
    pair_id        = pair_id,
    origin_lat     = .data[[cn$origin_lat]],
    origin_long    = .data[[cn$origin_long]],
    dest_lat       = .data[[cn$dest_lat]],
    dest_long      = .data[[cn$dest_long]],
    border_mode    = .data[[cn$border_mode]],
    dest_zone      = .data[[cn$dest_zone]],
    weight         = .data[[cn$weight]]
  )

# --- Write outputs -----------------------------------------------------------
readr::write_csv(dplyr::select(pairs, -.pair_key), cfg$out$pairs)
readr::write_csv(row_map, cfg$out$row_map)
log_msg(sprintf("Wrote %s (%d rows) and %s (%d rows)",
                cfg$out$pairs, nrow(pairs), cfg$out$row_map, nrow(row_map)))

log_msg("01_filter.R done.")
