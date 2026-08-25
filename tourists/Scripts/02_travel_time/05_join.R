# ============================================================
# 05_join.R
# Merge car + PT times onto the unique pairs, then expand back to every
# observation via row_to_pair. Produces the final mode-choice model input.
#
# Run:  source("Scripts/02_travel_time/00_setup.R"); source("Scripts/02_travel_time/05_join.R")
# ============================================================

source("Scripts/02_travel_time/00_setup.R")

# --- Load pieces -------------------------------------------------------------
pairs   <- readr::read_csv(cfg$out$pairs,     show_col_types = FALSE)
row_map <- readr::read_csv(cfg$out$row_map,   show_col_types = FALSE)

car <- if (file.exists(cfg$out$car_times)) {
  readr::read_csv(cfg$out$car_times, show_col_types = FALSE) |>
    dplyr::select(pair_id, car_time_min, car_dist_km)
} else {
  log_msg("WARNING: car_times.csv missing; car columns will be NA.")
  dplyr::tibble(pair_id = pairs$pair_id,
                car_time_min = NA_real_, car_dist_km = NA_real_)
}

pt <- if (file.exists(cfg$out$pt_times)) {
  readr::read_csv(cfg$out$pt_times, show_col_types = FALSE) |>
    dplyr::select(pair_id, pt_time_min)
} else {
  log_msg("WARNING: pt_times.csv missing; PT column will be NA.")
  dplyr::tibble(pair_id = pairs$pair_id, pt_time_min = NA_real_)
}

# --- Attach both modes to the unique pairs ----------------------------------
pairs_tt <- pairs |>
  dplyr::left_join(car, by = "pair_id") |>
  dplyr::left_join(pt,  by = "pair_id")

readr::write_csv(pairs_tt, "data/output/pairs_with_times.csv")

# --- Expand back to every observation ---------------------------------------
model_input <- row_map |>
  dplyr::left_join(
    dplyr::select(pairs_tt, pair_id, car_time_min, car_dist_km, pt_time_min),
    by = "pair_id"
  )

readr::write_csv(model_input, cfg$out$final)

# --- Report ------------------------------------------------------------------
log_msg(sprintf("Final model input: %s (%d observations)",
                cfg$out$final, nrow(model_input)))
log_msg(sprintf("  car time present: %d | missing: %d",
                sum(!is.na(model_input$car_time_min)),
                sum(is.na(model_input$car_time_min))))
log_msg(sprintf("  PT  time present: %d | missing: %d",
                sum(!is.na(model_input$pt_time_min)),
                sum(is.na(model_input$pt_time_min))))
log_msg(sprintf("  observed mode split: %s",
                paste(names(table(model_input$border_mode)),
                      table(model_input$border_mode), sep="=", collapse=", ")))
log_msg("05_join.R done. model_input.csv is ready for your choice model (mlogit/apollo).")
