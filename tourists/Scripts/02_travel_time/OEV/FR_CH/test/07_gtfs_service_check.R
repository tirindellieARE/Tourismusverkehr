# ============================================================
# 07_gtfs_service_check.R
# Lists the trains in the r5r network feed that run directly between two
# stations on the routing date. Use it to check suspicious r5r results
# (e.g. a long walk although a train seems to exist) against the GTFS data.
#
# For each pair in PAIRS: stops are found by name, trips running on
# cfg$pt$departure_date are selected (calendar_dates, plus calendar.txt if
# present), and trips calling at the first stop and then at the second are
# listed with departure, arrival and pickup_type (0 = boarding allowed).
# Only direct trips are listed; connections are not searched.
#
# INPUT   data/r5_rail_ch_fr/fr.gtfs.zip (French feed of the network built by 03_)
# OUTPUT  <test_dir>/gtfs_service_check.csv (all direct trips of all pairs) + printed lists
#
# Run from the project root:
#   source("Scripts/02_travel_time/OEV/FR_CH/test/07_gtfs_service_check.R")
# ============================================================

source("Scripts/00_setup.R")   # provides cfg, log_msg

# --- Settings ----------------------------------------------------------------
GTFS_ZIP <- "data/r5_rail_ch_fr/fr.gtfs.zip"
TEST_DIR <- cfg$oev_ch_fr$test_dir
OUT_CSV  <- file.path(TEST_DIR, "gtfs_service_check.csv")
DATE     <- cfg$pt$departure_date
# from / to are regular expressions on stop_name (case-insensitive)
PAIRS <- list(
  list(label = "Voujeaucourt -> Montbeliard (T057)",     from = "^Voujeaucourt$", to = "^Montb.liard$"),
  list(label = "Altkirch -> Mulhouse (T065, T049)",      from = "^Altkirch$",     to = "^Mulhouse( Ville)?$"),
  list(label = "Lyon Part-Dieu -> Geneve (T014)",        from = "Lyon Part.Dieu", to = "Gen.ve"),
  list(label = "Lyon Saint-Paul -> Lyon Part-Dieu (T014)", from = "Lyon Saint.Paul", to = "Lyon Part.Dieu")
)

# --- Read feed -----------------------------------------------------------------
tmp <- file.path(tempdir(), "gtfs_service_check"); ensure_dir(tmp)
unzip(GTFS_ZIP, exdir = tmp, junkpaths = TRUE)
rd <- function(f) if (file.exists(file.path(tmp, f))) data.table::fread(file.path(tmp, f), colClasses = "character", encoding = "UTF-8") else NULL
stops <- rd("stops.txt"); routes <- rd("routes.txt"); trips <- rd("trips.txt"); st <- rd("stop_times.txt")
cd <- rd("calendar_dates.txt"); cal <- rd("calendar.txt")

d8 <- gsub("-", "", DATE)
wd <- c("sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday")[as.POSIXlt(as.Date(DATE))$wday + 1]
active <- character()
if (!is.null(cd))  active <- union(active, cd[date == d8 & exception_type == "1", service_id])
if (!is.null(cal)) active <- union(active, cal[start_date <= d8 & end_date >= d8 & get(wd) == "1", service_id])
if (!is.null(cd))  active <- setdiff(active, cd[date == d8 & exception_type == "2", service_id])
log_msg("Date ", DATE, " (", wd, "): ", length(active), " active services, ",
        trips[service_id %in% active, .N], " of ", nrow(trips), " trips")

routes[, rname := trimws(paste(route_short_name, route_long_name))]
hhmm <- function(x) substr(x, 1, 5)

check_pair <- function(p) {
  f <- stops[grepl(p$from, stop_name, ignore.case = TRUE), stop_id]
  t <- stops[grepl(p$to,   stop_name, ignore.case = TRUE), stop_id]
  cat("\n---", p$label, ": ", length(f), "from-stop(s),", length(t), "to-stop(s)\n")
  if (!length(f) || !length(t)) return(NULL)
  a <- st[stop_id %in% f & trip_id %in% trips[service_id %in% active, trip_id],
          .(trip_id, from_stop = stop_id, from_seq = as.integer(stop_sequence), departure = departure_time,
            pickup_type = if ("pickup_type" %in% names(st)) pickup_type else NA_character_)]
  b <- st[stop_id %in% t & trip_id %in% trips[service_id %in% active, trip_id],
          .(trip_id, to_stop = stop_id, to_seq = as.integer(stop_sequence), arrival = arrival_time)]
  m <- merge(a, b, by = "trip_id")[to_seq > from_seq]
  if (!nrow(m)) { cat("no direct trip on", DATE, "\n"); return(NULL) }
  m <- merge(m, trips[, .(trip_id, route_id, trip_headsign)], by = "trip_id")
  m <- merge(m, routes[, .(route_id, rname, route_type)], by = "route_id")
  m <- merge(m, stops[, .(stop_id, from_name = stop_name)], by.x = "from_stop", by.y = "stop_id")
  m <- merge(m, stops[, .(stop_id, to_name = stop_name)],   by.x = "to_stop",   by.y = "stop_id")
  m <- m[order(departure)]
  m[, pair := p$label]
  cat(nrow(m), "direct trip(s) on", DATE, "\n")
  print(m[, .(rname, from_name, dep = hhmm(departure), to_name, arr = hhmm(arrival), pickup_type)], nrows = 60)
  m
}
res <- data.table::rbindlist(lapply(PAIRS, check_pair), fill = TRUE)

ensure_dir(TEST_DIR)
if (nrow(res)) data.table::fwrite(res[, .(pair, rname, route_type, trip_headsign, from_name, departure, pickup_type, to_name, arrival)], OUT_CSV)
log_msg("Wrote ", OUT_CSV)
log_msg("07_gtfs_service_check.R done.")
