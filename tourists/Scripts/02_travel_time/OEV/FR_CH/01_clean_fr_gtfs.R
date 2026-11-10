# ============================================================
# 01_clean_fr_gtfs.R
# Cleans the Île-de-France Mobilités GTFS feed so it can be combined with
# the SNCF feed in an r5r network:
#   1. removes on-demand (GTFS-Flex) trips that have no arrival/departure times
#   2. optionally removes bus routes (route_type 3) to save memory
#   3. optionally drops shapes.txt (geometry only, no effect on travel times)
#   4. drops files R5 doesn't use (pathways, booking_rules, ...)
#   5. removes stops, routes, agencies, services and transfers left unused
# and writes a new, smaller zip.
#
# Needs: data.table, zip   (install.packages(c("data.table", "zip")))
# Memory: reading stop_times (~1 GB, 12M rows) needs roughly 4-6 GB of RAM.
# ============================================================

# Run from the project root:
#   source("Scripts/02_travel_time/OEV/FR_CH/01_clean_fr_gtfs.R")

source("Scripts/00_setup.R")   # sets the project root, loads data.table, defines ensure_dir()

# ---- Settings -----------------------------------------------------------
IN_ZIP      <- "data/input/gtfs/fr.Paris.gtfs.zip"            # original IDFM feed (raw input)
OUT_ZIP     <- "data/output/gtfs/fr.Paris.gtfs.clean.zip"     # cleaned feed; 02_... copies it into the r5r network folder
ensure_dir(dirname(OUT_ZIP))
KEEP_BUSES  <- FALSE   # TRUE = keep all ~2,000 bus routes (much bigger network)
DROP_SHAPES <- TRUE    # TRUE = leave out shapes.txt (travel times unaffected)
WORK_DIR    <- file.path(tempdir(), "idfm_clean")   # unzip location (~1.4 GB free space needed)

# Files R5 does not use for routing -- never copied to the output
UNUSED_FILES <- c("pathways.txt", "booking_rules.txt", "object_codes_extension.txt",
                  "ticketing_deep_links.txt", "attributions.txt")

# ---- Helpers ------------------------------------------------------------
msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), sprintf(...), "\n", sep = "")

read_gtfs_file <- function(name, ...) {
  # Everything as character so IDs, times and dates are written back unchanged;
  # empty fields become NA and are written back as empty.
  fread(file.path(WORK_DIR, name), colClasses = "character", na.strings = "",
        encoding = "UTF-8", ...)
}

write_gtfs_file <- function(dt, name) {
  fwrite(dt, file.path(OUT_DIR, name), na = "", quote = "auto")
}

# ---- Unzip --------------------------------------------------------------
stopifnot(file.exists(IN_ZIP))
unlink(WORK_DIR, recursive = TRUE)
dir.create(WORK_DIR, recursive = TRUE)
OUT_DIR <- file.path(WORK_DIR, "out")
dir.create(OUT_DIR)

in_files <- unzip(IN_ZIP, list = TRUE)$Name
needed   <- setdiff(in_files, c(UNUSED_FILES, if (DROP_SHAPES) "shapes.txt"))
msg("Unzipping %d of %d files from %s ...", length(needed), length(in_files), IN_ZIP)
unzip(IN_ZIP, files = needed, exdir = WORK_DIR)

# ---- Read ---------------------------------------------------------------
agency  <- read_gtfs_file("agency.txt")
routes  <- read_gtfs_file("routes.txt")
trips   <- read_gtfs_file("trips.txt")
stops   <- read_gtfs_file("stops.txt")
has_cal       <- "calendar.txt" %in% needed
has_cal_dates <- "calendar_dates.txt" %in% needed
has_transfers <- "transfers.txt" %in% needed
if (has_cal)       calendar       <- read_gtfs_file("calendar.txt")
if (has_cal_dates) calendar_dates <- read_gtfs_file("calendar_dates.txt")
if (has_transfers) transfers      <- read_gtfs_file("transfers.txt")

msg("Reading stop_times.txt (large, takes a while) ...")
# Leave out the GTFS-Flex columns (pickup windows, booking rules, local zones):
# R5 doesn't use them, and they're empty for every trip we keep.
st_header <- names(fread(file.path(WORK_DIR, "stop_times.txt"), nrows = 0))
st_cols   <- intersect(c("trip_id", "arrival_time", "departure_time", "stop_id",
                         "stop_sequence", "stop_headsign", "pickup_type",
                         "drop_off_type", "timepoint", "shape_dist_traveled"),
                       st_header)
stop_times <- read_gtfs_file("stop_times.txt", select = st_cols)

n0 <- list(routes = nrow(routes), trips = nrow(trips), stops = nrow(stops),
           stop_times = nrow(stop_times))
msg("Read: %d routes, %d trips, %d stops, %d stop_times.",
    n0$routes, n0$trips, n0$stops, n0$stop_times)

# ---- 1. Remove on-demand trips without times ----------------------------
flex_trips <- unique(stop_times[is.na(arrival_time) | is.na(departure_time), trip_id])
msg("On-demand trips without times: %d (removed).", length(flex_trips))
trips <- trips[!trip_id %in% flex_trips]

# ---- 2. Optionally remove buses -----------------------------------------
if (!KEEP_BUSES) {
  bus_routes <- routes[route_type == "3", route_id]
  msg("Removing %d bus routes (route_type 3).", length(bus_routes))
  routes <- routes[!route_id %in% bus_routes]
  trips  <- trips[route_id %in% routes$route_id]
}
msg("Route types kept: %s",
    paste(names(table(routes$route_type)), table(routes$route_type), sep = "=", collapse = ", "))

# ---- 3. Cascade: keep only what the remaining trips use -----------------
stop_times <- stop_times[trip_id %in% trips$trip_id]
routes     <- routes[route_id %in% trips$route_id]
agency     <- agency[agency_id %in% routes$agency_id]

if (DROP_SHAPES && "shape_id" %in% names(trips)) trips[, shape_id := NA_character_]

# Stops: those served by stop_times, plus their parent stations (repeat in
# case a parent itself has a parent).
keep_stops <- unique(stop_times$stop_id)
repeat {
  parents <- stops[stop_id %in% keep_stops & !is.na(parent_station), unique(parent_station)]
  new <- setdiff(parents, keep_stops)
  if (length(new) == 0) break
  keep_stops <- c(keep_stops, new)
}
stops <- stops[stop_id %in% keep_stops]
if ("level_id" %in% names(stops)) stops[, level_id := NA_character_]  # levels.txt not kept

services <- unique(trips$service_id)
if (has_cal)       calendar       <- calendar[service_id %in% services]
if (has_cal_dates) calendar_dates <- calendar_dates[service_id %in% services]
if (has_transfers) transfers      <- transfers[from_stop_id %in% stops$stop_id &
                                                 to_stop_id %in% stops$stop_id]

# ---- Checks before writing ----------------------------------------------
stopifnot(
  all(stop_times$stop_id %in% stops$stop_id),
  all(trips$route_id %in% routes$route_id),
  all(routes$agency_id %in% agency$agency_id),
  all(is.na(stops$parent_station) | stops$parent_station %in% stops$stop_id),
  !anyNA(stop_times$arrival_time), !anyNA(stop_times$departure_time)
)
service_ok <- services %in% c(if (has_cal) calendar$service_id,
                              if (has_cal_dates) calendar_dates$service_id)
if (!all(service_ok)) warning(sum(!service_ok), " trip service_ids have no calendar entry.")

dates <- c(if (has_cal) c(calendar$start_date, calendar$end_date),
           if (has_cal_dates) calendar_dates$date)
msg("Service dates in cleaned feed: %s to %s -- your departure_date must fall in this range.",
    min(dates), max(dates))

# ---- Write --------------------------------------------------------------
msg("Writing cleaned files ...")
write_gtfs_file(agency,     "agency.txt")
write_gtfs_file(routes,     "routes.txt")
write_gtfs_file(trips,      "trips.txt")
write_gtfs_file(stops,      "stops.txt")
write_gtfs_file(stop_times, "stop_times.txt")
if (has_cal)       write_gtfs_file(calendar,       "calendar.txt")
if (has_cal_dates) write_gtfs_file(calendar_dates, "calendar_dates.txt")
if (has_transfers) write_gtfs_file(transfers,      "transfers.txt")
if (!DROP_SHAPES && "shapes.txt" %in% needed) {
  file.copy(file.path(WORK_DIR, "shapes.txt"), OUT_DIR)
}

dir.create(dirname(OUT_ZIP), recursive = TRUE, showWarnings = FALSE)
if (file.exists(OUT_ZIP)) file.remove(OUT_ZIP)
zip::zip(normalizePath(OUT_ZIP, winslash = "/", mustWork = FALSE),
         files = list.files(OUT_DIR), root = OUT_DIR)

msg("Done: %s (%.0f MB)", OUT_ZIP, file.size(OUT_ZIP) / 1e6)
msg("Kept %d/%d routes, %d/%d trips, %d/%d stops, %d/%d stop_times.",
    nrow(routes), n0$routes, nrow(trips), n0$trips, nrow(stops), n0$stops,
    nrow(stop_times), n0$stop_times)

unlink(WORK_DIR, recursive = TRUE)
