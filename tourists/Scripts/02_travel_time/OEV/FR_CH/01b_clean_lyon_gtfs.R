# ============================================================
# 01b_clean_lyon_gtfs.R
# Fixes the TCL (Lyon) GTFS feed so that r5r accepts it.
# r5r refuses to build a network with HIGH-priority GTFS errors. The TCL feed has stops
# whose parent_station does not exist in stops.txt (e.g. stops 21938 and 21939 point to
# "S21938"). The parent_station of such stops is emptied (they become plain stops);
# everything else is left unchanged.
#
# INPUT   data/input/gtfs/fr.Lyon.gtfs.zip          raw TCL feed (see 03_ for where to get it)
# OUTPUT  data/output/gtfs/fr.Lyon.gtfs.clean.zip   used by 03_pt_r5r_ch_fr_build_network.R
#
# Run from the project root:
#   source("Scripts/02_travel_time/OEV/FR_CH/01b_clean_lyon_gtfs.R")
# ============================================================

source("Scripts/00_setup.R")   # sets the project root, loads data.table, defines ensure_dir()

# ---- Settings -----------------------------------------------------------
IN_ZIP   <- "data/input/gtfs/fr.Lyon.gtfs.zip"
OUT_ZIP  <- "data/output/gtfs/fr.Lyon.gtfs.clean.zip"
WORK_DIR <- file.path(tempdir(), "tcl_clean")   # unzip location (~200 MB free space needed)
ensure_dir(dirname(OUT_ZIP))

# ---- Unzip ----------------------------------------------------------------
stopifnot(file.exists(IN_ZIP))
unlink(WORK_DIR, recursive = TRUE); dir.create(WORK_DIR, recursive = TRUE)
unzip(IN_ZIP, exdir = WORK_DIR, junkpaths = TRUE)

# ---- Fix parent_station that does not exist ---------------------------------------
stops <- fread(file.path(WORK_DIR, "stops.txt"), colClasses = "character", na.strings = "", encoding = "UTF-8")
bad <- !is.na(stops$parent_station) & !stops$parent_station %in% stops$stop_id
log_msg(sprintf("Stops with a missing parent_station: %d (%s) -- parent_station emptied.",
                sum(bad), paste(stops$stop_id[bad], collapse = ", ")))
stops[bad, parent_station := NA_character_]
fwrite(stops, file.path(WORK_DIR, "stops.txt"), na = "", quote = "auto")

# ---- Write ----------------------------------------------------------------
if (file.exists(OUT_ZIP)) file.remove(OUT_ZIP)
zip::zip(normalizePath(OUT_ZIP, winslash = "/", mustWork = FALSE),
         files = list.files(WORK_DIR), root = WORK_DIR)
log_msg(sprintf("Done: %s (%.0f MB)", OUT_ZIP, file.size(OUT_ZIP) / 1e6))
unlink(WORK_DIR, recursive = TRUE)
