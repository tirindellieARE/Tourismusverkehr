# ============================================================
# 03_car_osrm.R
# Car travel time origin -> destination for every unique pair, via a
# LOCAL OSRM server.
#
# -------- Starting a local OSRM server (one-time, outside this script) ------
# OSRM runs best as a server. If Docker Desktop is available, the official
# image is the simplest reproducible route (see git history of this file for
# the docker run commands). Where Docker isn't available (e.g. this machine),
# use the R package **osrm.backend**, which downloads real OSRM binaries for
# Windows/macOS/Linux (no Docker, no compiler) and drives
# extract/partition/customize/serve for you:
#
#   install.packages("osrm.backend",
#     repos = c("https://e-kotov.r-universe.dev", "https://cloud.r-project.org"))
#   library(osrm.backend)
#   osrm_install()                                   # one-time binary download
#   osrm_start("data/osm/network.osm.pbf", algorithm = "mld")  # extract+partition+customize+serve
#
# IMPORTANT: osrm_start() launches osrm-routed.exe as a CHILD of the R
# process that called it -- on Windows that child dies when the parent R
# session exits. Run it in a session/terminal you leave open (or a detached
# background process), not in a one-off `Rscript -e ...` that returns
# immediately. Re-running osrm_start() after the graph is already built
# skips straight to starting the server (no re-extraction).
#
# The osrm R package then queries http://127.0.0.1:5001 (osrm.backend's
# default port; set in config.yml).
#
# Run:  source("Scripts/02_travel_time/00_setup.R"); source("Scripts/02_travel_time/03_car_osrm.R")
# ============================================================

user = "MR"
if(user == "MR"){setwd("E:/ARE/ProjekteTIE/Turismusverkehr/RScript/tourists")}
if(user == "CP"){setwd("P:/Verkehrsmodellierung/06_Jobs/188_touristische_Verkehr/Model_trafic_touristique/GitRepo/tourists")}

source("Scripts/02_travel_time/00_setup.R")
library(osrm)

options(osrm.server = paste0(cfg$osrm$server, "/"),
        osrm.profile = cfg$osrm$profile)

if (!file.exists(cfg$out$pairs)) stop("Run 01_filter.R first.")
pairs <- readr::read_csv(cfg$out$pairs, show_col_types = FALSE)
log_msg(sprintf("Routing %d pairs by car via OSRM at %s",
                nrow(pairs), cfg$osrm$server))

# --- Quick connectivity check ------------------------------------------------
test <- tryCatch(
  osrmRoute(src = c(pairs$origin_long[1], pairs$origin_lat[1]),
            dst = c(pairs$dest_long[1],   pairs$dest_lat[1]),
            overview = FALSE),
  error = function(e) {
    stop("Could not reach the OSRM server at ", cfg$osrm$server,
         ".\nStart the server (see header of this script) and retry.\n",
         "Original error: ", conditionMessage(e))
  }
)
log_msg("OSRM server reachable. Beginning batch.")

# --- Route each pair one-to-one ---------------------------------------------
# osrmRoute is point-to-point; loop with light error handling. For 13k rows
# against a local server this is fast (seconds to a couple of minutes).
n <- nrow(pairs)
car_min <- rep(NA_real_, n)
car_km  <- rep(NA_real_, n)

for (i in seq_len(n)) {
  r <- tryCatch(
    osrmRoute(src = c(pairs$origin_long[i], pairs$origin_lat[i]),
              dst = c(pairs$dest_long[i],   pairs$dest_lat[i]),
              overview = FALSE),
    error = function(e) NULL
  )
  if (!is.null(r)) {
    # osrm >= 5.0.0: osrmRoute(overview=FALSE) returns a named numeric vector
    # c(duration=<min>, distance=<km>), not a data.frame/list -- no $ accessor.
    car_min[i] <- as.numeric(r["duration"])
    car_km[i]  <- as.numeric(r["distance"])
  }
  if (i %% 500 == 0) log_msg(sprintf("  car routed %d / %d", i, n))
}

car_times <- pairs |>
  dplyr::select(pair_id, origin_lat, origin_long, dest_lat, dest_long) |>
  dplyr::mutate(car_time_min = car_min, car_dist_km = car_km)

readr::write_csv(car_times, cfg$out$car_times)
log_msg(sprintf("Wrote %s | missing car times: %d / %d",
                cfg$out$car_times, sum(is.na(car_min)), n))
log_msg("03_car_osrm.R done.")
