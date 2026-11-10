# ============================================================
# 00_setup.R
# Loads packages, reads config, defines shared helpers.
# Source this at the top of every other script:  source("R/00_setup.R")
# ============================================================

# --- Package management ------------------------------------------------------
# Install any missing CRAN packages, then load.
.pkgs <- c(
  "yaml",      # read config.yml
  "sf",        # spatial: CH polygon filter, points
  "dplyr",     # data wrangling
  "readr",     # fast/robust CSV IO
  "stringr",   # string helpers
  "osrm"       # car routing against a local OSRM server
  # r5r is loaded separately in 04_pt_r5r.R AFTER setting JVM memory
)

.missing <- .pkgs[!vapply(.pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(.missing)) {
  install.packages(.missing, repos = "https://cloud.r-project.org")
}
invisible(lapply(.pkgs, library, character.only = TRUE))

user = "MR"
if(user == "MR"){setwd("E:/ARE/ProjekteTIE/Turismusverkehr/RScript/tourists")}
if(user == "CP"){setwd("P:/Verkehrsmodellierung/06_Jobs/188_touristische_Verkehr/Model_trafic_touristique/GitRepo/tourists")}

# --- Project root ------------------------------------------------------------

cfg <- yaml::read_yaml("Scripts/02_travel_time/config.yml")

# --- Helper: ensure a directory exists --------------------------------------
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}
ensure_dir("data/input")
ensure_dir("data/output")
ensure_dir("data/osm")
ensure_dir("data/gtfs")

# --- Helper: timestamped logging --------------------------------------------
log_msg <- function(...) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(...)))
}

# --- Helper: build a stable pair key ----------------------------------------
# Rounds coordinates to 6 dp so identical points collapse consistently.
make_pair_key <- function(olat, olon, glat, glon) {
  sprintf("%.6f_%.6f_%.6f_%.6f",
          round(olat, 6), round(olon, 6),
          round(glat, 6), round(glon, 6))
}

log_msg("Setup complete. Config loaded from config.yml.")
