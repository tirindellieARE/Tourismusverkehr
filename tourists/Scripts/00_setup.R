# ============================================================
# 00_setup.R
# Single entry point for every script: sets the project root,
# loads packages, reads config.yml, defines shared helpers.
#
# Usage (first line of every script, working directory = project root):
#   source("Scripts/00_setup.R")
# ============================================================

# --- Project root ------------------------------------------------------------
# Set your user here. Add a line for a new machine/user.
user <- "CP"

project_roots <- c(
  MR = "E:/ARE/ProjekteTIE/Turismusverkehr/RScript/tourists",
  CP = "P:/Verkehrsmodellierung/06_Jobs/188_touristische_Verkehr/Model_trafic_touristique/GitRepo/tourists"
)
if (!user %in% names(project_roots)) {
  stop("Unknown user '", user, "'. Add it to project_roots in Scripts/00_setup.R.")
}
setwd(project_roots[[user]])

# --- Package management ------------------------------------------------------
# Install any missing CRAN packages, then load.
# Script-specific packages (ggplot2, viridis, mapview, r5r, ...) are loaded
# in the scripts that need them.
.pkgs <- c(
  "yaml",       # read config.yml
  "sf",         # spatial: zone assignment, points, polygons
  "data.table", # fast tables (used by 01_generation)
  "dplyr",      # data wrangling
  "readr",      # fast/robust CSV IO
  "stringr",    # string helpers
  "osrm"        # car routing against a local OSRM server
  # r5r is loaded separately in 04_pt_r5r.R AFTER setting JVM memory
)

.missing <- .pkgs[!vapply(.pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(.missing)) {
  install.packages(.missing, repos = "https://cloud.r-project.org")
}
invisible(lapply(.pkgs, library, character.only = TRUE))

# --- Config ------------------------------------------------------------------
cfg <- yaml::read_yaml("Scripts/config.yml")

# --- Project folders (from config) -------------------------------------------
# Shared constants so scripts don't redefine them.
CSV_IN_DIR   <- cfg$paths$csv_in
GPKG_IN_DIR  <- cfg$paths$gpkg_in
CSV_DIR      <- cfg$paths$csv_out
GPKG_DIR     <- cfg$paths$gpkg_out
FIG_DIR      <- cfg$paths$fig_out

# --- Helper: ensure a directory exists --------------------------------------
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}
for (.d in c(CSV_IN_DIR, GPKG_IN_DIR, CSV_DIR, GPKG_DIR, FIG_DIR)) {
  ensure_dir(.d)
}
rm(.d)

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

log_msg("Setup complete. User: ", user, " | root: ", getwd(), " | config: Scripts/config.yml")
