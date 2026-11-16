# ============================================================
# 01_agqpv_clean.R
# Cleans the raw border survey (AGQPV 2015) for the tourist model.
#
# Input  : <csv_in>/Finale_Auswertungsdatenbank_AGQPV2015_V2.csv
# Output : <csv_out>/agqpv_clean.csv
#
# Run from the project root:  source("Scripts/01_generation/01_agqpv_clean.R")
# ============================================================

source("Scripts/00_setup.R")
gen <- cfg$generation

# --- Read raw survey ----------------------------------------------------------
raw <- fread(file.path(CSV_IN_DIR, gen$survey_csv), encoding = gen$survey_encoding)

# --- Filter: foreign start, destination in CH, leisure trips -------------------
agqpv <- raw[STARTORTLANDISO != gen$filter$exclude_start_country &
               ZIELORTLAND == gen$filter$dest_country_code &
               FAHRTZWECK  == gen$filter$trip_purpose]

# --- Select and rename columns -------------------------------------------------
old_names <- c("BEFRAGUNGSORTID", "BEFRAGUNGSORT", "WOHNORTLAND", "WOHNORTORTLATITUDE", "WOHNORTORTLONGITUDE",
               "STARTORTLAND", "STARTORTORTLATITUDE", "STARTORTORTLONGITUDE",
               "ZIELORTORTLATITUDE", "ZIELORTORTLONGITUDE", "ZIELORTORT",
               "ANZAHLUEBERNACHTUNGEN", "VERKEHRSTRAEGER", "GEWICHT")
new_names <- c("grenz_id", "grenz", "nationality", "residence_lat", "residence_long",
               "origin_country", "origin_lat", "origin_long",
               "dest_lat", "dest_long", "dest_commune", "n_nights", "border_mode", "weight")
agqpv <- agqpv[, ..old_names]
setnames(agqpv, old_names, new_names)

# --- Origin and destination coordinates must be complete -----------------------
# (needed for zone assignment and routing). Other NAs (residence, weight) are kept.
no_od <- is.na(agqpv$origin_lat) | is.na(agqpv$origin_long) |
  is.na(agqpv$dest_lat)   | is.na(agqpv$dest_long)
log_msg(sprintf("Dropped %d of %d rows with missing origin/destination coordinates.",
                sum(no_od), nrow(agqpv)))
agqpv <- agqpv[!no_od]

na_counts <- colSums(is.na(agqpv))
if (any(na_counts > 0)) {
  log_msg(sprintf("%d of %d rows have at least one NA (kept). NA per column:",
                  sum(!complete.cases(agqpv)), nrow(agqpv)))
  print(na_counts[na_counts > 0])
}

# --- Border mode label ---------------------------------------------------------
agqpv[, border_mode_label := fcase(border_mode == 1, "road",
                                   border_mode == 2, "rail",
                                   default = "unknown")]

# --- Write ----------------------------------------------------------------------
# fread(encoding = "Latin-1") only marks strings; fwrite would write the raw
# Latin-1 bytes. Convert text columns to real UTF-8 so the output is valid UTF-8.
chr_cols <- names(agqpv)[vapply(agqpv, is.character, logical(1))]
agqpv[, (chr_cols) := lapply(.SD, enc2utf8), .SDcols = chr_cols]

out_file <- file.path(CSV_DIR, gen$out$agqpv_clean)
fwrite(agqpv, out_file)
log_msg(sprintf("Wrote %s (%d rows, %d columns)", out_file, nrow(agqpv), ncol(agqpv)))
