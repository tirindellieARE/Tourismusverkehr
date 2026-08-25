# ============================================================
# 00_geocode_grenz.R
# Geocode each named border-crossing survey site (`grenz` /
# BEFRAGUNGSORT in the raw AGQPV data) to a coordinate, via the
# free swisstopo SearchServer API (no key needed; already used
# elsewhere in this repo, see CFFreplication/prepare_hotel_data.R).
#
# WHY THIS EXISTS: `grenz` is a place name, not a coordinate, so it
# can't be zone-assigned by point-in-polygon like origin/dest/
# residence. Earlier we (wrongly) approximated it by picking an
# arbitrary zone within the matching commune -- that's not a real
# geographic assignment, and communes aren't the same thing as the
# transit zones we actually need to assign to. This script instead
# gets a real coordinate for each site, so a later step can
# point-in-polygon it against the actual transit zones in
# zones_communes.gpkg (not communes).
#
# QUERY STRATEGY (an assumption -- please sanity check the output,
# especially anything flagged match_type != "customs_point"):
#   For most sites we try, in priority order, until one hit is
#   itself a labelled customs-office point (label matches
#   "Zollamt|Douane|Dogana" -> match_type = "customs_point", highest
#   confidence -- it's the actual crossing, not just a nearby place):
#     Q1 = "<name, slashes->spaces, motorway suffix KEPT> <customs word>"
#     Q2 = "<name, slashes->spaces, motorway suffix STRIPPED> <customs word>"
#     Q3 = "<name, slashes->spaces, motorway suffix STRIPPED>" (no customs word)
#   The customs word is chosen from GRENZABSCHNITT: DE/AT -> "Zollamt"
#   (German), FR -> "douane", IT -> "dogana". If nothing hits a
#   customs-office label, we keep Q3's result as match_type =
#   "place_fallback" (typically just the named place's centroid --
#   lower precision).
#
#   Motorway suffix (-Autobahn/Autostrada/Autoroute) is tried BOTH
#   kept and stripped because testing showed it cuts both ways: for
#   "Kreuzlingen-Autobahn" keeping "Autobahn" is what finds the actual
#   motorway checkpoint (vs. just the town), but for "Chiasso
#   Autostrada" keeping "Autostrada" instead confused the search into
#   an unrelated fuzzy match. Trying both and preferring whichever
#   lands on a real customs-office label sidesteps having to guess.
#
#   A SMALL set of sites get a hand-written override query instead,
#   because their literal name actively misleads free-text search
#   (verified by testing -- these are not guesses):
#     - The 17 aggregated "Gruppe ..." catchments (e.g. "Gruppe A:
#       BS/AG - DE, Landkreis Lörrach"): searching the literal
#       "Gruppe ..." text returns garbage (the word "Gruppe" itself
#       matches unrelated places). These don't have one true point
#       anyway -- each is queried on a representative place for that
#       corridor instead, and flagged match_type = "regional_approx".
#     - "Au" (109): raw search collided with "Hallau" (SH), on the
#       wrong end of the country from the actual crossing near Au (SG).
#       Overridden to "Au SG".
#     - "Schaanwald (FL)" (312): raw search returned an unrelated
#       Solothurn hit. Overridden to "Sevelen" (the CH-side gateway
#       commune on that FL/AT corridor) -- see note in
#       01_agent_generation.R for why this is itself an approximation.
#     - "Gotthardtunnel" (201) / "Gotthard" (401): raw search collided
#       with an unrelated hamlet also named "Gotthard" in canton SG.
#       Overridden to "Gotthard Strassentunnel" / "Gotthardpass"
#       respectively (the latter reuses the query that correctly
#       resolved site 203 "Gotthardpass" itself).
#   These overrides are still run through the same Q1/Q2/Q3 ladder
#   (using the override text as the base name), not hardcoded points.
#
# OUTPUT: data/output/grenz_coordinates.csv
#   grenz_id, grenz, grenz_abschnitt, query_used, override_used,
#   match_type, matched_label, lon, lat, lv95_x, lv95_y
#
# Run:  source("Scripts/01_generation/00_geocode_grenz.R")
# ============================================================

library(data.table)
library(jsonlite)

user = "CP"
if(user == "MR"){setwd("E:/ARE/ProjekteTIE/Turismusverkehr/RScript/tourists")}
if(user == "CP"){setwd("P:/Verkehrsmodellierung/06_Jobs/188_touristische_Verkehr/Model_trafic_touristique/GitRepo/tourists")}

dir.create("data/output", recursive = TRUE, showWarnings = FALSE)

# --- Distinct border-crossing sites from the raw survey data -----------------
raw <- fread("data/input/Finale_Auswertungsdatenbank_AGQPV2015_V2.csv", encoding = "Latin-1",
             select = c("BEFRAGUNGSORTID", "BEFRAGUNGSORT", "GRENZABSCHNITT"))
sites <- unique(raw[, .(grenz_id = BEFRAGUNGSORTID, grenz = BEFRAGUNGSORT, grenz_abschnitt = GRENZABSCHNITT)])
setorder(sites, grenz_id)
cat(sprintf("Distinct border-crossing sites: %d\n", nrow(sites)))

# --- Hand-written overrides (see header comment for why each is needed) ------
grenz_query_override <- c(
  `109` = "Au SG",
  `312` = "Sevelen",
  `201` = "Gotthard Strassentunnel",
  `401` = "Gotthardpass",
  `318` = "Le Châtelard-Frontière",  # "Châtelard" alone collides with unrelated communes elsewhere in CH
  `114` = "Basel",                 # Gruppe A: BS/AG - DE, Landkreis Lörrach
  `115` = "Koblenz",                # Gruppe B: AG/ZH/SH - DE, Landkreis Waldshut
  `116` = "Kreuzlingen",             # Gruppe C: SH/TG - DE, Landkreise .../Konstanz
  `117` = "St. Margrethen",          # Gruppe D: SG/FL - AT
  `118` = "Samnaun",                 # Gruppe E: GR - AT
  `119` = "Poschiavo",               # Gruppe F: GR - IT
  `120` = "Chiasso",                 # Gruppe G: TI - IT, Provinz Como
  `121` = "Tresa",                   # Gruppe H: TI - IT, Provinzen Lecco, Varese
  `122` = "Centovalli",              # Gruppe I: TI - IT, Provinz Domodossola
  `123` = "Zwischbergen",            # Gruppe K: VS - IT, Provinz Valle d'Aosta
  `124` = "Finhaut",                 # Gruppe L: VS - FR, Dép. Haute-Savoie
  `125` = "Thônex",                  # Gruppe M: GE - FR, Dép. Haute-Savoie
  `126` = "Bardonnex",               # Gruppe N: GE/VD - FR, Dép. Ain
  `127` = "Vallorbe",                # Gruppe O: VD/NE - FR, Dép. Jura, Doubs
  `128` = "Boncourt",                # Gruppe P: JU - FR, Dép. Doubs, Belfort, Haut-Rhin
  `129` = "Basel",                   # Gruppe Q: SO/BL/BS - FR, Dép. Haut-Rhin
  `204` = "Bregaglia"                # Gruppe: Maloja/Bernina/Lukmanier/Ofenpass/Nufenenpass/Splügen
)
is_gruppe_id <- c("114","115","116","117","118","119","120","121","122","123",
                   "124","125","126","127","128","129","204")

# --- Query construction --------------------------------------------------

customs_word <- function(abschnitt) {
  dplyr::case_when(
    abschnitt %in% c("DE", "AT") ~ "Zollamt",
    abschnitt == "FR"            ~ "douane",
    abschnitt == "IT"            ~ "dogana",
    TRUE                         ~ NA_character_   # blank: Alpine-pass sites
  )
}

strip_suffix <- function(x) {
  x <- sub(" Autobahn$", "", x)
  x <- sub("-Autobahn$", "", x)
  x <- sub(" Autostrada$", "", x)
  x <- sub("-Autoroute$", "", x)
  trimws(x)
}
slashes_to_spaces <- function(x) trimws(gsub("/", " ", x))

sites[, cw := customs_word(grenz_abschnitt)]
sites[, lang := dplyr::case_when(
  grenz_abschnitt %in% c("DE", "AT") ~ "de",
  grenz_abschnitt == "FR"            ~ "fr",
  grenz_abschnitt == "IT"            ~ "it",
  TRUE                                ~ "de"
)]
sites[, base_name := ifelse(
  as.character(grenz_id) %in% names(grenz_query_override),
  grenz_query_override[as.character(grenz_id)],
  grenz
)]
sites[, override_used := as.character(grenz_id) %in% names(grenz_query_override)]

# --- swisstopo SearchServer -------------------------------------------------

query_swisstopo <- function(query, lang = "de") {
  url <- paste0(
    "https://api3.geo.admin.ch/rest/services/api/SearchServer?",
    "searchText=", URLencode(query, reserved = TRUE),
    "&lang=", lang, "&type=locations&limit=1"
  )
  Sys.sleep(0.25)
  tryCatch({
    r <- fromJSON(url, simplifyVector = TRUE)
    if (!is.null(r$results) && length(r$results) > 0 && nrow(r$results) > 0) {
      a <- r$results$attrs[1, ]
      list(label = gsub("<[^>]+>", "", a$label), x = a$x, y = a$y, lon = a$lon, lat = a$lat)
    } else NULL
  }, error = function(e) {
    message("  API error for '", query, "': ", conditionMessage(e))
    NULL
  })
}

is_customs_point <- function(label) grepl("Zollamt|Douane|Dogana", label, ignore.case = TRUE)

build_candidates <- function(base_name, cw) {
  kept     <- slashes_to_spaces(base_name)
  stripped <- slashes_to_spaces(strip_suffix(base_name))
  cands <- character(0)
  if (!is.na(cw)) cands <- c(cands, paste(kept, cw), paste(stripped, cw))
  cands <- c(cands, stripped)
  unique(cands)
}

n <- nrow(sites)
match_type    <- character(n)
matched_label <- character(n)
query_used    <- character(n)
lon <- lat <- lv95_x <- lv95_y <- rep(NA_real_, n)

for (i in seq_len(n)) {
  s <- sites[i]
  cat(sprintf("[%d/59] %s (%s)%s ... ", i, s$grenz, s$grenz_abschnitt,
              if (s$override_used) sprintf(" [override: %s]", s$base_name) else ""))

  # Try each candidate query; a customs-office hit wins immediately (it's the
  # actual crossing). Otherwise fall back to the LAST candidate that returned
  # something -- that's the plain unadorned name (no customs word), which
  # testing showed is more trustworthy than a customs-word query that missed:
  # an embellished query that fails to find a customs point often returns
  # unrelated noise instead of just a weaker match on the right place.
  cands <- build_candidates(s$base_name, s$cw)
  best <- NULL; best_q <- NA_character_; best_is_customs <- FALSE
  for (q in cands) {
    r <- query_swisstopo(q, s$lang)
    if (is.null(r)) next
    if (is_customs_point(r$label)) { best <- r; best_q <- q; best_is_customs <- TRUE; break }
    best <- r; best_q <- q   # keep overwriting; last non-null candidate wins if no customs hit
  }

  query_used[i] <- best_q
  if (!is.null(best)) {
    matched_label[i] <- best$label
    lon[i] <- best$lon; lat[i] <- best$lat
    lv95_x[i] <- best$y + 2000000   # swisstopo returns LV03 (x=northing, y=easting)
    lv95_y[i] <- best$x + 1000000   # LV95 = LV03 + (2'000'000, 1'000'000) false origin shift
    if (best_is_customs) {
      match_type[i] <- "customs_point"
    } else if (as.character(s$grenz_id) %in% is_gruppe_id) {
      match_type[i] <- "regional_approx"
    } else if (is.na(s$cw)) {
      match_type[i] <- "pass"
    } else {
      match_type[i] <- "place_fallback"
    }
    cat(sprintf("%s -> %s\n", match_type[i], matched_label[i]))
  } else {
    match_type[i] <- "unmatched"
    cat("NO RESULT\n")
  }
}

sites[, `:=`(query_used = query_used, match_type = match_type, matched_label = matched_label,
             lon = lon, lat = lat, lv95_x = lv95_x, lv95_y = lv95_y)]
sites[, c("cw", "lang", "base_name") := NULL]

cat("\nMatch type summary:\n")
print(sites[, .N, by = match_type][order(-N)])

out_file <- "data/output/grenz_coordinates.csv"
fwrite(sites, out_file)
cat(sprintf("\nWrote %s (%d rows)\n", out_file, nrow(sites)))

if (any(sites$match_type == "unmatched")) {
  cat("\nUNMATCHED sites (no coordinate found) -- need a manual query rewrite:\n")
  print(sites[match_type == "unmatched", .(grenz_id, grenz, grenz_abschnitt)])
}
cat("\nplace_fallback / regional_approx / pass rows (lower precision -- worth a look):\n")
print(sites[match_type != "customs_point", .(grenz_id, grenz, match_type, matched_label)])
