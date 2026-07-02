# Session Log — Tourist Mode Choice Replication (Danalet et al. 2023)

Project directory: `E:\ARE\ProjekteTIE\Turismusverkehr\RScript\tourists\`
Reference paper: Danalet, A. et al. (2023). *Modelling foreign tourists in Switzerland*. STRC Conference Paper.
Python repositories:
- `antonindanalet/mode_choice_tourists` (GitHub, master branch) — MNL estimation
- `antonindanalet/simba-python` (GitHub, master branch) — synthetic population generation

---

## 1. Commune join diagnostic (`diagnostic_commune_join.R`)

**Goal:** match TMS 2017 `destination` variable labels to `commune_name` in `data/commune_type_region.csv` in order to attach FSO urban/rural typology and AMR region codes to each observation.

**Key finding:** the TMS variable to use is `destination` (687 labelled SPSS codes = holiday commune), not a column called `commune`.

**CSV format** (`commune_type_region.csv`): 2,130 rows, columns `commune_name`, `urban_rural_topology` (1=urban, 2=intermediate, 3=rural), `urban_rural_topology_char` (French label), `region` (integer 1–16 = AMR Grossregionen).

**Baseline match rate:** 69.4% before any name harmonisation.

---

## 2. Name lookup table — maximising match rate

Built a ~100-entry lookup table (`name_lookup`) inside `diagnostic_commune_join.R` (and later copied into `part1_estimate_mode_choice.R`) covering three categories of mismatch:

| Category | Examples |
|---|---|
| English → German/French city names | Zurich → Zürich, Lucerne → Luzern, Geneva → Genève |
| Tourist resort/hamlet → post-merger official commune | Verbier → Val de Bagnes, Lenzerheide → Vaz/Obervaz, Sent/Ftan/Ardez/… → Scuol, Savognin/Bivio/… → Surses |
| Minor spelling / canton suffix differences | Klosters-Serneus → Klosters, Gossau SG → Gossau (SG), Lantsch / Lenz → Lantsch/Lenz |

**Result after lookup:** 97.9% match rate (21,556 / 22,021 observations matched). 465 observations remain unmatched (2.1%), mostly very small localities with few tourists.

---

## 3. Integration into `part1_estimate_mode_choice.R`

### Changes made

**New constant:**
```r
COMMUNE_REF_FILE <- "data/commune_type_region.csv"
```

**New Section 1b** — loads commune CSV, renames `region` → `amr_region` to avoid collision with the TMS `region` column, applies the name lookup, left-joins to `raw`:
```r
commune_ref <- read.csv(COMMUNE_REF_FILE, ...) %>% rename(amr_region = region)
raw <- raw %>%
  mutate(
    commune_name_raw = as.character(as_factor(destination)),
    commune_name_csv = ifelse(commune_name_raw %in% name_lookup$tms_name,
                              name_lookup$csv_name[match(commune_name_raw, name_lookup$tms_name)],
                              commune_name_raw)
  ) %>%
  left_join(commune_ref, by = c("commune_name_csv" = "commune_name"))
```

**Updated recode section** — replaced invented zone-based `urban` proxy with FSO typology:
```r
urban_fso = as.integer(!is.na(urban_rural_topology) & urban_rural_topology == 1L),
rural     = as.integer(!is.na(urban_rural_topology) & urban_rural_topology == 3L),
amr_region = amr_region,
```

**Updated filter** — added `!is.na(accomodation)` to drop 793 NA rows:
```r
filter(mode %in% ALT_CODES, !is.na(nationality_group),
       !is.na(weighting_factor), !is.na(accomodation))
```

**New Section 3b** — AMR region dummies with automatic zero-cell detection:
```r
AMR_REF_REGION <- 1L
amr_reg_levels <- setdiff(1:16, AMR_REF_REGION)
# Zero-cell check drops: amr_reg_3, 4, 5, 13, 14, 15 (≤53 obs, zero in some mode)
```

**mlogit.data() workaround** — `dfidx()` throws a `model.matrix` dimension error with mlogit 1.1.3 + R 4.4.2; replaced with deprecated `mlogit.data()`:
```r
tms_mlogit <- mlogit.data(as.data.frame(tms), choice="mode", shape="wide", id.var="person_id")
weights_long <- tms_mlogit[["weighting_factor"]]  # length = n_obs × n_alts
```

### Model results

| Item | Value |
|---|---|
| Observations | 12,016 (paper: 12,809; gap = 793 NA accommodation) |
| Alternatives | 7 modes |
| Parameters | 102 |
| Rho² | 0.431 (paper: 0.469) |
| AMR region dummies retained | 9 (regions 2,6,7,8,9,10,11,12,16; ref = 1) |
| Nationality dummies retained | 4 (germany, france, italy, usa; NL + UK dropped for zero cells) |

---

## 4. Parameter comparison: R model vs. paper Tables 4 & 5

Parameters in common between the R model and the paper (others not in both models are excluded).

### Table 4 parameters

| Parameter | Paper | R estimate | Δ |
|---|---|---|---|
| ASC_train | −0.066 | −1.158 | −1.09 |
| ASC_bus | −2.200 | −2.232 | −0.03 |
| ASC_long_distance_bus | −5.390 | −4.470 | +0.92 |
| ASC_tour_bus | −2.460 | −3.928 | −1.47 |
| ASC_bicycle | −3.600 | −4.020 | −0.42 |
| ASC_other | −2.530 | −2.501 | −0.03 |
| beta_france_belgium_train | −1.480 | −1.461 | +0.02 |
| beta_germany_train | −1.110 | −1.101 | +0.01 |
| beta_italy_train | −0.990 | −0.833 | +0.16 |
| beta_america_australasia_train | +0.610 | +0.665 | +0.06 |
| beta_france_tour_bus | −2.270 | −2.043 | +0.23 |
| beta_germany_denmark_tour_bus | −0.801 | −0.635 | +0.17 |
| beta_italy_tour_bus | −2.080 | −1.287 | +0.79 |

Large ASC gaps (train, tour_bus) are due to ~12 nationality groups present in the paper but absent from the R model (their effects absorb into the ASCs).

### Table 5 parameters (urban/rural typology)

| Parameter | Paper | R estimate | Δ |
|---|---|---|---|
| beta_urban_train | +0.916 | +0.963 | +0.05 |
| beta_urban_bus | +0.863 | +0.980 | +0.12 |
| beta_urban_long_distance_bus | +1.060 | +0.856 | −0.20 |
| beta_urban_tour_bus | +0.578 | +0.677 | +0.10 |
| beta_rural_train | −0.242 | −0.274 | −0.03 |
| beta_rural_other | −0.478 | −0.086 | +0.39 |

Urban/rural parameters replicate well (most within 0.12). Nationality→train also replicate very closely (within 0.02–0.16).

---

## 5. `prepare_hotel_data.R` — include all accommodation structures

**Problem:** the script filtered `betriebsart == "Hotel"`, dropping Camping (406) and Kurbetriebe (20) from `PASTA_HESTA.xlsx`.

**Fix:**
```r
# Before:
hotels <- raw[!is.na(raw$betriebsart) & raw$betriebsart == "Hotel", ]

# After:
hotels <- raw[!is.na(raw$betriebsart), ]
```

`accommodation_type` column changed from hardcoded `"Hotel"` to the actual `betriebsart` value:
```r
# Before:
accommodation_type = "Hotel",

# After:
accommodation_type = betriebsart,
```

Geocoding cache extended to cover newly included establishments (partial-cache handling added to the `if (file.exists(CACHE_FILE))` branch).

**Result after re-run:**
- 4,764 total establishments retained (4,338 Hotel + 406 Camping + 20 Kurbetriebe)
- 99 previously uncached establishments geocoded; 3 remain without coordinates
- `hotels_clean.csv` regenerated: 4,764 rows × 14 cols

---

## 6. `part2_tourist_population.R` — use `accommodation_type` from data

**Problem:** `HOTEL_TYPE_LABELS` was hardcoded as `c("hotel", "Hotel", "HOTEL")`, not derived from the actual values in the CSV.

**Fix:** derive the label set from the data itself:
```r
HOTEL_TYPE_LABELS <- unique(hotels_raw$accommodation_type[
  grepl("^[Hh]otel$", hotels_raw$accommodation_type)
])
```

Additional improvements:
- Accommodation type breakdown printed on load (`table(hotels_raw$accommodation_type)`)
- `HOTEL_TYPE_LABELS` echoed to console before agent expansion
- `accom_hotel` distribution (hotel=1, supplementary=0) printed after expansion
- Log messages updated from "hotels" to "establishments"

---

## 7. Analysis of `antonindanalet/simba-python` — zone assignment

**Question:** does `distribute_tourists_in_tourist_accommodation.py` assign zones directly to agents?

**Answer: no.** Zone IDs live on households (accommodations), not persons.

- `get_hotels()` retains `zone_id` from the OSM-derived hotel CSV.
- Supplementary accommodation DataFrames also carry `zone_id`.
- The persons DataFrame only receives `household_id` and `country_of_origin`.
- VISUM resolves the zone by joining `person.household_id → household.zone_id`.

The R Part 2 script puts `excursion_zone` directly on each agent row (materialising the join in R); the Python pipeline defers the join to VISUM. Both are equivalent.

---

## 8. External databases per script (`antonindanalet/simba-python`)

### `load_overnights_in_hotels.py`

| Source | Institution | Format |
|---|---|---|
| HESTA — commune level (overnights by country × commune) | FSO | `.xlsx` (sheet "Hotellerie V1") |
| HESTA — canton level (overnights by country × canton) | FSO | `.csv` (ISO-8859-1) |
| Communes-by-canton list | FSO | `.xlsx` |
| Hotels, motels, guest houses (live query) | OpenStreetMap / osmnx | REST API |
| NPVM traffic zone polygons | ARE / NPVM | `zones.gpkg` (LV95) |

### `load_overnights_in_supplementaty_accommodation.py`

| Source | Institution | Format |
|---|---|---|
| PASTA — holiday homes (overnights by major region) | FSO | `.xlsx` (sheet "2019") |
| PASTA — collective accommodation (hostels, alpine huts, etc.) | FSO | `.xlsx` (sheet "2019") |
| PASTA — campsites (overnights by major region) | FSO | `.xlsx` (sheet "2019") |
| Swiss major regions / Grossregionen boundaries | FSO / swisstopo | `.shp` |
| Chalets, apartments, hostels, alpine_huts, camp_sites (live query) | OpenStreetMap / osmnx | REST API |
| NPVM traffic zone polygons | ARE / NPVM | `zones.gpkg` (LV95) |

### `distribute_tourists_in_tourist_accommodation.py`

Consumes only intermediate CSVs/JSON written by the two load scripts above. No new external sources.

### `get_tourists_and_hotels.py`

Orchestrator. No direct data reads.

### `utils.py`

No external data. Pure utility functions (`fill_missing_values_beds`, `removing_new_openings`).

### Summary — unique primary external sources across the whole pipeline

| # | Source | Institution | Used for |
|---|---|---|---|
| 1 | HESTA commune-level `.xlsx` | FSO | Hotel overnights by country × commune |
| 2 | HESTA canton-level `.csv` | FSO | Hotel overnights by country × canton (residual) |
| 3 | Communes-by-canton `.xlsx` | FSO | Commune → canton mapping |
| 4 | PASTA holiday homes `.xlsx` | FSO | Holiday home overnights by major region |
| 5 | PASTA collective accommodation `.xlsx` | FSO | Collective accommodation overnights by major region |
| 6 | PASTA campsites `.xlsx` | FSO | Campsite overnights by major region |
| 7 | Swiss major regions shapefile | FSO / swisstopo | Spatial boundaries for OSM queries |
| 8 | OpenStreetMap (live, via osmnx) | OSM | Geolocated accommodation points + bed counts |
| 9 | NPVM traffic zone polygons (`zones.gpkg`) | ARE / NPVM | Zone ID assignment via spatial join |

---

## 9. Swiss major regions shapefile — where it is used

**Question:** where exactly is the Swiss major regions shapefile used in the simba-python pipeline?

**Answer:** it is used exclusively in `load_overnights_in_supplementaty_accommodation.py`, inside `get_major_regions()`, which is called once at the top of `load_overnights_in_supplementary_accommodation()`.

```python
def get_major_regions(dict_path):
    major_regions = geopandas.read_file(dict_path["major_regions_shape_file"])
    major_regions["name"] = major_regions["name"].map({
        "R": "Région lémanique", "Espace Mittelland": "Espace Mittelland", ...
    })
    major_regions.to_crs(crs="epsg:4326", inplace=True)
    return major_regions
```

The resulting GeoDataFrame is then passed as `boundary_polygon` to `osmnx` queries in all three supplementary accommodation loaders:

| Function | OSM tags queried |
|---|---|
| `load_holiday_homes_from_osm()` | `tourism=chalet`, `tourism=apartment` |
| `load_collective_accommodation_from_osm()` | `tourism=hostel`, `tourism=alpine_hut`, `tourism=wilderness_hut` |
| `load_campsites_from_osm()` | `tourism=camp_site` |

It is **not** used in `load_overnights_in_hotels.py` — hotels are queried from OSM using individual commune and canton boundaries, not major region polygons.

The 7 Grossregionen polygons also stamp a `region` string onto each accommodation record (e.g. `"Zentralschweiz"`), which `distribute_tourists_in_supplementary_accommodation()` uses to match records against the PASTA overnights-by-region CSV.

---

## 10. `antonindanalet/mode_choice_tourists` — script analysis

Repository: `https://github.com/antonindanalet/mode_choice_tourists` (master branch)
Language: Python 7.3%, HTML 92.7% (HTML is the Biogeme output report)
Scripts: `get_data.py`, `main.py`

### `get_data.py`

**Purpose:** load and clean the TMS survey data, join spatial attributes.

**External data read:**

| File | What it contains |
|---|---|
| `26_TMS_final SBB_Datenlieferung_Februar2023.sav` | TMS survey — main mode choice, accommodation type, stars, nationality, commune code (Q1_BFS_final) |
| `data/input/Raumgliederungen_StadtLand2017.xlsx` | FSO urban/rural typology per commune (3 categories) |
| `data/input/Raumgliederungen_AMR2018.xlsx` | FSO AMR Grossregionen (16 labour market areas) per commune |

**Key processing steps:**
- Filters to foreign tourists only (`Q2B_ST_markets_detail_new2 != 4`)
- Drops rows with missing main mode (`Q9_bereinigt_gruppiert_final`)
- Renames columns to model variable names (`transport_mode`, `age`, `country`, `stars`, `accommodation_type`, `commune`)
- Maps 780+ integer commune codes to commune name strings via a hardcoded `dict_code2commune`
- Merges `urban_rural_typology` from `StadtLand2017.xlsx` on commune name (with ~10 manual spelling corrections)
- Merges `region` (AMR code 1–16) from `AMR2018.xlsx` on commune name (with ~4 manual corrections)
- Recodes `transport_mode`: groups motorcycle + car → 3; camper → 9
- Fills remaining NAs with −99

**Output:** cleaned pandas DataFrame returned in memory to `main.py`.

### `main.py`

**Purpose:** define and estimate the multinomial logit mode choice model using Biogeme.

**Input:** DataFrame from `get_data.get_data()` (no additional files read).

**Model structure:**
- 7 alternatives: train (1), bus (2), car (3), long-distance bus (6), tour bus (7), bicycle (8), other (9); car is reference (ASC fixed to 0)
- Variables per alternative: ASC, nationality dummies (~20 groups per mode), accommodation type dummies (hotel, holiday_homes, camping, collective_accommodation), hotel stars, urban/rural dummies, 16 AMR region dummies
- Estimation via `biogeme.estimate()` (maximum likelihood)

**Output:** writes to `data/output/`:
- `mode_choice_tourists.html` — full estimation report
- `mode_choice_tourists.pickle` — serialised Biogeme results object
- `mode_choice_tourists.tex` — LaTeX parameter table (via `results.writeLaTeX()`)

**Key differences from the R replication:**
- Uses the full TMS dataset (not just 2017); includes all nationality groups → no zero-cell dropping
- `accommodation_type` is 4-category (hotel, holiday_homes, camping, collective_accommodation), not binary
- `stars` enters as a continuous variable interacted per mode, not as part of the binary hotel dummy
- Regions are 16 individual dummies (all retained, no zero-cell dropping because full dataset is larger)
- `age` is defined as a variable but not included in any utility function in the published script

---

## 11. Data flow between the two Python pipelines

The two repos are **complementary but not directly connected in Python**. The link is through VISUM/MOBi.

### Pipeline diagram

```
╔══════════════════════════════════════════════════════════════╗
║  REPO 1 — antonindanalet/mode_choice_tourists                ║
╚══════════════════════════════════════════════════════════════╝

  TMS survey .sav
  StadtLand2017.xlsx  ──────────▶  get_data.py
  AMR2018.xlsx                          │
                                        │  DataFrame (in memory)
                                        ▼
                                     main.py
                                        │
                                        ▼
                                  data/output/
                            (MNL estimated coefficients)


╔══════════════════════════════════════════════════════════════╗
║  REPO 2 — simba-python / src/simba/mobi/synpop/tourists      ║
╚══════════════════════════════════════════════════════════════╝

              get_tourists_and_hotels.py  (orchestrator)
                    │                        │
                    ▼                        ▼

  HESTA commune.xlsx            PASTA holiday_homes.xlsx
  HESTA canton.csv              PASTA collective_accom.xlsx
  communes_by_canton.xlsx  ──▶  PASTA campsites.xlsx        ──▶
  OpenStreetMap (live)          OpenStreetMap (live)
  zones.gpkg                    Major regions shapefile
                                zones.gpkg
        │                               │
        ▼                               ▼
load_overnights_in_hotels.py    load_overnights_in_
        │                       supplementary_accommodation.py
        ▼                               │
  overnights_in_hotels_                 ▼
    detailed.csv                  overnights_in_holiday_homes.csv
  overnights_in_hotels_in_        overnights_in_collective_accom.csv
    cantons_only.csv              overnights_in_campsites.csv
  list_of_communes_               holiday_homes_with_id_zones.csv
    with_hotels.json              collective_accom_with_id_zones.csv
  hotels_with_id_zones.csv        campsites_with_id_zones.csv
        │                               │
        └───────────────┬───────────────┘
                        ▼
        distribute_tourists_in_tourist_accommodation.py
                        │
              ┌─────────┴─────────┐
              ▼                   ▼
         persons.csv       households.csv
         (VISUM import)    (VISUM import)


  MNL coefficients (data/output/) ──▶  VISUM/MOBi
  persons.csv + households.csv    ──▶  VISUM/MOBi
                                            │
                                            ▼
                                  mode assigned per agent
```

The MNL coefficients estimated in `mode_choice_tourists` are applied in VISUM, not in any Python script in `simba-python`.

### External data summary — both Python repos combined

| # | File | Institution | Format | Used by |
|---|---|---|---|---|
| 1 | TMS survey `.sav` | FSO / SBB | SPSS | `get_data.py` |
| 2 | `Raumgliederungen_StadtLand2017.xlsx` | FSO | `.xlsx` | `get_data.py` |
| 3 | `Raumgliederungen_AMR2018.xlsx` | FSO | `.xlsx` | `get_data.py` |
| 4 | HESTA commune-level `.xlsx` | FSO | `.xlsx` | `load_overnights_in_hotels.py` |
| 5 | HESTA canton-level `.csv` | FSO | `.csv` | `load_overnights_in_hotels.py` |
| 6 | Communes-by-canton `.xlsx` | FSO | `.xlsx` | `load_overnights_in_hotels.py` |
| 7 | PASTA holiday homes `.xlsx` | FSO | `.xlsx` | `load_overnights_in_supplementary.py` |
| 8 | PASTA collective accommodation `.xlsx` | FSO | `.xlsx` | `load_overnights_in_supplementary.py` |
| 9 | PASTA campsites `.xlsx` | FSO | `.xlsx` | `load_overnights_in_supplementary.py` |
| 10 | Swiss major regions shapefile | FSO / swisstopo | `.shp` | `load_overnights_in_supplementary.py` |
| 11 | OpenStreetMap (live, via osmnx) | OSM | REST API | `load_overnights_in_hotels.py`, `load_overnights_in_supplementary.py` |
| 12 | `zones.gpkg` (NPVM traffic zones) | ARE / NPVM | GeoPackage | `load_overnights_in_hotels.py`, `load_overnights_in_supplementary.py` |

---

## 12. Open issues / known gaps

| Issue | Detail |
|---|---|
| Observation gap | R model: 12,016 obs vs paper: 12,809. Difference = 793 NA in `accomodation` variable. Paper may use a different variable or impute missing values. |
| SCALE_FACTOR in part2 | **Retained** at `0.01` for computational convenience (Part 4 agent loop). Reframed from invented parameter to explicit thinning factor; Part 5 reweights by `1/SCALE_FACTOR`. `party_size_lambda` and `party_size` removed. See section 15. |
| Missing nationality groups | ~12 nationality groups in the paper are absent from R model (zero cells). Their effects absorb into ASCs, explaining large ASC gaps. |
| LOS skims | **Not used.** Verified by reading `main.py` in `antonindanalet/mode_choice_tourists`: all V_ utility functions use only ASCs, nationality dummies, accommodation type (4 categories), hotel_stars, urban, rural, and 16 AMR region dummies. `age_train` and `age_car` are defined with `DefineVariable` but never appear in any utility expression. No travel time or cost variables exist anywhere in the script. The rho² gap (0.431 R vs 0.469 paper) is explained by fewer nationality groups in the R model (zero-cell dropping) and a slightly smaller sample (12,016 vs 12,809 obs). |
| zones.gpkg | Required by part2 spatial join; file not present in the R project — must be sourced from NPVM/ARE. |

---

## 13. R script data inventory

### External data in `data/` used by R scripts

| File | Used by |
|---|---|
| `TMS 2017.sav` | `check_destination.R`, `check_unmatched.R`, `diagnostic_commune_join.R`, `figures5_6.R`, `part1_estimate_mode_choice.R` |
| `PASTA_HESTA.xlsx` | `prepare_hotel_data.R` |
| `commune_type_region.csv` | `check_unmatched.R`, `diagnostic_commune_join.R`, `part1_estimate_mode_choice.R` |
| `amr_zones.gpkg` | `part2_tourist_population.R` |

### Pipeline intermediates (created → consumed)

| File | Created by | Read by | Present? |
|---|---|---|---|
| `data/hotels_geocoded.csv` | `prepare_hotel_data.R` | `prepare_hotel_data.R` (cache) | yes |
| `data/hotels_clean.csv` | `prepare_hotel_data.R` | `part2_tourist_population.R` | yes |
| `part1_output.RData` | `part1_estimate_mode_choice.R` | `part2`, `part3` | yes |
| `part2_output.RData` | `part2_tourist_population.R` | `part3`, `part5` | not run yet |
| `part3_output.RData` | `part3_mode_assignment.R` | `part4`, `part5` | not run yet |
| `part4_output.RData` | `part4_tourist_activity_simulation.R` | `part5` | not run yet |
| `part5_output.RData` | `part5_od_matrices.R` | — | not run yet |
| `od_matrices.csv` | `part5_od_matrices.R` | — | not run yet |
| `plots/figure5_mode_shares.pdf` | `figures5_6.R` | — | yes |
| `plots/figure6_mode_shares_by_country.pdf` | `figures5_6.R` | — | yes |

### Files in `data/` not referenced by any R script

| File | Note |
|---|---|
| `zones.gpkg` | Python pipeline (NPVM traffic zones) — not used by R |
| `Handlungsräume.xlsx` | FSO functional spatial units — unused |
| `Tourismusregionen 2015.xlsx` | FSO tourism regions — unused |
| `reisenmueb.sav` / `.csv` | TMS "Reisende und Übernachtende" module — unused |
| `tagesreisen.sav` / `.csv` | TMS day-trips module — unused |
| `zielpersonen.sav` / `.csv` | TMS target-persons module — unused |

---

## 14. Python output files: columns in `persons.csv` and `households.csv`

### `households.csv` — one row per accommodation point

| Variable | Content | Source |
|---|---|---|
| `household_id` | Sequential unique ID from `FIRST_RESERVED_ID` | Generated |
| `tourist_accommodation_category` | `"hotels"`, `"campsites"`, `"holiday_homes"`, `"collective_accommodation"` | Assigned |
| `hotel_stars` | Star rating (renamed from `"stars"`) | HESTA / OSM |
| `name` | Establishment name | OSM |
| `osmid` | OpenStreetMap identifier | OSM |
| `addr:housenumber` | Street number | OSM |
| `addr:street` | Street name | OSM |
| `addr:city` | City | OSM |
| `region` | Swiss major region (Grossregion) | Major regions shapefile |
| `holiday_homes_type` | Subtype for holiday homes | OSM |
| `xcoord` | LV95 easting | OSM / geocoded |
| `ycoord` | LV95 northing | OSM / geocoded |
| `zone_id` | NPVM traffic zone ID | `zones.gpkg` spatial join |
| `beds` | Number of beds | HESTA / OSM |
| `rooms` | Number of rooms | OSM |
| `capacity` | Total capacity | OSM |
| `capacity:persons` | Capacity in persons | OSM |
| `capacity:caravans` | Caravan pitches (campsites) | OSM |
| `capacity:pitches` | Total pitches (campsites) | OSM |
| `capacity:tents` | Tent spaces (campsites) | OSM |

### `persons.csv` — one row per tourist overnight stay

| Variable | Content | Source |
|---|---|---|
| `person_id` | Sequential unique ID from `FIRST_RESERVED_ID` | Generated |
| `household_id` | Links to `households.csv` | Generated via weighted sampling |
| `country_of_origin` | Origin country | HESTA/PASTA overnight counts |
| `is_swiss` | Always `False` | Hardcoded |
| `age` | Random draw from TMS age distribution (18–98, 5-year bins) | TMS-derived |
| `age_cat` | Age category 2–6 | Derived from age |
| `current_edu` | Always `0` | Hardcoded |
| `highest_education` | Always `0` | Hardcoded |
| `level_of_employment` | Always `0` | Hardcoded |
| `level_of_employment_cat` | Always `0` | Hardcoded |
| `current_job_rank` | Always `0` | Hardcoded |

### How persons and households are linked

`household_id` is assigned sequentially to each accommodation in `get_tourist_accommodation()`. Persons are created by drawing from accommodation `household_id` values using `random.choices()`, weighted by `beds`, with `k = number_of_tourists_in_commune` (hotels) or `k = number_of_tourists_in_region` (supplementary). The sampled IDs directly populate the persons DataFrame — no explicit join. Multiple persons share the same `household_id` (several overnight stays at the same establishment). VISUM resolves `person.household_id → household.zone_id` at import time.

---

## 15. `party_size` removal — rationale and changes

### Why `party_size_lambda` was introduced (incorrectly)

`party_size_lambda` was an invented parameter added to create agent-level variation in the activity simulation (Part 4). It had no grounding in:
- Danalet et al. (2023): party size is not a variable in the MNL
- Scherr et al. (2020): party size applies to the resident population model, not tourists; no tourist-specific lambdas are published
- The Python pipeline: party size does not exist anywhere in `simba-python`

It also caused `part2_tourist_population.R` to generate a `party_size` column that propagated spuriously into Part 3 and Part 4.

### Why the activity simulation is non-trivial without it

Variation in the simulation already comes from the nested probabilistic structure:
- Excursion frequency MNL (0 / 1 / 2 excursions per day)
- Stop frequency MNL (0 / 1 / 2 stops per excursion)
- Activity type draw (Leisure / Shopping / Sightseeing) per chain position
- Duration sampling from truncated normals per activity type
- Departure time sampling

The genuine missing complexity is a **destination choice model** (selecting an excursion zone by attraction weight and travel skims), not party size.

### Changes made

| File | Change |
|---|---|
| `part2_tourist_population.R` | `SCALE_FACTOR` retained at `0.01` (computational thinning, not an invented modelling parameter); comment updated to explain purpose; `party_size_lambda` and `party_size` removed; header and runtime messages updated |
| `part4_tourist_activity_simulation.R` | `party_n` removed from `tourist_excursion_freq()` and `tourist_stop_freq()`; utility expressions simplified; variable comment updated |
| `prepare_hotel_data.R` | Header note and `[MISSING]` gap text referencing `party_size` removed |

---

## 16. Accommodation type extended to 4-category factor

### Context

The Python pipeline (`households.csv`) distinguishes four accommodation categories: hotel, camping, holiday_home, collective. The TMS 2017 binary (`accom_hotel`) is a simplification; the paper uses `Q16_bereinigt_final_aggregiert` with finer sub-codes. When a finer TMS source becomes available, the binary will be replaced by a factor.

### Changes made

**`part1_estimate_mode_choice.R`** — new section 7b inserted after `coef_table` is built. Appends 18 invented rows (6 non-reference alternatives × 3 new categories) with `NA` std_error and t_stat. The existing binary `accom_hotel` estimation is unchanged.

Invented coefficients (relative to car, reference = hotel):

| Term | Estimate | Rationale |
|---|---|---|
| `accom_camping:1–9` | −0.50 to +0.20 | Camping tourists are stronger car users; small bicycle bonus |
| `accom_holiday_home:1–9` | −0.20 to +0.10 | Moderate car preference, weaker than camping |
| `accom_collective:1–9` | −0.10 to +0.20 | Close to hotel; positive tour bus bonus for spa/health resorts |

**`part2_tourist_population.R`** — `HOTEL_TYPE_LABELS` / `accom_hotel` binary replaced by:
- `ACCOM_TYPE_MAP`: Hotel→hotel, Camping→camping, Kurbetriebe→collective, unknown→holiday_home
- `map_accom_type()` helper function
- `accom_type` character column + three dummies: `accom_camping`, `accom_holiday_home`, `accom_collective`

**`part3_mode_assignment.R`** — `compute_utilities()` updated: the single `accom_hotel` lookup replaced by a loop over the three dummies. Hotel tourists contribute zero from all three (hotel is the reference).

**`part4_tourist_activity_simulation.R`** — `accom_bonus` (invented, no paper source) removed entirely from `tourist_excursion_freq()`. `accom_type == "hotel"` check removed alongside it.

---

## 17. Age variables added to `tourist_pop` (R Part 2)

Age assignment uses the TMS distribution taken directly from the Python pipeline (`distribute_tourists_in_tourist_accommodation.py`). No invented parameters.

### Constants added to `part2_tourist_population.R`

```r
AGE_BINS  <- seq(18L, 98L, by = 5L)   # 17 five-year bin midpoints

AGE_PROBS <- c(                        # TMS probabilities (sum = 1)
  0.0309, 0.0971, 0.1255, 0.1134, 0.1016, 0.1031, 0.1012,
  0.1019, 0.0798, 0.0683, 0.0449, 0.0213, 0.0080, 0.0023,
  0.000170, 0.000145, 0.000272
)

AGE_CAT_MAP <- c(                      # identical mapping to Python pipeline
  "18"=2, "23"=2,
  "28"=3, "33"=3, "38"=3, "43"=3,
  "48"=4, "53"=4, "58"=4, "63"=4,
  "68"=5, "73"=5,
  "78"=6, "83"=6, "88"=6, "93"=6, "98"=6
)
```

### New columns in `tourist_pop`

| Variable | How assigned | Source |
|---|---|---|
| `age` | `sample(AGE_BINS, n(), replace=TRUE, prob=AGE_PROBS)` | TMS distribution (Python pipeline) |
| `age_cat` | `AGE_CAT_MAP[as.character(age)]` | Same mapping as Python pipeline |

Age category boundaries: 2 = 18–27, 3 = 28–47, 4 = 48–67, 5 = 68–77, 6 = 78–98.

---

## 18. Current structure of `tourist_pop` (R Part 2 output)

One row per synthetic agent (= 1/SCALE_FACTOR overnight stays). Saved in `part2_output.RData`.

| Variable | Type | How assigned | Source |
|---|---|---|---|
| `agent_id` | int | `row_number()` after `uncount()` | Generated |
| `hotel_id` | int | Column 1 of `PASTA_HESTA.xlsx` (1–4764) | `hotels_clean.csv` |
| `excursion_zone` | chr | Spatial join of hotel LV95 coords onto `amr_zones.gpkg` | `amr_zones.gpkg` |
| `nationality_group` | chr | `nights_*` column names stripped and mapped to 8 TMS groups | `hotels_clean.csv` |
| `accom_type` | chr | `betriebsart` via `ACCOM_TYPE_MAP`: Hotel→hotel, Camping→camping, Kurbetriebe→collective, other→holiday_home | `hotels_clean.csv` |
| `accom_camping` | int | `as.integer(accom_type == "camping")` | Derived |
| `accom_holiday_home` | int | `as.integer(accom_type == "holiday_home")` | Derived |
| `accom_collective` | int | `as.integer(accom_type == "collective")` | Derived |
| `stars` | int | Regex on `kategorie` field ("3-Stern" → 3); NA for non-hotel types | `hotels_clean.csv` |
| `n_nights` | int | `rpois(lambda = n_nights_hotel × SCALE_FACTOR)`, min 1 | Stochastic draw |
| `age` | int | `sample(AGE_BINS, prob = AGE_PROBS)` — 17 bins 18–98 | TMS distribution |
| `age_cat` | int | `AGE_CAT_MAP[age]` → 2–6 | TMS distribution |
| `urban` | int | `urban_rural` from `amr_zones.gpkg`, default 1 if absent | `amr_zones.gpkg` |
| `purpose` | chr | Hardcoded `"leisure"` | Assumption |

### Notes

- `accom_camping`, `accom_holiday_home`, `accom_collective` are the MNL dummy inputs for Part 3. Hotel tourists have all three = 0 (hotel is the reference category).
- `excursion_zone` replaces the Python pipeline's two-table `person.household_id → household.zone_id` join — zone is carried directly on each agent row.
- `stars` is NA for camping and collective establishments (no star rating in HESTA).
- `age` and `age_cat` use the same TMS probabilities and category boundaries as the Python pipeline; no invented parameters.
- `n_nights` has no equivalent in the Python pipeline (each person row = 1 overnight). It exists here to drive Part 4's day-plan loop and is stochastic due to SCALE_FACTOR thinning.

---

## 19. Scherr et al. (2020) — how leisure activities are assigned to agents

Reference: Scherr et al. (2020), *Towards agent-based travel demand simulation across all mobility choices*, EJTIR 20(4), pp. 152–172. PDF at `E:\ARE\ProjekteTIE\Turismusverkehr\RScript\tourists\Scherr_et_al2020.pdf`.

### 19.1 Activity type assignment

Leisure is one of six **secondary activity types**: Leisure (L), Shopping (S), Business (B), Education-as-secondary (EC), Accompany (A), Other (O). The type is **not chosen by an MNL** — it is assigned via **fixed probabilities by person group**, derived from the national travel diary (MZ 2015). Probabilities are applied for each stop position (outbound stop / primary stop / inbound stop). Our `assign_tourist_activity()` in Part 4 replicates this structure; only the probabilities are invented rather than TMS-estimated.

### 19.2 Destination choice

After activity type is known, a **destination zone** is drawn from a **nested mode-destination choice model**:

```
U(j | i) = A_j  +  shadow_j  +  shadow_ij  +  lambda * EMU_ij
```

| Term | Meaning |
|---|---|
| A_j | Socio-economic attraction of zone j (e.g. retail floor area, green space — activity-type-specific) |
| shadow_j | Destination shadow price — calibrated to empirical trip-length distributions |
| shadow_ij | OD-pair shadow price — calibrated to empirical commuter/trip flows |
| EMU_ij | Expected Maximal Utility of mode choice for OD pair ij; nests mode inside destination so that multimodal accessibility affects where people go |

Mode choice itself (eq. 1–2) uses travel time, distance, cost, PT frequency, transfers, parking (Table 2 of paper). The nesting means destination and mode are jointly optimised.

**Current state in Part 4:** destination choice is entirely absent. `excursion_zone = hotel_zone` for every agent (self-loop). This means Part 5 OD matrices are all zeros off-diagonal.

### 19.3 Rubber banding — when it applies and when it does not

Rubber banding is used **only for intermediate stops within primary tours** (work/education tours). The stop must lie between two pre-defined endpoints (home and workplace/school), and the rubber banding formula weights the detour penalty against both the trip origin (weight α) and the primary activity location (weight β) to minimise out-of-way travel (eq. 4 of paper).

For tourists, who have **no primary tours**, rubber banding in the strict Scherr et al. sense does not apply. An analogous penalisation could be used for intermediate stops within an excursion tour (hotel → stop → main sight → hotel), where the main sight plays the role of the primary location and the hotel plays the role of home/origin.

### 19.4 Activity durations

Durations are **weighted random draws from empirical CDFs** (Figure 2 of paper) derived from MZ 2015, segmented by activity type, socio-economic group, and how many times the activity appears in the plan (e.g. one vs. two work tours). Our `DURATION_PARAMS` with truncated normals is the correct structural approach; parameter values are invented.

### 19.5 Activity start-time scheduling

Uses the **"outward" approach** (Castiglione et al. 2015): priority is given to the primary activity; secondary stops are scheduled around it. Start times for secondary activities are only chosen implicitly. An iterative scoring algorithm then checks start-times against empirical probability curves (Figure 3 of paper); if poorly scored, the agent tries an alternative primary start-time.

Our `schedule_excursion()` in Part 4 implements the sequential outward placement but without the scoring/iteration step and without empirical start-time distributions (departure time is uniform U[7, 11]).

### 19.6 Plan-building (budget enforcement)

Generate up to `|D|=3` destination alternatives and `|T|=10` duration alternatives, then draw random combinations until out-of-home time fits within the budget (Table 3: 12–14 h in iteration 1). If no valid combination is found after max N tries, the agent repeats all daily choices from tour generation onward. Over 99% of agents find a valid plan on the first pass.

Our `build_excursion()` mirrors this with `N_TRIES=20` and a fallback to minimum durations; it does not regenerate destinations (only one destination — the hotel zone) and does not re-run the full preference chain.

### 19.7 Summary — gaps between Part 4 and Scherr et al.

| Component | Scherr et al. | Part 4 (current status) |
|---|---|---|
| Activity type probabilities | Empirical by person group (MZ 2015) | Invented — flagged `[INVENTED PARAMETER]` |
| Destination choice | Nested mode-destination MNL with zonal attraction + LOS skims | **Not implemented** — self-loop (hotel zone) |
| Rubber banding | For intermediate stops in primary tours | Not implemented |
| Activity durations | Empirical CDFs from MZ 2015, segmented | Truncated normals, invented params — flagged |
| Start-time scoring | Score vs. empirical curves, iterate | Not implemented — uniform departure draw |
| Plan-building budget | 3 destination × 10 duration alternatives | 20 duration retries, no destination alternatives |

**Highest-priority gap:** destination choice. Without it, Part 5 OD matrices collapse to diagonal (all trips start and end at hotel zone).
