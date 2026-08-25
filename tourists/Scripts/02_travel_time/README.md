# Origin → Destination travel-time pipeline (car + PT) in R

Computes, for every trip origin outside Switzerland, the car and public-transport
travel time to the tourist's actual reported destination in Switzerland. `grenz`
(which border crossing the trip used) is carried through as a descriptive label
only -- it is not a coordinate and is not used in the routing. Output feeds a
mode-choice model.

## Layout

```
Scripts/02_travel_time/
├─ config.yml                       # ALL paths and parameters — edit here
├─ 00_setup.R                       # packages, config, helpers (source first, always)
├─ 01_filter.R                      # unique origin→destination pairs (agqpv trips only)
├─ 02_download.R                    # OSM merge/clip + GTFS download
├─ 03_car_osrm.R                    # car times, agqpv pairs only, via local OSRM
├─ 03b_car_osrm_ausland_zones.R     # car times, every ausland origin → every CH zone centroid
├─ 04_pt_r5r.R                      # PT times via r5r (off-peak), agqpv pairs only
├─ 04_pt_google.R                   # PT times via Google Routes API (alternative to 04_pt_r5r.R)
├─ 05_join.R                        # merge agqpv car/PT times back to every row → model_input.csv
└─ 06_build_tt_lookups.R            # CH-CH / ausland-CH / agqpv times from the OMX demand-model
                                     # matrices (a different, older data source -- see its own header)
data/input/                         # put agqpv.csv and zones_communes.gpkg here
data/osm/  data/gtfs/  data/r5/     # created by scripts
data/output/                        # all outputs
```

## Run order

Set the working directory to the project root (the folder with `config.yml`),
then from an R session **inside your activated conda `r5` env**:

```r
source("Scripts/02_travel_time/01_filter.R")     # → pairs_to_route_agqpv.csv, row_to_pair_agqpv.csv
source("Scripts/02_travel_time/02_download.R")   # → data/osm/network.osm.pbf, data/gtfs/*.zip
# start the OSRM server (see 03_car_osrm.R header) in a separate terminal
source("Scripts/02_travel_time/03_car_osrm.R")   # → car_times_agqpv.csv (agqpv pairs only)
source("Scripts/02_travel_time/03b_car_osrm_ausland_zones.R")  # → origins_ausland.csv, car_times_ausland_CH.fst
# run 04 in a FRESH R session (JVM heap must be set before r5r loads)
source("Scripts/02_travel_time/04_pt_r5r.R")     # → pt_times.csv (or 04_pt_google.R as an alternative)
source("Scripts/02_travel_time/05_join.R")       # → model_input.csv
```

`car_times_agqpv.csv`/`pairs_to_route_agqpv.csv`/`row_to_pair_agqpv.csv` all
carry the `_agqpv` suffix because they cover only the origin→destination pairs
that actually occur in agqpv.csv (real respondent trips) -- enough for a
mode-choice logsum on observed trips, but not enough for a destination-choice
model. `03b_car_osrm_ausland_zones.R` fills that gap: every unique ausland
origin (deduplicated from agqpv.csv, ~3,037) routed to every Swiss zone
centroid (~7,966, from `data/input/zones_communes.gpkg`) -- the OSRM
equivalent of `06_build_tt_lookups.R`'s `tt_ausland_CH.fst`, but from real
road routing instead of the OMX matrices. Its output is normalized rather
than one row per origin: `car_times_ausland_CH.fst` has columns
`origin_id, zone_no, car_time_min, car_dist_km`; `origin_id` resolves via
`origins_ausland.csv` and `zone_no` joins back to `NO` in
`zones_communes.gpkg`. It's resumable (safe to re-run after an interruption)
and chunked to stay under this OSRM server's undocumented request-size limits
-- see the comments next to `osrm.table_origin_chunk`/`table_dest_chunk` in
`config.yml` if those limits ever need re-tuning (e.g. after a server
rebuild).

## Prerequisites

- Conda env `r5` with: r5py's R cousin **r5r**, **sf**, **dplyr**, **readr**,
  **osrm**, plus **openjdk 21** and the **osmium-tool** CLI.
  (r5r is installed on first run of 04 if missing.)
- **OSRM server** for the car step — Docker image is simplest if Docker is
  available (see `03_car_osrm.R` header); otherwise the R package
  **osrm.backend** downloads real OSRM binaries and runs the server without
  Docker or a compiler. Either way, leave the server running before `03_car_osrm.R`.
- Some **GTFS feeds require manual download** (FR/IT/ES/AT). Put the zips in
  `data/gtfs/` and set their entries in `config.yml`.

## Reproducibility

`data/osm/` and `data/gtfs/` hold the exact input snapshots used. Archive these
alongside the scripts — Geofabrik and the GTFS portals serve *latest* data, so
pinning URLs is not enough; pin the files.

## Caching behaviour

Every step in `02_download.R` (each country extract, `merged.osm.pbf`,
`network.osm.pbf`, each GTFS zip) only checks whether its output file already
exists locally — it never checks whether the remote source has changed, and
there's no re-fetch/rebuild-on-change logic anywhere in the script. Re-running
after a partial run resumes where it left off; re-running after a full run
does nothing.

**Gotcha:** `network.osm.pbf` is clipped to a bounding box computed from
`pairs_to_route_agqpv.csv` *at the moment `02_download.R` runs*. If `01_filter.R`'s
output changes afterward (different filter logic, different rows), the bbox
is **not** recomputed — `network.osm.pbf` keeps whatever extent it was built
with, silently. To force a rebuild (new bbox, or to pick up newer upstream
OSM/GTFS data), manually delete the relevant file(s) in `data/osm/` /
`data/gtfs/` before re-running — the script has no flag or check to force
this itself.

## Known limitations (document these in your methodology)

1. **`03_car_osrm.R`/`04_pt_r5r.R` route to respondents' actual reported
   destination coordinate**, not a zone centroid (verified: median ~63km
   from the corresponding border crossing, not the same point). Car and PT
   both use the same destination point, so the modal comparison stays
   consistent. `03b_car_osrm_ausland_zones.R` is the exception: it routes to
   Swiss **zone centroids** deliberately, because it needs a time to every
   candidate zone for the destination-choice model, not just the one each
   respondent visited.
2. **`grenz` is a label, not a coordinate.** Some `grenz` values are
   aggregate "Gruppe" catchments covering several nearby crossings, so
   grouping/reporting by `grenz` mixes those together -- but this has no
   effect on the routing itself, which always uses the tourist's own
   reported destination coordinate.
3. **Italian (and some other) GTFS coverage is patchy**; PT times may be
   missing or overstated where feeds are thin. Missing PT times appear as
   `NA` in the output.
4. **Car routing is free-flow** (OSRM has no congestion layer) — consistent
   with the off-peak PT assumption, but not peak-hour realistic.
5. **PT time is median over a 2-hour off-peak window** on one representative
   weekday; long international trips are sensitive to this choice.
