# Routing: what was tried, what worked, what didn't

A record of every routing approach attempted for this pipeline, in case any
of it needs revisiting. Two travel modes, each with its own story: **car**
(settled quickly, then hit scale problems later) and **PT** (settled slowly,
eventually abandoned for the tiled/local approach in favor of speed).

## Car routing

### Option: Docker OSRM
**Not usable on this machine** -- Docker Desktop isn't available here.
Otherwise the standard, best-documented way to run OSRM. If this pipeline
ever moves to a machine with Docker, it's worth reconsidering (see git
history of `03_car_osrm.R` for the docker run commands used before this
constraint was found).

### Option: `osrm.backend` R package (chosen)
Downloads real OSRM binaries (Windows/macOS/Linux, no Docker, no compiler)
and drives extract/partition/customize/serve. **This is what's in use.**

Two bugs hit along the way, both fixed:
- **`osrm_start()`'s child process dies when the parent R session exits**
  (Windows process-tree behavior) -- fixed by starting the server from a
  script that blocks forever, run as a detached background process instead
  of a one-off `Rscript -e ...`.
- **`osrm` package v5.0.0 broke `osrmRoute()`'s return type** -- it now
  returns a named numeric vector (`c(duration=.., distance=..)`), not a
  data.frame/list, so the old `$duration`/`$distance` accessors silently
  broke. Fixed to `r["duration"]`/`r["distance"]`.

**Status: working, two datasets computed successfully.**
1. `car_times_agqpv.csv` -- the 7,726 unique origin/destination pairs that
   actually occur in agqpv.csv, routed one pair at a time via `osrmRoute()`
   (`03_car_osrm.R`). 0 missing.
2. `car_times_ausland_CH.fst` -- every unique ausland origin (3,037) to
   every Swiss zone centroid (7,966) = 24,192,742 pairs, routed via the
   `/table` (many-to-many) service instead of one-by-one (`03b_car_osrm_
   ausland_zones.R`). 0 missing, ~87 minutes total.

**Scaling this up (dataset 2) surfaced two undocumented limits on this
server** that took real trial and error to find (see also the comments next
to `osrm.table_origin_chunk`/`table_dest_chunk` in `config.yml`):

| Limit | Symptom | Threshold found |
|---|---|---|
| `--max-table-size` (matrix cells = src x dst) | Clean `400 TooBig` error | **10,000 cells** -- NOT the 8,000,000 documented as OSRM's default; this server is configured (or defaults, via `osrm.backend`) much lower |
| Undocumented request-size ceiling | Connection dies outright -- `curl: Server returned nothing (no headers, no data)`, no HTTP error at all | **~2,200-2,400 total coordinates** (src+dst combined), almost certainly a URL-length ceiling in osrm-routed's request parser, hit *before* the request reaches OSRM's own size check |

Both had to be respected simultaneously, which ruled out the naive plan
(200 origins x all 7,966 destinations per call -- instant crash on the very
first request). A further finding: **a balanced src/dst split routes ~4x
faster per cell than a skewed one** (0.15s/1,000 cells at 30 origins x 330
destinations, vs 0.64s/1,000 cells at 5 origins x 1,980 destinations, timed
directly against this server) -- consistent with OSRM's many-to-many search
cost scaling with the number of sources, so a few sources x many
destinations pays that per-source cost far more times per cell than a
balanced shape does. This is *this server's* behavior; if the OSRM binary,
version, or host ever changes, these numbers should be re-measured rather
than assumed (the bisection method is simple: fix one axis small, grow the
other until it breaks, then check whether the failure is a clean `TooBig`
or an empty reply).

### Options considered but not used for car
- **Hosted routing API (Google Routes API)** -- priced out (Essentials
  tier, $5/1,000 requests, 10,000/month free) but not needed once
  `osrm.backend` proved workable locally and for free. Ended up being used
  for PT instead (see below), where the local option stalled.
- **One-by-one `osrmRoute()` loop for the 24.2M ausland-to-zone-centroid
  pairs** -- technically possible but far too slow (each call is a single
  route; at that volume it would take on the order of days). The `/table`
  service computes an entire block in one HTTP call, which is why it was
  used instead once the size limits above were worked out.

## PT (public transport) routing

### Option: `r5r`, one network for the whole study area
**Failed outright.** R5 hard-rejects any street network whose bounding box
exceeds **975,000 km²**; the full extent here (CH + DE + FR + IT + AT + a
slice of ES) is roughly **3.6M km²**. This wasn't a performance problem --
it's a hard-coded limit in R5 itself, discovered after a ~2 hour failed
build attempt. No amount of memory or patience fixes it; the network has to
be split.

### Option: `r5r`, tiled (recursive geographic bisection)
The redesign in response to the above: split the study area into tiles,
each kept under an area cap, and route each tile's pairs against its own
R5 network (built from an osmium-clipped OSM extract + only the GTFS feeds
relevant to that tile's border corridor). Several bugs surfaced and were
fixed along the way, but the approach was ultimately **abandoned for being
too slow**, not for being wrong:

- **AT (Austria) GTFS feed had its files inside a subfolder**
  (`GTFS_Fahrplan_2026/`) instead of at the zip root -- R5's strict parser
  throws `TableInSubdirectoryError`. Fixed by re-zipping with the GTFS
  files at the root.
- **CH GTFS included 8 routes with `route_type=1500`** ("Taxi" / demand-
  responsive transport), which R5 does not support -- this silently failed
  *every* tile, since every tile includes the CH feed. Fixed by stripping
  those 8 routes and their 717 trips / 16,596 stop_times rows (0.05% of the
  feed) before use.
- **Country-bbox GTFS feed selection was geometrically ineffective.**
  The original idea -- only load a country's GTFS feed into a tile if the
  tile's bounding box overlaps that country -- barely filtered anything,
  because Switzerland (always included) borders DE/IT/AT tightly enough
  that nearly every tile's bbox technically touched all of them regardless
  of real relevance. Replaced with **corridor-based filtering**: each
  origin/destination pair's real `GRENZABSCHNITT` (border corridor: DE/FR/
  IT/AT/blank) from the survey data determines which countries' GTFS feeds
  a tile actually needs.
- **A Windows-specific I/O bottleneck, initially misdiagnosed as a hang.**
  Tile builds would go silent for 60+ minutes at a "dataFileCache open"
  step. The first time this happened, it was assumed to be the same
  (eventually-resolving) pattern seen before and left running for 90+
  minutes -- it did not resolve, and had to be killed. The actual cause,
  confirmed via a `jstack` thread dump (`RUNNABLE` state, climbing
  accumulated CPU time -- i.e. genuinely working, not deadlocked): on
  Windows, flushing R5's large memory-mapped OSM database to disk
  (`MappedByteBuffer.force()`) is dramatically slower than on Linux. This
  is a JVM/OS-level cost, not fixable from R. **Mitigated, not eliminated**,
  by lowering the tile area cap from 900,000 km² to 400,000 km²
  (empirically: 2.3x smaller MapDB file, 2.7x faster to reach that
  checkpoint) -- smaller tiles mean smaller memory-mapped files, which
  flush faster.

**Why it was ultimately abandoned:** even after every fix above, each tile
took roughly 90 minutes to build and route, and the full run needed ~40
tiles -- **on the order of 60 hours**. The job was left running in the
background and was genuinely progressing (confirmed healthy through tile 8+
of 40), but that timeline was judged too slow to be worth continuing, and
the job was killed to refocus on car routing.

### Option: Google Routes API (`travelMode=TRANSIT`)
Written as a from-scratch alternative (`04_pt_google.R`) once the r5r
tiled approach's ~60 hour timeline became clear: one HTTP call per pair
instead of building any network at all. Verified against Google's own
SKU-trigger documentation (not assumed) that plain transit routing bills as
**Essentials tier: $5.00/1,000 requests, first 10,000/month free** -- this
dataset (7,726 pairs) fits entirely inside the free monthly allowance for a
single run. Built with a hard `dry_run_limit` safety gate (default 5
pairs), resumability (skips pairs that already have a time on a rerun), and
periodic checkpointing.

**Status: written and syntax-checked only -- never executed.** No API calls
have been made; running it requires a Google Cloud project with billing and
the Routes API enabled, plus a `GOOGLE_MAPS_API_KEY` environment variable
(see the script's header for details). This remains the fastest available
path to real PT travel times if/when it's needed.

### Option: r5r, single-country-pair rail-only experiments (CH+Italy, CH+France)
Revisited PT routing after the tiled approach was abandoned, with a narrower
goal: instead of one network for all 5 countries, build a much smaller
network for just Switzerland plus ONE neighbour, restricted to the RAIL
mode only (via r5r's `mode = c("WALK", "RAIL")` at query time -- R5 uses
GTFS purely for vehicle movement and OSM only for walk/access legs, so
this needed no GTFS or OSM filtering at build time, just a query-time
mode restriction). Origins are explicitly joined to their nearest actual
rail stop (a GTFS stop referenced by a rail-type `stop_times` row, not
just any geocoded point) before routing station-to-destination, so the
station used for each trip is visible and inspectable rather than
implicit inside r5r's own access-leg search.

**Bounding-box reality check, both times**: the full population of a
neighbour country's origins unioned with all CH destinations exceeds
R5's 975,000 km² limit in both cases (Italy: 1,236,324 km²; France:
1,227,072 km²) -- always check this before building, per the header
comments in both `pt_r5r_rail_ch_it.R` and `pt_r5r_rail_ch_fr.R`. A
country-specific split was needed in both cases, but the split criterion
differs by geography:
- **Italy**: a simple origin-latitude cutoff (>= 44°N, "North Italy")
  works cleanly, because Italy's outliers are almost entirely to the
  south (Sicily, Calabria, Puglia). Covers 2,622 / 2,786 rows (94.1%),
  bbox 296,927 km². The excluded south/central 164 rows do NOT shrink to
  fit even after removing Sicily specifically (1,212,831 km² either way)
  -- confirmed the real constraint is mainland Calabria/Puglia, not the
  island, so a latitude-only fix doesn't rescue that cluster.
- **France**: a latitude cutoff does NOT work -- France's outliers spread
  in both latitude AND longitude (Brittany, Paris, the south coast), not
  just north-south. Used a REGION-based split instead: all 987 unique
  French origins were reverse-geocoded (Transitous's `/reverse-geocode`
  endpoint, same one used in `geocode_origins.R`) to get their real
  French region, and the three regions that actually border Switzerland
  (Auvergne-Rhône-Alpes, Grand Est, Bourgogne-Franche-Comté) were kept --
  3,019 / 3,631 rows (83.1%), bbox 400,747 km².

**CH+Italy build**: 42.6 min, 6,383 rail stops (5,889 CH + only 494 IT --
`it.gtfs.zip` is Trenord/Lombardia regional rail only, not national).
100-tourist sample: 56/100 got a rail time. Two distinct failure causes
found in the sample:
1. 7/7 nearest-stations with a `missingSLOID_...` stop ID failed 100% of
   the time -- placeholder/orphan stop records in the Trenord feed
   (geocoded as real locations, but with zero actual `stop_times`).
2. The dominant cause turned out to be **missing regional GTFS
   coverage**, confirmed by reverse-geocoding all 550 unique North-Italy
   origins to real regions: only Lombardia (55.5% of origins) has ANY
   feed loaded. Piemonte (19.5%), Trentino-Alto Adige (6.5%), Veneto
   (4.9%), Emilia-Romagna (4.4%), Liguria (3.3%), Valle d'Aosta (2.7%),
   and Friuli-Venezia Giulia (1.6%) -- 43% of origins combined -- had
   ZERO regional rail GTFS loaded at all.
   Researched replacement feeds: found and verified working direct-
   download GTFS for **Piemonte**
   (`https://api.smartdatanet.it/api/ServizioProgrammatoDelTrasportoPubblicoRegionePiemonteTreniRegionali_5188/attachment/5187/1/GTFS_RAIL_IT_PIE.zip`)
   and **Liguria**
   (`https://srvcarto.regione.liguria.it/dtuff/download_statico/opendata/trasporti/GTFS/GTFS-IT-ITC3-TRENITALIA-20260614-20261212-pf.zip`,
   both confirmed valid GTFS zips by direct download+inspection, not just
   a search result) -- would bring coverage from 55.5% to ~78%. The
   remaining 5 regions (~20% of origins) are being migrated to
   **NeTEx-only** distribution via Italy's National Access Point
   (nap-1926.it) under the EU MMTIS regulation -- a national Trenitalia
   NeTEx L1 dataset was found and confirmed downloadable (1.72GB XML, no
   auth needed), but no NeTEx→GTFS converter with confirmed support for
   Italy's specific profile (EPIP-based) was found; the tools that do
   exist (`entur/netex-gtfs-converter-java`, `entur/damu`,
   `theoremus-urban-solutions/netex-gtfs-converter`) are built for other
   countries' profiles or are very new/unproven. **Status: Piemonte+
   Liguria links verified but not yet merged in; NeTEx conversion
   researched but not attempted.**

**CH+France build**: 60.3 min, 10,560 rail stops (5,889 CH + 4,671 FR --
`fr.gtfs.zip` is SNCF's genuinely NATIONAL feed, unlike Italy's regional-
only one; no missing-region problem here). 100-tourist sample: only
37/100 got a rail time -- worse than Italy despite far better raw data
coverage, which turned out to have nothing to do with data completeness:
1. Re-routing the same 100 with the 30-min walk cap raised to 240 min
   dropped missing from 63 to 40 -- so the walk cap directly explained
   23 failures, fewer than a straight-line-distance estimate suggested
   (real street-network walking distance differs from as-the-crow-flies).
2. Investigating the remaining 40 via `r5r::detailed_itineraries()` hit a
   real r5r bug: it hard-refuses to run against ANY network built from a
   GTFS feed containing a `frequencies` table ("Assertion on
   'r5r_network' failed..."), and `ch.gtfs.zip` has 1,887 frequency-based
   trips (2.4% of its ~78,775 trips -- headway-based services like
   "every 60 min, 06:45-22:45" instead of explicit departure times).
   This blocked the diagnostic identically for all 40 cases -- confirmed
   via `jstack`-style elimination, not assumed.
   **Fix**: expanded those trips into explicit `stop_times` rows via
   `gtfstools::frequencies_to_stop_times()` (424 sec), wrote a fixed
   feed, and rebuilt the network (81.7 min -- did not reuse the OSM
   MapDB cache from the first build, took about as long as building from
   scratch). **Confirmed this fix changed ZERO routing outcomes** (63/100
   missing before AND after) -- the frequencies table was only ever
   blocking the r5r diagnostic tool itself, never real routing. This is
   worth remembering: a GTFS validity/structure issue can silently break
   analysis tooling while having no effect on the actual travel-time
   results, so "the diagnostic tool errors" is not evidence that
   "routing is broken because of X."
3. With `detailed_itineraries()` now unblocked (and a `drop_geometry =
   TRUE` fix for a second, unrelated bug in this script's own leg-
   printing code -- the function returns an `sf` geometry column by
   default, which crashed a `unique()` call), diagnosed all 63 originally-
   failing cases precisely:

   | Cause | Count | % |
   |---|---|---|
   | Access/egress walk distance too far (fails even at 240-min cap + 8h window) | 32 | 50.8% |
   | Genuine network-connectivity gap (no RAIL path exists, unlimited walk + 8h window) | 29 | 46.0% |
   | Search window too narrow (120 min -> works fine at 8h, standard 30-min cap) | 2 | 3.2% |

   **Net conclusion: this was never really a "Swiss data" problem** --
   the CH-specific bug fix didn't move the outcome at all. The real
   story is walk-distance limits (which any real-world tourist without a
   car would face too) and genuine connectivity gaps (no rail-only path
   exists between many station pairs, in either country's data, possibly
   because a real journey would require a bus leg that the RAIL-only
   mode deliberately excludes). The 29 connectivity-gap cases are the
   most interesting unresolved thread if this gets picked up again --
   unlike the walk-distance cases, they're not explained by a parameter
   choice.

Scripts: `pt_r5r_rail_ch_it.R`, `pt_r5r_rail_ch_fr.R` (build + first
test), `pt_r5r_rail_ch_fr_nowalkcap.R` (walk-cap isolation test),
`pt_r5r_rail_ch_fr_fixed.R` (frequencies fix + rebuild + full pipeline,
but crashed partway through its own diagnosis step on the geometry-column
bug), `pt_r5r_rail_ch_fr_diagnose_only.R` (reruns just the fixed
diagnosis step against the already-built network, without repeating the
81.7-min rebuild). All are validation/experiment scripts, not yet wired
into the numbered pipeline (`01_filter.R` -> `05_join.R`).

## Summary

| Dataset | Method | Status |
|---|---|---|
| `car_times_agqpv.csv` (7,726 pairs) | `osrm.backend` local server, one-by-one | Done, 0 missing |
| `car_times_ausland_CH.fst` (24.19M pairs) | `osrm.backend` local server, `/table` batched | Done, 0 missing |
| `pt_times.csv` (agqpv pairs) via r5r tiled | Local, free, but ~60h -- killed mid-run (tile 8+/40) | Abandoned (too slow), not resumed |
| `pt_times_google.csv` (agqpv pairs) via Google Routes API | Hosted, ~$0 for one run, fast | Written, never run |
| CH+North-Italy rail-only sample (100 tourists) | r5r, single network, RAIL mode only | Built, 56/100 routed; root cause = missing regional GTFS (43% of origins); 2 replacement feeds verified, not yet merged |
| CH+near-CH-France rail-only sample (100 tourists) | r5r, single network, RAIL mode only | Built, 37/100 routed; root causes fully diagnosed (51% walk-distance, 46% genuine connectivity gap, 3% search-window) |
