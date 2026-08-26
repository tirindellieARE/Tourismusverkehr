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

## Summary

| Dataset | Method | Status |
|---|---|---|
| `car_times_agqpv.csv` (7,726 pairs) | `osrm.backend` local server, one-by-one | Done, 0 missing |
| `car_times_ausland_CH.fst` (24.19M pairs) | `osrm.backend` local server, `/table` batched | Done, 0 missing |
| `pt_times.csv` (agqpv pairs) via r5r tiled | Local, free, but ~60h -- killed mid-run (tile 8+/40) | Abandoned (too slow), not resumed |
| `pt_times_google.csv` (agqpv pairs) via Google Routes API | Hosted, ~$0 for one run, fast | Written, never run |
