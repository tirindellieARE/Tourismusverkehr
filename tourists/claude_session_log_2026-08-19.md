# Session log — 2026-08-19

Written because the console became unreadable mid-session. This captures everything
discussed so far, in order.

---

## Part 1 — Git history: force-push and what happens on the other machine's `git pull`

### What happened on GitHub (`origin/master`)

1. `87454f6` pushed on Aug 11 — a clean merge.
2. More work landed on Aug 17, ending in a **bad merge** `f60c3f1` (15:58) that
   resurrected a bunch of stale files (`.Rhistory`, old `.RData`, PDFs,
   `tourists/old/*.R`, etc. — looked like an old stale branch got merged in by
   accident).
3. A revert commit `899dfb6` "Revert 'Merge branch...'" (19:17) was pushed to
   undo that bad merge.
4. `origin/master` was then **force-pushed** straight back to `87454f6`,
   discarding *both* the bad merge and the revert commit entirely. Confirmed via
   the remote-tracking reflog: `899dfb6 ... fetch: fast-forward` immediately
   followed by `87454f6 ... update by push` (a non-fast-forward push).

Right now GitHub's `master` = `87454f6`, and `87454f6` is an **ancestor** of the
discarded `899dfb6` (the discarded commits were built on top of it).

### What happens when pulling on the other machine

Depends on where that machine's local `master` currently sits:

- **If it never received `f60c3f1`/`899dfb6`** (still at `87454f6` or earlier):
  `git pull` is a totally normal fast-forward. No issues.

- **If it already has the bad merge and/or the revert** (likely, if that's the
  machine where the Aug 17 work happened, or if it pulled after the revert was
  pushed): `git pull` will **not error and won't lose anything** — but it also
  **won't clean anything up**. Since `87454f6` is an ancestor of what's already
  in that machine's local history, git just says "Already up to date." The
  local branch stays on `899dfb6`/`f60c3f1`, still containing the bad merge.
  Worse: that local branch will look "ahead of origin" — and if anyone runs a
  plain `git push` from that machine afterward, it would succeed as a
  fast-forward and **silently restore the bad merge + revert onto GitHub**,
  undoing the cleanup.

### Recommended procedure on the other machine

**Step 1 — diagnose only, no changes:**
```bash
git fetch origin
git status
git merge-base --is-ancestor 87454f656bb948a3c86e1ffac959e65e36b02021 master \
  && echo "local contains the known-good commit" \
  || echo "local does NOT contain it"
```

Read the `git status` output:
- **"up to date with 'origin/master'"** → already synced, nothing to do.
- **"behind ... can be fast-forwarded"** → safe to just `git pull`.
- **"ahead of 'origin/master' by N commits"** or **"have diverged"** → risky
  case, local history likely still has the bad merge/revert. **Do not
  `git push`.** Go to step 2.

**Step 2 — only if "ahead / diverged":**
```bash
# 1. Back up current state in case you need to inspect it later
git branch backup-before-cleanup-2026-08-18

# 2. Save any uncommitted work so reset doesn't touch it
git stash push -u -m "wip before reset to clean origin/master"

# 3. Point local master at the clean history from GitHub
git reset --hard origin/master

# 4. Inspect the stash BEFORE reapplying — it may contain files from the
#    bad-merge state you don't actually want back
git stash list
git stash show -p   # inspect before "git stash pop"
```

**Status: not yet executed** — waiting on you to run Step 1 on the other
machine and report back what `git status` says.

---

## Part 2 — `02_travel_time` pipeline: `01_filter.R` origin-in-CH filter investigation

### Context

Folder: `Scripts/02_travel_time/` — an origin → border (`grenz`) travel-time
pipeline (car + PT via OSRM/r5r) for foreign tourists entering Switzerland.
Run order: `00_setup.R` → `01_filter.R` → `02_download.R` → `03_car_osrm.R` →
`04_pt_r5r.R` → `05_join.R`. Input: `data/output/agqpv.csv` (produced by
`Scripts/01_generation/01_agent_generation.R`).

### What `01_filter.R` does

- Downloads/loads a Natural Earth world country-boundary polygon
  (`ne_10m_admin_0_countries`), extracts the Switzerland polygon.
- Does a point-in-polygon test (`sf::st_within`) on each row's
  `origin_lat`/`origin_long`.
- **Drops any row whose origin falls inside that CH polygon**, keeping only
  origin-abroad rows, then builds unique origin→grenz routing pairs.

### The question asked

`agqpv.csv` already carries an `origin_zone_country` column (CH / LI / Ausland),
computed upstream in `01_agent_generation.R` from a proper zone-assignment
system (`zones_communes.gpkg`) rather than a generic world polygon. Since the
upstream script's "consistency filter" already drops any row where
`origin_zone_country == "CH"`, does the geometric filter in `01_filter.R` agree
with that pre-computed classification?

### Verification performed

Ran the same point-in-polygon test as `01_filter.R` against the live
`data/output/agqpv.csv` (22,043 rows) and cross-tabbed against
`origin_zone_country`:

| | `origin_zone_country` = Ausland/LI |
|---|---|
| geometric test: origin **outside** CH | 20,205 |
| geometric test: origin **inside** CH  | **1,838** |

- `origin_zone_country` is never `"CH"` in this file (confirmed — the upstream
  consistency filter already guarantees this for all 22,043 rows).
- `dest_zone_country` is `"CH"` for all 22,043 rows (also confirmed).
- **Result: the two approaches disagree on 1,838 rows (8.3% of the dataset).**
  All disagreements run the same direction: geometric test says "inside CH"
  (so `01_filter.R` drops the row), while the pre-computed zone classification
  correctly says Ausland.

### Root cause (traced into `01_agent_generation.R` / `zones_communes.gpkg`)

`zones_communes.gpkg` (loaded at `Scripts/01_generation/01_agent_generation.R:36-45`)
has 8,718 usable zones: 7,966 Swiss communes, 11 Liechtenstein, and **741
explicit "Ausland" catchment zones** — each a deliberately modeled foreign town
or region just across the border, with `MAKROBEZ_STAAT = "Ausland"` and its own
representative point. `origin_zone_country` is read straight from that
attribute — a curated, purpose-built classification.

Traced all 1,838 mismatched rows back to their `origin_zone`: every one
direct-matched (no nearest-neighbor fallback) into a named Ausland zone. Top
offenders:

| Zone name | Real location | mismatched rows |
|---|---|---|
| Konstanz Altstadt | Germany, on Lake Constance shore | **1,131** |
| Bad Säckingen | Germany, on the Rhine | 176 |
| Rheinfelden Industrie | Germany, on the Rhine | 171 |
| Klettgau | Germany | 70 |
| Ferney-Mairie | France, near Geneva | 69 |
| Divonne-Centre | France, near Geneva | 47 |
| Hégenheim | France, near Basel | 28 |
| Varese | Italy, near Ticino | 33 |
| ...25 more zones, same pattern | | |

These are all real German/French/Italian towns immediately across the border
— the zone system classifies them correctly as Ausland.

**The bug is in `01_filter.R`'s method, not the data.** It tests each origin
point against a generic Natural Earth world-boundary polygon
(~1:10M generalization). Right along the CH border — especially across Lake
Constance and the Rhine near Basel, where the true border follows a
lake/river and Natural Earth's line is simplified — that generalized line
puts some near-border town centroids (in reality just meters to a few hundred
meters into Germany/France) on the Swiss side. `Konstanz Altstadt` alone
(directly across the lake from Kreuzlingen) accounts for 61.5% of all wrongly
dropped rows.

### Conclusion / recommended fix

`origin_zone_country` is the reliable source of truth (a curated
classification built for exactly this purpose). The Natural-Earth polygon
test in `01_filter.R` is too coarse for micro-geography at the border and is
currently discarding **1,838 legitimate foreign-origin trips (8.3% of the
dataset)**.

**Proposed fix (not yet applied):** replace the geometric CH-polygon test in
`01_filter.R` with a check on `origin_zone_country != "CH"` — or drop the
geometric step entirely, since `agqpv.csv` is already guaranteed
origin-abroad by the upstream consistency filter in `01_agent_generation.R`.

**Status: fix applied.** `01_filter.R` was rewritten to filter on
`origin_zone_country`/`residence_zone_country` instead of the Natural Earth
polygon (also requiring `residence_zone_country` outside CH). You then made
further edits of your own on top of that: the filter step moved further
upstream into `01_agent_generation.R`'s consistency check, so `01_filter.R`
now just does `ext = raw` with a comment explaining why (the file is already
guaranteed origin-abroad by the time it gets there). The `grenz` /
`grenz_lat`/`grenz_long` columns were also renamed/removed in favor of
`dest_lat`/`dest_long` (the tourist's actual reported destination, not the
border-crossing point) across this whole pipeline — see Part 3.

*(Note: Part 1 — the git force-push situation on the other machine — was
never confirmed resolved in this session. No `git status` report was
received back from Step 1 on that machine, so its status there is unknown
as of this update.)*

---

## Part 3 — Pipeline rename: "origin → grenz" to "origin → destination"

Once `grenz` (the border crossing) was confirmed to be just a descriptive
label — not the actual routing target — and no longer the destination
column in the data (destination is the tourist's real reported
`dest_lat`/`dest_long`, median ~63km from the border crossing, not the same
point), every `grenz`-as-coordinate reference was renamed throughout
`Scripts/02_travel_time/` (`02_download.R`'s bbox computation, `03_car_osrm.R`,
`04_pt_r5r.R`, column names in `config.yml`). `grenz` itself is kept as a
separate, non-coordinate label column (which border crossing a trip used —
real, useful metadata, just never used as a coordinate).

## Part 4 — `02_download.R`: OSM + GTFS acquisition

Downloads/merges/clips OSM extracts (Geofabrik, per-country) to a bounding
box computed from `pairs_to_route_agqpv.csv`, and downloads GTFS feeds
(national PT schedules) for the countries in `config.yml`.

Bugs fixed while first running it:
- A relative-path bug (`source("R/00_setup.R")` → should be
  `source("Scripts/02_travel_time/00_setup.R")`) — present in every script
  in this folder at the time, fixed in all of them.
- **60s default download timeout was too short** for the ~4.8GB Germany OSM
  extract — raised to `max(1800, getOption("timeout"))`, plus added
  delete-partial-file-on-error logic so a failed download doesn't leave a
  corrupt file that looks "already downloaded" on the next run.
- **The Swiss GTFS permalink 404'd** — opentransportdata.swiss's CKAN
  platform moved to `data.opentransportdata.swiss` in Jan 2025; found and
  set the new URL in `config.yml`.
- **FR/IT GTFS feeds found and added** (SNCF national for FR; Trenord/
  Lombardia regional rail for IT — covers Chiasso/Ticino and Tirano/
  Bernina-Poschiavo, not Piemonte or Valle d'Aosta). **ES/AT require manual
  download** (no stable direct-link URL) — AT (ÖBB) specifically requires
  accepting a terms-of-use click-through on `data.oebb.at` before the
  download link is even revealed, so it's not scriptable; documented in
  `config.yml` with instructions to download by hand and place the zip at
  `data/gtfs/at.gtfs.zip`.
- Caching behaviour (asked about, then documented in `README.md`): every
  step only checks whether its output file already exists — never checks
  whether the remote source changed. Re-running after a partial run resumes
  where it left off; re-running after a full run does nothing. **Gotcha:**
  `network.osm.pbf`'s bbox is fixed at whatever `pairs_to_route_agqpv.csv`
  looked like the moment `02_download.R` last ran — if the filter logic
  changes afterward, the bbox is not silently recomputed; the relevant file
  in `data/osm/`/`data/gtfs/` has to be deleted by hand to force a rebuild.

## Part 5 — Car routing: from Docker to `osrm.backend`

**Docker was the first plan** (start a local OSRM container, route against
it) — ruled out immediately: **Docker isn't available on this machine.**

**Landed on the R package `osrm.backend`** (downloads real OSRM binaries for
Windows, no Docker/compiler needed) after comparing it against native OSRM
binaries run by hand and a hosted API (priced out: Google Routes API,
Essentials tier, $5/1,000 requests, 10,000 free/month). Two Windows-specific
bugs fixed:
- `osrm_start()` launches `osrm-routed.exe` as a child of the calling R
  process — on Windows that child dies the moment the parent R session
  exits. Fixed by running the start-and-serve script as a detached
  background process that blocks forever, instead of a one-off `Rscript -e`.
- `osrm` package v5.0.0 changed `osrmRoute()`'s return type to a named
  numeric vector (no more `$duration`/`$distance` accessors) — fixed the
  accessor syntax in `03_car_osrm.R`.

**Result: `car_times_agqpv.csv`** — the 7,726 unique origin/destination
pairs occurring in `agqpv.csv`, routed one-by-one via `osrmRoute()`. 0
missing. Full detail on this and everything below in the dedicated
[`routing_readme.md`](routing_readme.md) (added once the routing story
across both modes got long enough to need its own file).

## Part 6 — PT routing: r5r's area limit, then tiling, then abandoned for time

Attempted a single-network `r5r` build for the whole study area first — it
**failed outright**: R5 hard-rejects any street network over 975,000 km²;
the actual extent (CH+DE+FR+IT+AT+part of ES) is ~3.6M km². Not a
performance issue, a hard limit — found after a ~2 hour failed build.

Redesigned into a **recursive geographic tiling** approach (split into ~40
tiles under an area cap, each with its own osmium-clipped OSM extract and
only the GTFS feeds relevant to that tile's actual border corridor). Fixed
along the way: an Austrian GTFS zip with files nested in a subfolder
(R5's parser requires them at zip root), Swiss GTFS `route_type=1500`
("Taxi") routes that R5 doesn't support (silently broke every tile until
removed), and a bbox-overlap GTFS-selection heuristic that turned out to be
geometrically useless (replaced with corridor-based selection from the
survey's real `GRENZABSCHNITT` data). Also chased down what looked like a
hung tile build — actually a genuine (if very slow) Windows-specific
`MappedByteBuffer.force()` disk-flush bottleneck, confirmed via a `jstack`
thread dump rather than assumed, and partially mitigated by shrinking the
tile area cap.

**Even with every fix applied, each tile took ~90 minutes** — ~60 hours for
the full run. The tiled job was left running, was genuinely progressing
(healthy through tile 8+/40), but was ultimately **killed** in favor of
refocusing on car routing, since that timeline was judged too slow to be
worth waiting on right now.

**Wrote (but did not run) `04_pt_google.R`** as a faster alternative: one
Google Routes API call per pair instead of building any network. Verified
against Google's own docs that plain transit routing bills as Essentials
tier ($5/1,000 requests, 10,000 free/month) — this whole dataset (7,726
pairs) fits the free tier for one run. Built with a hard dry-run safety
gate, resumability, and checkpointing, but **zero API calls have been made**
— running it needs a Google Cloud project with billing + the Routes API
enabled, and a `GOOGLE_MAPS_API_KEY` environment variable, neither of which
has been set up yet.

Full write-up of every routing attempt (car and PT), including the exact
numbers behind each decision, is in
[`routing_readme.md`](routing_readme.md).

## Part 7 — Scaling car routing up: the ausland → CH-zone dataset

Existing `car_times_agqpv.csv` only covers the origin/destination pairs
that occur in real survey trips — enough for a mode-choice logsum on
observed trips, but not enough for a destination-choice model, which needs
a travel time from every ausland entry point to every *candidate* Swiss
zone. Mirrors `06_build_tt_lookups.R`'s `tt_ausland_CH.fst` (built from the
OMX demand-model matrices) but via real OSRM road routing instead.

New script `03b_car_osrm_ausland_zones.R`: 3,037 unique ausland origins
(deduplicated from `agqpv.csv`) × 7,966 Swiss zone centroids (from
`zones_communes.gpkg`) = 24,192,742 pairs, routed via OSRM's `/table`
(many-to-many) service rather than one-by-one — at that volume a one-by-one
loop would take days. Getting this to actually work meant empirically
discovering **two undocumented limits on this OSRM server**: a
`--max-table-size` of 10,000 matrix cells (not the 8,000,000 the OSRM docs
describe as default — this server is evidently configured, or defaults,
much lower), and a separate ~2,200-2,400-total-coordinate ceiling that
kills the connection outright (empty reply, no clean error) rather than
rejecting cleanly — almost certainly a URL-length limit in osrm-routed's
request parser, hit before OSRM's own size check even runs. Also found that
a *balanced* origin/destination chunk shape routes ~4x faster per cell than
a skewed one. Full numbers in `routing_readme.md`.

**Result:** `car_times_ausland_CH.fst` (24,192,742 rows) + `origins_ausland
.csv` (the 3,037 unique origins), 0 missing, computed in ~87 minutes.

Renamed the agqpv-side outputs to make the two datasets unambiguous:
`pairs_to_route.csv` → `pairs_to_route_agqpv.csv`, `row_to_pair.csv` →
`row_to_pair_agqpv.csv`, `car_times.csv` → `car_times_agqpv.csv` (config.yml
paths updated; no script code changes needed since they all read the path
from `cfg$out$*`). `README.md` updated to match, including correcting an
outdated "origins are zone centroids" limitation note that no longer
applied.

## Part 8 — Committed and pushed

Discovered along the way that the entire `Scripts/` directory (this whole
pipeline, across this session and earlier ones) had never actually been
committed to git — `git status` showed 33 files "deleted" at the repo root
(really: moved into `Scripts/` subfolders at some earlier point, but never
`git add`ed) plus the whole `Scripts/` tree as untracked. Committed
everything (`git` correctly detected the moves as renames, not
delete+recreate) as `97a0dab`, "Reorganize scripts into Scripts/
subfolders; add full car+PT travel-time pipeline", and pushed to
`origin/master`. An unrelated sibling folder (`../benzoni_thesis/`, its own
separate nested git repo) was deliberately left untouched.

**Status: all of the above is complete and pushed.** Outstanding /
not-yet-done: the tiled r5r PT job (killed, tiles 1-7 have stale all-NA
results from before the CH GTFS fix, tiles 8+ never finished); the Google
PT script (written, never run — needs an API key); `05_join.R` has not yet
been run against real PT data.
