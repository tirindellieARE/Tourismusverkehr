# Script Map

What every script in this folder reads, writes, and does. Paths are relative to
`RScript/tourists/` unless noted. Scripts are grouped: the active pipeline
(numbered `01`–`04` plus `aggregate_regions.R`, stays in `tourists/`), and
everything no longer used to produce a result in `README_results.md` —
legacy one-off model runs (pre-EMU, superseded by `04_destination_choice.R`)
and ad-hoc analysis/debug scripts — which live in `old/`.

## Data directory layout

`data/` is split into three parts:

- **`data/input/`** — raw files, not produced by any script. The bare-minimum set needed to start from scratch (see the list at the bottom).
- **`data/output/`** — intermediary files: produced by one script, read by another. Everything here is fully reproducible by re-running `01` → `02` → `03` in order.
- **`data/final_output/`** — reserved for files that are terminal results, not read by any script. Currently unused/empty: every file `data/` holds today is either a raw input or something a later script still reads. The actual final results (model estimates, coefficient tables) live in the separate top-level `results_output/` folder, not under `data/`.

`data/amr101.gpkg` and `data/erreichbarkeit_tt2023.gpkg` live at the `data/` root rather than under `input/` or `output/` — they're raw inputs (used by `aggregate_regions.R`) but predate the `input/`/`output/` split and were left in place rather than moved, since moving them would have required re-checking every path that reads them. Anything else in `data/` not referenced by any script (old survey extracts, unused shapefiles, etc.) was also left in place at the `data/` root.

---

## Core pipeline

Run `01` → `02` → `03` → `04_destination_choice.R` for the zone-level destination choice model. For the AMR-region variant, `aggregate_regions.R` is NOT script "05" in that same sequence — it must run BEFORE `04_destination_choice_amr.R` (which reads its output), despite being listed after `04_destination_choice.R` below since it also depends on `03`'s logsum output: `01` → `02` → `03` → `aggregate_regions.R` → `04_destination_choice_amr.R`.

### `01_agent_generation.R`
- **Reads**: `data/input/Finale_Auswertungsdatenbank_AGQPV2015_V2.csv` (raw AGQPV 2015 survey), `data/input/zones_communes.gpkg`
- **Writes**: `data/output/agqpv.csv`, `data/output/agents.csv`
- **Does**: filters the raw survey to foreign-origin, Switzerland-destined leisure trips (`STARTORTLANDISO != "CH"`, `ZIELORTLAND == 1`, `FAHRTZWECK == 5`); renames columns; spatially joins origin/destination/residence coordinates to traffic zones; joins destination zone topology (`STALAN2020`) and, for each of the three zone types, the zone's country (`origin_zone_country`, `dest_zone_country`, `residence_zone_country`, from `zones_communes.gpkg`'s `MAKROBEZ_STAAT` column: `"CH"` / `"LI"` / `"Ausland"`) -- a sanity check that `origin_zone`/`residence_zone` are genuinely abroad and `dest_zone` is in Switzerland. A consistency filter then drops observations that contradict "foreign tourist entering Switzerland": `origin_zone_country == "CH"` (27 obs), `dest_zone_country != "CH"` (27 obs), or a `nationality`/`residence_zone_country` mismatch (3 obs -- `nationality == 1` implies residence should be `"CH"` and vice versa; likely a residence coordinate geocoded just across the border). 54 obs dropped in total (3 matched more than one criterion), 22,097 -> 22,043; writes `agqpv.csv`. Then replicates rows by survey weight (÷1000 scaling) to build a synthetic expanded population, `agents.csv`.
- **Note**: one zone in `zones_communes.gpkg` (`NO` 910000000) has an empty geometry and no attributes at all, including no `MAKROBEZ_STAAT` -- dropped before zone assignment.
- **Note**: `agents.csv` is produced here but not consumed by any other current script except the debug script `old/tmp_omx_inspect.R`.

### `02_build_tt_lookups.R`
- **Reads**: `data/output/agqpv.csv`, `data/input/zones_communes.gpkg`, `data/input/TTC.omx`, `data/input/RITA.omx`, `data/input/EGT.omx`, `data/input/ACT.omx`
- **Writes**: `data/output/tt_agqpv.fst`, `data/output/tt_ausland_CH.fst`, `data/output/tt_CH_CH.fst`
- **Does**: builds car (`tt_miv` = TTC) and PT (`tt_oev` = RITA+EGT+ACT) travel-time lookups from the OMX matrices. All three outputs share the same destination side (Swiss zones only, `MAKROBEZ_STAAT == "CH"`, ~7,966 -- none of the three ever needs an Ausland/LI zone as a destination), so all three are derived from one bulk travel-time computation (origin = every real zone, CH or abroad, ~8,718, x destination = the ~7,966 CH zones), sliced three ways: `tt_agqpv` = only the origin-destination pairs that actually occur in `agqpv.csv` (10,886 rows); `tt_ausland_CH` = every zone abroad (`MAKROBEZ_STAAT != "CH"`, i.e. "Ausland"/"LI", 752 zones) x every Swiss zone -- the candidate universe the destination choice model needs (foreign entry point -> Swiss destination); `tt_CH_CH` = every Swiss zone x every other Swiss zone (63.5M rows, ~1.1GB -- domestic zone-to-zone travel times, not currently consumed by any other script but available for e.g. a future within-Switzerland trip model).
- **Note**: the master travel-time table (8,718 origins x 7,966 destinations = ~69.4M rows) is built with ONE bulk 2D read per OMX matrix (`h5file[["data/…"]][row_index_vector, col_index_vector]`), not a loop over origin zones. An earlier version looped origin-by-origin (~8,718 individual single-row HDF5 reads); at that scale it did not complete in over an hour and was killed -- HDF5 chunk/compression overhead makes thousands of small scattered reads far slower than one large hyperslab read. Verified the two approaches return identical values on a random test slice before switching.
- **Note**: OMX encodes "no path found" / unreachable OD pairs with a sentinel travel time of 999999 per leg (so a fully-unreachable PT trip, `tt_oev = rita+egt+act`, sums to 2,999,997). Converted to `NA` per matrix (`mat_ttc`, `mat_rita`, `mat_egt`, `mat_act` each individually, threshold >= 900,000) right after the bulk read, so `tt_miv`/`tt_oev` are `NA` rather than a huge fake travel time. This matters because car and PT are not always unreachable together: of the 5,990,432 `tt_ausland_CH` rows, 238,980 have car unreachable and 167,471 have PT (any leg) unreachable, but only 167,286 have *both* unreachable -- the other ~71,700 need the reachable mode's real travel time preserved, not discarded. Fed raw into the mode-choice utility (before this fix), sentinel values produced logsum values as extreme as -3,695 for the pooled model instead of the normal ~[-2, 1.5] range -- see the matching note in `03_estimate_logsum.R` below for how `compute_logsum()` handles the resulting per-mode `NA`s.

### `03_estimate_logsum.R`
- **Reads**: `data/output/tt_agqpv.fst`, `data/output/agqpv.csv`, `data/output/tt_ausland_CH.fst`, `data/output/tt_CH_CH.fst`
- **Writes**: `results_output/mode_choice_model.rds` (+ `_tagesreise`/`_reisemitue`), `data/output/tt_ausland_CH_logsum.fst` (+ `_tagesreise`/`_reisemitue`), `data/output/tt_CH_CH_logsum.fst` (+ `_tagesreise`/`_reisemitue`) (+ Apollo's own per-model `results_output/mode_choice*_iterations.csv`, `*_output.txt`, `*_estimates.csv`)
- **Does**: estimates a binary MNL mode choice model (car vs. PT) on non-Swiss agqpv respondents' real origin→destination trips, using `border_mode` as the observed choice. Run three times: pooled (all non-Swiss), Tagesreise only (`n_nights == 0`), Reise mit Ü. only (`n_nights > 0`). For each, computes the EMU/logsum (`ln(exp(V_car)+exp(V_pt))`) for every OD pair in BOTH `tt_ausland_CH` (origin abroad × destination in Switzerland only -- the accessibility variable actually consumed by `04_destination_choice.R`) and `tt_CH_CH` (origin CH × destination CH -- domestic zone-to-zone EMU, not currently consumed by any other script) and saves each. Uses `tt_ausland_CH`/`tt_CH_CH` rather than a table that also includes Ausland/LI as a destination, so neither logsum lookup ever has an entry for an Ausland/LI zone as a destination.
- **Note**: `results_output/mode_choice*.rds`, `*_iterations.csv`, `*_output.txt`, `*_estimates.csv` are Apollo's own artifacts in the top-level `results_output/` folder (untouched by the `data/` reorganisation).
- **Note**: `compute_logsum()` treats a `NA` `tt_miv`/`tt_oev` (mode unreachable, see `02_build_tt_lookups.R`'s sentinel-value note) as that mode's utility being `-Inf`, not as the whole OD pair being missing -- so a zone reachable by only one mode still gets a valid logsum equal to that mode's utility. Only OD pairs unreachable by *both* modes end up `NA` in the output (167,286 / 5,990,432 rows in `tt_ausland_CH_logsum.fst`).

### `04_destination_choice.R`
- **Reads**: `data/output/agqpv.csv`, `data/input/zones_communes.gpkg`, `data/output/tt_ausland_CH_logsum.fst` (default; `data/output/tt_ausland_CH_logsum_tagesreise.fst` / `_reisemitue.fst` when called with a subgroup-specific `logsum_dt`), `../benzoni_thesis/output/attractivity_indexes.csv`
- **Writes**: whatever the invoking call names — e.g. `results_output/destination_mnl_batch_*.csv`, `results_output/destination_mnl_bootstrap_*.csv`, `results_output/destination_mnl_tagesreise_vs_reisemitue*.csv` — plus Apollo's own per-model `results_output/<modelName>_iterations.csv`, `_output.txt`, `_estimates.csv`, `.rds`. All in the top-level `results_output/` folder, not under `data/`.
- **Does**: defines the reusable MNL destination choice framework (not a single fixed model): `build_destination_data()` samples agents/alternatives and joins EMU + topology + optional attractivity variables; `run_destination_model()` estimates one Apollo MNL and prints a results summary; `run_destination_batch()` runs several specs and saves them together; `run_destination_bootstrap()` repeats a spec with fresh with-replacement agent resamples. Configurable: sample size, choice-set size, attractivity variable subset, whether topology is included, which agent subgroup, which EMU lookup, and (`interact_nationality`) whether every variable gets its own coefficient per nationality group or a single pooled coefficient. When sourced as-is, the bottom of the script runs one example (EMU + topo + 6 attractivity vars, 1,000 agents, 300 alts).
- **Note**: the candidate destination universe (`all_zones`, sampled to build each agent's alternative set) is restricted to `zones_communes.gpkg` rows with `MAKROBEZ_STAAT == "CH"` (~7,966 zones) — `zones_communes.gpkg` now also includes Ausland/LI zones, which have no entry in the CH-only logsum lookup above and are never valid tourism destinations here.
- **Note**: this filter must explicitly exclude `NA` (`MAKROBEZ_STAAT == "CH" & !is.na(MAKROBEZ_STAAT)`) -- unlike `data.table`, indexing an `sf`/data.frame object with a logical vector containing `NA` keeps an all-`NA` row at that position rather than dropping it, so a plain `== "CH"` filter let the one broken empty-geometry zone back into the candidate universe, where it was occasionally sampled as an alternative with no logsum match and crashed Apollo ("utilities ... not finite").
- **Note**: 7 / 7,966 CH zones have no `STALAN2020` topology value (apparently newly-added/reclassified communes) and are also excluded from the candidate universe -- the topology term has no `NA`-handling (unlike the logsum join, which only warns), so sampling one of these as an alternative produced the same "not finite" crash.

### `aggregate_regions.R`
- **Reads**: `data/input/zones_communes.gpkg`, `data/amr101.gpkg`, `../benzoni_thesis/output/attractivity_indexes.csv`, `data/output/tt_ausland_CH_logsum.fst` / `_tagesreise.fst` / `_reisemitue.fst`, `data/erreichbarkeit_tt2023.gpkg`
- **Writes**: `data/output/zone_to_amr101.csv`, `data/output/amr101_population.csv`, `data/output/tt_full_logsum_amr101.fst` (+ `_tagesreise.fst` / `_reisemitue.fst`), `data/output/erreichbarkeit_amr101.csv`
- **Does**: aggregates the zone-level EMU (all three subgroup variants) and Erreichbarkeit up to the 101 AMR regions in `data/amr101.gpkg`, so the destination choice model can optionally use these coarser regions as its choice set instead of individual NPVM zones. Zones are first restricted to genuine CH zones (`MAKROBEZ_STAAT == "CH"`, 7,966 / 8,719), then each is assigned to the AMR region with which it shares the **largest overlapping area** (`st_join(..., largest = TRUE)` on the zone/region polygons, not just the zone's centroid; only 1 / 7,966 CH zones has no polygon overlap with any AMR region and falls back to nearest-polygon). Aggregation is a population-weighted average of each variable over the zones within a region, using each zone's resident population (`v02_resident_population`) as the weight; zones missing from the Benzoni population data (1 / 7,966) get weight 0.
- **Note**: the CH filter uses `MAKROBEZ_STAAT`, not canton code (`KTKZ`) — `KTKZ` is `NA` for 7 genuine CH zones in addition to the Ausland/LI zones, so it would wrongly exclude those 7. An earlier version of this script used `KTKZ`, which was a harmless no-op against the original CH-only `zones_communes.gpkg` but became a latent bug once that file was updated to also include Ausland/LI zones.
- **Note**: consumed by `04_destination_choice_amr.R`, a separate script from `04_destination_choice.R` (see below) — the zone-level framework itself was not modified to use regions.

### `compute_zone_shares.R`
- **Reads**: `data/output/zone_to_amr101.csv`
- **Writes**: `data/output/zone_prob_given_region.csv` -- `npvm_id`, `BAE2018`, `BAE2018_fr`, `population`, `region_population`, `zone_share`
- **Does**: computes P(zone | region) for every zone, as its share of its AMR region's total resident population (`zone_share = population_zone / region_population`) -- a proxy for the probability a specific zone is the destination, given that its region was already chosen. Intended to disaggregate a region-level destination choice probability from `04_destination_choice_amr.R` back down to individual zones: `P(zone) = P(region) * P(zone | region)`. Zones with zero population get `zone_share = 0`; shares sum to 1 within every region (checked with `stopifnot`).

### `04_destination_choice_amr.R`
- **Reads**: `data/output/agqpv.csv`, `data/output/zone_to_amr101.csv`, `data/output/amr101_population.csv`, `data/output/erreichbarkeit_amr101.csv`, `data/output/tt_full_logsum_amr101.fst` / `_tagesreise.fst` / `_reisemitue.fst` (all built by `aggregate_regions.R`)
- **Writes**: `results_output/destination_mnl_amr_tagesreise_vs_reisemitue_pop_erreichbarkeit.csv`, plus Apollo's own per-model `results_output/<modelName>_iterations.csv`, `_output.txt`, `_estimates.csv`, `.rds`
- **Does**: a variant of the `04_destination_choice.R` framework where the choice set is the 101 AMR regions instead of individual NPVM zones. Because there are only 101 regions, every agent's full choice set is used — no McFadden alternative sampling, unlike `04_destination_choice.R`. Each respondent's chosen alternative is the AMR region containing their real destination zone (`zone_to_amr101.csv`); only the destination side is aggregated to regions, origin stays a zone. `build_destination_data_amr()` / `run_destination_model_amr()` mirror the zone-level framework's structure (same `interact_nationality` toggle, same coefficient-naming and printing conventions) but are separate, self-contained functions rather than shared code, since the region-keyed joins (`BAE2018` region codes, no alternative sampling, no topology) differ enough from the zone-keyed ones to make a shared implementation more error-prone than two parallel ones. When sourced as-is, the bottom of the script runs one example (EMU + population + Erreichbarkeit, nationality-interacted, full sample, both Tagesreise and Reise mit Ü. subgroups).
- **Note**: `BAE2018` region codes are zero-padded strings (e.g. `"01012"` for Genève) — every `fread()` of a column containing them uses `colClasses = c(BAE2018 = "character")` to avoid silently losing the leading zero (R's default CSV type-guessing would otherwise parse it as integer `1012`, breaking the join against the `.fst` files, which keep it as character throughout since they never round-trip through CSV).

---

## Legacy one-off model runs (pre-EMU; superseded by `04_destination_choice.R`)

**Moved to `old/`** — none of these currently produce a model mentioned in `README_results.md`. They all use the raw OMX-blended `tt_avg` (0.9×car + 0.1×PT) travel-time variable directly, predating the mode-choice/EMU approach. Kept for historical reference only.

| Script | Reads (beyond `data/output/agqpv.csv`, `data/input/zones_communes.gpkg`, `data/input/tt_avg_lookup.rds`) | Writes | Does |
|---|---|---|---|
| `old/run_attr_nat.R` | `../benzoni_thesis/output/attractivity_indexes.csv` (13 vars) | `results_output/attr_nat_results.csv` | MNL: tt×nat(5, incl. AT) + topo + 13 attr×nat(4) |
| `old/run_attr_nat2.R` | same (13 vars) | `results_output/attr_nat2_results.csv` | MNL: tt×nat(3) + topo×nat(3) + 13 attr×nat(3), AT merged into "other" |
| `old/run_attr_minimal.R` | same, 6-var subset (v01,v02,v03,v07,v09,v12) | `results_output/attr_minimal_results.csv` | MNL: tt×nat(3) + topo×nat(3) + 6 attr×nat(3), 1,000 agents / 300 alts |
| `old/run_attr_minimal_10k.R` | same 6-var subset | `results_output/attr_minimal_10k_results.csv` | Same spec, 10,000 agents / 500 alts |
| `old/run_attr_baseline.R` | same, v02 only | `results_output/attr_baseline_results.csv` | MNL: tt×nat(3) + topo×nat(3) + population×nat(3), 1,000 agents / 300 alts |
| `old/run_attr_baseline_10k.R` | same, v02 only | `results_output/attr_baseline_10k_results.csv` | Same spec, 10,000 agents / 300 alts |
| `old/run_nalts.R` | — (CLI args: `N_ALTS`, `N_AGENTS` [default 5000], `SEED` [default 42]) | `results_output/nalts_n<N_AGENTS>_results.csv` (appends) | MNL: tt×nat + topo, sweeps choice-set size |
| `old/run_one_sample.R` | — (CLI arg: `SEED`) | `results_output/bootstrap_results.csv` (appends) | One bootstrap replicate of the tt×nat + topo MNL, 100 alts |
| `old/run_10samples.R` | — | `results_output/bootstrap_results.csv` (appends) | Same model, loops 10 replicates (seeds 1–10) in one run |

**⚠ `data/input/tt_avg_lookup.rds` is not rebuilt by any current script.** It used to be built inline by an earlier version of `04_destination_choice.R` (0.9×car + 0.1×PT blend from the OMX matrices, cached to this file). That code no longer exists — `04_destination_choice.R` now uses the EMU/logsum approach instead. The file still exists on disk from that earlier run (moved into `data/input/` since nothing regenerates it, functionally the same as a raw input now); if it's ever deleted, all nine scripts above will fail until it's rebuilt by hand or the old caching logic is restored.

---

## Analysis / debug / one-off scripts

**Also moved to `old/`.**

| Script | Reads | Writes | Does |
|---|---|---|---|
| `old/compare_models.R` | — (hardcoded LL/param values from earlier manual runs) | — (console only) | Recomputes ρ²/AIC/BIC and LRT statistics for three hardcoded model results |
| `old/debug_model.R` | `results_output/mnl_nalts100.RData` | — (console only) | Inspects a saved Apollo model object. **File does not currently exist** — stale reference from early experimentation |
| `old/inspect_zones.R` | `data/input/zones_communes.gpkg`, `../benzoni_thesis/output/attractivity_indexes.csv` | — (console only) | Checks whether zone ID columns (`NO` vs. Benzoni's `zone_id`/`npvm_id`) actually match |
| `old/plot_nalts.R` | `results_output/nalts_n10000_results.csv` | `results_output/plot_coefficients_vs_nalts.png`, `results_output/plot_pctchange_vs_nalts.png` | Plots legacy tt×nat coefficients vs. choice-set size |
| `old/tmp_add1000.R` | `results_output/mnl_nalts1000_n10000_iterations.csv` | `results_output/nalts_n10000_results.csv` (appends) | Extracts the converged N_ALTS=1000 estimates from an Apollo iterations file into the results CSV |
| `old/tmp_compare.R` | `results_output/nalts_results.csv`, `results_output/mnl_nalts300_n10000_iterations.csv` | — (console only) | Compares 5k- vs. 10k-agent fit/coefficients at 300 alts |
| `old/tmp_nalts10k_summary.R` | `results_output/nalts_n10000_results.csv` | — (console only) | Prints fit-by-N_ALTS and coefficient tables for the 10k-agent sweep |
| `old/tmp_nalts_summary.R` | `results_output/nalts_results.csv` | — (console only) | Same, for the older (non-suffixed) results file |
| `old/tmp_omx_inspect.R` | `data/output/agents.csv`, `data/input/TTC.omx`, `data/input/RITA.omx`, `data/input/EGT.omx`, `data/input/ACT.omx` | — (console only) | Prints raw travel-time matrix values for one sample origin/destinations |
| `old/tmp_read_benzoni.R` | `tmp_benzoni_paper.pdf` (local, still at `tourists/` root — not moved) | — (console only) | Extracts and prints text from the Benzoni paper PDF (`pdftools`) |
| `old/tmp_readpdf.R` | `../activity_simulation/Scherr_et_al2020.pdf` (**outside this project folder**) | — (console only) | Extracts and prints text from an unrelated PDF (`pdftools`) |
| `old/tmp_summary.R` | `results_output/bootstrap_results.csv` | — (console only) | Prints mean/SD/min/median/max across bootstrap samples |

**Note**: `results_output/nalts_results.csv` (read by `old/tmp_compare.R`, `old/tmp_nalts_summary.R`) is a different file from `results_output/nalts_n10000_results.csv` — it predates `run_nalts.R`'s current dynamic `nalts_n<N_AGENTS>_results.csv` naming and won't be regenerated under that exact name by the script as it stands today. It still exists on disk from an earlier run.

**⚠ Working directory**: all 21 scripts in `old/` use paths like `"data/output/agqpv.csv"` and `"results_output/..."` relative to `tourists/`, not to `old/`. Since they now live one level deeper, they must still be run (or sourced) with `tourists/` as the working directory — e.g. `Rscript old/run_nalts.R` from within `tourists/`, not from within `tourists/old/`. Running them with `old/` as the working directory will fail to find their inputs.

---

## Required initial input files (to run the pipeline from scratch on another machine)

Only files that are **not** produced by any script here — i.e. what you'd need to copy into `data/input/` (or `data/`, for the two region-related files) before running `01` → `02` → `03` → `04` → `05`. This is exactly the contents of `data/input/` plus `data/amr101.gpkg` and `data/erreichbarkeit_tt2023.gpkg`, minus `tt_avg_lookup.rds` (only needed for the legacy scripts, see below).

```
data/input/Finale_Auswertungsdatenbank_AGQPV2015_V2.csv    Raw AGQPV 2015 survey (01_agent_generation.R)
data/input/zones_communes.gpkg                              Traffic zone geometries + STALAN2020 topology (01, 02, 04, 05)
data/input/TTC.omx                                          Car travel-time matrix (02_build_tt_lookups.R)
data/input/RITA.omx                                         PT travel-time matrix, leg 1 (02_build_tt_lookups.R)
data/input/EGT.omx                                          PT travel-time matrix, leg 2 (02_build_tt_lookups.R)
data/input/ACT.omx                                          PT travel-time matrix, leg 3 (02_build_tt_lookups.R)
../benzoni_thesis/output/attractivity_indexes.csv          Zone-level attractivity indexes, external project (04_destination_choice.R, aggregate_regions.R)
data/amr101.gpkg                                            101 AMR region polygons (aggregate_regions.R)
data/erreichbarkeit_tt2023.gpkg                              Zone-level Erreichbarkeit/accessibility index (aggregate_regions.R)
```

If you also want to reproduce the **legacy pre-EMU model runs** (the `run_*.R` scripts in `old/`), you additionally need:

```
data/input/tt_avg_lookup.rds    Not rebuilt by any current script -- see warning above. Must be copied
                                  from an existing run, or rebuilt manually as 0.9*TTC + 0.1*(RITA+EGT+ACT)
                                  from the four OMX files above.
```
