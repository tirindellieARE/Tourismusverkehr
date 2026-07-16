# Script Map

What every script in this folder reads, writes, and does. Paths are relative to
`RScript/tourists/` unless noted. Scripts are grouped: the active numbered
pipeline (`01`–`04`, stays in `tourists/`), and everything no longer used to
produce a result in `README_results.md` — legacy one-off model runs (pre-EMU,
superseded by `04_destination_choice.R`) and ad-hoc analysis/debug scripts —
which live in `old/`.

## Data directory layout

`data/` is split into three parts:

- **`data/input/`** — raw files, not produced by any script. The bare-minimum set needed to start from scratch (see the list at the bottom).
- **`data/output/`** — intermediary files: produced by one script, read by another. Everything here is fully reproducible by re-running `01` → `02` → `03` in order.
- **`data/final_output/`** — reserved for files that are terminal results, not read by any script. Currently unused/empty: every file `data/` holds today is either a raw input or something a later script still reads. The actual final results (model estimates, coefficient tables) live in the separate top-level `results_output/` folder, not under `data/`.

Anything in `data/` not referenced by any script (old survey extracts, unused shapefiles, etc.) was left in place at the `data/` root — it doesn't fit any of the three categories since nothing currently reads it.

---

## Core pipeline (run in order: 01 → 02 → 03 → 04)

### `01_agent_generation.R`
- **Reads**: `data/input/Finale_Auswertungsdatenbank_AGQPV2015_V2.csv` (raw AGQPV 2015 survey), `data/input/zones_communes.gpkg`
- **Writes**: `data/output/agqpv.csv`, `data/output/agents.csv`
- **Does**: filters the raw survey to foreign-origin, Switzerland-destined leisure trips (`STARTORTLANDISO != "CH"`, `ZIELORTLAND == 1`, `FAHRTZWECK == 5`); renames columns; spatially joins origin/destination/residence coordinates to traffic zones; joins destination zone topology (`STALAN2020`); writes `agqpv.csv`. Then replicates rows by survey weight (÷1000 scaling) to build a synthetic expanded population, `agents.csv`.
- **Note**: `agents.csv` is produced here but not consumed by any other current script except the debug script `old/tmp_omx_inspect.R`.

### `02_build_tt_lookups.R`
- **Reads**: `data/output/agqpv.csv`, `data/input/zones_communes.gpkg`, `data/input/TTC.omx`, `data/input/RITA.omx`, `data/input/EGT.omx`, `data/input/ACT.omx`
- **Writes**: `data/output/tt_full.fst`, `data/output/tt_light.fst`
- **Does**: builds car (`tt_miv` = TTC) and PT (`tt_oev` = RITA+EGT+ACT) travel-time lookups from the OMX matrices, keyed to agqpv's origin zones. `tt_full` = every agqpv origin × every reachable destination zone (~8,000). `tt_light` = only the origin–destination pairs that actually occur in `agqpv.csv`.

### `03_prepare_choice_data.R`
- **Reads**: `data/output/tt_light.fst`, `data/output/agqpv.csv`, `data/output/tt_full.fst`
- **Writes**: `results_output/mode_choice_model.rds`, `data/output/tt_full_logsum.fst`, `results_output/mode_choice_model_tagesreise.rds`, `data/output/tt_full_logsum_tagesreise.fst`, `results_output/mode_choice_model_reisemitue.rds`, `data/output/tt_full_logsum_reisemitue.fst` (+ Apollo's own per-model `results_output/mode_choice*_iterations.csv`, `*_output.txt`, `*_estimates.csv`)
- **Does**: estimates a binary MNL mode choice model (car vs. PT) on non-Swiss agqpv respondents' real origin→destination trips, using `border_mode` as the observed choice. Run three times: pooled (all non-Swiss), Tagesreise only (`n_nights == 0`), Reise mit Ü. only (`n_nights > 0`). For each, computes the EMU/logsum (`ln(exp(V_car)+exp(V_pt))`) for every OD pair in `tt_full` and saves it — this is the accessibility variable consumed by `04_destination_choice.R`.
- **Note**: `results_output/mode_choice*.rds`, `*_iterations.csv`, `*_output.txt`, `*_estimates.csv` are Apollo's own artifacts in the top-level `results_output/` folder (untouched by the `data/` reorganisation).

### `04_destination_choice.R`
- **Reads**: `data/output/agqpv.csv`, `data/input/zones_communes.gpkg`, `data/output/tt_full_logsum.fst` (default; `data/output/tt_full_logsum_tagesreise.fst` / `_reisemitue.fst` when called with a subgroup-specific `logsum_dt`), `../benzoni_thesis/output/attractivity_indexes.csv`
- **Writes**: whatever the invoking call names — e.g. `results_output/destination_mnl_batch_*.csv`, `results_output/destination_mnl_bootstrap_*.csv`, `results_output/destination_mnl_tagesreise_vs_reisemitue*.csv` — plus Apollo's own per-model `results_output/<modelName>_iterations.csv`, `_output.txt`, `_estimates.csv`, `.rds`. All in the top-level `results_output/` folder, not under `data/`.
- **Does**: defines the reusable MNL destination choice framework (not a single fixed model): `build_destination_data()` samples agents/alternatives and joins EMU + topology + optional attractivity variables; `run_destination_model()` estimates one Apollo MNL and prints a results summary; `run_destination_batch()` runs several specs and saves them together; `run_destination_bootstrap()` repeats a spec with fresh with-replacement agent resamples. Configurable: sample size, choice-set size, attractivity variable subset, whether topology is included, which agent subgroup, which EMU lookup. When sourced as-is, the bottom of the script runs one example (EMU + topo + 6 attractivity vars, 1,000 agents, 300 alts).

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

Only files that are **not** produced by any script here — i.e. what you'd need to copy into `data/input/` before running `01` → `02` → `03` → `04`. This is exactly the contents of `data/input/`, minus `tt_avg_lookup.rds` (only needed for the legacy scripts, see below).

```
data/input/Finale_Auswertungsdatenbank_AGQPV2015_V2.csv    Raw AGQPV 2015 survey (01_agent_generation.R)
data/input/zones_communes.gpkg                              Traffic zone geometries + STALAN2020 topology (01, 02, 04)
data/input/TTC.omx                                          Car travel-time matrix (02_build_tt_lookups.R)
data/input/RITA.omx                                         PT travel-time matrix, leg 1 (02_build_tt_lookups.R)
data/input/EGT.omx                                          PT travel-time matrix, leg 2 (02_build_tt_lookups.R)
data/input/ACT.omx                                          PT travel-time matrix, leg 3 (02_build_tt_lookups.R)
../benzoni_thesis/output/attractivity_indexes.csv          Zone-level attractivity indexes, external project (04_destination_choice.R)
```

If you also want to reproduce the **legacy pre-EMU model runs** (the `run_*.R` scripts in `old/`), you additionally need:

```
data/input/tt_avg_lookup.rds    Not rebuilt by any current script -- see warning above. Must be copied
                                  from an existing run, or rebuilt manually as 0.9*TTC + 0.1*(RITA+EGT+ACT)
                                  from the four OMX files above.
```
