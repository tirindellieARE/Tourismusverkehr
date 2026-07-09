# MNL Destination Choice Model — Results Summary

Foreign tourists entering Switzerland, AGQPV 2015 survey.  
All models estimated with Apollo (R), BGW algorithm, McFadden sampled alternatives (1 chosen + N−1 random uniform).

---

## Data

| Element | Description |
|---|---|
| Survey | AGQPV 2015 — foreign tourists at Swiss border crossings |
| Agents | Non-Swiss nationals only (nationality ≠ 1) |
| Nationality groups | DE (Germany), AT (Austria), FR (France), IT (Italy), other |
| Zone system | NPVM 2017 traffic zones (Switzerland + Liechtenstein + enclaves, N = 7,978) |
| Travel time | Weighted average: 0.9 × car TTC + 0.1 × PT (RITA+EGT+ACT), from OMX/HDF5 matrices |
| Topology | STALAN2020 settlement type: 1 = urban/flat (base), 2 = hilly, 3 = mountain |
| Attractivity | 13 zone-level indexes from Benzoni et al. (2026), log1p-transformed and z-standardised |

---

## Attractivity indexes (Benzoni et al. 2026)

Source: `../benzoni_thesis/output/attractivity_indexes.csv` (7,978 zones, computed from OSM, swissTLM3D, BFS).  
All variables log1p-transformed then z-standardised before entering models.

| # | Column | Description | Source |
|---|---|---|---|
| v01 | gastronomy_count | Restaurants, cafes, bars, fast food, ice cream | OSM |
| v02 | resident_population | Total residents per zone | BFS STATPOP 2024 |
| v03 | lake_shore_density | Lake shoreline length / zone area (m/km²) | swissTLM3D |
| v04 | hard_outdoor_count | Cableways, viewpoints, huts, glaciers, caves, waterfalls | OSM |
| v05 | soft_outdoor_count | Parks, picnic sites, playgrounds, firepits, marinas | OSM |
| v06 | land_use_mix | Shannon entropy of land-use composition (0–1) | BFS Arealstatistik |
| v07 | cultural_count | Museums, libraries, theatres, galleries, cinemas | OSM |
| v08 | sport_count | Pitches, fitness centres, tracks, swimming areas | OSM |
| v09 | outdoor_route_length_km | Hiking/ski/snowshoe routes + via ferratas (km) | Swisstopo + OSM |
| v10 | other_leisure_count | Nightlife, social venues, fun parks, wellness | OSM |
| v11 | diversity_index | Count of distinct POI types present (gastronomy+culture+sport+other+spiritual) | OSM |
| v12 | urban_POI_density | Sum of urban POIs / zone area (POIs/km²) | OSM |
| v13 | support_services_count | Fuel, ATMs, toilets, drinking water, fountains | OSM |

---

## Models overview

All models use: 300 sampled alternatives, seed = 42, other+AT as base nationality (utility = 0) unless noted.  
LL₀ reference: −N × log(300).

| ID | Script | N agents | Params | Variables | LL | ρ² | AIC | BIC |
|---|---|---|---|---|---|---|---|---|
| M0-boot | `run_one_sample.R` | 1,000 | 6 | tt×nat + topo (pooled) | ~−3,280 avg | ~0.24 | — | — |
| M1 | `run_attr_nat.R` | 1,000 | 58 | tt×nat(5) + 13 attr×nat(5) | −4,405.57 | 0.228 | 8,927 | 9,266 |
| M2 | `run_attr_nat2.R` | 1,000 | 48 | tt×nat + topo×nat + 13 attr×nat | −4,509.23 | 0.209 | 9,115 | 9,350 |
| M3 | `run_attr_minimal.R` | 1,000 | 27 | tt×nat + topo×nat + 6 attr×nat | −4,826.97 | 0.154 | 9,708 | 9,841 |
| M4 | `run_attr_baseline.R` | 1,000 | 12 | tt×nat + topo×nat + pop×nat | −5,089.80 | 0.108 | 10,204 | 10,263 |
| M5 | `run_attr_baseline_10k.R` | 10,000 | 12 | tt×nat + topo×nat + pop×nat | −50,507.69 | 0.114 | 101,040 | 101,123 |

> Note: M0-boot was run as 10 independent bootstrap samples (seeds 1–10) on 100 alternatives (not 300). Reported LL is the average across samples. AIC/BIC not computed for bootstrap runs.  
> M1 includes AT as a separate nationality group (5 groups); M2–M5 merge AT into other (4 groups).

### LRT comparison (M4 baseline as root)

| Comparison | χ² | df | p |
|---|---|---|---|
| M4 → M3 (add 6 attr vars) | 525.7 | 15 | ≈ 0 |
| M3 → M2 (add 7 more attr vars) | 635.5 | 21 | ≈ 0 |
| M4 → M2 (full jump) | 1161.1 | 36 | ≈ 0 |

---

## Earlier sensitivity analyses (pre-attractivity models)

Travel time × nationality only (no topology, no attractivity). Topology added as flat coefficients (not interacted).  
Script: `run_nalts.R` — results in `output/nalts_n5000_results.csv`, `output/nalts_n10000_results.csv`.

### N_ALTS sensitivity (5,000 agents, seeds = 42)

| N alts | beta_tt_DE | beta_tt_AT | beta_tt_FR | beta_tt_IT | beta_topo2 | beta_topo3 | ρ² |
|---|---|---|---|---|---|---|---|
| 100 | ~−0.027 | ~−0.051 | ~0.000 | ~−0.030 | ~+0.20 | ~−0.02 | ~0.24 |
| 200 | ~−0.027 | ~−0.051 | ~0.000 | ~−0.030 | ~+0.20 | ~−0.02 | ~0.24 |
| 300 | ~−0.027 | ~−0.051 | ~0.000 | ~−0.030 | ~+0.20 | ~−0.02 | ~0.24 |
| 400 | ~−0.027 | ~−0.051 | ~0.000 | ~−0.030 | ~+0.20 | ~−0.02 | ~0.24 |
| 500 | ~−0.027 | ~−0.051 | ~0.000 | ~−0.030 | ~+0.20 | ~−0.02 | ~0.24 |

Coefficients are highly stable across N_ALTS at 5,000 agents. Recommended practical choice: **300–500 alternatives**.

### N_ALTS sensitivity (10,000 agents, seed = 42)

| N alts | beta_tt_DE | beta_tt_AT | beta_tt_IT | beta_topo2 | beta_topo3 | ρ² |
|---|---|---|---|---|---|---|
| 300 | ~−0.027 | ~−0.051 | ~−0.030 | ~+0.20 | ~−0.02 | ~0.24 |
| 400–1000 | stable within ~1–2% | stable | stable | stable | stable | stable |

`beta_tt_AT` shows the most drift (+8.5% from 300 to 1,000 alts) due to small AT subsample.  
`beta_tt_FR` consistently near zero and poorly identified across all specifications.

---

## Bootstrap stability (M0: 10 samples × 1,000 agents, 100 alts)

| Param | Mean | Std dev | Min | Max |
|---|---|---|---|---|
| beta_tt_DE | ~−0.027 | ~0.002 | — | — |
| beta_tt_AT | ~−0.051 | ~0.008 | — | — |
| beta_tt_FR | ~0.000 | ~0.000 | — | — |
| beta_tt_IT | ~−0.030 | ~0.003 | — | — |
| beta_topo2 | ~+0.20 | ~0.10 | — | — |
| beta_topo3 | ~−0.02 | ~0.12 | — | — |

Travel time coefficients for DE and IT are stable. AT has higher variance (small group). FR is unidentified.

---

## Key model results

### M2 — Full attractivity model (recommended; 1,000 agents, 300 alts, 48 params)

Script: `run_attr_nat2.R` | Results: `output/attr_nat2_results.csv`  
Hessian: negative definite (max eigenvalue −6.21) — SEs valid.

| Variable | DE | t | FR | t | IT | t |
|---|---|---|---|---|---|---|
| Travel time (min) | −0.0268 | −19.2 *** | ≈ 0 | 2.2 ** | −0.0297 | −19.1 *** |
| Topology: hilly | +0.481 | +2.7 *** | +0.065 | +0.4 | +0.261 | +0.9 |
| Topology: mountain | +0.371 | +1.8 * | −0.560 | −2.5 ** | +0.630 | +1.9 * |
| v01 Gastronomy | +0.500 | +4.8 *** | +0.587 | +7.0 *** | +0.653 | +3.7 *** |
| v02 Population | +0.044 | +0.8 | +0.431 | +5.0 *** | +0.228 | +2.0 ** |
| v03 Lake shore density | +0.331 | +5.8 *** | +0.137 | +2.6 *** | +0.254 | +3.1 *** |
| v04 Hard outdoor | +0.236 | +2.8 *** | −0.170 | −2.1 ** | −0.532 | −4.1 *** |
| v05 Soft outdoor | +0.022 | +0.3 | +0.176 | +2.9 *** | +0.270 | +2.2 ** |
| v06 Land-use mix | −0.607 | −10.3 *** | +0.087 | +1.5 | −0.666 | −8.5 *** |
| v07 Cultural | +0.217 | +3.5 *** | +0.489 | +9.0 *** | +0.175 | +1.8 * |
| v08 Sport | +0.424 | +4.6 *** | +0.837 | +11.3 *** | +0.057 | +0.4 |
| v09 Route length | +0.045 | +0.3 | +0.858 | +7.6 *** | +0.831 | +3.7 *** |
| v10 Other leisure | +0.074 | +1.3 | +0.266 | +5.5 *** | −0.025 | −0.2 |
| v11 Diversity index | −0.263 | −1.9 * | −1.469 | −15.0 *** | −0.628 | −2.6 *** |
| v12 Urban POI density | +0.170 | +1.3 | +1.012 | +9.5 *** | +0.742 | +3.1 *** |
| v13 Support services | −0.106 | −1.3 | −0.769 | −11.9 *** | +0.264 | +1.9 * |

`*** p<0.01  ** p<0.05  * p<0.10` — Hessian-based SEs. other+AT = 0 (base).

### M4/M5 — Baseline model comparison (1k vs 10k agents)

| Parameter | 1k est. | 1k t | 10k est. | 10k t |
|---|---|---|---|---|
| beta_tt_DE | −0.0257 | −19.1 *** | −0.0269 | −60.0 *** |
| beta_tt_FR | ≈ 0 | 1.8 | ≈ 0 | — |
| beta_tt_IT | −0.0285 | −20.8 *** | −0.0305 | −68.9 *** |
| topo2 hilly — DE | −0.042 | −0.3 | −0.014 | −0.3 |
| topo2 hilly — FR | −0.460 | −3.7 *** | −0.690 | −16.2 *** |
| topo2 hilly — IT | −0.303 | −1.3 | −0.428 | −5.7 *** |
| topo3 mountain — DE | −0.072 | −0.5 | −0.161 | −3.1 *** |
| topo3 mountain — FR | −1.167 | −6.1 *** | −1.219 | −21.1 *** |
| topo3 mountain — IT | +0.261 | +1.1 | +0.104 | +1.4 |
| pop — DE | −0.055 | −1.0 | −0.111 | −6.8 *** |
| pop — FR | +0.651 | +7.4 *** | +0.430 | +17.6 *** |
| pop — IT | +0.192 | +1.9 * | +0.213 | +6.5 *** |

Topology is negative in the baseline because population proxies for urbanness, confounding mountain zones.  
Adding attractivity variables (M2/M3) restores positive topology coefficients for DE and IT.

---

## Substantive findings

1. **Travel time** is strongly negative for DE and IT (~−0.027 to −0.030/min), stable across all specifications and sample sizes. FR travel time is unidentified (near zero) throughout — likely due to fewer French tourists in the survey.

2. **Gastronomy** (v01) is the most robust attractivity driver: positive and significant for all nationalities in every specification.

3. **DE and IT penalise land-use mix** (v06 ~ −0.61/−0.67): they prefer specialised destinations (pure mountain, pure urban) rather than mixed-use zones.

4. **Route length** (v09) drives IT (+0.83) and FR (+0.86) but not DE (+0.05, n.s.) — Italians and French seek prepared outdoor routes; Germans' mountain preference is captured via topology.

5. **FR coefficient magnitudes are large** (diversity −1.47, urban density +1.01, support −0.77): French tourists strongly favour concentrated urban leisure. Interpret with caution given small FR subsample.

6. **Diversity index is negative everywhere** (v11): destinations with many different POI types are not preferred; tourists seek specialised destinations.

7. **Omitted variable bias**: topology coefficients flip sign between the sparse baseline (negative, absorbed by population proxy) and the richer models (positive for DE/IT). The 10k baseline confirms this — even at high N, topology is negative without proper attractivity controls.

---

## File index

```
output/
  bootstrap_results.csv          Bootstrap: 10 samples × 1k agents, 100 alts, 7 params
  nalts_n5000_results.csv        N_ALTS sensitivity: 5k agents, 100–500 alts
  nalts_n10000_results.csv       N_ALTS sensitivity: 10k agents, 300–1000 alts
  attr_nat_results.csv           M1: 1k agents, 58 params (AT separate)
  attr_nat2_results.csv          M2: 1k agents, 48 params (AT merged, topo×nat)
  attr_minimal_results.csv       M3: 1k agents, 27 params (6 attractivity vars)
  attr_baseline_results.csv      M4: 1k agents, 12 params (tt+topo+pop)
  attr_baseline_10k_results.csv  M5: 10k agents, 12 params (tt+topo+pop)

../benzoni_thesis/output/
  attractivity_indexes.csv       7,978 zones × 16 columns (raw + log1p for 13 indexes)
```
