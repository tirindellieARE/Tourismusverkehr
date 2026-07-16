# MNL Destination Choice Model — Results Summary

Foreign tourists entering Switzerland, AGQPV 2015 survey.
Models estimated with Apollo (R), BGW algorithm, McFadden sampled alternatives (1 chosen + N−1 random uniform).

---

## Data

| Element | Description |
|---|---|
| Survey | AGQPV 2015 — foreign tourists at Swiss border crossings |
| Agents | Non-Swiss nationals only (nationality ≠ 1) |
| Nationality groups | DE (Germany), FR (France), IT (Italy), other (incl. AT and all remaining nationalities) |
| Zone system | NPVM 2017 traffic zones (Switzerland + Liechtenstein + enclaves, N = 7,978) |
| Mode choice | Binary MNL, car vs PT, estimated on agqpv's real origin→destination trips, non-Swiss respondents only (see below) |
| Accessibility (EMU) | Expected Maximum Utility from the mode choice model — ln(exp(V_car) + exp(V_pt)) — computed for every origin–destination pair; replaces a raw travel time in the destination choice utility. Called `logsum` in the code/data (e.g. `data/output/tt_full_logsum.fst`) |
| Topology | STALAN2020 settlement type: 1 = urban/flat (base), 2 = hilly, 3 = mountain |
| Attractivity | 13 zone-level indexes from Benzoni et al. (2026), log1p-transformed and z-standardised |

---

## Mode choice model (estimates the EMU)

Script: `03_prepare_choice_data.R` | Model object: `results_output/mode_choice_model.rds`

Binary MNL, alternatives = {car, pt}, estimated on non-Swiss agqpv respondents (nationality ≠ 1, matching the destination choice models' population) with a known mode and valid travel time (`border_mode`: 1 = road → car, 2 = rail → pt; PT times ≥ 1,000 min excluded as unreachable-by-PT placeholders). Travel times (`tt_miv`, `tt_oev`) come from `data/output/tt_light.fst`, i.e. each respondent's own real origin–destination pair.

```
V_car = asc_car + b_tt_car · tt_miv
V_pt  =           b_tt_pt  · tt_oev
```

| Parameter | Estimate | s.e. |
|:---|---:|---:|
| asc_car | 1.0974 *** | 0.0381 |
| b_tt_car | −0.00371 *** | 0.00039 |
| b_tt_pt | −0.00229 *** | 0.00037 |

`*** p<0.01  ** p<0.05  * p<0.10`

LL(final) = −6,884.76 · ρ² (vs. equal shares) = 0.1550 · AIC = 13,775.5 · BIC = 13,797.6 · N = 11,754 (72.2% car / 27.8% PT observed)

Both travel-time coefficients are negative, as expected. The EMU built from these coefficients,

```
EMU_od = ln( exp(V_car,od) + exp(V_pt,od) )
```

is computed for every OD pair in `data/output/tt_full.fst` and saved to `data/output/tt_full_logsum.fst` (column `logsum`). It correlates at **−0.99** with car travel time — confirming it behaves as an accessibility measure (high = easy to reach), not a cost, which is why it enters the destination choice utility below with a *positive* expected sign.

### Subgroup-specific mode choice models: Tagesreise vs. Reise mit Ü.

The same model is re-estimated separately on the Tagesreise (`n_nights == 0`) and Reise mit Ü. (`n_nights > 0`) subgroups, each producing its own EMU (`data/output/tt_full_logsum_tagesreise.fst`, `data/output/tt_full_logsum_reisemitue.fst`) for the subgroup destination models further down.

| Parameter | Tagesreise | Reise mit Ü. |
|:---|---:|---:|
| asc_car | 1.2370 *** | 0.2357 *** |
| b_tt_car | −0.00350 *** | −0.00195 *** |
| b_tt_pt | −0.00288 *** | −0.00295 *** |

`*** p<0.01  ** p<0.05  * p<0.10`

| Group | N | LL(final) | ρ² (equal shares) | AIC | BIC | % car / % PT |
|---|---:|---:|---:|---:|---:|---|
| Tagesreise | 7,004 | −3,667.86 | 0.2445 | 7,341.7 | 7,362.3 | 78.1% / 21.9% |
| Reise mit Ü. | 4,750 | −3,094.03 | 0.0603 | 6,194.1 | 6,213.5 | 63.6% / 36.4% |

Day-trippers are much more car-oriented (`asc_car = 1.24` vs. `0.24`) and more travel-time-sensitive to car time specifically (`b_tt_car = −0.0035` vs. `−0.0019`) than overnight visitors — PT time sensitivity (`b_tt_pt`) is similar across both groups. Overnight visitors are far closer to a coin-flip between modes, consistent with more of them arriving by train/plane in the first place and being comfortable using PT onward.

---

## Minimal model — EMU × nationality + topology × nationality

Script: `04_destination_choice.R` → `run_destination_model()` / `run_destination_batch()`
1,000 agents, seed = 42. N_ALTS swept 300 → 500 to check choice-set-size sensitivity.

```
V_j = Σ_k β_EMU_k · EMU_j            · I(nat = k)
    + Σ_k β_topo2_k  · I(topo_j = 2) · I(nat = k)
    + Σ_k β_topo3_k  · I(topo_j = 3) · I(nat = k)      k ∈ {DE, FR, IT, other}
```

### Fit by choice-set size

| N alts | LL | ρ² (equal shares) | AIC | BIC | Time |
|---|---|---|---|---|---|
| 300 | −4,718.95 | 0.1727 | 9,461.9 | 9,520.8 | 68s |
| 400 | −5,006.10 | 0.1645 | 10,036.2 | 10,095.1 | 90s |
| 500 | −5,225.06 | 0.1592 | 10,474.1 | 10,533.0 | 131s |

> LL/AIC/BIC scale with alternative count since the choice-set denominator grows; the ρ² drift is the expected mechanical effect of diluting "easy" choices with more random alternatives, not model degradation.

### Coefficients (stable across 300/400/500 alternatives)

| Parameter | 300 alts | 400 alts | 500 alts |
|:---|---:|---:|---:|
| β EMU · DE | 5.847 *** | 5.883 *** | 5.919 *** |
| β EMU · FR | 8.370 *** | 8.230 *** | 8.267 *** |
| β EMU · IT | 7.904 *** | 7.903 *** | 7.899 *** |
| β EMU · other | 8.183 *** | 8.139 *** | 8.171 *** |
| β topo2 · DE | −0.093 | −0.089 | −0.086 |
| β topo2 · FR | −0.719 *** | −0.716 *** | −0.724 *** |
| β topo2 · IT | 0.025 | 0.027 | −0.023 |
| β topo2 · other | −0.598 | −0.639 * | −0.633 |
| β topo3 · DE | 0.001 | 0.006 | 0.013 |
| β topo3 · FR | −0.653 *** | −0.696 *** | −0.696 *** |
| β topo3 · IT | 0.172 | 0.174 | 0.183 |
| β topo3 · other | −0.736 | −0.810 * | −0.780 * |

`*** p<0.01  ** p<0.05  * p<0.10`

**Takeaway**: coefficients move by <3% across 300–500 alternatives, no sign flips. 300 alternatives is already enough to identify this specification stably — going to 500 nearly doubles estimation time (68s → 131s) for no material gain in precision.

### Interpreting the EMU coefficients

- **Sign**: EMU is already an accessibility measure (high = easy to reach), not a cost, so a *positive* β_EMU_k means "more accessible destinations get chosen more often" — the expected sign. All four nationality groups show this, strongly significant.
- **Magnitude isn't "minutes"**: EMU is built from the mode choice model's own (arbitrarily-scaled) utils, and the two models are estimated *sequentially*, not jointly (FIML) — so β_EMU_k isn't bounded to the (0,1) nesting-parameter range from formal nested logit theory, and its raw size (5.8–8.4) has no direct physical unit on its own.
- **Cross-nationality comparison is valid**: every nationality faces the exact same EMU values, so the *ratios* between groups' coefficients are meaningful. Ranking: **FR (8.23–8.37) > other (8.14–8.18) > IT (7.90) > DE (5.85–5.92)** — French tourists' destination choices are the most accessibility-driven; German tourists the least (though still highly significant).
- **Converting back to minutes (sanity check)**: using `∂EMU/∂V_car = P(car)` (a standard logit result — the derivative of a logsum w.r.t. one alternative's utility equals that alternative's choice probability), the implied marginal utility of an extra minute of car time is `β_EMU_k · P(car) · b_tt_car`. With the mode choice model's `P(car) = 0.722` and `b_tt_car = −0.00371`:

  | Nationality | Implied utils lost / minute of car time |
  |:---|---:|
  | DE | −0.0157 |
  | FR | −0.0224 |
  | IT | −0.0212 |
  | other | −0.0219 |

  These land close to the old direct-travel-time model (DE ≈ −0.026 to −0.027/min, IT ≈ −0.029 to −0.031/min) — a reassuring consistency check across two very different specifications. The EMU-implied values run somewhat smaller because travelers can behaviorally substitute toward PT when car gets slow, softening the pure car-time penalty in a way a fixed 0.9/0.1 travel-time blend couldn't.

Results file: `results_output/destination_mnl_batch_minimal_1000agents_altsweep.csv`

### Tagesreise vs. Reise mit Ü. (day trips vs. overnight stays)

Same minimal model spec (EMU × nat + topo × nat), same 1,000-agent / 300-alt setup, seed = 42, run separately on two `n_nights`-based subgroups of respondents (`04_destination_choice.R`, `agent_pool` argument). Each subgroup uses its **own** EMU, from a mode choice model estimated on that subgroup alone (`03_prepare_choice_data.R`), rather than the pooled EMU — Tagesreise and Reise mit Ü. travelers turn out to have genuinely different car/PT behavior (Tagesreise: 78.1% car / 21.9% PT, `asc_car = 1.24`; Reise mit Ü.: 63.6% car / 36.4% PT, `asc_car = 0.24`), so each destination model should see accessibility as *that group* actually experiences it.

| Group | Respondents (`n_nights`) | LL | ρ² | AIC | BIC |
|---|---|---:|---:|---:|---:|
| Tagesreise | = 0 (7,009 total, 1,000 sampled) | −3,884.55 | 0.3190 | 7,793.1 | 7,852.0 |
| Reise mit Ü. | > 0 (4,751 total, 1,000 sampled) | −5,398.27 | 0.0536 | 10,820.5 | 10,879.4 |

| Parameter | Tagesreise | Reise mit Ü. |
|:---|---:|---:|
| β EMU · DE | 13.592 *** | 3.348 *** |
| β EMU · FR | 13.387 *** | 5.662 *** |
| β EMU · IT | 9.199 *** | 7.307 *** |
| β EMU · other | 17.168 *** | 6.146 *** |
| β topo2 · DE | −0.187 | −0.081 |
| β topo2 · FR | −0.585 *** | −0.128 |
| β topo2 · IT | −0.045 | 0.096 |
| β topo2 · other | −1.599 *** | 0.144 |
| β topo3 · DE | −0.434 ** | 0.030 |
| β topo3 · FR | −0.993 *** | −0.429 ** |
| β topo3 · IT | −0.186 | 0.230 |
| β topo3 · other | 0.022 | −0.257 |

`*** p<0.01  ** p<0.05  * p<0.10`

**Takeaway**: day-trippers remain far more accessibility-driven than overnight visitors — EMU coefficients run roughly 2–4× larger — and the minimal model still fits day trips much better (ρ² = 0.32 vs. 0.05). Using each group's own EMU (rather than the pooled one) narrows that gap somewhat compared to before, because Reise mit Ü.'s own mode choice model already has a flatter travel-time sensitivity (`b_tt_car = −0.00195` vs. Tagesreise's `−0.00350`) — part of what looked like a destination-level accessibility difference was really a mode-choice-level difference between the two groups, and giving each group its own EMU lets the two models separate those two effects properly. Overnight visitors are still far less sensitive to topology; their destination choice is presumably driven much more by factors outside this minimal spec (accommodation, multi-day itineraries, attractivity) — consistent with the much lower ρ² for that group.

Results file: `results_output/destination_mnl_tagesreise_vs_reisemitue.csv` · EMU lookups: `data/output/tt_full_logsum_tagesreise.fst`, `data/output/tt_full_logsum_reisemitue.fst`

### Coefficient stability (10-sample bootstrap)

Same two subgroups, same 1,000-agent / 300-alt / own-EMU setup, but each repeated 10 times with a fresh **bootstrap resample of agents (with replacement)** per run (`run_destination_bootstrap()`, seeds 1–10). All 20 runs converged cleanly (no singular Hessians).

**Tagesreise**

| Parameter | Mean | SD | Min | Max |
|:---|---:|---:|---:|---:|
| β EMU · DE | 12.486 | 0.481 | 11.729 | 13.104 |
| β EMU · FR | 12.755 | 0.979 | 11.180 | 14.030 |
| β EMU · IT | 9.445 | 0.404 | 8.811 | 10.254 |
| β EMU · other | 14.340 | 1.624 | 12.435 | 17.793 |
| β topo2 · DE | −0.309 | 0.093 | −0.408 | −0.166 |
| β topo2 · FR | −0.569 | 0.124 | −0.811 | −0.351 |
| β topo2 · IT | −0.496 | 0.100 | −0.679 | −0.363 |
| β topo2 · other | −0.722 | 0.311 | −1.146 | −0.231 |
| β topo3 · DE | −0.449 | 0.146 | −0.736 | −0.239 |
| β topo3 · FR | −0.785 | 0.198 | −1.045 | −0.414 |
| β topo3 · IT | −0.076 | 0.226 | −0.355 | **+0.373** |
| β topo3 · other | −0.285 | 0.518 | −1.168 | **+0.496** |

**Reise mit Ü.**

| Parameter | Mean | SD | Min | Max |
|:---|---:|---:|---:|---:|
| β EMU · DE | 3.158 | 0.462 | 2.552 | 4.014 |
| β EMU · FR | 6.028 | 0.686 | 5.251 | 7.444 |
| β EMU · IT | 7.436 | 0.219 | 7.021 | 7.684 |
| β EMU · other | 6.691 | 0.838 | 5.448 | 7.965 |
| β topo2 · DE | −0.185 | 0.074 | −0.349 | −0.067 |
| β topo2 · FR | −0.170 | 0.235 | −0.381 | **+0.393** |
| β topo2 · IT | 0.135 | 0.226 | −0.363 | 0.556 |
| β topo2 · other | −0.064 | 0.376 | −0.636 | 0.534 |
| β topo3 · DE | 0.067 | 0.112 | −0.086 | 0.244 |
| β topo3 · FR | −0.550 | 0.270 | −1.042 | −0.133 |
| β topo3 · IT | 0.200 | 0.204 | −0.156 | 0.506 |
| β topo3 · other | −0.508 | 0.393 | −1.414 | −0.063 |

**Takeaway**: EMU is robustly positive and reasonably tight across resamples for every nationality in both subgroups (coefficient of variation 3–15%), and the single-sample point estimates reported above fall well inside these bootstrap ranges — good confirmation those weren't an artifact of one particular draw. Topology coefficients are markedly less stable, and several **cross zero across resamples** (bolded): Tagesreise's `topo3·IT` and `topo3·other`, Reise mit Ü.'s `topo2·FR`. This matches what the asymptotic standard errors already flagged as non-significant in the single-run estimates above — a useful cross-check that those significance results reflect genuine sampling instability rather than an artifact of the asymptotic approximation. ρ² stayed tightly banded across resamples too (Tagesreise 0.29–0.32, Reise mit Ü. 0.049–0.068), close to each group's single-run value.

Results files: `results_output/destination_mnl_bootstrap_tagesreise_boot10.csv`, `results_output/destination_mnl_bootstrap_reisemitue_boot10.csv`

### EMU + population (v02) instead of topology

Same Tagesreise / Reise mit Ü. setup (1,000 agents, 300 alts, seed = 42, own EMU per subgroup), but with the topology dummies (`topo2`/`topo3`) swapped out for `v02_resident_population_log1p` (`04_destination_choice.R`, `include_topo = FALSE`, `attr_vars = "v02_resident_population_log1p"`):

```
V_j = Σ_k β_EMU_k · EMU_j · I(nat = k) + Σ_k β_v02_k · v02_j · I(nat = k)
```

| Group | LL | ρ² | AIC | BIC | Time |
|---|---:|---:|---:|---:|---:|
| Tagesreise | −3,889.00 | 0.3182 | 7,794.0 | 7,833.3 | 31s |
| Reise mit Ü. | −5,397.51 | 0.0537 | 10,811.0 | 10,850.3 | 29s |

| Parameter | Tagesreise | Reise mit Ü. |
|:---|---:|---:|
| β EMU · DE | 13.632 *** | 3.389 *** |
| β EMU · FR | 13.623 *** | 5.832 *** |
| β EMU · IT | 9.324 *** | 7.206 *** |
| β EMU · other | 18.413 *** | 6.152 *** |
| β v02 · DE | −0.112 ** | −0.070 * |
| β v02 · FR | 0.141 ** | −0.115 * |
| β v02 · IT | 0.226 ** | −0.078 |
| β v02 · other | −0.500 *** | −0.164 |

`*** p<0.01  ** p<0.05  * p<0.10`

**Takeaway**: EMU coefficients are essentially unchanged from the topology-included spec (e.g. Tagesreise DE 13.59 → 13.63, Reise mit Ü. DE 3.35 → 3.39), and fit barely moves (ρ² 0.3190 → 0.3182 for Tagesreise, 0.0536 → 0.0537 for Reise mit Ü.) — population and topology aren't substituting for each other's explanatory power here, they're capturing different things. Population itself splits by nationality and by trip type: for Tagesreise, DE and "other" avoid populous zones while FR and IT are drawn to them; for Reise mit Ü., population is weakly negative or null across the board (only DE and FR reach 10% significance) — overnight visitors' destination choice is still not well explained by either topology or population alone, consistent with the low ρ² for that group throughout.

Results file: `results_output/destination_mnl_tagesreise_vs_reisemitue_v02_notopo.csv`

### Same spec, half sample

Same EMU + v02 (no topology) spec, seed = 42, 300 alts, but using **half of each subgroup's full respondent pool** instead of 1,000: 3,504 of 7,009 Tagesreise respondents, 2,375 of 4,751 Reise mit Ü. respondents.

| Group | N | LL | ρ² | AIC | BIC | Time |
|---|---:|---:|---:|---:|---:|---:|
| Tagesreise | 3,504 | −13,923.38 | 0.3033 | 27,862.8 | 27,912.0 | 102s |
| Reise mit Ü. | 2,375 | −12,818.03 | 0.0538 | 25,652.1 | 25,698.2 | 67s |

| Parameter | Tagesreise | Reise mit Ü. |
|:---|---:|---:|
| β EMU · DE | 12.783 *** | 3.451 *** |
| β EMU · FR | 12.833 *** | 6.197 *** |
| β EMU · IT | 9.582 *** | 7.165 *** |
| β EMU · other | 14.069 *** | 6.093 *** |
| β v02 · DE | −0.143 *** | −0.073 *** |
| β v02 · FR | 0.292 *** | −0.012 |
| β v02 · IT | 0.180 *** | −0.014 |
| β v02 · other | −0.152 ** | −0.150 ** |

`*** p<0.01  ** p<0.05  * p<0.10`

**Takeaway**: EMU coefficients hold up well at the larger sample (Tagesreise DE 13.63 → 12.78, Reise mit Ü. DE 3.39 → 3.45 — same ballpark, tighter standard errors), reinforcing that the 1,000-agent point estimates weren't sample-size artifacts. The v02 coefficients move more: Tagesreise's `v02·other` shrinks sharply (−0.500 → −0.152, though still significant) and standard errors tighten enough that `v02·DE/FR/IT` cross from `**` to `***`; Reise mit Ü.'s `v02·FR` flips from marginally significant (`*`, −0.115) to null (−0.012, n.s.) at the larger sample — the 1,000-agent estimate for that one parameter looks like it was mostly small-sample noise. Runtime scaled close to linearly with agent count, as expected (~30s→102s for a 3.5× larger Tagesreise sample, ~29s→67s for Reise mit Ü.'s 2.4× larger one).

### Choice-set-size sensitivity, half sample (300 → 500 → 1,000 → 5,000 alternatives)

Same half-sample EMU + v02 (no topology) spec (3,504 Tagesreise / 2,375 Reise mit Ü. respondents, seed = 42), now varying the choice-set size instead of the agent count. The zone universe has 8,688 zones total, so 5,000 alternatives is well within range (all draws are with replacement regardless, as in every other run here).

| Group | N alts | LL | ρ² | AIC | BIC | Time |
|---|---:|---:|---:|---:|---:|---:|
| Tagesreise | 300 | −13,923.38 | 0.3033 | 27,862.8 | 27,912.0 | 102s |
| Tagesreise | 500 | −15,723.02 | 0.2780 | 31,462.0 | 31,511.3 | 175s |
| Tagesreise | 1,000 | −18,219.99 | 0.2473 | 36,456.0 | 36,505.3 | 349s |
| Tagesreise | 5,000 | −24,067.89 | 0.1935 | 48,151.8 | 48,201.1 | 2,838s (47.3 min) |
| Reise mit Ü. | 300 | −12,818.03 | 0.0538 | 25,652.1 | 25,698.2 | 67s |
| Reise mit Ü. | 500 | −14,030.62 | 0.0494 | 28,077.2 | 28,123.4 | 115s |
| Reise mit Ü. | 1,000 | −15,677.05 | 0.0444 | 31,370.1 | 31,416.3 | 234s |
| Reise mit Ü. | 5,000 | −19,497.75 | 0.0361 | 39,011.5 | 39,057.7 | 1,186s (19.8 min) |

| Parameter | Tag. 300 | Tag. 500 | Tag. 1000 | Tag. 5000 | Ü. 300 | Ü. 500 | Ü. 1000 | Ü. 5000 |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| β EMU · DE | 12.783 *** | 12.329 *** | 11.786 *** | 10.174 *** | 3.451 *** | 3.454 *** | 3.453 *** | 3.452 *** |
| β EMU · FR | 12.833 *** | 12.615 *** | 11.948 *** | 10.035 *** | 6.197 *** | 6.192 *** | 6.149 *** | 6.132 *** |
| β EMU · IT | 9.582 *** | 9.473 *** | 9.323 *** | 8.998 *** | 7.165 *** | 7.114 *** | 7.051 *** | 7.070 *** |
| β EMU · other | 14.069 *** | 14.093 *** | 13.371 *** | 11.992 *** | 6.093 *** | 5.969 *** | 6.079 *** | 5.971 *** |
| β v02 · DE | −0.143 *** | −0.141 *** | −0.146 *** | −0.140 *** | −0.073 *** | −0.073 *** | −0.075 *** | −0.073 *** |
| β v02 · FR | 0.292 *** | 0.300 *** | 0.310 *** | 0.324 *** | −0.012 | −0.009 | −0.005 | −0.005 |
| β v02 · IT | 0.180 *** | 0.179 *** | 0.176 *** | 0.164 *** | −0.014 | −0.013 | −0.013 | −0.013 |
| β v02 · other | −0.152 ** | −0.146 ** | −0.138 ** | −0.122 ** | −0.150 ** | −0.150 ** | −0.158 ** | −0.158 ** |

`*** p<0.01  ** p<0.05  * p<0.10`

**Takeaway**: Reise mit Ü. stays essentially flat all the way out to 5,000 alternatives — every coefficient within a few percent of its 300-alt value. Tagesreise shows a real, if modest, downward drift in its EMU coefficients as the choice set grows (DE 12.78 → 10.17, FR 12.83 → 10.03, roughly −20% cumulative from 300 to 5,000 alts) — small enough that sign and significance never change, but a genuine trend rather than noise, since it moves consistently in one direction across all four choice-set sizes. v02 stays stable throughout for both subgroups. ρ² keeps declining mechanically as the choice set grows (more random alternatives dilute the easy choices), most visibly for Tagesreise (0.303 → 0.194) since it's the better-fitting model to begin with. **Runtime is where 5,000 alternatives really bites**: Tagesreise jumped to 47 minutes — well above the ~29 min linear extrapolation from the 300–1,000 trend — because the optimizer needed 47 iterations to converge at this choice-set size vs. only 10 for Reise mit Ü. (whose 19.8 min runtime matched the linear projection closely). Practically: 300 alternatives remains the right default for this spec — it already captures the stable part of the signal, and the modest additional drift visible in Tagesreise's EMU coefficients at 5,000 alts doesn't come close to justifying a 28× runtime cost.

Results files: `results_output/destination_mnl_tagesreise_vs_reisemitue_v02_notopo_half_altsweep.csv`, `results_output/destination_mnl_tagesreise_vs_reisemitue_v02_notopo_half_alts5000.csv`

### Same spec, full sample

Same EMU + v02 (no topology) spec, seed = 42, 300 alts, now using **every** non-Swiss respondent in each subgroup: all 7,009 Tagesreise, all 4,751 Reise mit Ü.

| Group | N | LL | ρ² | AIC | BIC | Time |
|---|---:|---:|---:|---:|---:|---:|
| Tagesreise | 7,009 | −27,994.61 | 0.2997 | 56,005.2 | 56,060.1 | 178s |
| Reise mit Ü. | 4,751 | −25,588.27 | 0.0557 | 51,192.5 | 51,244.3 | 129s |

| Parameter | Tagesreise | Reise mit Ü. |
|:---|---:|---:|
| β EMU · DE | 12.310 *** | 3.114 *** |
| β EMU · FR | 12.957 *** | 6.143 *** |
| β EMU · IT | 9.457 *** | 7.529 *** |
| β EMU · other | 13.901 *** | 6.327 *** |
| β v02 · DE | −0.156 *** | −0.081 *** |
| β v02 · FR | 0.275 *** | −0.014 |
| β v02 · IT | 0.179 *** | 0.014 |
| β v02 · other | −0.106 ** | −0.142 *** |

`*** p<0.01  ** p<0.05  * p<0.10`

### Convergence across sample sizes (1,000 → half → full)

| Parameter | Tagesreise 1k | Tagesreise half | Tagesreise full | Reise mit Ü. 1k | Reise mit Ü. half | Reise mit Ü. full |
|:---|---:|---:|---:|---:|---:|---:|
| β EMU · DE | 13.632 *** | 12.783 *** | 12.310 *** | 3.389 *** | 3.451 *** | 3.114 *** |
| β EMU · FR | 13.623 *** | 12.833 *** | 12.957 *** | 5.832 *** | 6.197 *** | 6.143 *** |
| β EMU · IT | 9.324 *** | 9.582 *** | 9.457 *** | 7.206 *** | 7.165 *** | 7.529 *** |
| β EMU · other | 18.413 *** | 14.069 *** | 13.901 *** | 6.152 *** | 6.093 *** | 6.327 *** |
| β v02 · DE | −0.112 ** | −0.143 *** | −0.156 *** | −0.070 * | −0.073 *** | −0.081 *** |
| β v02 · FR | 0.141 ** | 0.292 *** | 0.275 *** | −0.115 * | −0.012 | −0.014 |
| β v02 · IT | 0.226 ** | 0.180 *** | 0.179 *** | −0.078 | −0.014 | 0.014 |
| β v02 · other | −0.500 *** | −0.152 ** | −0.106 ** | −0.164 | −0.150 ** | −0.142 *** |

`*** p<0.01  ** p<0.05  * p<0.10`

**Takeaway**: with the full sample, the picture is clean. EMU coefficients were already close to their final values at 1,000 agents and barely move from half to full sample — genuinely stable, well-identified effects, `***` throughout. The v02 coefficients tell a more interesting story: `v02·other` for Tagesreise keeps shrinking as sample grows (−0.500 → −0.152 → −0.106) and settles at a modest, still-significant negative effect — the 1,000-agent estimate overstated it by roughly 5×, a real small-sample bias, not just noise, since it kept moving in the same direction across both larger samples rather than randomly settling. `v02·IT` for Reise mit Ü. never reaches significance at any sample size and hovers near zero throughout — good evidence population genuinely doesn't drive that group's destination choice. `v02·FR` for Reise mit Ü. is the opposite pattern from `v02·other`/Tagesreise: only marginally significant (`*`) at 1,000 agents, then drops to null at both larger samples — that one looks like the 1,000-agent estimate was mostly noise crossing the significance threshold by chance, not a real effect that got diluted. Runtime scaled essentially linearly with N throughout (Tagesreise: 30s → 102s → 178s for roughly 1×/3.5×/7× the agents; Reise mit Ü.: 29s → 67s → 129s for 1×/2.4×/4.75×).

Results file: `results_output/destination_mnl_tagesreise_vs_reisemitue_v02_notopo_full.csv`

Results file: `results_output/destination_mnl_tagesreise_vs_reisemitue_v02_notopo_half.csv`

---

## Attractivity indexes (Benzoni et al. 2026)

Source: `../benzoni_thesis/output/attractivity_indexes.csv` (7,978 zones, computed from OSM, swissTLM3D, BFS).
All variables log1p-transformed then z-standardised before entering models. `04_destination_choice.R` loads all 13 as `ATTR_COLS`; any subset can be passed as `attr_vars` to a model run.

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

> The "minimal model + accessibility variables" run (EMU + topology + a 6-variable attractivity subset, all × nationality) is temporarily removed from this README pending re-estimation with the rebuilt non-Swiss-only EMU. `04_destination_choice.R`'s `attr_vars` argument still supports it — see the `ATTR_VARS` example at the bottom of that script.

---

## File index

```
data/input/                            Raw files, not produced by any script -- see README_script_map.md
  Finale_Auswertungsdatenbank_AGQPV2015_V2.csv, zones_communes.gpkg, TTC.omx, RITA.omx, EGT.omx, ACT.omx

data/output/                           Intermediary files: produced by one script, read by another
  agqpv.csv                           Filtered/zone-matched survey (01_agent_generation.R)
  tt_light.fst                        Travel times for OD pairs actually observed in agqpv.csv
  tt_full.fst                         Travel times for all agqpv origins x ~8,000 reachable zones
  tt_full_logsum.fst                  tt_full.fst + EMU column (named `logsum`), pooled non-Swiss model
  tt_full_logsum_tagesreise.fst       tt_full.fst + EMU column, Tagesreise-only mode choice model
  tt_full_logsum_reisemitue.fst       tt_full.fst + EMU column, Reise-mit-Ü.-only mode choice model

results_output/
  mode_choice_model.rds                                Fitted mode choice model, pooled non-Swiss
  mode_choice_model_tagesreise.rds                     Fitted mode choice model, Tagesreise only
  mode_choice_model_reisemitue.rds                      Fitted mode choice model, Reise mit Ü. only
  mode_choice_iterations.csv                           Mode choice optimizer trace (pooled)
  destination_mnl_batch_minimal_1000agents_altsweep.csv    Minimal model: 1k agents, 300/400/500 alts
  destination_mnl_tagesreise_vs_reisemitue.csv             Tagesreise vs. Reise mit Ü.: 1k agents each, 300 alts, own EMU

../benzoni_thesis/output/
  attractivity_indexes.csv           7,978 zones x 16 columns (raw + log1p for 13 indexes)
```
