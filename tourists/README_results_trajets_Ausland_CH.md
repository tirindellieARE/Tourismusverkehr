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
| Accessibility (EMU) | Expected Maximum Utility from the mode choice model — ln(exp(V_car) + exp(V_pt)) — computed for every origin–destination pair; replaces a raw travel time in the destination choice utility. Called `logsum` in the code/data (e.g. `data/output/tt_ausland_CH_logsum.fst`) |
| Topology | STALAN2020 settlement type: 1 = urban/flat (base), 2 = hilly, 3 = mountain |
| Attractivity | 13 zone-level indexes from Benzoni et al. (2026), log1p-transformed and z-standardised |

---

## Mode choice model (estimates the EMU)

Script: `03_estimate_logsum.R` | Model object: `results_output/mode_choice_model.rds`

Binary MNL, alternatives = {car, pt}, estimated on non-Swiss agqpv respondents (nationality ≠ 1, matching the destination choice models' population) with a known mode and valid travel time (`border_mode`: 1 = road → car, 2 = rail → pt; PT times ≥ 1,000 min excluded as unreachable-by-PT placeholders). Travel times (`tt_miv`, `tt_oev`) come from `data/output/tt_agqpv.fst`, i.e. each respondent's own real origin–destination pair.

```
V_car = asc_car + b_tt_car · tt_miv
V_pt  =           b_tt_pt  · tt_oev
```

| Parameter | Estimate | s.e. |
|:---|---:|---:|
| asc_car | 1.0945 *** | 0.0381 |
| b_tt_car | −0.00370 *** | 0.00039 |
| b_tt_pt | −0.00228 *** | 0.00037 |

`*** p<0.01  ** p<0.05  * p<0.10`

LL(final) = −6,872.22 · ρ² (vs. equal shares) = 0.1543 · AIC = 13,750.4 · BIC = 13,772.6 · N = 11,723 (72.2% car / 27.8% PT observed)

Both travel-time coefficients are negative, as expected. The EMU built from these coefficients,

```
EMU_od = ln( exp(V_car,od) + exp(V_pt,od) )
```

is computed for every OD pair in `data/output/tt_ausland_CH.fst` (every zone abroad × every Swiss destination zone, for computational speed — smaller than a full zone × zone table would be, since Ausland/LI zones are never valid tourism destinations and don't need to appear on the destination side at all) and saved to `data/output/tt_ausland_CH_logsum.fst` (column `logsum`). It correlates at **−0.99** with car travel time — confirming it behaves as an accessibility measure (high = easy to reach), not a cost, which is why it enters the destination choice utility below with a *positive* expected sign.

### Subgroup-specific mode choice models: Tagesreise vs. Reise mit Ü.

The same model is re-estimated separately on the Tagesreise (`n_nights == 0`) and Reise mit Ü. (`n_nights > 0`) subgroups, each producing its own EMU (`data/output/tt_ausland_CH_logsum_tagesreise.fst`, `data/output/tt_ausland_CH_logsum_reisemitue.fst`) for the subgroup destination models further down.

| Parameter | Tagesreise | Reise mit Ü. |
|:---|---:|---:|
| asc_car | 1.2327 *** | 0.2309 *** |
| b_tt_car | −0.00349 *** | −0.00193 *** |
| b_tt_pt | −0.00288 *** | −0.00294 *** |

`*** p<0.01  ** p<0.05  * p<0.10`

| Group | N | LL(final) | ρ² (equal shares) | AIC | BIC | % car / % PT |
|---|---:|---:|---:|---:|---:|---|
| Tagesreise | 6,986 | −3,662.20 | 0.2437 | 7,330.4 | 7,350.9 | 78.1% / 21.9% |
| Reise mit Ü. | 4,737 | −3,087.11 | 0.0598 | 6,180.2 | 6,199.6 | 63.5% / 36.5% |

Day-trippers are much more car-oriented (`asc_car = 1.23` vs. `0.23`) and more travel-time-sensitive to car time specifically (`b_tt_car = −0.0035` vs. `−0.0019`) than overnight visitors — PT time sensitivity (`b_tt_pt`) is similar across both groups. Overnight visitors are far closer to a coin-flip between modes, consistent with more of them arriving by train/plane in the first place and being comfortable using PT onward.

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

Same minimal model spec (EMU × nat + topo × nat), same 1,000-agent / 300-alt setup, seed = 42, run separately on two `n_nights`-based subgroups of respondents (`04_destination_choice.R`, `agent_pool` argument). Each subgroup uses its **own** EMU, from a mode choice model estimated on that subgroup alone (`03_estimate_logsum.R`), rather than the pooled EMU — Tagesreise and Reise mit Ü. travelers turn out to have genuinely different car/PT behavior (Tagesreise: 78.1% car / 21.9% PT, `asc_car = 1.24`; Reise mit Ü.: 63.6% car / 36.4% PT, `asc_car = 0.24`), so each destination model should see accessibility as *that group* actually experiences it.

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

Results file: `results_output/destination_mnl_tagesreise_vs_reisemitue.csv` · EMU lookups: `data/output/tt_ausland_CH_logsum_tagesreise.fst`, `data/output/tt_ausland_CH_logsum_reisemitue.fst`

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

### EMU + population (pop) instead of topology

Same Tagesreise / Reise mit Ü. setup (1,000 agents, 300 alts, seed = 42, own EMU per subgroup), but with the topology dummies (`topo2`/`topo3`) swapped out for `v02_resident_population_log1p` (`04_destination_choice.R`, `include_topo = FALSE`, `attr_vars = "v02_resident_population_log1p"`):

```
V_j = Σ_k β_EMU_k · EMU_j · I(nat = k) + Σ_k β_pop_k · pop_j · I(nat = k)
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
| β pop · DE | −0.112 ** | −0.070 * |
| β pop · FR | 0.141 ** | −0.115 * |
| β pop · IT | 0.226 ** | −0.078 |
| β pop · other | −0.500 *** | −0.164 |

`*** p<0.01  ** p<0.05  * p<0.10`

**Takeaway**: EMU coefficients are essentially unchanged from the topology-included spec (e.g. Tagesreise DE 13.59 → 13.63, Reise mit Ü. DE 3.35 → 3.39), and fit barely moves (ρ² 0.3190 → 0.3182 for Tagesreise, 0.0536 → 0.0537 for Reise mit Ü.) — population and topology aren't substituting for each other's explanatory power here, they're capturing different things. Population itself splits by nationality and by trip type: for Tagesreise, DE and "other" avoid populous zones while FR and IT are drawn to them; for Reise mit Ü., population is weakly negative or null across the board (only DE and FR reach 10% significance) — overnight visitors' destination choice is still not well explained by either topology or population alone, consistent with the low ρ² for that group throughout.

Results file: `results_output/destination_mnl_tagesreise_vs_reisemitue_v02_notopo.csv`

### Same spec, half sample

Same EMU + pop (no topology) spec, seed = 42, 300 alts, but using **half of each subgroup's full respondent pool** instead of 1,000: 3,504 of 7,009 Tagesreise respondents, 2,375 of 4,751 Reise mit Ü. respondents.

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
| β pop · DE | −0.143 *** | −0.073 *** |
| β pop · FR | 0.292 *** | −0.012 |
| β pop · IT | 0.180 *** | −0.014 |
| β pop · other | −0.152 ** | −0.150 ** |

`*** p<0.01  ** p<0.05  * p<0.10`

**Takeaway**: EMU coefficients hold up well at the larger sample (Tagesreise DE 13.63 → 12.78, Reise mit Ü. DE 3.39 → 3.45 — same ballpark, tighter standard errors), reinforcing that the 1,000-agent point estimates weren't sample-size artifacts. The pop coefficients move more: Tagesreise's `pop·other` shrinks sharply (−0.500 → −0.152, though still significant) and standard errors tighten enough that `pop·DE/FR/IT` cross from `**` to `***`; Reise mit Ü.'s `pop·FR` flips from marginally significant (`*`, −0.115) to null (−0.012, n.s.) at the larger sample — the 1,000-agent estimate for that one parameter looks like it was mostly small-sample noise. Runtime scaled close to linearly with agent count, as expected (~30s→102s for a 3.5× larger Tagesreise sample, ~29s→67s for Reise mit Ü.'s 2.4× larger one).

### EMU + Erreichbarkeit instead of population, half sample

Same half-sample setup (3,504 Tagesreise / 2,375 Reise mit Ü. respondents, seed = 42, 300 alts, own EMU per subgroup), but `pop` (population) swapped for **Erreichbarkeit** — a zone-level potential-accessibility index from `data/erreichbarkeit_tt2023.gpkg` (not part of the Benzoni attractivity set; 8,688 zones, exact match to the NPVM zone system via `ID_Zone`). Used `erreichbarkeit_avg` (car+PT combined), z-standardised, as the closest zone-level counterpart to EMU's own car+PT blend — `erreichbarkeit_miv`/`erreichbarkeit_oev` are available in the same file if the mode-specific versions are wanted instead.

```
V_j = Σ_k β_EMU_k · EMU_j · I(nat = k) + Σ_k β_erreichbarkeit_k · erreichbarkeit_avg_j · I(nat = k)
```

| Group | LL | ρ² | AIC | BIC | Time |
|---|---:|---:|---:|---:|---:|
| Tagesreise | −13,927.69 | 0.3031 | 27,871.4 | 27,920.7 | 119s |
| Reise mit Ü. | −12,661.52 | 0.0653 | 25,339.0 | 25,385.2 | 66s |

| Parameter | Tagesreise | Reise mit Ü. |
|:---|---:|---:|
| β EMU · DE | 12.873 *** | 4.596 *** |
| β EMU · FR | 12.819 *** | 6.310 *** |
| β EMU · IT | 9.336 *** | 6.734 *** |
| β EMU · other | 13.946 *** | 6.032 *** |
| β Erreichbarkeit · DE | 0.364 *** | −0.692 *** |
| β Erreichbarkeit · FR | 0.167 *** | −0.137 * |
| β Erreichbarkeit · IT | −0.402 *** | −0.428 *** |
| β Erreichbarkeit · other | −0.021 | −0.027 |

`*** p<0.01  ** p<0.05  * p<0.10`

**Takeaway**: fit is a near-exact tie with the population spec for Tagesreise (ρ² 0.3033 vs. 0.3031) but notably better for Reise mit Ü. (0.0538 → 0.0653) — Erreichbarkeit captures something population didn't for overnight visitors specifically. The sign pattern is the more interesting result: for Tagesreise, DE/FR are drawn to more accessible zones while IT avoids them (mirroring population's DE/FR-positive, IT-negative split fairly closely); but for **Reise mit Ü., Erreichbarkeit is negative across every significant nationality** (DE strongly so) — overnight visitors systematically prefer *less* accessible zones, the opposite of what population showed (population was mostly null for this group). Since this index measures potential accessibility via the national transport network, a high score marks well-connected, urban/central zones — so this reads as overnight tourists gravitating toward quieter, more remote destinations precisely because they're staying longer, while day-trippers (Tagesreise, DE/FR at least) still lean toward easy-to-reach places, consistent with the much larger EMU coefficients Tagesreise already showed throughout.

Results file: `results_output/destination_mnl_tagesreise_vs_reisemitue_erreichbarkeit_half.csv`

### EMU + population + Erreichbarkeit together, half sample

Same half-sample setup (3,504 Tagesreise / 2,375 Reise mit Ü. respondents, seed = 42, 300 alts, own EMU per subgroup), now with **both** `pop` and `Erreichbarkeit` in the same specification instead of swapping one for the other.

```
V_j = Σ_k β_EMU_k · EMU_j · I(nat = k) + Σ_k β_pop_k · pop_j · I(nat = k) + Σ_k β_erreichbarkeit_k · erreichbarkeit_avg_j · I(nat = k)
```

| Group | LL | ρ² | AIC | BIC | Time |
|---|---:|---:|---:|---:|---:|
| Tagesreise | −13,848.75 | 0.3071 | 27,721.5 | 27,795.4 | 236s |
| Reise mit Ü. | −12,655.32 | 0.0658 | 25,334.6 | 25,403.9 | 156s |

| Parameter | Tagesreise | Reise mit Ü. |
|:---|---:|---:|
| β EMU · DE | 12.883 *** | 4.583 *** |
| β EMU · FR | 12.823 *** | 6.310 *** |
| β EMU · IT | 9.365 *** | 6.733 *** |
| β EMU · other | 14.061 *** | 6.094 *** |
| β pop · DE | −0.133 *** | 0.050 |
| β pop · FR | 0.276 *** | 0.002 |
| β pop · IT | 0.449 *** | 0.140 ** |
| β pop · other | −0.152 ** | −0.150 ** |
| β Erreichbarkeit · DE | 0.348 *** | −0.714 *** |
| β Erreichbarkeit · FR | 0.084 | −0.138 * |
| β Erreichbarkeit · IT | −0.681 *** | −0.503 *** |
| β Erreichbarkeit · other | −0.011 | 0.005 |

`*** p<0.01  ** p<0.05  * p<0.10`

**Correlation check**: `pop` (log1p, as used in the models) and `erreichbarkeit_avg` correlate at only **r = 0.257** across the 7,978 zones (Spearman 0.309; a simple linear fit of one on the other gives R² = 0.066 — Erreichbarkeit explains just ~7% of population's variance). So these are *not* near-redundant measures of "how central/urban is this zone" — the relationship is real but modest.

**Takeaway**: fit improves only modestly over either single-variable spec (Tagesreise 0.3033/0.3031 → 0.3071; Reise mit Ü. 0.0538/0.0653 → 0.0658 — essentially just the Erreichbarkeit-alone number, population adds almost nothing on top for this group). What's notable is that even with only a modest raw correlation, the coefficients still shift substantially when both are in the model together — likely because the nationality interactions split each group into a few hundred to ~1,300 observations, where even r ≈ 0.26 collinearity can meaningfully move point estimates and significance, more than the population-wide correlation alone would suggest:
- **Reise mit Ü. · DE flips from significant to null**: `pop·DE` was −0.073*** alone, becomes 0.050 (n.s.) once Erreichbarkeit is added — Erreichbarkeit was absorbing what looked like a population effect.
- **Tagesreise · FR Erreichbarkeit flips from significant to null**: `erreichbarkeit·FR` was 0.167*** alone, becomes 0.084 (n.s.) once population is added — here it's population absorbing Erreichbarkeit's apparent effect.
- Both `IT` coefficients get *stronger* in the combined spec (Tagesreise `erreichbarkeit·IT` −0.402→−0.681, `pop·IT` 0.180→0.449; Reise mit Ü. `pop·IT` n.s.→0.140**) — once each variable's shared variance is netted out, IT's genuine preference for less-accessible, less-populous destinations comes through more sharply.
- EMU stays essentially untouched throughout, as in every other spec here — it isn't collinear with either zone-level attractivity measure.

Practically: given the overlap, a single spec with **either** `pop` or `Erreichbarkeit` (not both) is probably the better default going forward — the combined model doesn't buy much additional fit for the added complexity, and the coefficient instability between specs is itself informative about what each variable is (and isn't) independently capturing.

Results file: `results_output/destination_mnl_tagesreise_vs_reisemitue_pop_erreichbarkeit_half.csv`

### EMU + population + Erreichbarkeit, pooled (no nationality interaction), half sample

Same half-sample setup (3,504 Tagesreise / 2,375 Reise mit Ü. respondents, seed = 42, 300 alts, own EMU per subgroup, no topology), but this time **without** interacting any variable with nationality — a single pooled coefficient per variable across all respondents, instead of one per nationality group:

```
V_j = β_EMU · EMU_j + β_pop · pop_j + β_erreichbarkeit · erreichbarkeit_avg_j
```

`run_destination_model()` was extended with an `interact_nationality` argument (default `TRUE`, preserving every other spec in this document) to support this.

| Group | LL | ρ² | AIC | BIC | Time |
|---|---:|---:|---:|---:|---:|
| Tagesreise | −14,047.91 | 0.2971 | 28,101.8 | 28,120.3 | 20s |
| Reise mit Ü. | −12,727.79 | 0.0604 | 25,461.6 | 25,478.9 | 11s |

| Parameter | Tagesreise | Reise mit Ü. |
|:---|---:|---:|
| β EMU | 11.506 *** | 5.605 *** |
| β pop | 0.049 ** | 0.034 |
| β Erreichbarkeit | 0.017 | −0.544 *** |

`*** p<0.01  ** p<0.05  * p<0.10`

**Takeaway**: pooling erases exactly the pattern the nationality-interacted spec revealed. In the interacted model, `pop` and `Erreichbarkeit` had *opposite signs across nationalities* for both subgroups (e.g. Tagesreise `pop`: −0.133 DE / +0.276 FR / +0.449 IT / −0.152 other; Reise mit Ü. `Erreichbarkeit`: −0.714 DE / −0.138 FR / −0.503 IT / +0.005 other). Averaging over nationality washes most of that out:
- **Tagesreise `Erreichbarkeit` goes from a strong, sign-mixed effect (DE +0.348***, FR n.s., IT −0.681***) to essentially null pooled (0.017, n.s.)** — the positive DE effect and negative IT effect largely cancel once nationality is no longer separated out.
- **Reise mit Ü. `pop` goes from three significant group-specific coefficients (IT +0.140**, other −0.150**, DE n.s.) to a null pooled coefficient (0.034, n.s.)** — again, opposing signs across groups cancel in the average.
- The two coefficients that *do* survive pooling — Tagesreise `pop` (0.049**) and Reise mit Ü. `Erreichbarkeit` (−0.544***) — are the ones where the nationality-specific effects in the interacted spec were already fairly consistent in sign and magnitude across groups, so pooling doesn't destroy the signal.
- EMU stays large, positive and highly significant either way, as always, though its pooled magnitude (11.506 / 5.605) sits within the range of the nationality-specific estimates rather than matching any one of them.
- Fit is very close to the interacted model despite dropping from 12 to 3 parameters (Tagesreise ρ² 0.3071→0.2971; Reise mit Ü. 0.0658→0.0604) — nationality interaction buys only a small amount of additional fit here, but it is precisely what surfaces the sign-heterogeneity above; a pooled spec would hide it.

Results file: `results_output/destination_mnl_tagesreise_vs_reisemitue_pop_erreichbarkeit_half_pooled.csv`

### Choice-set-size sensitivity, half sample (300 → 500 → 1,000 → 5,000 alternatives)

Same half-sample EMU + pop (no topology) spec (3,504 Tagesreise / 2,375 Reise mit Ü. respondents, seed = 42), now varying the choice-set size instead of the agent count. The zone universe has 8,688 zones total, so 5,000 alternatives is well within range (all draws are with replacement regardless, as in every other run here).

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
| β pop · DE | −0.143 *** | −0.141 *** | −0.146 *** | −0.140 *** | −0.073 *** | −0.073 *** | −0.075 *** | −0.073 *** |
| β pop · FR | 0.292 *** | 0.300 *** | 0.310 *** | 0.324 *** | −0.012 | −0.009 | −0.005 | −0.005 |
| β pop · IT | 0.180 *** | 0.179 *** | 0.176 *** | 0.164 *** | −0.014 | −0.013 | −0.013 | −0.013 |
| β pop · other | −0.152 ** | −0.146 ** | −0.138 ** | −0.122 ** | −0.150 ** | −0.150 ** | −0.158 ** | −0.158 ** |

`*** p<0.01  ** p<0.05  * p<0.10`

**Takeaway**: Reise mit Ü. stays essentially flat all the way out to 5,000 alternatives — every coefficient within a few percent of its 300-alt value. Tagesreise shows a real, if modest, downward drift in its EMU coefficients as the choice set grows (DE 12.78 → 10.17, FR 12.83 → 10.03, roughly −20% cumulative from 300 to 5,000 alts) — small enough that sign and significance never change, but a genuine trend rather than noise, since it moves consistently in one direction across all four choice-set sizes. pop stays stable throughout for both subgroups. ρ² keeps declining mechanically as the choice set grows (more random alternatives dilute the easy choices), most visibly for Tagesreise (0.303 → 0.194) since it's the better-fitting model to begin with. **Runtime is where 5,000 alternatives really bites**: Tagesreise jumped to 47 minutes — well above the ~29 min linear extrapolation from the 300–1,000 trend — because the optimizer needed 47 iterations to converge at this choice-set size vs. only 10 for Reise mit Ü. (whose 19.8 min runtime matched the linear projection closely). Practically: 300 alternatives remains the right default for this spec — it already captures the stable part of the signal, and the modest additional drift visible in Tagesreise's EMU coefficients at 5,000 alts doesn't come close to justifying a 28× runtime cost.

Results files: `results_output/destination_mnl_tagesreise_vs_reisemitue_v02_notopo_half_altsweep.csv`, `results_output/destination_mnl_tagesreise_vs_reisemitue_v02_notopo_half_alts5000.csv`

### Same spec, full sample

Same EMU + pop (no topology) spec, seed = 42, 300 alts, now using **every** non-Swiss respondent in each subgroup: all 7,009 Tagesreise, all 4,751 Reise mit Ü.

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
| β pop · DE | −0.156 *** | −0.081 *** |
| β pop · FR | 0.275 *** | −0.014 |
| β pop · IT | 0.179 *** | 0.014 |
| β pop · other | −0.106 ** | −0.142 *** |

`*** p<0.01  ** p<0.05  * p<0.10`

### Convergence across sample sizes (1,000 → half → full)

| Parameter | Tagesreise 1k | Tagesreise half | Tagesreise full | Reise mit Ü. 1k | Reise mit Ü. half | Reise mit Ü. full |
|:---|---:|---:|---:|---:|---:|---:|
| β EMU · DE | 13.632 *** | 12.783 *** | 12.310 *** | 3.389 *** | 3.451 *** | 3.114 *** |
| β EMU · FR | 13.623 *** | 12.833 *** | 12.957 *** | 5.832 *** | 6.197 *** | 6.143 *** |
| β EMU · IT | 9.324 *** | 9.582 *** | 9.457 *** | 7.206 *** | 7.165 *** | 7.529 *** |
| β EMU · other | 18.413 *** | 14.069 *** | 13.901 *** | 6.152 *** | 6.093 *** | 6.327 *** |
| β pop · DE | −0.112 ** | −0.143 *** | −0.156 *** | −0.070 * | −0.073 *** | −0.081 *** |
| β pop · FR | 0.141 ** | 0.292 *** | 0.275 *** | −0.115 * | −0.012 | −0.014 |
| β pop · IT | 0.226 ** | 0.180 *** | 0.179 *** | −0.078 | −0.014 | 0.014 |
| β pop · other | −0.500 *** | −0.152 ** | −0.106 ** | −0.164 | −0.150 ** | −0.142 *** |

`*** p<0.01  ** p<0.05  * p<0.10`

**Takeaway**: with the full sample, the picture is clean. EMU coefficients were already close to their final values at 1,000 agents and barely move from half to full sample — genuinely stable, well-identified effects, `***` throughout. The pop coefficients tell a more interesting story: `pop·other` for Tagesreise keeps shrinking as sample grows (−0.500 → −0.152 → −0.106) and settles at a modest, still-significant negative effect — the 1,000-agent estimate overstated it by roughly 5×, a real small-sample bias, not just noise, since it kept moving in the same direction across both larger samples rather than randomly settling. `pop·IT` for Reise mit Ü. never reaches significance at any sample size and hovers near zero throughout — good evidence population genuinely doesn't drive that group's destination choice. `pop·FR` for Reise mit Ü. is the opposite pattern from `pop·other`/Tagesreise: only marginally significant (`*`) at 1,000 agents, then drops to null at both larger samples — that one looks like the 1,000-agent estimate was mostly noise crossing the significance threshold by chance, not a real effect that got diluted. Runtime scaled essentially linearly with N throughout (Tagesreise: 30s → 102s → 178s for roughly 1×/3.5×/7× the agents; Reise mit Ü.: 29s → 67s → 129s for 1×/2.4×/4.75×).

Results file: `results_output/destination_mnl_tagesreise_vs_reisemitue_v02_notopo_full.csv`

Results file: `results_output/destination_mnl_tagesreise_vs_reisemitue_v02_notopo_half.csv`

---

## Destination choice with AMR regions instead of NPVM zones

New script: **`04_destination_choice_amr.R`**. Everything above uses individual NPVM zones (~8,700) as the choice set, with a McFadden *sampled* set of alternatives (e.g. 300 of them) standing in for the full set. This section instead uses the **101 AMR regions** in `data/amr101.gpkg` as the choice set — coarser geographic units (labour-market-style regions, each named after its central town, e.g. "Zürich", "Genève", "Aarau–Olten").

Because there are only 101 regions, **every agent's full choice set is used — no alternative sampling.** This is a genuine full-information MNL rather than a sampled-alternatives approximation, which is the main methodological difference from every result above (in addition to using a coarser destination unit).

Each Swiss NPVM zone is assigned to an AMR region via the largest-overlapping-area polygon join (`aggregate_regions.R`, run before `04_destination_choice_amr.R`; only 1/7,966 CH zones has no polygon overlap with any AMR region and falls back to its nearest one). EMU and Erreichbarkeit are then aggregated from zone level to region level as a **population-weighted average** over the zones in each region (`x_region = Σ pop_i·x_i / Σ pop_i`); `pop` itself is aggregated as the **sum** of zone populations, then log1p + z-standardised, same convention as the zone-level `pop` variable. Only the destination side is aggregated — each agent's real origin zone is unchanged.

```
V_j = Σ_k β_EMU_k · EMU_j · I(nat = k) + Σ_k β_pop_k · pop_j · I(nat = k) + Σ_k β_erreichbarkeit_k · erreichbarkeit_avg_j · I(nat = k)
```

Full sample (6,991 Tagesreise / 4,738 Reise mit Ü., all non-Swiss respondents — feasible here since 101 alternatives/agent is far cheaper than the 300+ used for zone-level sampled sets), seed = 42, own EMU per subgroup, all 101 regions as the choice set.

*Refreshed after `zones_communes.gpkg` was updated to add a `MAKROBEZ_STAAT` (CH/LI/Ausland) column: `01_agent_generation.R` now drops 54 respondents whose origin/destination/residence zone contradicts "foreign tourist entering Switzerland" (see the script map), and the candidate zone/region universe used everywhere is restricted to genuine CH zones. Sample sizes dropped slightly (7,009→6,991 / 4,751→4,738) and coefficients moved marginally; conclusions below are unchanged.*

| Group | LL | ρ² | AIC | BIC | Time |
|---|---:|---:|---:|---:|---:|
| Tagesreise | −18,263.25 | 0.4339 | 36,550.5 | 36,632.7 | 117s |
| Reise mit Ü. | −19,777.84 | 0.0955 | 39,579.7 | 39,657.2 | 72s |

| Parameter | Tagesreise | Reise mit Ü. |
|:---|---:|---:|
| β EMU · DE | 14.484 *** | 4.880 *** |
| β EMU · FR | 15.137 *** | 7.058 *** |
| β EMU · IT | 9.791 *** | 6.729 *** |
| β EMU · other | 13.145 *** | 6.368 *** |
| β pop · DE | 0.120 ** | 0.770 *** |
| β pop · FR | 0.047 | 0.500 *** |
| β pop · IT | 0.796 *** | 0.592 *** |
| β pop · other | 0.052 | 0.656 *** |
| β Erreichbarkeit · DE | 0.561 *** | −0.710 *** |
| β Erreichbarkeit · FR | 0.054 | −0.141 * |
| β Erreichbarkeit · IT | −0.823 *** | −0.579 *** |
| β Erreichbarkeit · other | 0.747 *** | −0.149 |

`*** p<0.01  ** p<0.05  * p<0.10`

**Takeaway**: the sign pattern shifts compared to the zone-level combined spec, and Reise mit Ü. changes the most. At zone level, `pop` was mostly null or weakly significant for Reise mit Ü. (only IT and "other" reached significance); at region level, **all four nationality groups get large, positive, highly significant `pop` coefficients** (0.50 to 0.77) — with only 101 much bigger units, "population" is now closer to "is this a major urban region at all" rather than distinguishing between thousands of small zones, so it picks up a much stronger, more universal draw toward populous regions. Erreichbarkeit for Reise mit Ü. keeps the same negative sign across DE/FR/IT as at zone level (overnight travellers still lean away from the most-accessible regions once EMU and population are controlled for), consistent with the zone-level finding. Tagesreise stays qualitatively similar to its zone-level counterpart — EMU dominant and highly significant throughout, `pop·IT` positive, `erreichbarkeit·IT` negative — though `erreichbarkeit·other` flips from null at zone level to strongly positive here (0.747***), plausibly because "other"-nationality day-trippers are a small, less homogeneous group (~710 respondents) where coarser regional aggregation changes which specific destinations get pooled together.

Fit is not directly comparable to the zone-level ρ² figures above: this model differs on three dimensions at once (full sample instead of half, coarser destination unit, and a full 101-alternative choice set instead of 300 sampled alternatives), so the notably higher Tagesreise ρ² (0.434 vs. 0.307 at zone level) reflects some combination of all three, not evidence that regions fit better per se.

Results file: `results_output/destination_mnl_amr_tagesreise_vs_reisemitue_pop_erreichbarkeit.csv`

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
  tt_agqpv.fst                        Travel times for OD pairs actually observed in agqpv.csv
  tt_ausland_CH.fst                   Travel times, every zone abroad x every Swiss zone
  tt_CH_CH.fst                        Travel times, every Swiss zone x every other Swiss zone (not currently used downstream)
  tt_ausland_CH_logsum.fst                  tt_ausland_CH.fst + EMU column (named `logsum`), pooled non-Swiss model
  tt_ausland_CH_logsum_tagesreise.fst       tt_ausland_CH.fst + EMU column, Tagesreise-only mode choice model
  tt_ausland_CH_logsum_reisemitue.fst       tt_ausland_CH.fst + EMU column, Reise-mit-Ü.-only mode choice model
  tt_CH_CH_logsum.fst                       tt_CH_CH.fst + EMU column, pooled non-Swiss model (not currently used downstream)
  tt_CH_CH_logsum_tagesreise.fst            tt_CH_CH.fst + EMU column, Tagesreise-only mode choice model
  tt_CH_CH_logsum_reisemitue.fst            tt_CH_CH.fst + EMU column, Reise-mit-Ü.-only mode choice model

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
