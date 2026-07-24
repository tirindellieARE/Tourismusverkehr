# Domestic (CH → CH) Mode Choice — Results Summary

Companion to `README_results_trajets_Ausland_CH.md`, which covers foreign-entry
(Ausland → CH) trips. This file covers **domestic** trips — Swiss zone to
Swiss zone — estimated separately since the data source, alternatives, and
population are all different from the tourist-destination side of the
project.

---

## Data

| Element | Description |
|---|---|
| Survey | `data/output/mc15.csv` — 8,730 revealed-preference domestic trips (origin_zone, dest_zone, mode) |
| Alternatives | car, PT, foot, bike |
| Zone system | NPVM 2017 traffic zones, Switzerland only (`MAKROBEZ_STAAT == "CH"`, N = 7,966) |
| Travel time | `data/output/tt_CH_CH.fst` (built by `02_build_tt_lookups.R`): `tt_miv` (car, from TTC.omx), `tt_oev` (PT, from RITA+EGT+ACT.omx), `tt_fgv` (foot, from TT_FGV.omx), `tt_velo` (bike, from TT0_VELO.omx) |
| Cost | `cost_car` (from MIV_KOS.omx), `cost_pt` (from OEV_KOS.omx) — no cost data available for foot/bike |
| Script | `03_estimate_logsum_CH.R` |

`mc15`'s raw "walk" mode is relabelled "foot" to match `tt_fgv`; the small "other" category (183 / 8,730 rows) is dropped, since it isn't one of the four modelled alternatives. Rows with a missing travel time/cost for any of the four modes are also dropped (4 rows, all missing PT time). Estimation sample: **8,543 trips** (30.2% car, 0.9% pt, 65.5% foot, 3.4% bike).

Reference alternative throughout: **foot** (`asc_foot` fixed at 0 by not including it as a parameter) — it's the most common mode (65.5%), making it a natural, well-populated baseline.

No pooled/Tagesreise/Reise-mit-Ü. split here (unlike the Ausland→CH side) — domestic mode choice is estimated once, pooled, since `mc15` has no equivalent day-trip/overnight distinction.

---

## Model 1 — time only (no cost)

```
V_car  = asc_car             + b_tt_car  · tt_miv
V_pt   = asc_pt              + b_tt_pt   · tt_oev
V_foot =                        b_tt_foot · tt_fgv      (reference)
V_bike = asc_bike            + b_tt_bike · tt_velo
```

| Parameter | Estimate | s.e. | t-stat |
|:---|---:|---:|---:|
| asc_car | −2.330 *** | 0.0496 | −46.94 |
| asc_pt | −5.504 *** | 0.1648 | −33.40 |
| asc_bike | −3.376 *** | 0.0706 | −47.82 |
| b_tt_car | +0.106 *** | 0.0119 | 8.93 |
| b_tt_pt | +0.038 *** | 0.0058 | 6.61 |
| b_tt_foot | −0.051 *** | 0.0032 | −15.73 |
| b_tt_bike | −0.012 * | 0.0065 | −1.78 |

`*** p<0.01  ** p<0.05  * p<0.10`

LL(final) = −4,517.46 · LL(0) = −11,843.11 · ρ² (vs. equal shares) = 0.6186 · ρ² (vs. observed shares) = 0.3356 · AIC = 9,048.9 · BIC = 9,098.3 · N = 8,543

**Takeaway**: strong overall fit (ρ² = 0.62), and `b_tt_foot` has the expected negative sign (slower walk routes are less attractive) and is highly significant. But **`b_tt_car` and `b_tt_pt` are positive and significant** — the opposite of what a travel-time coefficient should be. This is a **distance confound**, not a data error: 65% of trips in the sample are on foot, and foot trips are necessarily short, so car/PT get chosen disproportionately for *longer* trips — not because people prefer more travel time, but because the destination is far and foot/bike aren't feasible. Since car/PT's own travel time is itself large for those long trips, the model picks up "long trip → chose car → car time is large" as a positive association, even though the genuine behavioral effect (faster car route → more attractive) points the other way. `b_tt_bike` is negative as expected but only marginally significant (`*`, N = 291 bike trips is a small subsample).

---

## Model 2 — time + cost

```
V_car  = asc_car  + b_tt_car  · tt_miv  + b_cost_car · cost_car
V_pt   = asc_pt   + b_tt_pt   · tt_oev  + b_cost_pt  · cost_pt
V_foot =             b_tt_foot · tt_fgv                            (reference)
V_bike = asc_bike + b_tt_bike · tt_velo
```

(no cost data for foot/bike, so those two alternatives stay time-only)

| Parameter | Estimate | s.e. | t-stat |
|:---|---:|---:|---:|
| asc_car | −2.481 *** | 0.0540 | −45.90 |
| asc_pt | −4.479 *** | 0.2502 | −17.90 |
| asc_bike | −3.320 *** | 0.0688 | −48.26 |
| b_tt_car | +0.169 *** | 0.0131 | 12.97 |
| b_tt_pt | +0.029 *** | 0.0099 | 2.97 |
| b_tt_foot | −0.052 *** | 0.0031 | −17.11 |
| b_tt_bike | −0.030 *** | 0.0062 | −4.84 |
| b_cost_car | −0.559 *** | 0.0634 | −8.82 |
| b_cost_pt | −0.335 *** | 0.0917 | −3.65 |

`*** p<0.01  ** p<0.05  * p<0.10`

LL(final) = −4,466.87 · LL(0) = −11,843.11 · ρ² (vs. equal shares) = 0.6228 · ρ² (vs. observed shares) = 0.3431 · AIC = 8,951.8 · BIC = 9,015.2 · N = 8,543

**Takeaway**: both new cost coefficients have the expected negative sign and are significant — costlier trips are less attractive, as they should be — and fit improves modestly over Model 1 (ρ² 0.6186 → 0.6228, AIC 9,048.9 → 8,951.8). Adding cost also sharpens `b_tt_bike` from marginally significant (`*`) to clearly significant (`***`), since separating out the cost effect removes some noise from the other coefficients. However, **the distance confound on `b_tt_car`/`b_tt_pt` is not fixed by adding cost — if anything `b_tt_car` moves further positive** (0.106 → 0.169). This is expected: cost is itself correlated with distance in much the same way travel time is (a longer car trip costs more fuel/fare, just as it takes more time), so it doesn't separately identify the "faster is better, holding trip length fixed" effect that the positive sign is masking. Addressing that will need distance segmentation (splitting or interacting by trip-distance band), not additional cost/time variables.

---

## Model 3 — time + cost + distance, pooled (not segmented)

```
V_car  = asc_car  + b_tt_car  · tt_miv  + b_cost_car · cost_car + b_dist_car · dist_miv
V_pt   = asc_pt   + b_tt_pt   · tt_oev  + b_cost_pt  · cost_pt
V_foot =             b_tt_foot · tt_fgv                                        (reference)
V_bike = asc_bike + b_tt_bike · tt_velo
```

`dist_miv` (car network distance, from `DIS_MIV.omx`) added to car's utility only, as a control for trip length. `b_dist_pt` was tried too (using `dist_miv` as a proxy at first, then the real PT route distance once `DIS_OEV.omx` became available) but was never significant, so it's dropped here.

Two other distance attempts along the way, both discarded:
- Adding the bike-route distance (`dist`, from `DIS_VELO.omx`) to **all four** alternatives' utilities produced a **singular, non-converging model** — coefficients exploded toward ±10⁵. `dist` is essentially a constant-speed transform of `tt_fgv`/`tt_velo` (r ≈ 0.986 on a 250k-pair sample), so including both in the same alternative's utility is near-exact collinearity.
- `dist_miv` correlates with `tt_miv` at r ≈ 0.96 and with `tt_oev` at r ≈ 0.90 — high, but not extreme enough to break convergence when used for car alone.

| Parameter | Estimate | s.e. | t-stat |
|:---|---:|---:|---:|
| asc_car | −2.482 *** | 0.0542 | −45.83 |
| asc_pt | −4.606 *** | 0.2541 | −18.13 |
| asc_bike | −3.320 *** | 0.0688 | −48.29 |
| b_tt_car | +0.169 *** | 0.0131 | 12.92 |
| b_tt_pt | +0.030 *** | 0.0099 | 3.01 |
| b_tt_foot | −0.051 *** | 0.0031 | −16.32 |
| b_tt_bike | −0.026 *** | 0.0064 | −4.01 |
| b_cost_car | −1.783 *** | 0.3910 | −4.56 |
| b_cost_pt | −0.277 *** | 0.0939 | −2.95 |
| b_dist_car | +0.389 *** | 0.1212 | 3.21 |

`*** p<0.01  ** p<0.05  * p<0.10`

LL(final) = −4,461.74 · ρ² (vs. equal shares) = 0.6233 · AIC = 8,943.5 · BIC = 9,014.0 · N = 8,543

**Takeaway**: adding a distance control to a *pooled* model does not fix anything — `b_tt_car`/`b_tt_pt` are essentially unchanged from Model 2, and **`b_dist_car` itself comes out positive and significant**, the same "wrong" sign pattern showing up on the new variable. That's the tell that this isn't a simple omitted-variable problem: within a pooled sample, *any* variable correlated with trip length will pick up "longer trips lean away from foot" rather than a genuine distance/time effect, because foot dominates short trips almost by construction. Fixing this needs the sample itself split by distance, not another linear control — see the segmentation below.

---

## Distance-band segmentation — the fix (superseded, see Model 4)

Trips are split at **`dist_miv` (car network distance) = 5km** into "short" (< 5km) and "long" (≥ 5km) bands, and a **fully separate model** (own ASCs, own time/cost/distance coefficients) is estimated per band. A median split was tried first and rejected — the sample is heavily right-skewed by short foot trips, so a median split (at just 0.22km) leaves only 7 PT observations in the short band; the round 5km cutoff gives a much better-balanced 43 PT-short / 33 PT-long.

Car's utility uses `dist_miv` (car network distance); PT's uses `dist_pt`, the actual PT route distance from `DIS_OEV.omx` (r ≈ 0.947 with `tt_oev` — high, but PT's own distance, not a proxy).

```
V_car  = asc_car  + b_tt_car  · tt_miv  + b_cost_car · cost_car + b_dist_car · dist_miv
V_pt   = asc_pt   + b_tt_pt   · tt_oev  + b_cost_pt  · cost_pt  + b_dist_pt  · dist_pt
V_foot =             b_tt_foot · tt_fgv                                        (reference)
V_bike = asc_bike + b_tt_bike · tt_velo
```

### Short band (< 5km) — 7,242 trips (19.2% car, 0.6% pt, 76.6% foot, 3.7% bike)

| Parameter | Estimate | s.e. | t-stat |
|:---|---:|---:|---:|
| asc_car | −2.716 *** | 0.0623 | −43.56 |
| asc_pt | −5.929 *** | 0.2628 | −22.56 |
| asc_bike | −3.367 *** | 0.0758 | −44.41 |
| b_tt_car | +0.179 *** | 0.0199 | 9.02 |
| b_tt_pt | +0.013 | 0.0359 | 0.37 |
| b_tt_foot | −0.125 *** | 0.0110 | −11.41 |
| b_tt_bike | −0.188 *** | 0.0319 | −5.89 |
| b_cost_car | −5.145 *** | 1.0915 | −4.71 |
| b_cost_pt | ~0 | 0.0014 | 0.005 |
| b_dist_car | +0.793 ** | 0.3217 | 2.47 |
| b_dist_pt | −0.272 | 0.1781 | −1.53 |

`*** p<0.01  ** p<0.05  * p<0.10`

LL(final) = −3,827.45 · ρ² (vs. equal shares) = 0.6188 · ρ² (vs. observed shares) = 0.2153 · AIC = 7,676.9 · BIC = 7,752.7 · N = 7,242

**`b_tt_car` is still positive here.** Even under 5km there's apparently enough length heterogeneity (a 0.2km trip and a 4.9km trip are very different in mode feasibility) for the same confound to persist at a smaller scale. `b_tt_pt`/`b_cost_pt`/`b_dist_pt` are all non-significant — unsurprising with only 43 PT trips in this band.

### Long band (≥ 5km) — 1,301 trips (91.8% car, 2.5% pt, 3.9% foot, 1.8% bike)

First estimated with the full car+PT cost/distance specification:

| Parameter | Estimate | s.e. | t-stat |
|:---|---:|---:|---:|
| asc_car | +2.427 *** | 0.3530 | 6.87 |
| asc_pt | −0.728 | 0.5606 | −1.30 |
| asc_bike | −1.478 *** | 0.4718 | −3.13 |
| b_tt_car | −0.078 *** | 0.0199 | −3.91 |
| b_tt_pt | −0.043 ** | 0.0173 | −2.48 |
| b_tt_foot | −0.018 *** | 0.0039 | −4.61 |
| b_tt_bike | −0.034 *** | 0.0102 | −3.38 |
| b_cost_car | −0.973 *** | 0.3451 | −2.82 |
| b_cost_pt | −0.143 | 0.1091 | −1.31 |
| b_dist_car | +0.276 *** | 0.1070 | 2.58 |
| b_dist_pt | ~0 | 0.0424 | −0.01 |

LL(final) = −438.91 · ρ² (vs. equal shares) = 0.7566 · ρ² (vs. observed shares) = 0.0889 · AIC = 899.8 · BIC = 956.7 · N = 1,301

**`b_tt_car` and `b_tt_pt` are both correctly negative here** — segmentation works, at least in the long band. This is the headline result: the "wrong" sign was a between-band artifact of mixing short foot-dominated trips with longer car-dominated ones, not a genuine feature of car/PT time-sensitivity.

**Refined (current)**: `b_cost_pt`/`b_dist_pt` were never significant (t ≈ −1.3 / −0.01), so they're held fixed at 0 (`apollo_fixed`) rather than estimated (PT becomes time-only in practice), and the short band is reused unchanged:

| Parameter | Estimate | s.e. | t-stat |
|:---|---:|---:|---:|
| asc_car | +2.326 *** | 0.3443 | 6.76 |
| asc_pt | −0.623 | 0.4876 | −1.28 |
| asc_bike | −1.473 *** | 0.4711 | −3.13 |
| b_tt_car | −0.066 *** | 0.0176 | −3.75 |
| b_tt_pt | −0.059 *** | 0.0135 | −4.40 |
| b_tt_foot | −0.016 *** | 0.0029 | −5.64 |
| b_tt_bike | −0.029 *** | 0.0069 | −4.25 |
| b_cost_car | −1.018 *** | 0.3285 | −3.10 |
| b_dist_car | +0.299 *** | 0.1014 | 2.94 |

`*** p<0.01  ** p<0.05  * p<0.10`

LL(final) = −440.21 · ρ² (vs. equal shares) = 0.7559 · ρ² (vs. observed shares) = 0.0862 · AIC = 898.4 · BIC = 945.0 · N = 1,301 · 9 estimated params (down from 11)

**Takeaway**: dropping the two non-significant PT terms barely changes the fit (ρ² 0.7566 → 0.7559, AIC 899.8 → 898.4 with 2 fewer parameters) and **`b_tt_pt` gets *more* significant** (t: −2.48 → −4.40) once the noisy cost/distance terms are no longer competing for the same signal. Both `b_tt_car` and `b_tt_pt` remain correctly negative.

**Numerical note (investigated and resolved)**: the first version of this refined model was coded with a runtime `if` branch inside `apollo_probabilities()` to drop the PT cost/distance terms, which silently broke Apollo's analytical-gradient support (it can't parse a conditional utility expression) and fell back to numerical (finite-difference) derivatives -- that run converged to a **saddle point** (positive Hessian eigenvalues) with `NaN` non-robust standard errors for `b_cost_car`/`b_dist_car`. Fixed by holding `b_cost_pt`/`b_dist_pt` at 0 via `apollo_fixed` instead, keeping the utility expression's shape static so analytical differentiation works again. Re-estimating gave a proper negative-definite Hessian ("Maximum found") with essentially identical point estimates and LL to the saddle-point run -- confirming that run had already found the right answer, just with unreliable precision estimates.

**Net effect on the logsum**: 99.5% of `tt_CH_CH`'s 63.5M rows fall in the long band (most zone *pairs* are far apart, even though most actual *trips* are short) — so the vast majority of the saved logsum now rests on a correctly-signed model; only the 0.5% short-distance pairs still carry the residual confound.

---

## Model 4a — pooled, topology × travel-time interaction, origin-zone topology

Segmentation by distance band is abandoned here in favour of a single pooled model again (matching Model 3's base spec: car = time+cost+distance, PT = time+cost, foot/bike = time-only), with each mode's travel-time coefficient allowed to shift by the **urban/rural topology of the trip's origin zone** (`STALAN2020`, the same 1 = urban / 2 = hilly / 3 = mountain coding used in the Ausland→CH destination models). Origin zone (not destination) is used here, since for a mode-choice decision it's the built environment where the trip *starts* — car ownership, PT density, walkability — that should shape sensitivity to travel time, unlike the destination-choice context where the candidate zone's topology matters.

```
V_car  = asc_car  + (b_tt_car  + b_tt_car_topo2  · I(topo=2) + b_tt_car_topo3  · I(topo=3)) · tt_miv  + b_cost_car · cost_car + b_dist_car · dist_miv
V_pt   = asc_pt   + (b_tt_pt   + b_tt_pt_topo2   · I(topo=2) + b_tt_pt_topo3   · I(topo=3)) · tt_oev  + b_cost_pt  · cost_pt
V_foot =             (b_tt_foot + b_tt_foot_topo2 · I(topo=2) + b_tt_foot_topo3 · I(topo=3)) · tt_fgv                    (reference)
V_bike = asc_bike + (b_tt_bike + b_tt_bike_topo2 · I(topo=2) + b_tt_bike_topo3 · I(topo=3)) · tt_velo
```

Topology (1 = urban) is the reference level; `topo2`/`topo3` coefficients are shifts relative to it. 760/8,719 zones have no `STALAN2020` value, so trips starting there are dropped, leaving **8,540 trips** (30.2% car, 0.9% pt, 65.5% foot, 3.4% bike; topology split 6,022 urban / 1,524 hilly / 994 mountain).

| Parameter | Estimate | Rob. s.e. | Rob. t-stat |
|:---|---:|---:|---:|
| asc_car | −2.476 *** | 0.0583 | −42.46 |
| asc_pt | −4.639 *** | 0.3683 | −12.60 |
| asc_bike | −3.324 *** | 0.0753 | −44.15 |
| b_tt_car | +0.132 *** | 0.0230 | 5.77 |
| b_tt_pt | +0.020 * | 0.0128 | 1.55 |
| b_tt_foot | −0.063 *** | 0.0078 | −8.01 |
| b_tt_bike | −0.040 *** | 0.0114 | −3.49 |
| b_tt_car_topo2 | +0.116 *** | 0.0307 | 3.77 |
| b_tt_car_topo3 | +0.174 *** | 0.0488 | 3.57 |
| b_tt_pt_topo2 | +0.042 *** | 0.0149 | 2.83 |
| b_tt_pt_topo3 | +0.043 *** | 0.0156 | 2.73 |
| b_tt_foot_topo2 | +0.027 ** | 0.0120 | 2.24 |
| b_tt_foot_topo3 | +0.052 *** | 0.0116 | 4.47 |
| b_tt_bike_topo2 | +0.039 *** | 0.0118 | 3.29 |
| b_tt_bike_topo3 | +0.054 *** | 0.0142 | 3.80 |
| b_cost_car | −1.872 *** | 0.4842 | −3.87 |
| b_cost_pt | −0.277 ** | 0.1502 | −1.84 |
| b_dist_car | +0.427 *** | 0.1517 | 2.81 |

`*** p<0.01  ** p<0.05  * p<0.10` (robust s.e./t-stats)

LL(final) = −4,432.22 · LL(0) = −11,838.95 · ρ² (vs. equal shares) = 0.6256 · Adj. ρ² = 0.6241 · ρ² (vs. observed shares) = 0.348 · AIC = 8,900.5 · BIC = 9,027.4 · N = 8,540 · 18 estimated params

**Takeaway**: every topology-shift coefficient is positive and significant across all four modes — travel time matters *less* (the negative foot/bike coefficients shrink toward zero, and the already-"wrong"-signed car/PT coefficients get even more positive) as the origin zone gets more rural. That's the expected direction: in hilly/mountain areas trips are more dispersed and car-dependent, so a given zone pair's raw travel time is a weaker deterrent (or, for car/PT, a weaker proxy for "long trip") than the same travel time would be in a dense urban zone. Since this is a **pooled** model again (no distance-band split), the underlying **distance confound on `b_tt_car`/`b_tt_pt` is still present** (both still positive, same as Models 2-3) — the topology interaction is a separate, complementary lever (capturing spatial heterogeneity in time-sensitivity) and isn't meant to fix that sign; it's an open question whether the two could be combined (topology interaction *within* each distance band) if the confound needs revisiting later.

**Data bug found and fixed while validating this model**: the first run of this model (before the fix below) converged cleanly but produced an obviously-corrupted logsum (`range [-33.052, 13944.802]`, vs. a sane `[-33.052, 137.946]` after the fix) — full investigation and resolution below.

**Sentinel-value bug in `tt_fgv`/`tt_velo`/`cost_car`/`cost_pt`/`dist`/`dist_miv`/`dist_pt`**: `02_build_tt_lookups.R` already converted the OMX sentinel placeholder (999999, meaning "no path found") to `NA` for `tt_miv`/`tt_oev`, but an earlier, insufficiently-sampled check (300-500 of 7,966 zones) had concluded the seven newer matrices (foot/bike time, car/PT cost, the three distance matrices) had no such sentinel values, so no conversion was applied to them. In fact a full scan found the sentinel present in 15,930/63,457,156 rows for `tt_fgv`/`tt_velo`/`dist` (e.g. destination zone 834202072 had `tt_fgv = 12,765,945` and `tt_velo = 999999`) and 170 rows for `dist_pt`; `cost_car`/`cost_pt`/`dist_miv` were genuinely clean. These huge placeholder values, left untouched, blew up the EMU logsum for the (rare) zone pairs that hit them. Fixed by applying the same `>= 900000 → NA` conversion already used for `tt_miv`/`tt_oev` to all seven matrices; `tt_CH_CH.fst` was rebuilt and the model above re-estimated against the corrected data (coefficients are unchanged, since none of `mc15`'s actual survey trips touch the affected zone pairs — only the logsum, which spans all 63.5M zone pairs, was corrupted).

---

## Model 4b — pooled, topology × travel-time interaction, destination-zone topology (current)

Same specification as Model 4a, but the topology interaction uses the trip's **destination zone** instead of its origin — matching how topology is attributed on the Ausland→CH destination-choice side (candidate/destination zone topology), and testing whether the built environment of the trip's *end* point explains time-sensitivity better than its start point. Only the topology join changes (`tt_ch_ch[zone_topo, on = c(dest_zone = "NO"), topo := i.topo]`); everything else — base spec, reference categories, estimation code — is identical to Model 4a.

760/8,719 zones have no `STALAN2020` value; trips *ending* there are dropped, again leaving **8,540 trips** (30.2% car, 0.9% pt, 65.5% foot, 3.4% bike), now split 5,907 urban / 1,547 hilly / 1,086 mountain destinations (vs. 6,022/1,524/994 by origin — a similar but not identical split, since a given trip's origin and destination zones don't always share the same topology).

| Parameter | Estimate | Rob. s.e. | Rob. t-stat |
|:---|---:|---:|---:|
| asc_car | −2.462 *** | 0.0578 | −42.61 |
| asc_pt | −4.659 *** | 0.3052 | −15.27 |
| asc_bike | −3.316 *** | 0.0729 | −45.50 |
| b_tt_car | +0.113 *** | 0.0206 | 5.47 |
| b_tt_pt | +0.007 | 0.0117 | 0.60 |
| b_tt_foot | −0.070 *** | 0.0071 | −9.77 |
| b_tt_bike | −0.054 *** | 0.0125 | −4.33 |
| b_tt_car_topo2 | +0.177 *** | 0.0417 | 4.25 |
| b_tt_car_topo3 | +0.090 ** | 0.0539 | 1.67 |
| b_tt_pt_topo2 | +0.037 ** | 0.0203 | 1.82 |
| b_tt_pt_topo3 | +0.052 *** | 0.0193 | 2.69 |
| b_tt_foot_topo2 | +0.049 *** | 0.0133 | 3.66 |
| b_tt_foot_topo3 | +0.037 *** | 0.0138 | 2.69 |
| b_tt_bike_topo2 | +0.064 *** | 0.0141 | 4.54 |
| b_tt_bike_topo3 | +0.046 ** | 0.0210 | 2.19 |
| b_cost_car | −1.685 *** | 0.4152 | −4.06 |
| b_cost_pt | −0.246 ** | 0.1180 | −2.08 |
| b_dist_car | +0.380 *** | 0.1251 | 3.04 |

`*** p<0.01  ** p<0.05  * p<0.10` (robust s.e./t-stats)

LL(final) = −4,424.76 · LL(0) = −11,838.95 · ρ² (vs. equal shares) = 0.6263 · Adj. ρ² = 0.6247 · ρ² (vs. observed shares) = 0.3491 · AIC = 8,885.5 · BIC = 9,012.5 · N = 8,540 · 18 estimated params

**Takeaway**: essentially the same story as Model 4a — every topology-shift coefficient is again positive, and all but `b_tt_car_topo3` remain significant at conventional levels, so time-sensitivity is weaker for every mode when the trip ends in a hillier/more mountainous zone, same direction and similar magnitude as the origin-based version. Fit is marginally better than 4a (LL −4,424.76 vs. −4,432.22, AIC 8,885.5 vs. 8,900.5, ρ² 0.6263 vs. 0.6256 — a small edge with the same 18 parameters), and `b_tt_pt` loses its (already marginal) significance here (robust p = 0.27 vs. 0.06 in 4a). As in 4a, this is still a **pooled** model, so the underlying **distance confound on `b_tt_car`/`b_tt_pt` remains** (both still positive) — the topology interaction, whichever zone it's keyed to, is a complementary lever, not a fix for that sign. Logsum: range [-32.649, 121.695], mean 0.059, 55,762/63,457,156 NA (same missing-topology/unreachable count as 4a, since the same 7 zones are missing `STALAN2020` regardless of whether they're checked as origin or destination).

---

## Nested logit — two tree structures tried, neither improves on the flat MNL (Model 4b)

Tried per request: two-level nested logit trees on top of Model 4b's exact specification (same variables, same coefficients, same reference categories), to test whether some modes' unobserved utility is correlated in a way flat MNL's IIA assumption can't capture.

### Tree 1: `{foot}` vs. `{car, pt, bike}` — walkable vs. not

```
root
|-- foot                    (walkable-distance choice; own singleton nest, no separate scale needed)
`-- moto = {car, pt, bike}  (given the trip isn't walkable, which of these three)
```

`lambda_moto` is the moto nest's scale parameter; RUM-consistency requires `0 < lambda_moto <= 1` (root scale fixed at 1), with `lambda_moto = 1` meaning "no correlation, identical to flat MNL."

**First attempt** (lambda_moto estimated directly, unconstrained, starting at 0.5) converged to **lambda_moto = 3.14** — outside the valid range. Apollo itself flagged this ("the nesting parameter... should be between 0 and 1... yet its value is 3.1436"). The whole coefficient vector also inflated by roughly the same ~2.7-3× factor relative to the MNL (`b_cost_car`: −1.68 → −4.56, `asc_pt`: −4.66 → −10.88) — the signature of an out-of-bounds nesting parameter being used to rescale utilities rather than capture real within-nest correlation. The LR test looked highly significant (LR = 73.16, p ≈ 1e-17), but this number isn't trustworthy given the invalid λ.

**Fixed** by reparametrizing: `lambda_moto = 1 / (1 + exp(-raw_lambda_moto))`, estimating the unconstrained `raw_lambda_moto` instead, which can never leave (0, 1) regardless of what the optimizer picks. Re-estimating with this constraint, the optimizer pushed `raw_lambda_moto` to +20,380 — i.e. **`lambda_moto` converged to the boundary, 1.0000**. At λ=1 a nested logit is mathematically identical to the flat MNL, and indeed every coefficient matches the Model 4b run almost exactly (`b_tt_car` 0.112707 vs. 0.112701, `asc_car` −2.462191 vs. −2.462191). Apollo reported the expected symptoms of a boundary solution (BHHH matrix singular, "some parameter values may be tending to +/- infinity"), and the LR test now reads **LR = 0.0000, p = 0.999 — no improvement at all**.

Model saved to `results_output/mode_choice_model_ch_nl.rds`, logsum to `data/output/tt_CH_CH_logsum_nl.fst` (range [-32.649, 121.698], essentially identical to Model 4b's own logsum, as expected at λ=1).

### Tree 2: `{foot, bike}` vs. `{car, pt}` — self-propelled/free vs. powered/priced

```
root
|-- active = {foot, bike}   (self-propelled, no fare -- possibly shared unobserved determinants like fitness, weather sensitivity)
`-- moto   = {car, pt}      (powered, priced -- possibly shared unobserved determinants like schedule/cost sensitivity, licence ownership)
```

Unlike Tree 1, **both** nests have 2 real alternatives (neither is a singleton), so both get their own nesting parameter, `lambda_active` and `lambda_moto2` — both reparametrized the same way from the start (`raw_lambda_active`/`raw_lambda_moto2` via the logistic transform), given what Tree 1 already showed about leaving nesting parameters unconstrained.

Both nesting parameters converged to their boundary: `raw_lambda_active = 7,652.6` and `raw_lambda_moto2 = 40.4`, i.e. **both `lambda_active` and `lambda_moto2` → 1.0000**. Every coefficient again matches Model 4b almost exactly, Apollo again reported a singular BHHH matrix and parameters "tending to +/- infinity" (the expected boundary-solution symptoms), and **LR = 0.0000 (df=2), p = 1 — no improvement at all**.

Model saved to `results_output/mode_choice_model_ch_nl2.rds`, logsum to `data/output/tt_CH_CH_logsum_nl2.fst` (range [-32.647, 121.701], again essentially identical to Model 4b's).

### Takeaway

Neither candidate nest structure finds any evidence of correlated unobserved utility between modes, in either direction (walkable/not, or self-propelled/powered). Once Model 4b's cost, car-distance, and topology×time-interaction terms are already in the utilities, the four modes behave consistently with flat MNL's IIA assumption regardless of how they're grouped — both nested models collapse exactly back to the flat MNL at their optimum. This is a genuine (double) negative result, not an estimation failure: the apparent "significant" nesting found in Tree 1's first, unconstrained attempt was entirely an artifact of an invalid out-of-range λ, not a real substitution pattern. **Model 4b (flat MNL) remains the preferred/current specification**; both nested logits are documented here for completeness, not adopted.

---

## Car/foot binary choice, non-linear travel time — log form fixes the sign confound

Tried per request ("remove pt and bike and try non-linear specifications for walk and car travel times"): a **binary car-vs-foot** choice model (`03e_carfoot_nonlinear_CH.R`, a separate script from the 4-mode models above), testing whether the shape of the travel-time effect -- not just distance segmentation -- can resolve the "wrong-sign" `b_tt_car` confound documented since Model 1.

**Scope**: PT/bike are dropped from the choice set entirely, not just the utility functions -- the estimation sample is restricted to the `mc15` trips where the respondent actually chose car or foot (8,180 / 8,730 trips, 31.6% car / 68.4% foot). Topology×time interactions are left out here to isolate the travel-time functional form specifically. Cost and car-network distance are kept (car only), matching Model 3's base spec. No logsum is computed -- a car/foot-only choice set isn't a valid stand-in for full 4-mode accessibility.

Three functional forms for travel time were compared, all else (asc_car, b_cost_car, b_dist_car) held to the same specification:

| Model | `b_tt_car` | `b_tt_foot` | LL | npar | AIC | BIC |
|:---|---:|---:|---:|---:|---:|---:|
| **A. Linear** (`tt`, minutes) | +0.244 *** (wrong sign) | −0.062 *** | −2,772.01 | 5 | 5,554.0 | 5,589.1 |
| **B. Quadratic** (`tt/60`, hours) | tt: +20.60 *** / tt²: −45.36 *** | tt: −3.516 *** / tt²: −0.373 *** | −2,678.52 | 7 | 5,371.0 | 5,420.1 |
| **C. Log** (`log(tt+1)`, minutes) | **−1.557 *** (correct sign)** | −2.118 *** | **−2,560.71** | 5 | **5,131.4** | **5,166.5** |

`*** p<0.01` (robust t-stats). N = 8,180 for all three.

**Model B (quadratic)**: `tt_miv`/`tt_fgv` are rescaled to hours before squaring -- `tt_fgv` reaches ~2,113 minutes (~35h) for some long-distance car-chosen trips (a real network-distance value for the never-chosen walk alternative, not a data error), and squaring that in minutes (~4.5M) overflowed the utility scale when combined with Apollo's small test perturbations of the starting values ("Log-likelihood calculation fails at values close to the starting values"); hours keeps every squared term well-scaled. LR test vs. linear: χ²=186.99 (df=2), p≈2.5e-41 -- a highly significant improvement. The car utility is inverted-U shaped: `b_tt_car` is positive (confound-dominated) up to a turning point at **tt_miv ≈ 14 minutes**, then decreasing beyond it -- i.e. the genuine negative time-sensitivity only shows up past a threshold, while short car trips still show the confound. Foot's turning point is at a negative (physically meaningless) travel time, so foot utility is monotonically decreasing for all realistic `tt_fgv`, with the marginal disutility growing steeper for longer walks.

**Model C (log)**: the standout result. `log(tt_miv + 1)` gives `b_tt_car` its **theoretically correct negative sign** (−1.557, robust t≈−10.3, highly significant) -- the confound that motivated the entire distance-band-segmentation exercise earlier in this document disappears under this functional form, without needing to split the sample at all. It also has the best fit of all three models by a wide margin (AIC 5,131.4 vs. 5,554.0 linear / 5,371.0 quadratic), despite using the *same* 5 parameters as the linear model (not the 7 of the quadratic) -- log time isn't just a better fit, it's a more parsimonious one.

**Takeaway**: this suggests the "wrong sign" on `b_tt_car` found throughout this document (Models 1-4b) was, at least in part, a **functional-form misspecification**, not purely a sampling/segmentation problem. A linear travel-time term forces a single global slope that gets dominated by the confound; letting the marginal effect diminish (log) or reverse past a threshold (quadratic) reveals genuine negative time-sensitivity that a straight line can't represent. This was a car/foot-only diagnostic -- extending it to all four modes in the full pooled model (below) confirms and generalizes the finding.

Models saved to `results_output/mode_choice_model_ch_carfoot_{linear,quadratic,log}.rds`.

---

## Model 5 — pooled, log + quadratic travel time for all four modes: the sign confound is fully resolved

Tried per request ("try log travel time and squared for all four modes in the pooled model"): extends the car/foot finding above to the full 4-alternative pooled model, adding BOTH a log term and an hours-scaled quadratic term to every mode's travel time (in place of Model 4b's single linear term and topology interactions):

```
V_car  = asc_car  + b_tt_car_log  * log(tt_miv+1)  + b_tt_car_sq  * (tt_miv/60)^2  + b_cost_car * cost_car + b_dist_car * dist_miv
V_pt   = asc_pt   + b_tt_pt_log   * log(tt_oev+1)  + b_tt_pt_sq   * (tt_oev/60)^2  + b_cost_pt  * cost_pt
V_foot =             b_tt_foot_log * log(tt_fgv+1)  + b_tt_foot_sq * (tt_fgv/60)^2                          (reference)
V_bike = asc_bike + b_tt_bike_log * log(tt_velo+1) + b_tt_bike_sq * (tt_velo/60)^2
```

No topology interaction here (combining it with two travel-time terms per mode would double the interaction count -- 4 shifts per mode instead of 2 -- and the point is to isolate the non-linear time functional form across all four modes, matching the car/foot diagnostic's scope). Cost (car/PT) and car network distance are kept, same as Model 3/4b's base spec. No topology filter on the estimation sample either, so N is slightly larger than Model 4b's (8,543 vs. 8,540 -- only mode/travel-time/cost completeness is required).

| Parameter | Estimate | Rob. s.e. | Rob. t-stat |
|:---|---:|---:|---:|
| asc_car | −2.871 *** | 0.0680 | −42.22 |
| asc_pt | −6.316 *** | 0.4087 | −15.45 |
| asc_bike | −3.817 *** | 0.1075 | −35.50 |
| b_tt_car_log | **−1.214 *** (correct sign)** | 0.1178 | −10.30 |
| b_tt_car_sq | −0.942 * | 0.5297 | −1.78 |
| b_tt_pt_log | **−1.007 *** (correct sign)** | 0.1662 | −6.06 |
| b_tt_pt_sq | −1.150 ** | 0.4972 | −2.31 |
| b_tt_foot_log | −1.919 *** | 0.0842 | −22.80 |
| b_tt_foot_sq | −0.086 ** | 0.0466 | −1.85 |
| b_tt_bike_log | −1.907 *** | 0.1259 | −15.15 |
| b_tt_bike_sq | −0.117 * | 0.0544 | −2.15 |
| b_cost_car | −1.388 *** | 0.3261 | −4.26 |
| b_cost_pt | −0.019 (n.s.) | 0.0977 | −0.19 |
| b_dist_car | +0.398 *** | 0.1007 | 3.96 |

`*** p<0.01  ** p<0.05  * p<0.10` (robust s.e./t-stats)

LL(final) = −4,205.65 · LL(0) = −11,843.11 · ρ² (vs. equal shares) = 0.6449 · Adj. ρ² = 0.6437 · ρ² (vs. observed shares) = 0.3815 · AIC = 8,439.3 · BIC = 8,538.0 · N = 8,543 · 14 estimated params

**Not nested with Model 4b** (different travel-time functional form, no topology interaction), so no LR test applies -- AIC/BIC is the correct comparison instead:

| Model | LL | npar | N | AIC | BIC |
|:---|---:|---:|---:|---:|---:|
| Model 4b (linear tt + topology×tt) | −4,424.76 | 18 | 8,540 | 8,885.5 | 9,012.5 |
| **Model 5 (log+quadratic tt, no topology)** | **−4,205.65** | **14** | 8,543 | **8,439.3** | **8,538.0** |

**Takeaway**: Model 5 wins decisively on both AIC and BIC, with *fewer* parameters and no topology interaction at all. More importantly, **every travel-time coefficient (log and quadratic) is negative for all four modes** -- `b_tt_car_log` and `b_tt_pt_log` both have the theoretically correct sign, resolving the confound that motivated the entire distance-band-segmentation exercise, for both car AND PT, without splitting the sample or adding topology. `b_cost_pt` is the only coefficient that loses significance here (robust p=0.42) -- PT's cost effect appears to already be absorbed by its (now correctly-signed and highly significant) log travel-time term. This is currently the best-fitting and most theoretically coherent domestic mode-choice specification found in this project; it supersedes Model 4b as the leading candidate, though topology interactions haven't yet been re-tested on top of this functional form (a natural next step, since Model 5 doesn't include them at all).

Logsum: range [-94.286, 48.830], mean −16.445, 0/63,457,156 NA (every zone pair has a valid logsum here, since topology -- the one source of missing values in Model 4a/4b/nested-logit -- isn't used in this specification at all).

Model saved to `results_output/mode_choice_model_ch_nonlinear.rds`, logsum to `data/output/tt_CH_CH_logsum_nonlinear.fst`.

---

## File index

```
data/output/mc15.csv                       Raw domestic mode-choice survey (input)
data/output/tt_CH_CH.fst                   Zone x zone travel time/cost/distance/topology, all Swiss zones (02_build_tt_lookups.R)
data/output/tt_CH_CH_logsum.fst            tt_CH_CH.fst + EMU column (`logsum`) -- pooled MNL model (4b), topo x tt interaction applied per OD pair
data/output/tt_CH_CH_logsum_nl.fst         tt_CH_CH.fst + EMU column (`logsum`) -- nested logit, {foot} vs {car,pt,bike} tree, same result as tt_CH_CH_logsum.fst since lambda_moto converged to 1
data/output/tt_CH_CH_logsum_nl2.fst        tt_CH_CH.fst + EMU column (`logsum`) -- nested logit, {foot,bike} vs {car,pt} tree, same result again since both lambdas converged to 1
data/output/tt_CH_CH_logsum_nonlinear.fst  tt_CH_CH.fst + EMU column (`logsum`) -- Model 5 (log+quadratic tt, all 4 modes, no topology) -- best-fitting model, no missing-topology NAs
results_output/mode_choice_model_ch.rds    Fitted pooled MNL model (Model 4b: cost+distance+destination-topology x tt interaction)
results_output/mode_choice_model_ch_nl.rds Fitted nested logit model, {foot} vs {car,pt,bike} tree (same spec as 4b -- not adopted, see "Nested logit" section above)
results_output/mode_choice_model_ch_nl2.rds Fitted nested logit model, {foot,bike} vs {car,pt} tree (same spec as 4b -- not adopted either)
results_output/mode_choice_model_ch_nonlinear.rds Fitted Model 5 (log+quadratic tt, all 4 modes) -- current best model, see "Model 5" section above
results_output/mode_choice_ch_iterations.csv / _output.txt / _estimates.csv   Apollo's own per-model artifacts (MNL, Model 4b)
results_output/mode_choice_ch_nl_iterations.csv / _output.txt / _estimates.csv   Apollo's own per-model artifacts (nested logit, tree 1)
results_output/mode_choice_ch_nl2_iterations.csv / _output.txt / _estimates.csv   Apollo's own per-model artifacts (nested logit, tree 2)
results_output/mode_choice_ch_nonlinear_iterations.csv / _output.txt / _estimates.csv   Apollo's own per-model artifacts (Model 5)
results_output/mode_choice_model_ch_carfoot_{linear,quadratic,log}.rds   Car/foot-only diagnostic models (see "Car/foot binary choice" section above) -- not part of the main 4-mode pipeline
```

Note: all `tt_CH_CH_logsum*.fst`/`mode_choice_model_ch*.rds` files (excluding the car/foot diagnostics, which live in a separate script) are overwritten each time `03_estimate_logsum_CH.R` is re-run, so they always reflect the *latest* model version for each spec -- Models 1-3, the distance-band-segmented version, and Model 4a's (origin-topology) coefficients are preserved only in this document. The segmented version's `_short`/`_long` `.rds` files are no longer produced since segmentation was abandoned in favour of Model 4. Model 4b, both nested-logit trees, and Model 5 are all estimated and saved side by side in the same script run, not overwriting each other -- **Model 5 (log+quadratic tt) is currently the best-fitting and recommended specification**, ahead of Model 4b, per the AIC/BIC comparison in the "Model 5" section above.
