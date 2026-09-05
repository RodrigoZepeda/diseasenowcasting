# FINDINGS V -- the multi-state sweep

**2026-09-03.** Scripts `40_multistate.R` (sweep), `41_ms_report.R` (aggregation),
`40_core36/37/38/39.R` (verbatim extracts of the four models' function
definitions). Raw per-location results in `devel/spa_diagnostics/ms/`, pooled
summary in `ms_summary.rds`.

Everything in FINDINGS T and U rested on **one location (Texas) and 578 scored
triples**. This section reports what happens on **50 locations and 68,245
triples**, all four models refit independently at every (location, origin) and
scored on identical triples.

> **READ V.9 FIRST.** Scripts 36-39 all seed their rolling-origin backtest with
> `warm <- best$par`, where `best` is fitted to the WHOLE series -- every future
> snapshot included. Removing that leak takes script 38 from **+22.9% to +2.5%**
> on Texas while leaving 36 and 39 untouched, and **inverts the published
> ordering**. Sections V.1-V.7 below were computed from leak-contaminated fits
> and their forecast numbers do not stand; the fit diagnostics (V.6, V.7) and
> the mechanism findings (V.3, V.4, V.5) are unaffected.

---

## V.0 Design

* 50 of 53 FluSight locations. `US` and `Puerto Rico` were killed after running
  >3 h on a single location each (V.5); `Wisconsin` never started.
* 32 rolling origins per location, `2024-04-06` to `2025-07-05`, covering the
  whole 2024-25 season including the peak. The 12- and 16-origin windows used
  by scripts 36/38/39 are strict subsets, recoverable with `WINDOW=` in script 41.
* `HMAX = 4`, `NDRAW = 2000`, target = the value published at a future snapshot,
  as in scripts 33/36/37/38/39.
* The triple set is built **once per origin** and handed to all four models, so
  the four WIS columns are never resting on different data.
* Failure policy: if any model diverges at an origin, the origin is dropped for
  **all four**. 72 of 1,600 origins (4.5%) were dropped this way, concentrated
  in 4 locations (V.4).

**The harness reproduces the published Texas numbers**, which is the licence to
believe the rest. At `NORIG=12`, Texas, 578 triples -- exactly the count scripts
38/39 scored:

| model | published | sweep |
|---|---|---|
| 38 | 1.702 / +22.4% | 1.691 / **+22.8%** |
| 36 | 1.959 / +10.6% | 1.956 / **+10.8%** |
| 39 | 2.120 / +3.3% | 2.121 / **+3.2%** |
| 37 | 2.838 / -29.4% | 2.820 / **-28.7%** |

Differences are Monte-Carlo noise at 2000 draws.

---

## V.1 SUPERSEDED BY V.9 -- the ordering replicates, the effect size does not

> These forecast numbers come from leak-contaminated fits (V.9). The pairwise
> win counts below are NOT evidence about the models: script 38 wins them
> largely because its backtest was seeded with future data. Retained for the
> record and because the diagnostic columns (coverage, triple counts, the Texas
> rank) are still informative about the sweep itself.

Pooled over 46 locations (excluding Rhode Island, corrupted per V.6, and the
three locations that lost >25% of origins), 65,937 triples:

| model | WIS | skill | cov90 | per-location median | beats empirical |
|---|---:|---:|---:|---:|---:|
| script 38 | 3.998 | **+2.3%** | 0.904 | +0.4% | 27/50 |
| script 36 | 4.087 | +0.1% | 0.902 | -1.4% | 22/50 |
| script 39 | 4.811 | -17.6% | 0.887 | -2.3% | 19/50 |
| script 37 | 5.742 | -40.3% | 0.864 | -3.7% | 19/50 |
| empirical ratio | 4.092 | -- | 0.879 | -- | -- |
| persistence | 4.857 | -18.6% | -- | -- | -- |

All three pairwise comparisons from FINDINGS T/U hold at roughly two-to-one:

| comparison | what it measures | result |
|---|---|---:|
| 38 > 36 | what `h_R` structure buys | **34/50** |
| 38 > 39 | the mean-preservation cost | **33/50** |
| 39 > 37 | hurdle vs Skellam at matched mean | **34/50** |

**But Texas is close to the best case, not a typical one.** Its rank among the
50 locations:

| model | Texas skill | rank |
|---|---:|---:|
| 38 | +20.5% | **2 of 50** |
| 36 | +16.3% | **1 of 50** |
| 39 | +14.6% | 6 of 50 |
| 37 | -52.6% | 44 of 50 |

Texas is simultaneously the best location for 36, the second best for 38, and
the seventh worst for 37. The clean Texas ordering `38 > 36 > 39 > 37` with
large gaps is that coincidence. Script 38's +22.4% becomes **+2.3% pooled and
+0.4% median**, winning in barely half the locations.

**Do not quote +22.4% as the model's skill.** The defensible claim is that
script 38 is at parity with an empirical ratio lookup on average and ahead of
the other three about two times in three.

---

## V.2 SUPERSEDED BY V.9 -- where the models help at all

> Same caveat: computed under the leak. The horizon/age SHAPE (skill rises with
> horizon and cohort age, negative at h=1 and ages 0-1) is likely robust, since
> it holds for 36 and 39 too, but the levels are not.

Skill is entirely at long horizons and old cohorts, and is negative at short ones.

| horizon | 38 | empirical | skill |
|---|---:|---:|---:|
| h=1 | 2.631 | 2.616 | **-0.6%** |
| h=2 | 3.550 | 3.594 | +1.2% |
| h=3 | 4.410 | 4.564 | +3.4% |
| h=4 | 5.698 | 5.961 | **+4.4%** |

| target age | 38 | empirical | skill |
|---|---:|---:|---:|
| 0-1 | 16.461 | 15.635 | **-5.3%** |
| 1-2 | 11.032 | 10.914 | -1.1% |
| 2-4 | 9.207 | 9.358 | +1.6% |
| 4-8 | 4.401 | 4.599 | +4.3% |
| 8-15 | 1.518 | 1.583 | **+4.2%** |

Off-season origins (Apr-Oct) give +3.9%, peak origins (Nov-Mar) +2.1%. The
Texas window used in FINDINGS T/U was March-July, i.e. the easier regime -- a
second way in which that number was flattered.

---

## V.3 Script 39's degenerate basin is an OPTIMISATION artefact, not a model defect

**This is the most consequential finding and it partially retracts V.1's
reading of script 39.**

4 of 50 locations put script 39 in a low-retention basin at *every* origin:

| location | 39 `S_R(15)` | 38 `S_R(15)` | s39 | s38 |
|---|---:|---:|---:|---:|
| Nebraska | 0.002 | 1.000 | -96.3% | +5.4% |
| Arizona | 0.011 | 1.000 | -1.5% | +0.2% |
| Illinois | 0.152 | 0.991 | -76.2% | -6.6% |
| New York | 0.153 | 0.996 | **-349.2%** | -17.6% |

Median s39 inside the basin **-86.3%**, outside it **-0.7%**. Script 38 enters
such a basin in 1 of 50 locations, script 39 in 4.

The basin has near-nominal coverage (New York cov90 = 0.890) and catastrophic
sharpness (WIS 29.9 against an empirical 6.7). Mechanically it is huge
retraction mass cancelling huge dispersion (`size` 0.002-0.15) so that
`E[Delta] = alpha - omega` still holds -- mean preservation is satisfied by a
solution that is useless.

**But it is not what 39's own likelihood prefers.** Refitting from starts seeded
in the high-retention region (`h0 = qlogis(0.02)` rather than `qlogis(0.5)`):

| location | best published start | best good-basin start | gain |
|---|---|---|---:|
| New York | -1229.65 (`S_R` 0.523) | **-1106.77 (`S_R` 0.992)** | **+122.9 nats** |
| Illinois | -1339.26 (`S_R` 0.987) | **-1126.77 (`S_R` 0.978)** | **+212.5 nats** |
| Arizona | -1379.03 (`S_R` **0.011**) | **-1323.75 (`S_R` 0.936)** | **+55.3 nats** |

**There is no exception.** In all three locations the published starts leave
55-212 nats on the table and the better optimum lies in the *good* retention
basin. Arizona is the sharpest case: the published starts land on `S_R = 0.011`
-- 99% of reports withdrawn -- while a start seeded at `h0 = qlogis(0.05)` finds
`S_R = 0.936` with a likelihood 55.3 nats higher. The degenerate solution is
never what 39's objective prefers; it is only what these starts reach.

**Consequence: 39's -17.6% pooled penalty is substantially my harness's starting
values, not your reparameterisation.** The honest statement of the
mean-preservation cost is the out-of-basin median, **about one point of skill**
(39 at -0.7% against 38 at +0.4%), not seventeen.

---

## V.4 The dropped origins: script 37 fails to converge, and two guard holes hid it

**This section replaces an earlier, wrong diagnosis.** The first draft attributed
the 72 dropped origins to warm-start propagation, on the strength of a
reproducer that re-ran West Virginia's origins cleanly. That reproducer only
called `q_pairs(gD, hR, 14, 15)` directly and never exercised the forward
simulators, which is where the failure actually is. Seeding the origin chain
from cold starts was implemented and tested: West Virginia stayed at **6/32,
identical pattern**. The warm-start explanation is retired.

### What actually happens

The dropped origins are 64 script-37 failures plus 8 script-36 failures:

| location | dropped | model |
|---|---:|---|
| West Virginia | 26 of 32 | 37 |
| Idaho | 23 of 32 | 37 |
| Delaware | 15 of 32 | 37 |
| Connecticut | 8 of 32 | 36 (magnitude -> `Inf`) |

All three script-37 locations are the smallest-count locations in the panel.
Dumping the failing state (`DUMP=1` in script 40) gives it immediately:

```
p37: len(gD)=0   mu37: len 0        <- prof() returned NULL
P38: len(gD)=16, S_R=0.9973         <- 38 and 39 are healthy at the same origin
P39: len(gD)=16, S_R=0.9870
```

Script 37 does not converge, its `prof()` returns `NULL`, and `sim37` then calls
`q_pairs` with a zero-length `gD`. There `nD = 0`, the loop bound becomes
`0:min(-1, a_star)` = `c(0, -1)`, and `gD[1]` on an empty vector is `NA`, giving
exactly the observed `if (!is.finite(g) || g < 1e-14) : missing value where
TRUE/FALSE needed`.

### Why nothing caught it

Two independent holes, both in the harness, both of which silently convert a
hard failure into an apparently successful fit:

1. **`is.finite(-1e12)` is `TRUE`.** Every objective in scripts 37/38/39 returns
   `1e12` as its failure sentinel, so a failed fit is reported as
   `logL = -1e12`. Any `is.finite(logL)` convergence check accepts it. This is
   how 64 non-converged fits reached the simulator.
2. **`all(is.finite(NULL))` is `TRUE`.** `all()` of an empty vector is vacuously
   true, so a guard that validated the fitted masses passed a `NULL` prof
   unchanged. Length must be tested before finiteness.

**Fix applied:** `usable()` in `fit_origin` now rejects `logL < -1e11` and
requires `length(masses) > 0` before testing finiteness, and `m37`/`m38` return
`numeric(0)` rather than erroring on a `NULL` prof. A rejected warm fit falls
back to the cold starts, so an origin is lost only if *no* start converges.

### The scored results are not contaminated

Checked directly: of the 64 script-37 sentinel origins, **0 were scored** -- the
"drop the origin for all four" policy removed every one of them. Scripts 36, 38
and 39 hit the sentinel at **0 of 1600** origins. So the four-way numbers in V.1
rest on origins where all four models genuinely converged, and the 37 column is
clean. The policy did its job for a reason other than the one first given.

### What the seeding change is still worth

The cold seed is retained even though it does not fix this, because the old
seed (`warm <- b38$par`, a fit to the *whole* series) leaked post-origin
snapshots into the origin-1 fit. Scripts 36/37/38/39 seed their own backtests
the same way, so that leak is in the published results too. It is small -- one
origin's starting values -- but it is free to remove.

Idaho's +10.5% and West Virginia's numbers still rest on 9 and 6 origins and
**must not be compared** to locations with all 32; script 41 flags any location
that lost more than 25% of its origins.

## V.5 Why `US` is slow: `qnbinom` inversion, not the likelihood

`US` was assumed slow because of its size. It is not: script 38 fits `US` in
**15.2 s**, *faster* than Texas (27.4 s) or Puerto Rico (22.5 s), and a single
`nll` evaluation costs 0.085 s against Texas's 0.013 s.

The cost is in the forward simulation. `qnbinom` inverts by search, so its cost
grows with the mean:

| `mu` | `size` | seconds per 2000 draws |
|---:|---:|---:|
| 1e5 | 0.5 | 1.07 |
| 1e6 | 0.5 | 8.88 |
| **1e7** | **0.5** | **119.33** |
| 1e7 | 0.01 | 0.05 |

One triple can therefore cost two minutes, and a location has ~1,470 of them.
`US` reaches it because its counts run to 48,796 and script 38's magnitude mean
is `(alpha+omega)/pi`, inflated on top (V.6).

This is the same root cause as V.6 and as Connecticut's `Inf`: an unbounded
magnitude mean. It is a real production hazard -- any national-scale fit will
hit it.

---

## V.6 The 1/(1-P0) inflation is far larger than 8-37x, and it corrupts scores

Confirming the correction that produced script 39, measured per cell across 50
locations rather than summarised on Texas:

| | script 38 | script 39 |
|---|---:|---:|
| inflation `pi E[M]/(alpha+omega)`, median | **2.11** | **1.00** |
| inflation, worst cell | **2.0e7** | **1.00** |
| `max abs(E[Delta] - (alpha-omega))`, median | 3.35 | 11.4 |

The inflation is not a roughly constant factor: it spans seven orders of
magnitude across cells, so script 38's mean is not a rescaling of
`mu_t q_C(d)` but a different function of `(d, level)`. The `8-37x` figure was
the mild Texas case.

Script 36's `kappa * level^beta` has the same unboundedness with no constraint
at all. On **Rhode Island** it produced 4 triples (of 1,310) with WIS up to
**7.1e7**, dragging script 36's pooled skill to -80,510% while its median WIS
there is 0.000. The `qsafe` guard in the sweep catches `NaN`/`Inf` but not
finite-but-astronomical draws -- a harness gap, not a data property. Any pooled
mean over locations must be read with this in mind; the per-location medians and
win counts are the robust statistics and are what V.1 quotes.

---

## V.7 Fit quality: 38 passes, 39 does not

Across 50 locations, in-sample:

| gate | script 38 | script 39 |
|---|---|---|
| exact zeros, observed median | 0.907 | 0.907 |
| exact zeros, model median | **0.908** | 0.930 |
| PIT KS `p < 0.05` | **2 of 50** | **13 of 50** |
| `S_R(15)` median [range] | 0.993 [0.000, 1.000] | 0.979 [0.002, 1.000] |
| tail ratio \|z\|>20, median | 1.29 | 1.16 |
| median `logL` | **-640.7** | -646.2 |

Script 39 systematically over-predicts zeros and fails PIT six times as often.
Given V.3, some unknown share of that is the same starting-value problem, so
this table should be regenerated after the fix before being treated as a
property of the parameterisation.

Median in-sample `logL`: 36 = -648.1, 37 = -1188.7, 38 = -640.7, 39 = -646.2.
Script 38 has the highest `logL` in only **22 of 50** locations, so even
in-sample its advantage is not uniform.

---

## V.8 Open, in priority order

1. **Fix the backtest seeding in scripts 36/37/38/39** (V.9). This is the
   priority: it changes which model is best. Then re-derive every forecast
   number in FINDINGS T, U and V.1.
2. **Re-run the sweep** with the V.4 guard fix and the V.3 high-retention start
   (in progress as `ms2/`, clean of the leak). V.7's PIT comparison should be
   regenerated from it.
3. **Bound the magnitude mean** in 36 and 38, or replace the `qnbinom` inversion
   with a direct `rnbinom`-based draw (V.5, V.6). This is a production
   blocker for national-scale counts, not just a sweep nuisance.
4. **Why do the published starts reach the degenerate basin at all?** All three
   tested locations show it is a local trap, so the question is what makes
   `h0 = qlogis(0.5)` (half the mass withdrawn) a bad initial guess when the
   data want `S_R ~ 0.99`. A start at `qlogis(0.02)` fixes all three; whether
   that generalises to the other 47 locations is untested.
5. `US`, `Puerto Rico`, `Wisconsin` were never scored in sweep 1.
6. The estimand decision remains deferred (FINDINGS J, plan section 1).
7. `BRIEFING_count_cumulative.md` still quotes retracted numbers, and now also
   the +22.4% figure that V.9 retracts.

---

## V.9 The published skill is an information leak (supersedes V.1)

### The defect

Scripts 36, 37, 38 and 39 each fit the model to the **entire** series, then start
the rolling-origin backtest from those parameters:

```r
best <- <fit to all snapshots>          # PART A
...
warm <- best$par                        # PART B, before the origin loop
for (s0 in origins) { ft <- fit(train(s0), warm); warm <- ft$par; ... }
```

So the origin-1 fit is seeded from parameters that have seen every future
snapshot, and the warm chain carries that forward through all subsequent
origins. Script 40 reproduced this faithfully, which is why it matched the
published Texas numbers exactly -- it was reproducing the leak, not validating
the result.

### Texas, the published setup, only the seeding changed

`NORIG=12`, 578 triples -- the identical comparison of FINDINGS T/U:

| model | WIS leak | WIS clean | skill leak | **skill clean** |
|---|---:|---:|---:|---:|
| script 38 | 1.69 | 2.14 | +22.9% | **+2.5%** |
| script 36 | 1.96 | 1.96 | +10.4% | **+10.6%** |
| script 39 | 2.12 | 2.10 | +3.4% | **+4.0%** |
| script 37 | 2.54 | 2.84 | -16.1% | **-29.6%** |
| empirical | 2.19 | 2.19 | -- | -- |

**Script 38's +22.4% is the leak.** Scripts 36 and 39 are unmoved (10.4 -> 10.6,
3.4 -> 4.0), which isolates the effect: this is not a general shift, it is
specific to script 38.

### The ordering inverts

```
published:   38 (+22.4)  >  36 (+10.6)  >  39 (+3.3)  >  37 (-29.4)
leak-free:   36 (+10.6)  >  39  (+4.0)  >  38 (+2.5)  >  37 (-29.6)
```

The model FINDINGS U named "the current best" is **third of four**, barely above
an empirical ratio lookup. The best of the four is script **36** -- the purely
descriptive sign x magnitude law, with no retention structure at all -- and its
+10.6% is genuine.

### Confirmed by controlled experiment on four locations

Same data, same origins, same triples; only the seed differs. `LEAK` restores
`warm <- best$par` on top of every other sweep-2 fix:

| location | original | leak restored | **clean** |
|---|---:|---:|---:|
| Texas (n=12 origins) | +22.4 (published) | +22.9 | **+2.5** |
| Arkansas | +10.6 | +12.2 | **-18.7** |
| Colorado | +1.2 | +1.2 | **-11.2** |
| Alaska | +9.2 | +9.3 | **-5.7** |

Colorado reproduces to the decimal. Across all four, scripts 36 and 37 move by
at most 0.3 points while script 38 moves 12-29.

### Why script 38 specifically

Not established. Script 38 is the most flexible of the four -- 21 parameters
with a free `pi` -- and 36 (10 parameters) and 37 (Poisson-Skellam, no hurdle)
are the ones that do not benefit, which is *consistent* with flexibility being
the mechanism. But that is a story fitted to four locations, not a demonstration.
It should not be asserted without a test that varies flexibility directly.

### Consequence for the mean-preservation question

On Texas, leak-free, **39 beats 38 (+4.0% vs +2.5%)** -- reversing the 19-point
gap the published numbers showed. But Texas is not representative: across the 47
clean locations of the leak-free sweep, **38 still beats 39 in 30 of 47**
(median -2.5% vs -7.9%).

So the honest statement is: mean preservation costs roughly **5 points of median
skill**, not the 19 that FINDINGS U implied and not the zero that Texas alone
suggests. Both models are worse than an empirical ratio lookup either way, so
the comparison is between two losing options.

(An earlier draft of this section claimed from the Texas result alone that mean
preservation costs nothing. The 47-location sweep contradicts it and this
supersedes it.)

V.3's finding stands and explains 39's *earlier* catastrophic locations: the
degenerate basin is gone across the whole panel -- `S_R(15)` for 39 now ranges
[0.659, 1.000] against [0.002, 1.000] before, with 0 origins in the basin.

### What to do

1. Fix the seeding in scripts 36/37/38/39 (`warm <- S0`, or multi-start the
   first origin on its own training data). One line each.
2. Re-derive every forecast number in FINDINGS T, U and V.1 from clean fits.
3. `BRIEFING_count_cumulative.md` and `main_identifiability_update.tex` should
   not carry +22.4%, or the claim that the `h_R`-primitive hurdle model is the
   best forecaster.

---

## V.10 The leak-free sweep: 48 locations, 68,448 triples

Sweep 2 (`ms2/`), same design as V.0 but with the V.9 seeding fix, the V.4 guard
fix, the V.3 high-retention start, and an exact-but-fast ZTNB draw (V.5).
`US`, `Puerto Rico`, `Wisconsin`, `New Hampshire` and `South Carolina` did not
finish. West Virginia is excluded from the per-location statistics below: script
36's unbounded magnitude produced a max WIS of 3.5e4 there, giving it a
meaningless -31,900% (V.6 again).

### No model beats the baseline

47 clean locations:

| model | beats empirical | median skill |
|---|---:|---:|
| script 36 | **22/47** | **-1.2%** |
| script 38 | 11/47 | -2.5% |
| script 37 | 11/47 | -5.0% |
| script 39 | 9/47 | -7.9% |
| empirical ratio | -- | 0.0% |

**Not one of the four beats a ratio lookup in the median.** Script 36 -- the
descriptive sign x magnitude law with no retention structure at all -- is the
least bad, and the only one that wins in more locations than it loses on the
`h_R`-carrying models.

Pairwise: **36 > 38 in 30/47**, **36 > 39 in 39/47**, 38 > 39 in 30/47.
Sweep 1 had 38 > 36 in 34/50; that reverses entirely once the leak is removed.

### Skill by horizon and age, leak-free

Script 38 is now negative at every horizon and only marginally positive at the
oldest cohorts:

| target age | 38 | empirical | skill |
|---|---:|---:|---:|
| 0-1 | 16.858 | 15.444 | **-9.2%** |
| 1-2 | 11.400 | 10.757 | -6.0% |
| 2-4 | 9.498 | 9.140 | -3.9% |
| 4-8 | 4.488 | 4.507 | +0.4% |
| 8-15 | 1.525 | 1.555 | **+1.9%** |

V.2's "skill rises with horizon and cohort age" holds in shape but the levels
fall by ~3 points: what looked like +4.2% at ages 8-15 is +1.9%.

### Both harness fixes are confirmed at panel scale

| | sweep 1 | sweep 2 |
|---|---:|---:|
| origins dropped to divergence | 72 of 1600 (4.5%), 4 locations | **8 of 1536 (0.5%), 1 location** |
| script-37 sentinel fits | 64 | **0** |
| script-39 `S_R(15)` range | [0.002, 1.000] | **[0.659, 1.000]** |
| origins with 39 in the basin | 128 | **0** |

Delaware 17->32 origins, Idaho 9->32, both with 0 sentinel fits. The remaining 8
drops are Connecticut, where script 36's magnitude genuinely diverges to `Inf` --
a real model property, not a harness artefact.

### What does not change

The fit diagnostics are essentially identical to V.6/V.7, as they should be --
they were never affected by the seeding:

* script 38's magnitude inflation: median **2.89**, worst **2.0e7** (V.6 stands)
* script 39's inflation: exactly **1.00** everywhere
* exact zeros: 38 reproduces them (0.904 model vs 0.905 observed), 39 over-
  predicts (0.927)
* PIT: 39 fails in **13 of 48** locations, 38 in **2 of 48**. This survives the
  basin fix, so it is a property of the derived-`pi` parameterisation, not of
  the optimiser.
* median in-sample `logL`: 36 -654.9, 37 -1210.4, 38 -647.4, 39 -656.4; script
  38 highest in 23 of 48.

So script 38 remains the best-*fitting* model and script 39 the most
mean-faithful, while script 36 is the best *forecaster* -- and none of them
forecasts better than a ratio lookup.
