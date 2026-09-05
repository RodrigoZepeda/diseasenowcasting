# Why the count-cumulative FluSight model prefers the low-`p` solution

**2026-09-01, `skellam` branch.** Diagnostics run against the plan in
`Next diagnostic steps for the FluSight count-cumulative Skellam model.md`.
All numbers are Texas, windowed to the snapshot era and truncated to the last
event date, Poisson / AR1 / lognormal, one stage.

Scripts are in `devel/spa_diagnostics/`. They use `pkgload::load_all(".")`; the
installed build is a different, older package.

> **UPDATE 2026-09-02 — the decisive parameter-recovery experiment is done
> (scripts `18_param_recovery.R`, `19_real_tail_constraint.R`). Read section I
> first.** It reframes everything below. In one line: the estimator is *sound* —
> it recovers `p` when retraction lands within the observation horizon — and the
> low-`p` collapse is **finite-horizon confounding between `p` and the tail of
> `g_C`** (decision-tree Result C), demonstrated both in simulation and on the
> real data. Batch misspecification (the Conclusion below) is a *residual*
> contributor, not the whole cause. The "batch revisions are THE cause" framing in
> the Conclusion is superseded by section I.

---

## Conclusion

```
MODEL MISSPECIFICATION OF THE REVISION PROCESS
  - SADDLEPOINT APPROXIMATION PROBLEM ......... RULED OUT
  - MARGINAL-LIKELIHOOD INFORMATION LOSS ...... REAL, BUT MINOR
  - POISSON OVER/UNDER-DISPERSION ............. RULED OUT (NB behaves identically)
  - GENUINE WEAK IDENTIFICATION ............... NO (the profile is sharply peaked)
  - BATCH REVISIONS THE MODEL CANNOT GENERATE . THIS IS THE CAUSE
```

**The preference for low `p` comes from 272 outlier cells, not from a systematic
signal.** Decomposing the 14,275-nat gap on Texas:

| cell type | n | gap | per cell |
|---|---:|---:|---:|
| `z = 0` | 1304 | +1,445 | +1.1 |
| `z > 0` | 231 | +7,164 | **+31.0** |
| `z < 0` | 41 | +5,665 | **+138.2** |

90% of the gap comes from the 17% of cells that are non-zero, and the **top ten
cells alone account for 53%**. They are single-week revisions of `+2512`, `+2408`,
`-386`, `-385`, `-382`, `-225`, at cells where the plausible solution expects
`alpha ~ 421`, `omega ~ 60`.

A jump of +2512 in one week is not 2512 people independently deciding to be
reported. It is an administrative batch correction, and the marked-Poisson
observation model has no way to represent a batch. Its only route to giving one
non-negligible probability is to inflate `mu_t` until the batch sits a few standard
deviations out -- which is exactly what `p -> 0.1` buys, with the retraction stream
added to cancel the inflation in the mean. That is section 8 item 6 of the response
document: revisions reflecting reconciliation or batching that violate the assumed
mechanism.

This explains every other result at once:

* the profile is sharply peaked, not flat -- 272 cells at 31-138 nats each;
* the pairwise likelihood moved `p` only 0.10 -> 0.20, because dependence was never
  the issue;
* `nb_likelihood()` does not help, because a frailty shared across delays within an
  origin cannot make a single *cell* jump;
* the SPA was irrelevant;
* fixing `p` wins on WIS but costs coverage -- it blocks the inflation, and the
  inflation was the model's only cover for the batches.

**It also reprioritises section 19.** The concentration of the gap in batch-like
extreme cells makes revision-process misspecification the leading explanation, so
the exact joint likelihood is no longer the highest-priority diagnostic. It is
*likely* that the joint also prefers low `p` -- no rearrangement of the same cells
into pairs or trajectories obviously rescues a `+2512` the model cannot generate --
but that is an expectation, **not a result**, and it is not known until calculated.
The joint remains useful for measuring how much of the low-`p` preference survives
once the complete trajectory dependence is retained.

### What this means for `p`

The observed retraction total identifies `p` cleanly and the likelihood does not:

| | observed | low-`p` predicts | high-`p` predicts |
|---|---:|---:|---:|
| total down-revisions (Texas) | 2,741 | **295,186** (108x) | 2,975 (+8.5%) |
| share landing by delay 1 | 0.565 | 0.152 | 0.564 |

Across the `p` grid the ratio `expected/observed` down-revisions passes through 1.0
at `p ~ 0.96` -- the empirical value -- while the likelihood peaks at 0.10.

---

## A. The saddlepoint approximation is exonerated

Checked against an independent exact reference, `sum_w Pois(w+z; alpha) Pois(w; omega)`,
which shares no machinery with either production branch. (A Bessel reference would
*not* be independent: the "ascending series" branch in `.log_skellam_increment()`
IS the Bessel series.)

| | max per-cell \|e\| | total over 1470 approximated cells |
|---|---|---|
| low `p` fit | 1.6e-3 nats | −0.41 |
| high `p` fit | 1.4e-3 nats | −0.02 |

| solution | SPA logL | exact logL |
|---|---:|---:|
| low `p` (0.137) | −12211.8234 | −12211.4170 |
| high `p` (0.9572) | −26486.5792 | −26486.5564 |

`delta_SPA = +14274.76`, `delta_exact = +14275.14`. The approximation's net effect
on the comparison is **−0.38 nats against a 14,275-nat gap (0.003%), and in the
direction that DISfavours the low-`p` solution.** Across the whole `p` profile,
`max |rel_SPA - rel_exact| = 0.41` nats over a curve spanning 20,250.

### A.1 A real but immaterial normalisation deficit

Below `alpha + omega ~ 30` the blend is exact to 1e-14. Above it the pmf is
correctly shaped but sums to slightly under 1: worst near rate 50 (1.2e-3 nats),
decaying as ~1/rate (6.2e-4 at 100, 6.2e-5 at 1000). Parameter-dependent, so in
principle an unintended extra likelihood term; measured at 0.003% of the quantity
of interest here. Documented in the roxygen on `.log_skellam_increment()`, not fixed.

### A.2 A trap worth recording

The first version of the exact reference summed over the total `n = A + W` and
truncated at `qpois(1 - eps, alpha + omega)`. That bounds the **marginal** total,
but conditional on an extreme `Delta` the mass sits far further out: the dominant
term is at `w* = (-|z| + sqrt(z^2 + 4 alpha omega)) / 2`, so `n* = |z| + 2 w*`. On a
real cell (`z = 2512, alpha = 1204, omega = 974`) that is `n* = 3317` against a
cutoff of 2528 — **the sum was cut off before reaching its own maximum**, and
understated `logP` by 182 nats. It looked like a catastrophic SPA bug.

Nothing in the usual checks caught it: the pmf still summed to 1, the moments were
still exact, Monte Carlo still agreed. Only the far tail was wrong — and under the
low-`p` solution that is exactly where the likelihood lives. Monte Carlo could not
adjudicate either (probability ~e^-1269, zero hits in 4e7 draws); sweeping the term
budget did.

The reference now sums over `w` centred on `w*`, widening until both edges are 40
nats below the peak. It agrees with the SPA to ~1e-6 nats at 49 standard deviations.

---

## B. The mechanism, quantified

Profile in `p` (fit with `p` fixed at each grid value, all other parameters
optimised):

| `p` | rel logL | `g_D` median (wk) | `g_C` median | `g_C(1)` mass | A_total | W_total | churn |
|---|---:|---:|---:|---:|---:|---:|---:|
| 0.05 | −186.99 | 41.80 | 0.042 | 0.994 | 486,410 | 448,799 | 14.8 |
| **0.10** | **0.00** | **24.76** | **0.046** | **0.994** | **406,405** | **357,051** | **12.0** |
| 0.15 | −44.64 | 13.31 | 0.053 | 0.993 | 329,909 | 275,390 | 9.6 |
| 0.30 | −411.75 | 3.91 | 0.673 | 1.000 | 199,309 | 138,754 | 5.3 |
| 0.60 | −2369.99 | 1.51 | 0.674 | 1.000 | 110,458 | 44,147 | 2.4 |
| 0.90 | −10102.20 | 0.91 | 0.710 | 0.811 | 75,380 | 17,076 | 1.6 |
| 0.96 | −14599.66 | 0.78 | 0.954 | 0.730 | 69,406 | 2,769 | 1.14 |
| 0.99 | −20249.67 | 0.69 | 1.166 | 0.676 | 65,685 | 654 | 1.05 |

At the optimum the model posits **406,405 expected additions and 357,051 expected
retractions** to explain a series whose total absolute movement is ~63,000 — twelve
times more gross flow than net — with **99.4% of `g_C` on lag 1**.

**This is not a flat ridge.** `p = 0.15` is already 44.6 nats down and the empirical
value 14,600. The likelihood is sharply, confidently wrong. That rules out the
"weak identification" reading and points at a missing constraint.

---

## C. The dependence evidence, and why it is weaker than it first looked

Using the identity (user's, proved):

```
Cov(Delta_t^d, Delta_t^e) = -mu_t (1-p) g_D(min{d,e}) g_C(|e-d|),   d != e
```

so for adjacent delays `Cov = -mu_t (1-p) g_D(d) g_C(1)`. With `g_C(1) = 0.9939`
the low-`p` solution requires a sustained strong negative correlation at *every*
delay; the high-`p` solution requires almost none.

**Neither way of estimating the empirical counterpart is clean, and the first
version of this section over-claimed by not saying so.**

* **Residuals** `r = Delta - (alpha - beta)`: contaminated by mean misfit. Under
  the low-`p` fit the mean is badly wrong (a 15-week appearance delay), and
  systematic mean error correlates adjacent residuals positively.
* **Raw increments**: contaminated the other way. The model's covariance is
  *within* an event time, but the estimate is taken *across* event times, where
  adjacent delays both scale with `mu_t`. That induces positive correlation which
  masks the within-`t` negative covariance. The ~0 obtained is the sum of two
  opposing contaminations, not a clean zero.

What survives, taking each solution's residuals under **its own** fitted means (a
goodness-of-fit test per solution rather than one shared estimate):

| | `d = 0` | `d >= 2` |
|---|---|---|
| low `p`: implied | -0.62 | -0.46 at every delay |
| low `p`: empirical | -0.92 | **positive**, +0.44 to +0.99 |
| high `p`: implied | -0.06 | -0.03 to -0.04 |
| high `p`: empirical | -0.92 | ~0 |

The low-`p` solution is contradicted **in sign** at `d >= 2`; the high-`p` solution
is consistent there. Both under-predict `d = 0` by two to four orders of magnitude,
by a mechanism neither captures -- the increments are constrained to sum to the
observed cumulative, which induces negative dependence with no retraction involved.

This discriminates against the low-`p` solution, but as corroboration, not proof.

---

## D. Status of the pairwise experiment

An exact adjacent-pair likelihood was derived by decomposing the reports touching
the pair `(Delta_d, Delta_{d+1})` into five Poisson-thinned types, of which exactly
one — `N1`, the false reports appearing at `d` and retracted at `d+1` — is shared:

```
Delta_d = N1 + N2 - N4,   Delta_{d+1} = -N1 + N3 - N5

lam1 = mu(1-p) g_D(d) g_C(1)                      (+1, -1)
lam2 = mu g_D(d) [1 - (1-p) g_C(1)]               (+1,  0)
lam3 = mu g_D(d+1)                                ( 0, +1)
lam4 = mu(1-p) g_W(d)                             (-1,  0)
lam5 = mu(1-p) [g_W(d+1) - g_D(d) g_C(1)]         ( 0, -1)

P(u, v) = sum_n Pois(n; lam1) Skellam(u-n; lam2, lam4) Skellam(v+n; lam3, lam5)
```

Verified to reproduce both marginals, both variances and `Cov = -lam1`.

### D.1 Three truncation failures, and why they are a finding

The pairwise implementation needed three corrections, all the same shape: a
summation window that did not reach where the mass is.

1. `bin_type = 1` forced into every inner Skellam, when a structurally-zero rate
   must take the Poisson branch (`lam4`, `lam5` are proportional to `(1 - p)` and
   vanish as `p -> 1`). Corrupted the high-`p` end.
2. The window sized from `lam1`'s spread rather than from FEASIBILITY. At `d = 0`
   both `lam4` and `lam5` are structurally zero, so `Delta_1 = -N1 + N3`, and an
   observed `v = -103` requires `n >= 103` -- while the window stopped at 37. Every
   term came back `-Inf` for a cell whose true log-probability is finite (~ -195).
3. In the exact reference (section A.2), truncating at the marginal Poisson
   quantile rather than at the conditional mode.

This is a property of the model, not bad luck. Under the low-`p` solution the
likelihood lives 20-50 standard deviations into the tail -- that is what "406,405
additions and 357,051 retractions nearly cancelling" means arithmetically. Any
window sized by a mean-and-variance heuristic is wrong there, and wrong *silently*:
the pmf still normalises, the moments still check out, and Monte Carlo has nothing
to say at probabilities of e^-1269. The production `.log_skellam_increment()`
avoids series truncation for exactly this reason, and its roxygen already records
that a fixed-length series "silently under-sums once `alpha * beta` grows".

**Any future work here that sums over a latent count needs a feasibility-driven
window, not a moment-based one.**

### D.2 Result

| `p` | rel_marg | rel_pair |
|---|---:|---:|
| 0.05 | -474 | -2906 |
| **0.10** | **0** | -985 |
| **0.20** | -85 | **0** |
| 0.40 | -1017 | -97 |
| 0.60 | -3546 | -2741 |
| 0.90 | -17465 | -15840 |
| 0.96 | -25993 | -24199 |
| 0.99 | -36986 | -35099 |

Marginal optimum `p = 0.10`; pairwise optimum `p = 0.20`, with 0.20-0.40 much
flatter. `g_C(1)` stays at 0.992 at the pairwise optimum and high `p` remains
~24,000 nats down, so their section 19's criterion -- "moves `p` out of the
pathological region and moves `g_C` away from its boundary" -- is **not met**.

**Caveat, and it is not small.** This evaluates the pairwise likelihood along the
`theta(p)` path optimised under the MARGINAL likelihood. A true pairwise profile
would re-optimise the nuisance parameters, and at `p = 0.20` those still include a
`g_D` median of 8.07 weeks. The experiment is suggestive, not conclusive.

---

## F. Steps 5 and 6 of the response document

**Step 5 -- is `P(D_C = infinity) = 0`?** Yes, by construction. The parametric
families are proper on `(0, inf)` and `sum(g_C) = 1.000000` at `conf_D = 15` for any
realistic lag; the only shortfall is observation-horizon truncation (0.03 at
`conf_D = 4` with a long lag). **The implementation assumes every false report is
eventually retracted**, so mature retained fractions are legitimate information
about `p`.

**Step 6 -- finite-follow-up bias of `p_empirical`.** Texas retained fraction by
minimum follow-up age: 1.000 (a=0), 0.9735 (1), 0.9588 (4), 0.9544 (9), 0.9527 (20),
still drifting ~0.0002/step. Asymptote ~0.952 against `p_empirical = 0.9572`, so the
inflation is about **0.005** -- far smaller than the +0.10 previously allowed for.
`p_empirical` is a reasonable estimate of the asymptotic `p` on this data.

## G. A prediction that FAILED

Batch severity does **not** explain why fixing `p` helps Texas and California but
mildly hurts New York and Florida:

| state | max abs(z) | p99 abs(z) | mean abs(z) | share abs(z) > 100 |
|---|---:|---:|---:|---:|
| Texas | 4216 | 2537 | 233.0 | 0.053 |
| California | 3969 | 3408 | 222.5 | 0.045 |
| New York | 2884 | 2535 | 212.3 | 0.037 |
| Florida | 3332 | 2546 | 229.6 | 0.057 |

The four are indistinguishable. The state heterogeneity in the backtest remains
unexplained.

## H. The publication cadence has holes, and that is most of the story

**Steps 1-2 of the v3 response plan.** Deleting influential cells moves the
optimum sharply:

| cells deleted | 0 | 5 | 10 | 25 | 50 |
|---|---|---|---|---|---|
| optimum `p` | 0.10 | 0.20 | 0.20 | **0.60** | 0.60 |

25 of 1576 cells -- 1.6% -- carry the optimum from 0.10 to 0.60. The plan's
acceptance criterion 6 is met emphatically. And the influential cells cluster in
calendar time: every one of the top seven falls in a New Year window
(2024-01-06/13/20, 2025-01-04/11/18/25).

### H.1 The mechanism is a missing snapshot, not a batch

FluSight does not publish every week. **63 snapshots cover 105 event weeks**, with
gaps of 14, 18, 21, 42 and **203** days (the off-season).

```
snapshot cadence, New Year 2025:
2024-12-28   2025-01-11   2025-01-25   2025-02-01
             ^^ 01-04 skipped   ^^ 01-18 skipped
```

Event week 68 (`target_end_date = 2025-01-04`) was FIRST published on 2025-01-11 at
C = 2512. No delay-0 snapshot for it ever existed. But `prepare_data()` builds a
DENSE (event-time x delay) array and zero-fills it, so the model is told:

```
delay 0 : observed zero        <- FALSE, nothing was published
delay 1 : +2512                <- really the FIRST observation
```

**688 of 1576 cells (43.7%) correspond to snapshots that were never published**, all
entering the likelihood as observed zeros.

### H.2 The zeros are a real defect but NOT the cause

Profiling over real cells only still gives `p = 0.10`, by 12,412 nats at `p = 0.96`.
The fabricated zeros contribute about 15% of the gap. **Hypothesis refuted.** What
matters about a missing snapshot is not the zero it invents but what it does to the
interpretation of the neighbouring real cell.

### H.3 The intervals are GIVEN, not detected

The plan's sections 10-17 build a detector: scan `(a,b]`, score the rescue,
calibrate a threshold against the look-elsewhere effect, test endpoint stability,
cross-fit so the detector is not fitted to the batch. All of that infers an unknown
interval.

Here the interval is **known**. The observable delays for an event week are exactly
those carrying a snapshot; consecutive observed delays give `(d_prev, d_next]`. That
collapses sections 10, 14, 15, 16 and 20 -- the intervals partition the horizon, so
every observation is used once by construction and double counting is impossible.

Section 5's unit test passes exactly: `(d-1, d]` reproduces the point model to 1e-12.

The rescue is decisive:

| t | first obs at delay | z | as a point cell | as `(-1, d]` |
|---|---|---|---|---|
| 68 | 1 | 2512 | **97.9 sd** | **3.2 sd** |
| 70 | 1 | 2408 | **95.5 sd** | **2.8 sd** |
| 15 | 0 | 1913 | 0.3 sd | 0.3 sd |
| 17 | 0 | 1773 | -0.0 sd | -0.0 sd |

The two largest outliers -- the top two drivers of the entire pathology -- are not
anomalies. They are first observations after a skipped snapshot, misread as
delay-1 increments. Event weeks whose first snapshot IS at delay 0 are unchanged,
as they must be.

### H.4 Profiling under the interval likelihood

| `p` | dense point cells | cadence intervals |
|---|---:|---:|
| **0.10** | **0.00** | -1041 |
| 0.40 | -811 | -10.6 |
| **0.60** | -2370 | **0.00** |
| 0.90 | -10102 | -3310 |
| 0.96 | -14600 | **-6117** |

Optimum moves 0.10 -> 0.60, the penalty at the empirical value more than halves,
and the shape becomes a broad 0.40-0.60 plateau rather than a sharp wrong peak.
(Levels are not comparable between columns -- 1576 terms vs 888 -- but the profile
shape within each column is.)

**It does not reach 0.96, and the nuisance parameters are still fitted under the
POINT likelihood**: at the interval optimum `g_C(1)` remains pinned at 0.99998.
Every staged evaluation in this investigation has understated the objective being
tested, so a proper refit under the interval likelihood is the honest next step.

### H.4b TRUE refit under the interval likelihood -- it helps, it does not solve it

The H.4 profile was staged: the interval likelihood evaluated along parameters
fitted under the POINT likelihood. Here both objectives get their nuisance
parameters re-optimised under themselves, with an identical treatment so the
comparison is fair -- `lambda_t` saturated (one free value per event week, profiled
out by 1-D optimisation; exact and separable, and more flexible than AR1 so it
cannot handicap either side), outer Nelder-Mead over
`(delay_mu, delay_sigma, retract_mu, retract_sigma)`.

The point arm reproduces `p = 0.10`, which validates the machinery.

| `p` | point | interval | `g_C(1)` interval |
|---|---:|---:|---:|
| 0.10 | **0.00** | -668 | 1.000 |
| 0.20 | -197 | -536 | 1.000 |
| **0.40** | -648 | **0.00** | **0.854** |
| 0.60 | -1907 | -54 | 0.927 |
| 0.90 | -8969 | -2419 | 0.665 |
| 0.96 | -13220 | -4727 | 0.619 |

**Achieved:** optimum moves 0.10 -> 0.40; the sharp peak becomes a broad plateau
(0.40 and 0.60 within 54 nats); `g_C(1)` leaves the boundary (1.000 -> 0.854); the
penalty at the empirical value falls from 13,220 to 4,727 nats.

**Not achieved:** `p ~ 0.4-0.6` is still far from the empirical 0.95.

**A prediction of mine failed here, in the direction I said it would not.** I argued
from the pairwise precedent that the staged evaluation would UNDERSTATE the refit.
It overstated it: staged gave 0.60, the true refit gives 0.40. Re-optimising under
the interval objective lets the nuisance parameters exploit the interval structure
as well, which I had not considered.

**One fit to distrust:** at `p = 0.40` the fitted `g_C` median is 3.2e11 -- the
optimiser has found a degenerate corner. The `p = 0.60` fit beside it is sane
(`g_C` median 0.375, 54 nats worse), so the plateau is real but the precise location
of the optimum within it is not.

**Conclusion.** The publication cadence is a *contributing artefact, not the cause*.
Worth finding -- 43.7% fabricated observations is a genuine defect and it accounts
for roughly a third of the gap on the log scale -- but the preference for low `p`
survives a correct interval treatment.

### H.5 The negative revisions are separate and still unexplained

Event weeks 15-17 have every snapshot present, so their `-386 / -385 / -382` are
genuine ~20% downward recalibrations on three consecutive weeks. The interval
reading does not touch them, and they remain the plan's "negative batch" category.

---

## E. What is NOT yet established

* Whether re-optimising the nuisance parameters **under** the pairwise likelihood
  (rather than evaluating it along the marginal-optimised path) moves the optimum
  to the plausible region. The staged evaluation used here is suggestive, not
  conclusive.
* The exact joint likelihood on short horizons has not been run. The concentration
  of the likelihood gap in batch-like extreme cells makes revision-process
  misspecification the leading explanation, so the full joint is no longer the
  highest-priority diagnostic -- but its result is **not mathematically known until
  calculated**. It remains useful for determining how much of the low-`p`
  preference survives once the complete trajectory dependence is retained.
* Interval undercoverage is untouched and is a separate problem; it must not be
  assumed to follow from fixing this one.
* Why fixing `p` helps Texas and California but mildly hurts New York and Florida.
  Batch severity does not explain it (section G).
* Whether the extreme revisions are temporally coarsened ordinary trajectories or
  separate administrative batch shocks. This is the central question for the next
  phase.

(Two earlier bullets have been removed as stale: the finite-follow-up bias of
`p_empirical` was quantified at ~0.005 on Texas, and `P(D_C = infinity) = 0` was
confirmed by construction. Both are in section F.)

---

## I. Parameter recovery — the estimator is sound; `p` is finite-horizon-confounded with the retraction tail

**2026-09-02.** Scripts `18_param_recovery.R` (Monte-Carlo recovery) and
`19_real_tail_constraint.R` (real-data discriminator). This is the decisive
experiment the whole investigation was building toward: *simulate cumulative
trajectories from the model's own generative process, on the real FluSight
publication cadence and the corrected interval observation mechanism, then refit
`p` and ask whether it comes back.*

The estimator (`vskel` / `q_tab` / interval likelihood with `lambda_t` profiled
out) is the one from `17_true_interval_refit.R`; script 18 profiles `p` with the
nuisance parameters `(g_D, g_C)` re-optimised under the interval objective, using
a vectorised two-stage grid for the per-event-time `lambda_t` (validated to
reproduce the exact per-`t` `optimize()` profile to a few nats, peak at `p = 0.92`
on the reference dataset). Warm-start continuation across the `p` grid removes the
Nelder-Mead local-optimum jaggedness.

### I.1 The generative model and the three retraction-tail scenarios

Per event time `t`: `N_t ~ Poisson(lambda_t / p)` reports, `lambda_t` = the real
Texas mature (fully-retained) cumulative; each report gets a reporting delay
`D_rpt ~ g_D`, is true w.p. `p`, and a false report gets a retraction lag
`D_C ~ g_C`; it is present at delay `d` iff `D_rpt <= d` and
(`true` or `D_rpt + D_C > d`). Observe the cumulative at the real snapshot delays;
form cadence intervals; refit. `g_D` fixed (mean 0.8 wk). `g_C` varied:

| scenario | retraction tail | `Gbar(horizon)` = P(retract beyond `cD`) |
|---|---|---:|
| A | short (mean 1.5 wk) | ~0.000 |
| B | moderate (mean 6 wk) | 0.035 |
| C | long (mean 30 wk) | **0.799** |

The survival curve `Gbar_C(a)` over the *whole distribution of observed follow-up
ages* is the relevant object -- not the single scalar `Gbar_C(horizon)`, which is
why bounding that scalar alone does not help (section I.7 B): the model just moves
retraction to late within-horizon ages that young cohorts have not reached. At
follow-up age `a` a report is still present with probability
`r(a) = p + (1-p) Gbar_C(a)`, and finite cumulative data observe `r(a)`, not `p`.
When `Gbar_C` is negligible over all observed ages the retained fraction *is* `p`;
otherwise many `(p, g_C)` pairs give the same retention curve.

### I.2 Result — recovery (R = 12 replicates per cell, real Texas cadence)

| scenario | `Gbar(h)` | `p_true` | `r(h)` | mean `p_hat` | bias | sd |
|---|---:|---:|---:|---:|---:|---:|
| A short | ~0 | 0.60 | 0.600 | **0.583** | −0.017 | 0.001 |
| A short | ~0 | 0.95 | 0.950 | **0.970** | +0.020 | 0.000 |
| B moderate | 0.035 | 0.40 | 0.421 | 0.380 | −0.020 | 0.001 |
| B moderate | 0.035 | 0.60 | 0.614 | 0.544 | −0.056 | 0.007 |
| B moderate | 0.035 | 0.80 | 0.807 | 0.717 | −0.083 | 0.006 |
| B moderate | 0.035 | 0.95 | 0.952 | **0.917** | −0.033 | 0.009 |
| C **long** | **0.799** | 0.60 | 0.920 | **0.150** | **−0.450** | 0.000 |
| C **long** | **0.799** | 0.95 | 0.990 | 0.833 | −0.117 | **0.215** |

**Read three things off this table:**

1. **The estimator is not broken.** `p_true = 0.95 -> p_hat = 0.92-0.97` whenever
   retraction lands within the horizon (A, B). It never collapses to 0.1-0.4 when
   the data actually come from a high-`p` model with a short/moderate tail. This
   **rejects decision-tree Result A** (an intrinsic identification/implementation
   defect). The single most important question — *does `p_true = 0.95` come back?*
   — is answered **yes**, conditional on the tail.

2. **A long tail confounds `p`, and reproduces the exact pathology.** Scenario C,
   `p_true = 0.60 -> p_hat = 0.150`: the estimator collapses to the real-FluSight
   low-`p` value even though the truth is 0.60. And note `r(h) = 0.920` there — a
   retained-fraction-at-horizon that *looks like* the real Texas ~0.95 — yet
   `p_hat = 0.15`. At `p_true = 0.95` the long tail instead makes `p_hat`
   *unstable* (sd 0.215, ranging across the whole upper half of the interval): the
   ridge is flat, so `p_hat` wanders. This **confirms decision-tree Result C**:
   finite-horizon confounding `p <-> Gbar(a)` is real and by itself sufficient to
   produce the low-`p` collapse.

3. **A small, systematic downward bias** (≤0.08) exists even when identified
   (scenario B). Minor; candidate causes are the marginal-vs-joint information loss
   (section B/199) and the saddlepoint normalisation deficit (section A.1). Not the
   subject of this experiment.

### I.3 The real-data discriminator (script 19)

Simulation shows the mechanism *can* produce low `p`; script 19 asks whether the
**real** Texas data are actually in the confounded regime, by profiling `p` on the
real interval data under a free vs. a pinned-short retraction tail:

| retraction tail | `Gbar(horizon)` | optimum `p` on real Texas |
|---|---:|---:|
| **free** (optimised) | large (long fitted tail) | **0.50** |
| pinned short | 0.0000 | **0.55** |
| pinned short | 0.0004 | **0.70** |

Removing the model's freedom to place retraction mass **beyond** the horizon lifts
the optimum from ~0.50 to ~0.70. So the confounding is **active on the real data**:
the free fit buys a lower `p` by positing a long *unobserved* retraction tail that
the finite horizon cannot refute. (The observed within-horizon down-revisions are
fast — 56.5% by delay 1 — which pins `g_C(1)`, but says nothing about `Gbar(h)`,
and `Gbar(h)` is what confounds with `p`.)

But pinning the tail short stops at ~0.70, **short of the mature-cohort 0.95**. So
finite-horizon confounding explains the move `0.50 -> ~0.70`; the residual
`0.70 -> 0.95` is consistent with what section H.5 isolated — the negative
administrative batches (event weeks 15-17, fully-published, ~20%/wk down) that no
retraction *timing* can represent. **Both mechanisms are present; confounding is
the larger and the one that was mis-attributed.**

### I.4 What this settles, and what it means for production

* **"How does the model know what `p` is?"** From finite-horizon count-cumulative
  data alone, **it does not** — not separately from `Gbar(a)`, the mass of the
  retraction distribution beyond the observation horizon. The likelihood identifies
  `r(a) = p + (1-p)Gbar(a)`, and only external information about the tail (or about
  `p`) resolves the two.
* This is the **principled justification** for the production default
  (`default_priors()` fixes `p` at the empirical mature-cohort down-revision rate
  for count-cumulative). Handoff §B.5 framed it as "a guard against a misspecified
  revision model." That is now demoted to the *residual*; the primary reason is
  **non-identifiability of `p` from finite-horizon cumulative data** — a cleaner,
  stronger statement for the vignette (`Mathematics` §9.4) and the paper. The
  mature-cohort asymptote (~0.952, section F) is exactly the external tail/`p`
  information the likelihood lacks.
* This validates the response document's step 13: prefer an informative prior on
  `p` (or a constraint on the `g_C` tail, or joint mature/recent-cohort learning)
  to a hard `p = p_empirical`, so that uncertainty in `p` propagates into the
  nowcast. Fixing `p` is the pragmatic current default; a tail-constrained or
  weakly-informative-prior formulation is the principled successor.
* **Do not** now attribute the low-`p` collapse primarily to administrative
  batches (the pre-2026-09-02 Conclusion). The batches are real (H.5) and account
  for the residual `~0.70 -> 0.95`, but the dominant driver is confounding.

### I.5 Decision-tree verdict (response document §14)

| result | verdict |
|---|---|
| A — count-cumulative cannot identify `p` under the parameterisation | *partially*: it **can** when the tail is within-horizon; cannot when the tail runs past it |
| B — real revision process violates the generative model | **residual yes** — the negative batches, `~0.70 -> 0.95` |
| C — finite-horizon confounding `p <-> Gbar(a)` | **CONFIRMED — the primary cause** |
| D — deleting administrative negatives moves `p` to ~0.9 | not yet run in isolation (H.5 / handoff §B.4); now expected to close only the residual |
| E — none of the above | rejected |

### I.6 Open, in priority order

1. Isolate the residual: delete only event weeks 15-17 from the interval fit and
   re-profile (handoff §B.4). Expected to move the *pinned-short* optimum from ~0.70
   toward ~0.9, confirming the negatives are the residual `B` term.
2. Prototype the principled successor to fixing `p`: a weakly-informative prior on
   `logit(p)` centred on the mature-cohort value, or an explicit constraint that
   `Gbar(horizon)` be small, and check nowcast calibration against the hard-fix
   default.
3. The estimator's small downward bias (I.2 item 3) and interval undercoverage
   (section E) remain separate, lower-priority.

**Caveats.** R = 12 per cell (bias is precise, sd tiny because each 93-event-time
dataset pins `p_hat` sharply); the profile CI from the peak parabola is
anti-conservative here (coverage ~0) because the likelihood is so sharply peaked
that the ~0.02-0.08 finite-sample bias exceeds the 1.92-nat half-width — a property
of the CI construction, not of recovery. Scenarios A/B/C bracket the tail; the real
`g_C` tail is unknown by construction (that is the whole point).

### I.7 Follow-ups: the residual is diffuse; the successor needs tail-TIMING info

**Isolating the residual (`21_delete_negbatch.R`).** The three worst negatives are
event times 15/16/17 (2023-12-24/31, 2024-01-07; `-386/-385/-382`), confirmed by
`min(z)`. Deleting only those three, tail pinned short, lifts the optimum
`p = 0.648 -> 0.753` — a real +0.10, so the negative batches **are** a residual
term. But it stops at 0.75, not 0.9: the down-revisions are **diffuse** (weeks
11-16 and 75 all carry them), not three outliers. So the residual `0.75 -> 0.95` is
finite-horizon confounding plus *distributed* administrative recalibrations, not a
handful of deletable cells.

**Prototyping the successor (`22_prototype_successor.R`), real Texas, free tail.**

| lever | result |
|---|---|
| A. prior `logit(p) ~ N(logit 0.95, s^2)` | MAP: s=1.0→0.497, 0.5→0.499, 0.25→0.506, 0.10→0.558, 0.05→0.685 |
| B. bound `Gbar(horizon) <= tau` | p=0.550, flat over tau∈{0.30,0.10,0.02} |

* **A is a weak lever.** The free likelihood is confidently wrong (`p=0.5` is
  thousands of nats above `0.95`), so a weakly-informative prior is overwhelmed;
  only a near-delta prior (≈ the current hard fix) moves `p`, and even `s=0.05`
  reaches just 0.69. A weakly-informative prior on `p` does **not** yield ~0.95.
* **B (bounding beyond-horizon mass) is insufficient.** It gives 0.55 regardless of
  `tau`, because the model can still place retraction at **late within-horizon**
  lags that young cohorts never observe — that confounds too. Identifying `p`
  requires the tail constrained **short in shape** (pinned-short gives 0.65-0.75,
  scripts 19/21), i.e. external retraction-**timing** information, not just a bound
  on the tail total.

**Consequence for the successor.** Neither a weak `p`-prior nor a `Gbar(h)` bound
recovers 0.95. What works: external retraction-timing (a short/known `g_C`) or the
hard fix. So the production hard-fix is closer to **necessary** than hoped; the
principled improvement path is (i) an informative `g_C` from external
retraction-timing data, and (ii) a **separate** model component for the diffuse
negative administrative recalibrations (their shared snapshot-level pattern is
difficult to reconcile with independent individual retractions alone; some
administrative audit could in principle trigger many legitimate retractions at
once, so the modelling fact is the shared snapshot dependence, not the label put
on each removed record). A prior on `p` alone is not the answer.

---

## J. The `b_c` reparameterisation — `p` is *exactly* flat, and all its curvature came from the lognormal `g_C`

**2026-09-02, after the reviewer note.** Script `25_bc_reparam.R`, following
sections 1/2/7/28/30.1 of *Reviewer corrections and recommendations*. This
replaces the earlier informal statement that `p` is "weakly identified" with an
exact one.

### J.1 The reparameterisation

Carry the retraction masses the data can see,

```
b_c = (1 - p) g_C(c),      Bcum(m) = sum_{c<=m} b_c = (1 - p) G_C(m),
```

so the age-`a` retention probability is

```
r(a) = p + (1 - p) Gbar_C(a) = 1 - Bcum(a),
```

and the cadence interval rates lose `p` entirely,

```
alpha_t(a,b) / mu_t = sum_{r=a+1}^{b} gD(r) [1 - Bcum(b-r)]
omega_t(a,b) / mu_t = sum_{r=0}^{a}   gD(r) [Bcum(b-r) - Bcum(a-r)]
```

*provided* the epidemic scale is carried by the gross report rate `mu_t` and not
by `lambda_t = p mu_t` — which is reviewer section 7's correction, and the reason
earlier attempts to "sidestep `p` by changing the target" did not actually remove
it. Only `b_1..b_H` for `H = max` delay enter; `p = 1 - sum_{c=1}^{inf} b_c`
additionally needs the mass beyond `H`, which no snapshot has had time to show.

A useful implementation identity: `q_pairs(gD, b, p = 0, a, b)` **is** the
`b`-form — feed `b` in place of `g_C` and set `p = 0`. Verified against an
independently written interval-by-interval implementation.

### J.2 Numerical equivalence (part A)

`b`-form vs the `(p, g_C)` code path of script 23, on the real Texas `(a,b)`
pairs, for `p` in {0.15, 0.60, 0.95} x three retraction tails x three arms
(`H` = 15, 111, 196):

```
max |d alpha| <= 1.5e-14,   max |d omega| <= 1.5e-14
```

i.e. machine precision. The two parameterisations are the same model.

### J.3 `p` is exactly flat once the tail beyond `H` is free (part B)

Hold `b_1..b_15` fixed at a reference vector with `sum b = 0.1000`. Every
`p <= 1 - sum b = 0.900` is then reachable by parking the leftover retraction
mass `(1-p) - 0.1000` at lags `> H`, where no observation touches it. Profile
through the **original** `(p, g_C)` code path:

| `p` | free tail | lognormal `g_C` (script 23) |
|---:|---:|---:|
| 0.15 | 0 | −626.1 |
| 0.30 | 0 | −305.0 |
| 0.45 | 0 | **0.0** |
| 0.60 | 0 | −15.5 |
| 0.75 | 0 | −545.3 |
| 0.85 | 0 | −1409.3 |
| 0.92 | infeasible | −2742.7 |
| 0.97 | infeasible | −5256.0 |

Range of the free-tail profile: **0.00e+00 nats**. Not "flat to within tolerance"
— identically constant, because the likelihood is a function of `b` alone.

**Therefore:** the profile likelihood in `p` is exactly constant on
`(0, r(H)]` and decreasing above it (raising `p` above `r(H)` shrinks the feasible
set `{b >= 0, sum_{c<=H} b_c <= 1 - p}`). The data give an **upper bound**
`p <= r_hat(H)` and nothing else. Every one of the 5,256 nats separating
`p = 0.45` from `p = 0.97` in script 23 is contributed by the lognormal family
extrapolating observed early retraction behaviour into the unobserved tail — it
is *parametric* identification, not data identification (reviewer section 3).

This retires the row "Weak / flat identification — **No**, the profile is sharply
peaked" in briefing section 7. The parametric profile is sharply peaked; the
likelihood is flat.

### J.4 Empirical support by lag (part C, reviewer section 9)

Number of the 904 `w15` interval observations whose rate involves `b_c`:

| lag | 1 | 2 | 4 | 8 | 12 | 15 |
|---|---:|---:|---:|---:|---:|---:|
| `n_risk` | 761 | 738 | 654 | 420 | 202 | **49** |
| weight share | 1.00 | 0.98 | 0.94 | 0.85 | 0.71 | **0.30** |

Support thins by a factor of 15 from lag 1 to lag 15. Forecast validation should
be reported *as a function of target age*, not as "inside the horizon" — the last
few within-horizon lags are nearly as data-poor as the tail.

### J.5 The 71 + 97 vs 197 arithmetic (reviewer section 12)

Resolved. Texas has 197 event weeks, all of which have at least one published
row. The decomposition depends on how the delay is rounded to weeks:

| first published at | round() | floor() (used by scripts 23+) |
|---|---:|---:|
| delay 0 | 60 | 64 |
| delay <= 1 | **71** | 75 |
| delay 2..8 | **29** | 36 (delay 1..8) |
| delay > 8 (left-censored) | **97** | 97 |

So `71 + 29 + 97 = 197`. The briefing's "71" is the `round()`-based count of
event weeks first seen by delay 1; the 29 unexplained weeks are those first
published 2 to 8 weeks after the event — neither prompt nor left-censored.

---

## K. The retention curve is not the problem — the increment law is

**2026-09-02.** Scripts `26_free_retention.R` (fit `mu_t, g_D, b_1..b_15`
directly, reviewer sections 7/8/28/30.2) and `27_increment_law.R` (why the fit
looks the way it does). Section J removed `p` from the likelihood; this section
fits what is left, and the result redirects the whole programme.

### K.1 The nonparametric retention fit changes almost nothing

Texas, `w15` arm (99 event weeks, 904 intervals, `H = 15`). Stage 1 pools the
masses into five blocks and optimises by Nelder-Mead; stage 2 frees all 15 and
refines by BFGS (Nelder-Mead stalls badly — the four block starts spread over
200 nats, the BFGS starts over 120).

| model | parameters for `b` | logL |
|---|---:|---:|
| lognormal `g_C` + `p` (script 23) | 3 | −3065.20 |
| free `b_1..b_15` (script 26) | 15 | **−3055.41** |

**9.8 nats for 12 extra parameters.** Removing every parametric restriction on
the retraction timing buys essentially nothing, and lands on the same shape:

```
b_1 = 0.541   b_2 = 0.000   b_3 = 0.025   b_4..b_15 < 0.007 each
r(1) = 0.459  r(2) = 0.459  r(4) = 0.428  r(8) = 0.414  r(15) = 0.4078
g_D: mean 1.79 wk, P(0) = 0.399
```

So `r_hat(H) = 0.408`: by section J the profile in `p` is flat on `(0, 0.408]`
and falls above it. Over half of all reports are retracted at lag exactly 1.

**This retires the parametric-family explanation.** The low-`p` solution is not
the lognormal extrapolating; a completely free retraction-mass vector chooses the
same thing.

### K.2 It is not a retention curve — the flows are invented

Profiling `mu_t` back out at the fit:

| quantity | value |
|---|---:|
| model expected gross additions | 133,092 |
| model expected gross retractions | 74,206 |
| observed net up-movement | 61,636 |
| observed net down-movement | 2,741 (a *lower bound* on gross retraction) |
| **invented churn** `E_ret / obs_dn` | **27x** |
| model net vs observed net | 58,886 vs 58,895 |

The net is matched to 0.02%. The gross flows are fiction.

### K.3 Why: the data are 77% exact zeros with occasional large jumps

The empirical trajectory (script 27 part A) is **flat**: mean
`C_t(a) / C_t(last)` runs 1.012 at age 0, 1.020 at age 1, back to 1.000 by age 9.
Reporting is essentially **complete at the first snapshot**, and what follows is a
~2% net down-drift. The 805 increments after the first observation:

| | |
|---|---:|
| mean | 0.560 |
| variance | 1634.6 |
| var / \|mean\| | ~2,900 |
| range | −386 .. 615 |
| **exactly zero** | **620 (77%)** |
| \|z\| > 50 | 27 (3%) |

A `Skellam(alpha, omega)` has mean `alpha - omega` and variance `alpha + omega`.
Mean 0.56 with variance 1635 requires gross flows of ~1635 per interval — but at
`alpha + omega = 1635` the probability of an increment being *exactly* zero is
negligible, and 77% of them are. **The two requirements are incompatible.**

Predictive check at the script 26 fit (part C):

| | observed | model |
|---|---:|---:|
| increments exactly zero | 620 (77%) | **85 (11%)** |

Fitted `alpha + omega` ~ 185 (median 50, 95th pct 837): the optimum is a
compromise, churn pushed up until the zeros cost more than the jumps gain —
9x short of the dispersion the jumps need, 7x too few zeros.

### K.4 The synthesis

`p` is the only dispersion dial the Poisson-Skellam increment law has. Lowering
`p` raises `alpha + omega` at fixed net, so **every** dispersion source in the
data is absorbed by pushing `p` down: administrative revisions, missing
snapshots, and genuine retraction alike. This explains, in one mechanism, why
each earlier lever failed —

* a weak prior on `p` (I.7 A) is fighting the dispersion the data really have;
* bounding `Gbar_C(h)` (I.7 B) constrains *timing*, not the dispersion dial;
* deleting administratively-flagged snapshots (briefing 8.5) works — `p_hat`
  0.804 / 0.970 — precisely because it removes the jumps that were driving churn;
* freeing `b` nonparametrically (K.1) cannot help, because the retention
  parameterisation was never the binding constraint;
* the composite likelihood is not implicated: this is the *marginal* law of a
  single interval failing, before any dependence question arises.

**This reorders reviewer section 30.** Item 4, an increment law that can be
exactly zero most of the time and occasionally jump (section 17's sparse
contamination mixture, or a zero-inflated / gamma-frailty SkNB increment), is not
a refinement to be done after the finite-horizon reparameterisation — it is the
binding constraint. Items 5-9 (mature refit, cross-state pooling, expanded
recovery, composite reassessment, uncertainty calibration) are all downstream of
it: each currently measures a model whose marginal increment law is rejected by a
simple count of zeros.

### K.5 Caveats

* One state (Texas), one arm (`w15`, `H = 15`). The zero fraction and jump
  structure should be checked on the other states before generalising.
* The briefing's earlier "NB gives a virtually identical profile" was measured
  under the dense-grid formulation with fabricated zeros, not under the interval
  model. A gamma-frailty SkNB increment is a *different* test now and has not
  been run — it is the obvious first candidate, but note that a frailty scales
  both flows and may not produce exact zeros either. Zero-inflation, or section
  17's snapshot-level mixture, is the more targeted fix.

---

## L. Stage 0 — the structure generalises, but three design assumptions were wrong

**2026-09-02.** Script `28_generality.R`, stage 0 of `PLAN_increment_nowcasting.md`.
All 53 FluSight locations, `w15` arm, 42,606 post-first increments.

### L.1 The zero/jump structure generalises, and Texas is on the *active* end

| | |
|---|---|
| zero fraction | median **91%**, range 27-98% (Texas 77%, US aggregate 27%) |
| var / \|mean\| | median **54** (Poisson would be 1) |

Texas is one of the most active states, so K.3's 77% zeros understates the
problem elsewhere. The hurdle is needed everywhere. **Gate passed.**

### L.2 Participation is driven by cohort AGE, not by snapshot sparsity

This is the correction that matters most, and it inverts the plan's premise.

| cohort age | 1 | 2 | 3 | 4 | 8 | 15 |
|---|---:|---:|---:|---:|---:|---:|
| % of cells moving | **58.1** | 25.4 | 16.2 | 11.9 | 5.5 | **2.3** |

and the decay is consistent across locations (median 56.6% at age 1 with
q10-q90 32-85%; 0.0% at age 15 with q90 4.2%).

The snapshot concentration that motivated a sparse `pi_s` is largely an
artefact of **season**: the top snapshots by movement *mass* are all Jan-Mar
2025, when counts are largest. Pooled participation *rate* across snapshots
ranges only 0.102 (median) to 0.473 (max) — a 5x ratio, with just **1 of 62**
snapshots above 3x the median.

There is a real shared calendar component, but a modest one: mean pairwise
cross-state correlation of the per-snapshot participation rate is **0.213**
against a null of 0.000 (sd 0.003, 200 within-state permutations; z = 62). So
plan risk 1 fires *partially* — a national snapshot effect belongs in the model,
but as a second-order term. `pi` is primarily a function of age.

### L.3 Small moves are additive, large moves are multiplicative

| cells with median \|move\| > | n | CV absolute | CV relative | relative tighter |
|---|---:|---:|---:|---:|
| 0 | 232 | 1.16 | 1.21 | 44% |
| 10 | 84 | 1.02 | 1.01 | 56% |
| 25 | 33 | 1.05 | 0.88 | 52% |
| 100 | 6 | 1.01 | **0.56** | **67%** |

Briefing 8.3's "proportional changes are much tighter than absolute" holds only
for the large administrative corrections. Ordinary small revisions are additive.
So the revision scale needs **both** terms, `lamR = sigma0 + sigma1 * level` —
not the pure multiplicative `kappa_s` of reviewer section 16.

### L.4 Arrivals and revisions separate by age

| age | % up | % down | median up | median down | net |
|---|---:|---:|---:|---:|---:|
| 1 | 46.7 | 11.4 | +7 | −3 | +53,770 |
| 2 | 20.9 | 4.5 | +4 | −3 | +10,282 |
| 8 | 3.8 | 1.7 | +3 | −8 | +974 |
| 15 | 1.2 | 1.1 | +2.5 | −5 | −266 |

Early movement is **arrivals** (positive, large net); late movement is
**revision** (rare, balanced, with a heavier negative median). The two
components separate cleanly by age, which means the retraction curve `b_c` is
not needed at all: down-moves belong to the revision component, and the arrival
component can be a plain `mu_t G_D`. This is a simplification, not a loss —
section J already showed `b_c` only ever bought a bound on `p`.

### L.5 Revised specification, carried into script 29

```
alpha_t(a,b) = mu_t [G_D(b) - G_D(a)]                    arrivals, no retention
pi(b)        = plogis(p0 + p1 log(1+b))                  participation, age-driven
lamR_t(a)    = sigma0 + sigma1 mu_t G_D(a)               additive floor + multiplicative

Delta ~ (1-pi) Poisson(alpha) + pi Skellam(alpha + psi lamR, lamR)
```

with `pi = 0` on the first interval. A national snapshot effect on `pi` (L.2) is
deferred to stage 2 rather than dropped.

**Numerics note.** The prototype uses the *exact* Skellam pmf via the scaled
Bessel form, verified to 1e-13 against direct Poisson convolution over the whole
range of rates in play (z up to ±615, rates to 1200), at 2.6 ms per 900 cells.
The saddlepoint is not safe in a mixture, where the weights depend on relative
densities of two components with very different rates.

---

## M. Stage 1 — the hurdle works; the revision magnitude law is still wrong

**2026-09-02.** Scripts `29_hurdle.R` (one revision scale), `30_two_scale.R`
(two), `31_psi_age.R` (age-dependent asymmetry, running). Texas `w15`,
99 event weeks, 904 intervals of which 805 are post-first.

### M.1 A likelihood bug that invalidated the first pass — and why it survived

`lskel` implemented the Skellam log-pmf as

```
log f(z) = -(a+b) + (|z|/2) log(a/b) + log I_|z|(2 sqrt(ab))       WRONG
log f(z) = -(a+b) + ( z /2) log(a/b) + log I_|z|(2 sqrt(ab))       right
```

The Bessel *order* is `|z|`; the exponent on the rate ratio is `z`. The error
bites only when `z < 0` **and** `a != b`.

It survived a verification that agreed with direct Poisson convolution to 1e-13,
because that check used either `a = b` (where the error cancels identically) or
`z >= 0`. The check that catches it instantly is **summing the pmf**: it came to
1.4858 instead of 1. Both checks — randomised `(z, a, b)` over both signs with
`a != b`, and the unit-sum — are now in the scripts.

A second bug, separate: `log_besselI` indexed `x` with a logical mask, so a
scalar rate against a vector `z` returned `NA` past the first element. That only
ever hit the gate code (the fit always passes equal-length vectors), which is why
gates 2-3 crashed while the fit ran happily on wrong numbers.

**Everything below is post-fix.**

### M.2 The model

```
alpha_t(a,b) = mu_t [G_D(b) - G_D(a)]                   arrivals
pi(b)        = plogis(p0 + p1 log(1+b))                 participation
lam_s        = sigma_s                                  small revision, additive
lam_L        = sigma_1 mu_t G_D(a)                      large revision, multiplicative

Delta ~ (1-pi) Poisson(alpha)
      + pi(1-q) Skellam(alpha + psi lam_s, lam_s)
      + pi q    Skellam(alpha + psi lam_L, lam_L)
```

Skellam pmf exact (scaled Bessel, log-series once order > argument). `mu_t`
profiled by the two-stage grid used since script 18. `pi = 0` on the first
interval.

### M.3 Gates

| model | par | logL | zeros (obs 620) | \|z\|>50 in 95% PI | PIT KS p |
|---|---:|---:|---:|---:|---:|
| lognormal `g_C` + `p` (23) | 5 | −3065.20 | — | — | — |
| free retention `b_1..b_15` (26) | 17 | −3055.41 | 85 (11%) | — | — |
| hurdle, one scale (29) | 7 | −1612.19 | 558 (69%) | 11/27 (41%) | 5e-4 |
| hurdle, two scales (30) | 8 | **−1521.75** | **609 (76%)** | 15/27 (56%) | 1.6e-4 |

**Gate 1 passed** (609 against 620 observed; the model this replaces managed 85).
**Gate 4 passed**: +1534 nats over free retention with half the parameters. This
is the quantitative form of K.4 — the increment law, not the retention curve, was
the binding constraint.

**Gates 2 and 3 fail.** They are not yet a working nowcaster.

### M.4 What the fitted parameters say

```
g_D    : P(delay 0) = 0.998            reporting is complete at first publication
pi(a)  : 0.707 at age 1 -> 0.037 at 15  (stage 0 empirical: 0.58 -> 0.023)
q      : 0.411 of revisions are large
sigma_s: 15.64          small component sd ~5.6  (observed median move 3-5)
sigma_1: 5.7555 x level large component
psi    : 1.001          revisions are SYMMETRIC, not net-downward
```

Two of these matter beyond the fit. `g_D` collapsing to instant is the truth from
section 27 part A — the old model was inventing a 1.79-week reporting delay
purely so that retraction had something to cancel. And `psi = 1.001` says
post-publication movement is *symmetric*, which contradicts the premise the
entire original model was built on: that it is retraction.

The scale split also vindicates L.3, which M's first draft had written off. The
additive term was real; script 29 put it in the wrong place, as a floor inside a
single `lambda` (where it fitted to exactly 0) instead of as the scale of its own
component.

### M.5 The two failures, and what they point at

**Gate 3's shape moved diagnostically between the two fits:**

| | decile 1 | ... | decile 10 |
|---|---:|---|---:|
| one scale | 0.076 | body-heavy, both ends thin | 0.073 |
| two scales | 0.089 | rising | **0.138** |

One scale was *depleted at both ends* — predictive too wide in the body. Two
scales fixed that and left a PIT **rising toward 1**, which means observations
sit in the upper tail of the predictive too often: the predictive is centred too
**low**.

That is precisely the drift stage 0 table L.4 records and the model cannot yet
produce — at age 1, 46.7% of cells move up against 11.4% down; by age 15 it is
1.2% against 1.1%. Movement is strongly positive when young, symmetric when old.
Neither existing component can supply it:

* **arrivals cannot**, because `g_D` has `P(0) = 0.998`. Giving `g_D` mass at
  delay 1 adds `alpha` to *every* age-1 cell including the 42% that are exactly
  zero, and gate 1 forbids that;
* **the revision cannot**, because `psi` is one constant. Raising it tilts every
  revision at every age upward, when the drift is concentrated at ages 1-2.

Hence script 31: `psi(a) = exp(v0 + v1 log(1+a))`, one parameter, everything else
held at script 30 so the change is attributable.

**Gate 2 is a separate problem.** 56% coverage against a nominal 95% means the
tail is still far too thin: the largest observed moves reach ±615, while
`lam_L = 5.76 x level` gives sd ~107 for a cohort of 1000 — a 5.7-sigma event.
A discrete two-point scale mixture is a crude stand-in for a continuous one. If
`psi(a)` does not incidentally fix this by freeing the large component to widen,
the next step is a gamma frailty on `lam_L` — i.e. the large revision becomes
SkNB rather than Skellam, which is machinery the package already has in
`R/28_confirmation_likelihood.R`.

### M.6 Status against the plan

Stage 1 is **not passed**. Gates 1 and 4 pass; 2 and 3 do not. Per the plan's own
rule, stage 2 (out-of-sample WIS by target age) does not start until they do —
and every number in M.3 is in-sample, establishing only that the hurdle can
*represent* what the retention model provably could not.

### M.7 A process note

Background jobs launched with `nohup ... &` were repeatedly killed and restarted,
silently discarding roughly 40 minutes of fitting and, worse, producing a log
that looked like a running fit. Foreground calls with an extended timeout plus a
**per-start disk checkpoint** fixed it; a killed run now costs one start instead
of all of them. Any future long fit here should checkpoint.

---

## N. Stage 1 PASSED — sign and magnitude have to be modelled separately

**2026-09-02.** Script `32_sign_magnitude.R`. Texas `w15`, 805 post-first
increments.

### N.1 Why the Skellam family had to go

Script 31 tested the obvious fix to M.5's diagnosis — an age-dependent
asymmetry `psi(a)` — and it **failed informatively**: 0.40 nats for the extra
parameter, with `psi` returning 1.002 / 0.999 / 0.995 at ages 1 / 4 / 15. The
likelihood does not want an age tilt, so the obstruction was structural.

A `Skellam(alpha + psi lam, lam)` has mean `alpha + (psi-1)lam`, and the *same*
`lam` fixes both the spread and the balance of signs. The Texas non-zero moves
demand two incompatible things of it:

```
144 up (78%) vs 41 down (22%)          strongly asymmetric in SIGN
|move| median 5, max 615 (ratio 123)   enormous in RANGE
```

Covering +-615 forces a large `lam` (the fit chose 5.65 x level); at
`lam = 56,500` for a cohort of 10,000, moving `P(up)` from 0.50 to 0.78 needs
`psi ~ 1.02`, which shifts the mean by ~1,130 counts. **Gates 2 and 3 were never
two failures. They were one**, and no amount of mixing Skellams fixes it.

### N.2 The model that passes

```
pi(a)    = plogis(p0 + p1 log(1+a))              does this cohort move?
theta(a) = plogis(t0 + t1 log(1+a))              if it moves, is the move UP?
log M    ~ Normal(log kappa + beta log(level), s) discretised on {1,2,...}

Delta = 0  w.p. 1-pi(a) ;  +M w.p. pi(a) theta(a) ;  -M w.p. pi(a)(1-theta(a))
```

First interval stays `Poisson(mu_t G_D(b))` and is what identifies `mu_t`.
No Bessel anywhere — `pnorm` only — so it is **~50x cheaper** than 29-31: three
starts converge in 63 s where script 30 needed 10 minutes for one, and all three
land on the identical optimum.

### N.3 Gates

| model | par | logL | zeros (obs 620) | PIT KS p |
|---|---:|---:|---:|---:|
| lognormal `g_C` + `p` (23) | 5 | −3065.20 | — | — |
| free retention (26) | 17 | −3055.41 | 85 (11%) | — |
| hurdle, one scale (29) | 7 | −1612.19 | 558 | 5e-4 |
| hurdle, two scales (30) | 8 | −1521.75 | 609 | 1.6e-4 |
| psi(a) two-scale (31) | 9 | −1521.35 | — | — |
| **sign x magnitude (32)** | **9** | **−1500.94** | **620 (77%)** | **0.096** |

Tail, as a posterior predictive check:

| threshold | observed | expected | ratio |
|---|---:|---:|---:|
| \|z\| > 20 | 47 | 43.7 | 1.08 |
| \|z\| > 50 | 27 | 22.4 | 1.20 |
| \|z\| > 100 | 17 | 12.2 | 1.39 |
| \|z\| > 300 | 5 | 3.9 | 1.29 |

PIT outside the central 95%: **4.7%** against a nominal 5%.

**All four gates pass.** The tail is still under-predicted by 20-40% in the
middle of the range, which is worth watching but is not a rejection.

### N.4 Gate 2 was mis-specified, and the earlier numbers for it are void

The original gate 2 asked: *of the cells with `|z| > 50`, what fraction land
inside the central 95% predictive interval?* — and scored 41% (script 29), 56%
(30), 11% (32) against a nominal 95%.

**That is not a valid test.** Conditioning on the *observation* being extreme
selects cells whose PIT is necessarily near an end, so a perfectly calibrated
model fails it too, and the 95% target means nothing. The three numbers above
should not be read as a model comparison; they are an artefact of the selection.

The proper unconditional check is N.3's table (expected vs observed exceedance
counts) plus the fraction of *all* cells outside the central 95%. A CDF bug in
the same block — the `z > 0` branch omitted the normalising `Phi((log 0.5 - m)/s)`
term — was also fixed; it alone moved the PIT p-value from 0.008 to 0.096.

### N.5 What the fitted parameters say

```
pi(a)   : 0.707 / 0.323 / 0.057 at ages 1 / 4 / 15   (empirical 0.58 / 0.12 / 0.023)
theta(a): 0.782 / 0.778 / 0.773                      -- essentially CONSTANT in age
|move|  : median 0.06 x level^0.644, log-sd 2.04
```

Two things worth carrying forward:

1. **Only the probability of moving depends on age; the direction does not.**
   `theta` is flat at ~0.78 across the whole range. That is why script 31's
   age-dependent asymmetry was the wrong lever — the age structure lives in
   `pi`, not in the sign.
2. **The magnitude scales sub-linearly with the cohort level**, `beta = 0.644`,
   between purely additive (`beta = 0`) and purely multiplicative (`beta = 1`).
   This is L.3's additive/multiplicative split, resolved as a single exponent
   rather than a two-component mixture.

And the headline from M.4 stands and sharpens: post-publication movement is
**78% upward**. It is not retraction. The entire `p` / `g_C` apparatus was
modelling a process that is mostly late arrivals plus occasional large
corrections in both directions.

### N.6 Limitation, stated deliberately

Post-first *arrivals* are dropped: the revision law replaces them rather than
being convolved with them. That is defensible here because `g_D` fits
`P(delay 0) = 1.000` — FluSight counts are complete at first publication — but a
dataset with genuine late reporting needs the arrival term convolved back in.
The prototype does not do that, and the package integration must.

---

## O. Stage 2 — it forecasts better, and it is over-dispersed

**2026-09-02.** Script `33_backtest.R`. Texas, 16 rolling origins from 2025-03-08
to 2025-07-05, refit at every origin on `as_of <= s0` only, 794 scored
(cohort, origin, horizon) triples, horizons 1-4 publications ahead.

Estimand as per plan section 1: the value the cohort will carry in the snapshot
published at `s*`, i.e. keyed to a publication DATE, not to "h snapshots ahead".

### O.1 Two bugs found by the positive log-likelihoods

The first run reported **positive** log-likelihoods at several origins, which is
impossible when every term is a log-probability. Both causes were real.

**(a) Unguarded parabolic vertex.** The `mu_t` profile refinement
`y2 - 0.125(y1-y3)^2/den` explodes as `den -> 0-` (a flat profile). Now guarded
to apply only when the vertex lies within the bracketing grid interval. **This
code is present unguarded in scripts 18-31 as well**; it did not bite the
full-data fits (the profile is sharp there) but anything refitting on small
subsets should carry the guard.

**(b) A likelihood that could be gamed by underflow — the serious one.** `lmag`
computed

```
log(Phi(hi) - Phi(lo)) - log(1 - Phi((log 0.5 - m)/s))
```

with `pmax(., 1e-300)` floors on both. Driving the magnitude median far below 1
underflows *both* to the floor, so `lmag` returned `log(1e-300) - log(1e-300)`
= **0** — a log-probability of zero, i.e. certainty, for *every* magnitude. The
optimiser found it: warm-started along the origin chain it settled at
`beta = -1.08`, `s = 0.39`, logL **-110 on 736 observations**.

Rewritten using upper tails in log space (`pnorm(lower.tail = FALSE,
log.p = TRUE)`, with a `log(e^a - e^b)` helper), which is accurate in the far
tail and cannot exceed 0 because the bin mass is a subset of the normalising
mass by construction. Verified to sum to 1 across four parameter regimes
including the degenerate one. The degenerate point is still *reachable* — "if it
moves, it moves by exactly 1" — but is now correctly priced, since an observed
move of 615 costs `-Inf` rather than being free.

Stage 1 was re-run after both fixes and is **unchanged** (logL −1500.94, all
gates identical): the full-data fit was never in the degenerate region. Only the
backtest's warm-start chain wandered into it.

### O.2 Results

**By horizon** (WIS, lower better; skill = 1 − signmag/empirical):

| h | n | signmag | empirical | persist | skill | cov90 |
|---|---:|---:|---:|---:|---:|---:|
| 1 | 237 | 3.283 | 3.338 | 4.114 | +1.7% | 0.954 |
| 2 | 213 | 4.222 | 4.424 | 5.380 | +4.6% | 0.977 |
| 3 | 187 | 5.169 | 5.930 | 6.540 | +12.8% | 0.995 |
| 4 | 157 | 6.263 | 7.350 | 7.879 | +14.8% | 0.994 |

**By target age** (the plan's stage-2 gate, because support thins with age):

| target age | n | signmag | empirical | persist | skill | cov90 |
|---|---:|---:|---:|---:|---:|---:|
| 0-1 | 14 | 15.551 | 14.810 | 23.214 | −5.0% | 1.000 |
| 2 | 27 | 10.755 | 11.002 | 16.889 | +2.2% | 1.000 |
| 3-4 | 92 | 12.792 | 14.185 | 18.446 | +9.8% | 0.978 |
| 5-8 | 228 | 5.986 | 6.616 | 7.246 | +9.5% | 0.982 |
| 9-15 | 433 | 1.333 | 1.567 | **1.042** | +14.9% | 0.972 |

**Overall: 4.568 vs 5.033 empirical (+9.2%) vs 5.770 persistence (+20.8%).**
Skill rises with horizon, which is the right direction — the further ahead, the
more a model of the revision process should beat a ratio lookup.

### O.3 The model is over-dispersed

90% coverage is **0.977** against a nominal 0.90; the empirical baseline sits at
0.885. It still wins on WIS despite being too wide, which is encouraging, but
this is miscalibration and should be fixed rather than banked.

The likely cause is in the forward simulation, not the fitted law: each future
age draws move / sign / magnitude *independently*, so variance compounds over
the horizon, and the magnitude scale is recomputed from the *simulated* running
level, which lets the path random-walk away. Real cohorts that have settled tend
to stay settled — there is state persistence the simulation does not carry.
Testable next: condition the move indicator on whether the cohort moved at the
previous snapshot.

### O.4 A caveat that matters operationally

At target ages 9-15, plain **persistence beats the model** (1.042 vs 1.333),
even though the model beats the *empirical* baseline there by 15%. For settled
cohorts "nothing will change" is extremely hard to improve on, and the model
pays for its spread. A deployed nowcaster should probably shrink toward
no-change at old ages — or, equivalently, fix the over-dispersion in O.3, which
is where that loss comes from.

### O.5 Gate status

The plan's stage-2 gate named **`baselinenowcast` and the current production
model** as the comparators. What is done is WIS against an empirical
ratio-lookup baseline and against persistence, by horizon and by target age.
Those two named comparisons are **not yet run**: `baselinenowcast` 0.2.0 expects
a reporting triangle of incremental reports, which does not map cleanly onto a
revising cumulative series with negative increments, and the production model
needs `backtest()` on the same origins and target definition to be comparable.

So: **stage 2 passes against the baselines implemented, with the two named
comparators outstanding**, and with a known over-dispersion defect.

---

## P. The over-dispersion, revisited — a measurement error of mine, two rejected fixes, and the real defect

**2026-09-02.** Scripts `34_markov_state.R`, `35_pi_flex.R`.

### P.1 The coverage numbers in O.3 were mislabelled

Script 33 reported "90% coverage 0.977 against nominal 0.90". Those quantiles
were `QL[2]` and `QL[22]` — the 0.025 and 0.975 levels — so the nominal was
**0.95, not 0.90**. Measured at the right levels:

| level | model | nominal | empirical baseline |
|---|---:|---:|---:|
| 50% | 0.660 | 0.50 | — |
| 90% | 0.938 | 0.90 | 0.826 |
| 95% | 0.981 | 0.95 | 0.885 |

The model is **mildly** over-covered (+3-4 points), not catastrophically. And the
50% figure is not over-dispersion at all: where `P(no move) > 0.5` the 50%
interval collapses to the single point `{C0}`, so coverage must exceed 0.50 by
construction. That is discreteness, and any correctly specified model with a
large atom shows it.

So the defect that motivated scripts 34 and 35 was substantially my own
measurement error. The empirical baseline, meanwhile, is *under*-covered at both
levels (0.826, 0.885).

### P.2 Rejected fix 1 — the Markov move state (script 34)

State persistence is unambiguously real. Over all 53 locations, 37,907 cells
with a previous increment:

| target age | P(move \| prev moved) | P(move \| prev didn't) | odds ratio |
|---|---:|---:|---:|
| 1-2 | 0.320 | 0.088 | 4.9 |
| 3-4 | 0.291 | 0.085 | 4.5 |
| 5-8 | 0.256 | 0.051 | 6.4 |
| 9-15 | 0.259 | 0.027 | **12.5** |

Adding `gamma * 1{prev moved}` to the participation logit fits `gamma = +0.603`
(odds ratio 1.8) and improves the in-sample calibration — PIT KS p 0.096 ->
**0.178**, outside-central-95% 4.7% -> **5.1%** against a nominal 5%, +4.3 nats
for one parameter under a common objective (−1505.75 vs −1510.05).

**But it does not improve the forecast**: WIS 4.583 vs 4.568, coverage
unchanged. Worth keeping for the calibration, not a fix for anything.

### P.3 Rejected fix 2 — a bendable `pi(a)` (script 35)

The fitted logistic-in-`log(1+a)` looked badly shaped against the empirical
move rate (under-predicting at ages 4-9, over-predicting from age 10 by 1.8x),
so a quadratic term was added. It fits `p2 = +0.059` for **0.02 nats**. The
apparent misfit was sampling noise — n is only ~55 per age for one state.

### P.4 The defect that IS real: the magnitude law scales with level, and the data do not

Texas, 185 movers, median `|move|` by level quintile:

| quintile | median level | observed | model |
|---|---:|---:|---:|
| 1 | 126 | 4 | 1.3 |
| 2 | 269 | 9 | 2.1 |
| 3 | 604 | 3 | 3.6 |
| 4 | 1520 | 7 | 6.5 |
| 5 | 3212 | 5 | **10.5** |

Observed magnitude is **flat in level** (4, 9, 3, 7, 5). The model scales it as
`0.058 x level^0.644`, so it under-predicts on small cohorts and over-predicts by
~2x on large ones, and it is 1.3x too wide in log space (log-sd 2.04 against an
empirical residual sd of 1.59).

**Why the likelihood chose `beta = 0.644` anyway** is worth understanding before
fixing it. The magnitude is a lognormal discretised onto `{1,2,...}` and
*renormalised*. At low levels the fitted median falls well below 1, so most of
the mass is truncated away and the renormalisation drags the effective median up
to ~1-2; at high levels it does not. The truncation is therefore doing real work
in the fit, and `beta` is partly compensating for it rather than describing the
data. A magnitude law whose support does not depend so heavily on renormalisation
— a zero-truncated negative binomial on `{1,2,...}`, or a lognormal in
`|z| - 0.5` — should be tried before concluding anything about level scaling.

This also explains O.4's persistence loss at ages 9-15: settled cohorts that do
move, move by ~5, and the model proposes ~10 with a wider spread.

### P.5 A third likelihood bug, and the assertion that ends this class of them

Script 34's first start returned `logL = 1.7e152`. The `|vertex| <= 1` guard from
O.1 is insufficient: with wildly asymmetric grid values, `den` and `(y1-y3)` are
both astronomical and the "correction" is enormous while the vertex still sits in
range. Now capped at 1 nat — a refinement over a grid of step 0.125 in `log mu`
cannot be worth more.

More usefully, the objective now **asserts `tot <= 0`** and returns `1e12`
otherwise. A sum of log-probabilities cannot be positive, and that single line
would have caught all three of the numerical failures in this investigation —
the parabolic explosion (O.1a), the `lmag` underflow (O.1b), and this one — at
the moment they appeared rather than several fits later. Scripts 32, 33, 34 and
35 all carry it now.

Note the logL values before this cap are not comparable to those after it: script
32's −1500.94 becomes −1510.05 under the capped objective, the difference being
up to 1 nat per event week of unearned refinement over 99 weeks.

### P.6 Status

Stage 2 stands where O.5 left it — beating both implemented baselines (+8.9%
vs empirical, +20.6% vs persistence), mildly over-covered, with `baselinenowcast`
and the production model still not run. The next substantive change is P.4's
magnitude law, not the participation law.

---

## Q. Re-verification after the parabolic-refinement bug

**2026-09-03.** The unguarded parabolic vertex refinement (P.5) was present in
scripts **18, 19, 21, 22, 23, 24, 25, 26** — the whole pre-session investigation,
i.e. everything FINDINGS I/J/K and briefing 8.5 rest on. All of them now carry
the guard (correction capped at 1 nat) and the `total <= 0` assertion. The
load-bearing fits were re-run. Pre-guard copies are kept as `*_PREGUARD.rds`.

The other five defects found this session (the `lskel` sign error, the
`log_besselI` recycling, the `lmag` underflow, the gate-2 mis-specification and
the coverage mislabel) live only in scripts 29-35 and were purged where they
occurred; none reaches backward.

### Q.1 Section J is unaffected, by construction

Script 25 re-run: the free-tail profile in `p` is still **0.00e+00 nats** across
all six feasible values. This was predictable rather than lucky — part A never
touches the profiler (it is machine-precision algebra), and part B compares the
*same* profiled objective at different `p` with identical `q` vectors, so the
refinement contributes identically to every term and cancels in the differences.

**The session's central result — finite-horizon data identify `b_c`, not `p` —
does not depend on the profiler at all.**

### Q.2 Section K: substance survives, one headline number does not

| quantity | pre-guard | post-guard | verdict |
|---|---:|---:|---|
| K.3 zeros predicted (observed 620) | 85 (11%) | **118 (15%)** | holds decisively |
| K.2 invented churn `E_ret / obs_dn` | 27.1x | **21.5x** | holds |
| `r(15)`, the bound on `p` | 0.408 | **0.467** | holds, still far below 0.95 |
| `g_D` mean, the invented delay | 1.79 wk | **1.32 wk** | holds |
| script 23 `p_hat` (w15) | 0.518 | **0.524** | holds |
| **K.1 free-retention gain over lognormal** | **9.8 nats** | **83.67 nats** | **CHANGES** |

**K.1 must be restated.** Script 23 re-fits to −3154.84 and script 26 to
−3071.17, so freeing all 15 retraction masses buys **83.67 nats for 12
parameters**, not 9.8. The bug was inflating the lognormal fit far more than the
free one (~90 nats against ~16).

What this does and does not change:

* **Retired:** "removing every parametric restriction buys essentially nothing."
  It buys a real, significant improvement (LRT ~167 on 12 df).
* **Stands:** the free fit still lands on the same *shape* — `b_1 = 0.510` (half
  of all reports retracted at lag exactly 1), `r(15) = 0.467` — and still
  produces 21.5x the observed down-revision and only 15% of the observed exact
  zeros. So the low-`p` solution is still not an artefact of the lognormal
  family, and the retention parameterisation is still not what fixes the zeros.
* **Unaffected:** K.4's synthesis, which rests on K.2/K.3, not on K.1's margin.

### Q.3 Comparison table, all values under the guarded/capped objective

Log-likelihoods before and after this fix are NOT comparable. Restated:

| model | par | logL |
|---|---:|---:|
| lognormal `g_C` + `p` (23) | 5 | −3154.84 |
| free retention `b_1..b_15` (26) | 17 | −3071.17 |
| sign x magnitude (32) | 9 | **−1510.05** |
| + Markov move state (34) | 10 | **−1505.75** |
| + bendable `pi(a)` (35) | 11 | −1505.73 |

The headline stands and strengthens: the increment law gains **+1561 nats over
free retention with half the parameters**.

### Q.4 Outstanding

Script 18's recovery Monte Carlo (8 cells x 12 replicates, ~50 min) is re-running
under the guard; it underpins FINDINGS I and the "estimator is sound" claim. An
early single-replicate timing fit came back at `p_hat = 0.970` for scenario B
`p_true = 0.95`, against a pre-guard cell mean of 0.917 — one replicate, but it
suggests the guard *reduces* the downward bias, which would strengthen rather
than weaken section I. Result to be recorded when it completes.

---

## R. The zero-truncated NB magnitude — best model so far, and my P.4 hypothesis was wrong

**2026-09-03.** Script `36_ztnb_magnitude.R`. Replaces the discretised-and-
renormalised lognormal magnitude with a zero-truncated negative binomial on
{1,2,...}, keeping script 34's Markov move state. Same parameter count.

### R.1 The motivation was sound

The lognormal's renormalisation does very different work at different levels:

| cohort level | fitted median | share of lognormal mass below 0.5, renormalised away |
|---:|---:|---:|
| 126 | 1.31 | **32%** |
| 604 | 3.58 | 17% |
| 3212 | 10.51 | 7% |
| 10000 | 21.85 | 3% |

A ZTNB needs only the `1/(1 - NB(0))` correction, which depends on the fitted
mean but not on any discretisation boundary, so it cannot absorb a
level-dependent share of the mass the way the lognormal could.

### R.2 But the conclusion I drew from it was wrong

P.4 argued that `beta = 0.644` was "partly compensating for the truncation
rather than describing the data", and predicted that removing the artefact would
reduce the level scaling. **It did the opposite**: `beta` fits **1.114**, with
`size = 0.020` (extremely heavy-tailed). The likelihood genuinely wants
level-scaled magnitudes.

The observed medians by level quintile (4, 9, 3, 7, 5) are still flatter than the
model's (3, 4, 6, 10, 16), but with n = 37 per quintile and a var/mean of 190,
the median of a heavy-tailed sample is a weak statistic. The likelihood uses all
185 movers and prefers scaling. **P.4's mechanism should be treated as
unresolved, not established.**

### R.3 Results

| | logL | WIS | skill vs empirical | cov90 |
|---|---:|---:|---:|---:|
| lognormal magnitude (33) | — | 4.568 | +9.2% | 0.938 |
| + Markov state (34) | −1505.75 | 4.583 | +8.9% | 0.938 |
| **ZTNB + Markov (36)** | **−1496.34** | **4.513** | **+10.3%** | 0.940 |

+9.4 nats in-sample at equal parameter count, and the first change that improves
the *forecast* (+1.2% WIS over script 33). Tail exceedance ratios improve at
every threshold but the last: 1.02 / 1.11 / 1.29 / 1.45 against the lognormal's
1.07 / 1.19 / 1.37 / 1.27.

By target age the picture is mixed, and worth stating plainly:

| target age | ZTNB skill | Markov skill | ZTNB WIS | persistence |
|---|---:|---:|---:|---:|
| 3-4 | **+16.2%** | +9.3% | 11.893 | 18.446 |
| 5-8 | +10.1% | +8.8% | 5.950 | 7.246 |
| 9-15 | **+4.2%** | +15.8% | 1.500 | **1.042** |

It gains at young and middle ages and **loses ground at old ages**, where plain
persistence still wins outright. Net +1.2%.

### R.4 What has not moved

Over-coverage is essentially identical across all four variants — 90% coverage
0.938 / 0.938 / 0.940, against a nominal 0.90. Three different magnitude and
participation laws have not touched it. That points at the forward simulation
rather than the increment law, and the next candidate is the one thing the
simulation still treats as independent across steps: the magnitude, which is
empirically autocorrelated (cor(log|z|, log|z_prev|) = 0.358, P.2 measured it and
nothing has used it).

---

## S. Parameter recovery, re-run under the guard — section I needs revision

**2026-09-03.** Script `18_param_recovery.R`, `PR_MODE=full PR_R=12`, all 8 cells,
~50 min. Pre-guard results preserved in `param_recovery_PREGUARD.rds`.

| scen | `Gbar(h)` | `p_true` | `r(h)` | pre-guard | **post-guard** | pre bias | **post bias** |
|---|---:|---:|---:|---:|---:|---:|---:|
| A short | ~0 | 0.60 | 0.600 | 0.583 | **0.593** | −0.017 | −0.007 |
| A short | ~0 | 0.95 | 0.950 | 0.970 | 0.970 | +0.020 | +0.020 |
| B mod | 0.035 | 0.40 | 0.421 | 0.380 | 0.379 | −0.020 | −0.021 |
| B mod | 0.035 | 0.60 | 0.614 | 0.544 | **0.551** | −0.056 | −0.049 |
| B mod | 0.035 | 0.80 | 0.807 | 0.717 | 0.714 | −0.083 | −0.086 |
| B mod | 0.035 | 0.95 | 0.952 | 0.917 | **0.965** | −0.033 | **+0.015** |
| C long | 0.799 | 0.60 | 0.920 | **0.150** | **0.334** | −0.450 | **−0.266** |
| C long | 0.799 | 0.95 | 0.990 | 0.833 | **0.596** | −0.117 | **−0.354** |

### S.1 Reading 1 of I.2 is STRENGTHENED

"The estimator is not broken." At `p_true = 0.95` with retraction inside the
horizon it now returns **0.965** (scenario B, was 0.917) and **0.970**
(scenario A). The bias at the decisive cell flips from −0.033 to **+0.015**.
The single most important question — *does `p_true = 0.95` come back?* — is
answered yes, more cleanly than before.

### S.2 Reading 2 of I.2 must be WEAKENED

The old text said scenario C "reproduces the exact pathology": `p_true = 0.60`
gave `p_hat = 0.150`, matching the real FluSight low-`p` value almost exactly,
and that coincidence carried a lot of rhetorical weight.

**It was partly an artefact.** Under the guard the same cell gives **0.334**.
The direction survives and is still large (bias −0.27), but the claim that the
simulation *reproduced the observed 0.15 collapse* is no longer supported, and
should not be repeated.

### S.3 Reading 2's second half CHANGES CHARACTER

The old text: at `p_true = 0.95` a long tail makes `p_hat` **unstable** —
mean 0.833 with sd **0.215**, "the ridge is flat, so `p_hat` wanders".

Post-guard: mean **0.596** with sd **0.065**. That is not instability, it is a
large *systematic* downward bias. The flat-ridge-wandering story was the
unguarded refinement adding spurious, replicate-dependent amounts to the profile.
The corrected finding is simpler and stronger: **a long retraction tail biases
`p` down hard and consistently.**

### S.4 Reading 3 mostly holds

"A small systematic downward bias (<= 0.08) even when identified" holds for
scenario B at 0.40 / 0.60 / 0.80 (−0.021 / −0.049 / −0.086) but not at 0.95,
which is now slightly positive.

### S.5 Unchanged, and still wrong

Coverage is **0.00 in every cell**, with CI widths of 0.0002 to 0.0095. The
profile interval is uselessly narrow. This was true pre-guard and is untouched by
it; it is the composite-likelihood uncertainty problem the reviewer flagged in
section 25, and no inverse-Hessian or profile interval from this objective should
be reported as a confidence interval.

### S.6 Net effect on the investigation

The finite-horizon confounding conclusion **stands** — scenario C still biases
`p` down by 0.27 to 0.35 while scenarios A and B recover it. What changes is that
one memorable number (0.150, "the exact real-data value") was inflated by the
bug, and one characterisation (instability at high `p`) was wrong. Neither
supports a different conclusion; both were over-claimed.

---

## T. The `main_identifiability_update.tex` model, forecast against ZTNB + Markov

**2026-09-03.** Script `37_tex_model.R`. 12 rolling origins, **578 triples**,
identical cohorts and target definition for every method.

### T.1 What the tex model is, in our terms

It takes the retraction kernel `h_R(l) = P(R = l)` as primitive, with survival
`S_R(a) = 1 - sum_{l<=a} h_R(l)`, and does not estimate `p` or `g_C` separately.
That is **exactly** the `b_c` reparameterisation of section J, `h_R == b_c`, and
its identifiability claim is J.3's theorem.

Its per-delay marginals are `alpha_t^d = mu_t g_D(d)` and
`omega_t^d = mu_t sum_{d1<d} g_D(d1) h_R(d-d1)`, with
`Delta_t^d ~ Skellam(alpha, omega)`.

For a **cadence interval** `(a,b]` — which is what FluSight gives, since
snapshots are missing — the product-over-`d` composite is unavailable, and
summing the per-delay marginals over the window would double count an
arrive-and-withdraw pair inside the window as two independent draws rather than
a deterministic zero. The exact marginal follows from the disjoint trajectory
classes and is the `q_pairs` form of script 26. So **the Poisson version of the
tex model is script 26's free-retention fit**, previously only ever evaluated
in-sample. Cross-check: script 37 returns `S_R(15) = 0.474-0.483` at every
origin against script 26's `r(15) = 0.467`.

### T.2 Result — it forecasts worse than both baselines

| model | WIS | skill vs empirical | cov50 | cov90 | cov95 |
|---|---:|---:|---:|---:|---:|
| **ZTNB + Markov (36)** | **1.959** | **+10.6%** | 0.742 | 0.943 | 0.978 |
| empirical ratio baseline | 2.192 | — | — | 0.860 | 0.915 |
| persistence | 2.474 | — | — | — | — |
| **tex model, free `h_R` (37)** | **2.838** | **−29.4%** | 0.727 | 0.979 | 0.986 |

By target age the gap is worst exactly where the data are quietest:

| target age | n | tex | ZTNB | empirical | persistence |
|---|---:|---:|---:|---:|---:|
| 3-4 | 64 | 6.757 | **4.603** | 5.687 | 7.766 |
| 5-8 | 164 | 2.492 | **1.731** | 2.133 | 2.195 |
| 9-15 | 321 | 1.731 | 1.021 | 0.997 | **0.676** |

### T.3 Interpretation: identifiability and forecasting are orthogonal problems

The tex reduction is **right about what it claims**. Section J proves the
identification result independently, and dropping `p` for a primitive `h_R` is
the correct response to it — `p` was never estimable from finite-horizon
cumulative data, and no amount of parametric structure changes that.

But removing `p` does not make the model forecast, because the *increment law*
is still Poisson-Skellam, and section K showed that law cannot represent this
data at all: 77% of increments are exactly zero and a Skellam with rates large
enough to cover the +-615 tail puts almost no mass on zero. Its predictive is
therefore far too diffuse, which is exactly what the coverage shows — 0.979 at
the 90% level and 0.727 at the 50%.

**The two problems are orthogonal, and this is the cleanest demonstration of it
in the whole investigation:**

* the tex reduction fixes **identifiability** and leaves forecasting worse than
  a ratio lookup;
* the sign x magnitude law fixes **forecasting** and is agnostic about `p`.

A production model should take **both**: `h_R`/`S_R` as the primitive retention
object (so nothing unidentified is estimated), with an increment law that can be
exactly zero most of the time and occasionally jump.

### T.4 Caveat on the numbers

The 12-origin WIS values are not comparable to the 16-origin values quoted in
sections O-R — the origin sets differ in difficulty (empirical baseline 2.192 vs
5.033). Both models were re-scored on the same 578 triples for this table, and
script 36's own "improvement +57.1%" line, which compares across triple sets, is
meaningless and should be ignored.

---

## U. The synthesis — `h_R` primitive with a ZTNB hurdle increment law

**2026-09-03.** Script `38_tex_ztnb.R`. This is T.3's recommendation built: the
tex model's identifiable retention object with an increment law that can be
exactly zero.

### U.1 Construction — the tex mean is preserved exactly

With `alpha = mu_t q_+(a,b)` and `omega = mu_t q_-(a,b)` from the tex model, set

```
theta = alpha / (alpha + omega)                sign probability, STRUCTURAL
E[M]  = (alpha + omega) / pi                   magnitude mean,   STRUCTURAL
Delta = 0 w.p. 1-pi ;  +M w.p. pi*theta ;  -M w.p. pi*(1-theta)
```

The intent is `E[Delta] = pi E[M] (2 theta - 1) = alpha - omega` for any `pi`,
leaving the tex model's `mu_t q_C(d)` untouched so its identifiability argument
carries over, while `pi` absorbs the zeros and the NB `size` absorbs the
dispersion.

> **CORRECTION 2026-09-03.** *As implemented this does not hold.* `lmag(k, m, size)`
> passes `m` to `dnbinom(..., mu = m)`, i.e. `m` is the **parent** NB mean, so
> `E[M] = m/(1 - P0) > m`. The realized mean is `(alpha-omega)/(1-P0)`, inflated
> by **8x to 37x** at the fitted `size = 0.015`. Mean preservation requires
> indexing the ZTNB by its **own** mean — `nu = Psi^{-1}` where
> `Psi(m) = m/(1-P0(m))` is a strictly increasing bijection `(0,inf) -> (1,inf)` —
> and carries the admissibility constraint `pi <= alpha + omega` (a non-null
> update has magnitude at least 1, so the expected flow bounds how often one can
> occur). The tex has been corrected accordingly (Lemma "Mean reparameterization"
> plus a remark). **The U.2/U.3 numbers below stand as what the fitted model does,
> but they are NOT the mean-preserving model** — script 38 must be re-run with
> the corrected indexing before U.3's `S_R(15) = 0.998` can be read as a
> statement about the tex model's retention kernel. `M` is zero-truncated NB; `pi` carries the age term and the Markov
state. Nothing estimates `p`; the primitive is still `h_R`. 21 parameters.

### U.2 It is the best model on every measure that matters

In-sample (Texas w15, 904 intervals, common objective):

| model | logL | zeros (obs 620) | PIT KS p | outside 95% |
|---|---:|---:|---:|---:|
| tex, Poisson-Skellam (37) | −3173.2 | ~118 | — | — |
| ZTNB descriptive (36) | −1496.34 | 620 | 0.174 | 5.2% |
| **h_R + ZTNB hurdle (38)** | **−1485.76** | **620** | **0.250** | **4.8%** |

Out of sample, 12 origins, 578 triples, all methods on identical triples:

| model | WIS | skill vs empirical | cov90 | cov95 |
|---|---:|---:|---:|---:|
| **h_R + ZTNB hurdle (38)** | **1.702** | **+22.4%** | 0.941 | **0.962** |
| ZTNB descriptive (36) | 1.959 | +10.6% | 0.943 | 0.978 |
| empirical ratio baseline | 2.192 | — | 0.860 | 0.915 |
| persistence | 2.474 | — | — | — |
| tex Poisson-Skellam (37) | 2.838 | −29.4% | 0.979 | 0.986 |

**It more than doubles the descriptive model's skill.** Skill by horizon rises
+5.2 / +16.7 / +26.4 / **+32.2%**, against the descriptive model's
+0.9 / +5.2 / +13.9 / +16.9%. By target age the gain is largest exactly where the
descriptive model was weakest: ages 5-8 **+36.3%** (was +18.8%) and ages 9-15
**+23.0%** (was −2.3%), where WIS closes to 0.768 against persistence's 0.676
from the descriptive model's 1.021.

Putting the retention structure back is therefore not decoration — the structural
`theta` and magnitude scale carry real forecasting information that free logistic
functions of age did not.

### U.3 The finding that matters most: the tex model's retention estimate is an artefact

`S_R(15)` at the fit is **0.998**. The tex model with Poisson-Skellam increments
puts it at **0.474** (script 37) and the free-retention fit at 0.467 (script 26) —
i.e. those models say **53% of all reports are eventually withdrawn**, while the
same retention object with a corrected increment law says **0.2%**.

The observed total down-movement is 2,741 against 61,636 up-movement, so 0.2% is
the credible figure. **The long retraction tail that the briefing spends sections
8-9 trying to explain is mostly an artefact of the misspecified increment law** —
the same invented churn as K.2 (21.5x), now visible directly in the estimated
kernel. Fixing the increment law dissolves it without any administrative-revision
component at all.

Note also that `g_D` stays concentrated (`P(0) = 0.994`) and the structural sign
still comes out at **0.762** against an observed 0.78, because `theta` is set by
the *ratio* of two small quantities — late-reporting mass over retraction mass —
not by their absolute size. The model reconciles "reporting is complete at first
publication" with "78% of later moves are up" through that ratio, which the
Poisson-Skellam could not do because late `g_D` mass destroyed its zeros.

### U.4 Where it is still weak

* **Tail under-prediction is worse** than the descriptive model: exceedance
  ratios 1.28 / 1.48 / 1.76 / 1.76 against 1.02 / 1.11 / 1.29 / 1.45. The
  structural magnitude mean `(alpha+omega)/pi` is less free than a fitted
  `level^beta`, and it costs tail mass.
* **Persistence still wins at ages 9-15** (0.676 vs 0.768), though the gap is now
  small.
* **Over-coverage persists at the 90% level** (0.941), the fourth model in a row
  to sit near 0.94. The 95% level does improve, 0.978 -> 0.962.
* One state, one arm, 12 origins.

---

## V. Mean preservation costs forecast skill — the tex mean structure is itself in tension with the data

**2026-09-03.** Script `39_meanpreserving.R`, built to fix U's error. It
reparameterises so that mean preservation holds by construction with no
numerical inversion: the parent NB mean `m` is free (age + Markov terms on
`log m`), `nu = Psi(m) = m/(1-P0(m))` is closed form and equals `E[M]`, and
`pi = (alpha+omega)/nu` is DERIVED. Then
`E[Delta] = pi*E[M]*(2 theta - 1) = alpha - omega` exactly. Verified in the fit:
residual `max |E[Delta] - (alpha-omega)| = 6.25`, entirely attributable to the
**11 of 805** cells where `pi > 1` must be clamped (admissibility binding).

### V.1 It is correct and it fits worse

| | logL | zeros (obs 620) | PIT KS p | tail ratio at \|z\|>100 |
|---|---:|---:|---:|---:|
| script 38 (mean NOT preserved) | **−1485.76** | 620 | **0.250** | 1.76 |
| script 39 (mean preserved) | −1575.00 | 697 | **3.6e-07** | 3.63 |

89 nats worse, over-predicts the zeros, badly under-predicts the tail, and the
PIT is decisively rejected.

### V.2 And the ordering is monotone in how tightly the mean is constrained

Same 578 triples, 12 origins:

| model | mean structure | WIS | skill | cov90 |
|---|---|---:|---:|---:|
| script 38 | inflated by `1/(1-P0)`, 8-37x | **1.702** | **+22.4%** | 0.941 |
| script 36 | free (`kappa * level^beta`) | 1.959 | +10.6% | 0.943 |
| script 39 | **exactly `mu_t q_C(d)`** | 2.120 | +3.3% | 0.813 |
| script 37 | exactly `mu_t q_C(d)`, Skellam | 2.838 | −29.4% | 0.979 |

**The more tightly the model is tied to the tex mean structure, the worse it
forecasts.** The two models that reproduce `E[C_t(d)] = mu_t q_C(d)` exactly are
the two worst; the best is the one whose mean is a mechanistically-driven but
unconstrained multiple of it.

### V.3 What this means

This is not a coding problem — script 39 is correct and the preservation is
verified numerically. It says the mean structure implied by `(g_D, h_R)` — every
report arrives once per `g_D` and is withdrawn per `h_R` — **does not match how
these series actually move**, and forcing it costs real skill. Script 39 is also
the first model in the series to be *under*-covered (90%: 0.813), and it fails
worst at young ages (coverage 0.000 at ages 0-1) while being the best of all
models at ages 9-15 (skill +31.3%).

Script 38 should therefore not be described as "the tex model with a ZTNB
increment law". It is a distinct, legitimate model: the magnitude's *parent* mean
is `(alpha+omega)/pi`, still mechanistically driven, but the implied update mean
is `(alpha-omega)/(1-P0)`. Only the description was wrong, not the model.

### V.4 Open question for the paper

The ZTNB subsection now in `main_identifiability_update.tex` presents the
mean-preserving version, with the admissibility constraint and the inversion
lemma. That is the principled object and it is correctly stated. But V.2 says it
is not the best forecaster, and a paper that presents it as the recommended
observation model should say so, or should present the unconstrained variant
alongside it with the mean relation stated honestly.
