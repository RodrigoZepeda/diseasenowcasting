# The `p` estimation problem for count-cumulative data

**Written 2026-09-01, `skellam` branch.** Everything here is measured on the
working tree (the fixed saddlepoint density), not the installed 2.1.0.

This document states the problem only. It does not argue for the fix currently in
`default_priors()`; that fix is a holding action and this is the material for
replacing it.

---

## 1. What `p` is supposed to be

For a count-cumulative stream the article models the signed increment at delay `d`
of event time `t` as a Skellam difference of two Poisson streams:

```
alpha_t^d = mu_t * g_D(d)                       [eq. alphasimplified]  additions
beta_t^d  = mu_t * (1 - p) * (g_D * g_C)(d)     [eq. omegadef]         retractions
```

with `mu_t = lambda_t / p` the **gross** report rate and `lambda_t` the settled
genuine mean — the nowcast target. So:

* `p` — probability a report is genuine and is never retracted,
* `g_D` — appearance delay, event to first report,
* `g_C` — retraction lag, report to retraction, supported on `{1, 2, ...}`,
* `g_W = g_D * g_C` — event to retraction.

The observation is `m_t^d = C_t(d) - C_t(d-1)`, the signed change in the published
cumulative.

## 2. The empirical target

`p` is directly measurable from the stream. The denominator must match
`alpha_t^d = mu_t g_D(d)`, which counts **every** appearance including `d = 0`, so
first reports belong in it:

```
p_empirical = 1 - (total down-revision) / (first reports + total up-revision)
```

FluSight, windowed to the snapshot era and truncated to the last event date, 105
event weeks each:

| state | first reports | up | down | `p_empirical` |
|---|---|---|---|---|
| Texas | 57,577 | 8,750 | 2,842 | **0.9572** |
| California | 58,087 | 4,372 | 674 | **0.9892** |
| New York | 44,750 | 3,227 | 480 | **0.9900** |
| Florida | 56,545 | 4,356 | 144 | **0.9976** |

This quantity is **stable**: on Texas it is 0.9599 over the full file and 0.9579
windowed. It does not depend on the analysis window, so it is a property of the
data source.

(Note the contrast with the *revision-only* ratio `up/(up+down)`, which is 0.71 for
Texas. That denominator excludes first reports and is **not** what the model's `p`
means. Session 1's "empirical centre 0.96" is the correct one; do not confuse them.)

## 3. What the likelihood actually does

Estimated on the same data, `p` goes to roughly **0.10**, and no amount of prior
concentration stops it. Windowed FluSight, Poisson / AR1 / lognormal, one stage:

| state | prior on `p` | `p_hat` | `g_C` median (weeks) | `delay_mu` | implied `g_D` median (weeks) |
|---|---|---|---|---|---|
| Texas (emp 0.958) | Beta, conc 10 | 0.111 | 0.042 | 3.082 | 21.8 |
| Texas | Beta, conc 100 | 0.120 | 0.043 | 2.965 | 19.4 |
| Texas | Beta, conc 300 | 0.142 | 0.045 | 2.709 | 15.0 |
| Texas | **fixed** at 0.958 | — | 0.981 | **−0.164** | **0.85** |
| California (emp 0.990) | Beta, conc 10 | 0.094 | 0.037 | 3.233 | 25.4 |
| California | Beta, conc 100 | 0.103 | 0.038 | 3.107 | 22.4 |
| California | Beta, conc 300 | 0.124 | 0.040 | 2.846 | 17.2 |
| California | **fixed** at 0.990 | — | 1.554 | **−0.122** | **0.89** |

Thirty times more prior information moves `p_hat` by **0.03** against a gap of
**0.85**. That is the signature of a likelihood with a second optimum, not of a
prior that is merely too weak.

## 3b. What a backtest says (six as-of dates per state)

A single as-of date suggested the free fit is simply broken. Backtested over six
as-of dates per state, it is not that simple:

| state | `p_empirical` | WIS, `p` fixed | WIS, `p` estimated | cov50 fixed / est | cov90 fixed / est |
|---|---|---|---|---|---|
| Texas | 0.9572 | **14.11** | 46.36 | 0.17 / 0.17 | 0.50 / 0.50 |
| California | 0.9892 | **9.79** | 31.54 | 0.17 / 0.33 | 0.50 / 0.67 |
| New York | 0.9900 | 9.23 | **8.97** | 0.33 / 0.50 | 0.83 / 0.83 |
| Florida | 0.9976 | 9.62 | **9.19** | 0.00 / 0.33 | 0.50 / 0.67 |
| **mean** | | **10.7** | 24.0 | 0.17 / 0.33 | 0.58 / 0.67 |

Three things follow, and they matter more than the headline mean:

1. **Fixing `p` is catastrophe-avoidance, not uniform improvement.** It wins by
   more than 3x on Texas and California, and loses marginally on New York and
   Florida. The mean is carried entirely by the two blow-ups.
2. **Fixing `p` makes coverage worse, everywhere.** The free arm covers as well or
   better in every state. That is the expected price of asserting `p` is known:
   `lambda_t = p mu_t`, so removing uncertainty in `p` removes it from the target.
3. **Both arms are badly under-dispersed.** Nominal 50% intervals cover 0.17-0.50,
   nominal 90% cover 0.50-0.83. This is a *separate* defect from the `p` problem
   and is not addressed by anything in this document. These runs use
   `poisson_likelihood()`; whether `nb_likelihood()` closes the gap is untested.

The two states where fixing helps (Texas 0.957, California 0.989) are not obviously
distinguishable from the two where it does not (New York 0.990, Florida 0.998) by
`p_empirical` alone -- California and New York are within 0.001 of each other and
land on opposite sides. Whatever separates them is not the retraction rate, and
finding out what it is would be worth more than tuning the prior.

## 4. The degenerate solution, stated plainly

Read the fitted parameters together and they describe one coherent story:

> Gross reports arrive at roughly **ten times** the true rate (`mu_t = lambda_t/p`
> with `p ≈ 0.1`), they arrive **very late** (`g_D` median 15–25 weeks), and about
> **90%** of them are retracted at the **shortest lag `g_C` allows**
> (`g_C` median 0.04, and `g_C` is supported on `{1, 2, ...}`, so essentially all
> its mass sits on lag exactly 1).

Why the likelihood likes this: a retraction at lag 1 cancels an addition one period
later. Over a run of periods the additions and retractions very nearly annihilate,
so the *observed* increments are almost unchanged, and the inflation is close to
free. Having bought a huge `mu_t`, the fit can then place `g_D` almost anywhere,
because only `mu_t * g_D(d)` is pinned, not the two factors separately.

Formally, the near-invariance is

```
(p, g_D)  ->  (p', g_D')     with     mu_t g_D(d) approximately invariant
```

and the churn term absorbing the residual. `p` and `g_D` are traded against one
another along an almost-flat ridge.

Two independent symptoms of that ridge:

* **`g_C` is pinned to its boundary.** A median of 0.04 on a support of
  `{1, 2, ...}` means the fit wants the shortest lag it is allowed. Boundary
  solutions are how a nuisance parameter signals it is being used as a free knob.
* **The Hessian is not positive definite** at the estimated optima on Texas
  (CHOLMOD "matrix not positive definite" at concentrations 10 and 100). Only the
  fixed-`p` fit is PD there.

## 5. Why there is no delay information to break the tie

For linelist and count-incidence data the appearance delay is identified by a
*separate* likelihood term over individually observed delays, pooled across strata
(`obs_delays` in `R/14_objective_joint.R`). That term is **skipped entirely** for
count-cumulative — the code says so:

```
# Skipped for confirmation: there are no individual delay observations -- the
# appearance delay is informed by the signed-increment likelihood directly.
```

So `g_D` and `p` are estimated from the **same** signed increments, with nothing
else to separate them. That is the structural difference between the two data
types, and it is why the linelist model has no analogous problem: there, `p` is
pinned by the cure block (a report standing unresolved for a long time is direct
evidence about the cure fraction), and in the count block `p` is exactly aliased
with the epidemic intercept, so the count likelihood cannot pull it around at all.

## 6. What is NOT the cause — ruled out by experiment

* **Left-truncation of the FluSight window.** Real and severe (126 of 197 Texas
  event weeks first appear at delay > 1 week, up to 145 weeks), and it *does*
  corrupt `g_D`. But `p_empirical` is invariant to it (0.9599 vs 0.9579) and
  windowing does not fix `p_hat`. Necessary to fix, not sufficient.
* **Missing zeros in the increment path.** Tested directly. Fitting with and
  without `tbl.now::complete_zeroes()` gives **bit-identical** results, because
  `prepare_data()` builds `increment_array` zero-initialised and the likelihood
  walks a dense `delay_seq <- 0:horizon_t`. 83.2% of the cells entering the
  Skellam path on Texas are already zeros. The zeros are present and load-bearing.
* **The broken Bessel density.** Fixed in session 1. All numbers here are
  post-fix; session 1's `p_hat = 0.073` was measured against the stale installed
  build and is not comparable.
* **Prior strength.** See section 3.

## 7. The current holding action, and why it is unsatisfying

`default_priors()` now **fixes** `p` at `p_empirical` for count-cumulative and
keeps the weak Beta for linelist / count-incidence. A user-supplied
`beta_prior()` still frees it.

It is defensible — it is the article's own "model constraint", it is the only arm
that recovers a credible `g_D` (0.85–0.89 week median rather than 15–25 weeks),
it is the only arm with a PD Hessian on Texas, and it more than halves mean
backtest WIS. But see section 3b: it wins on two states and loses on two, and it
costs coverage in every one.

But it is unsatisfying for three reasons:

1. **The plug-in estimate is biased upward by construction.** A cumulative stream
   shows "not retracted *yet*", i.e. `p + (1-p) P(not yet retracted)`, so it
   overstates `p` by about +0.007 at `p = 0.99`, rising to about +0.10 at
   `p = 0.7`. Fixing it propagates that bias with no uncertainty attached.
2. **No uncertainty in `p` reaches the nowcast.** `lambda_t = p * mu_t`, so an
   error in `p` is a proportional error in the target, and fixing `p` reports the
   nowcast as if `p` were known exactly.
3. **It does not generalise.** A stream with a genuinely low `p`, or one whose `p`
   drifts, is exactly where the constraint is most wrong and where the plug-in
   estimator's bias is largest.

## 8. Directions worth considering

Not recommendations — the space of solutions, for you to choose from.

1. **Constrain `g_C` away from its boundary.** The degenerate solution needs
   `g_C` at lag 1. A prior that keeps the retraction lag plausibly long (or a
   support starting later) makes churn expensive rather than free. Cheapest to
   try, and directly targets the mechanism rather than the symptom. Testable by
   re-running the concentration sweep with a `retract_mu` prior centred well
   above lag 1.
2. **Constrain `g_D` instead of `p`.** The ridge trades the two, so pinning either
   end identifies the other. An appearance delay is often known externally (a
   weekly hospitalisation stream reports within a week or two), and a
   moderately informative prior on `delay_mu` may be far less objectionable than
   fixing `p` — it also carries uncertainty forward.
3. **Reparametrise onto the identified direction.** If only `mu_t g_D(d)` is
   pinned, estimate that product and a single well-identified contrast, rather
   than `p` and `g_D` separately. More work, but it would make the geometry
   explicit instead of fighting it.
4. **A hierarchical `p` across strata.** All four FluSight states have
   `p_empirical` in 0.957–0.998. Pooling `p` across strata with a tight
   hierarchical prior adds real information without fixing any single value, and
   the stratified-`p` machinery already exists (`stratified_p`).
5. **Penalise the churn directly.** The degenerate solution is characterised by a
   large *cancelling* flow: `beta` large and nearly equal to a lagged `alpha`. A
   penalty on total retracted mass relative to observed net change would target it
   without constraining `p` or `g_D` individually.
6. **Accept it and report it.** Keep `p` fixed, but surface it: `print()` and
   `parameters()` state that `p` was constrained and at what value, so nobody reads
   a fixed `p` as an estimated one.

## 9. How to reproduce

```bash
Rscript devel/backtest_flusight_p.R      # p fixed vs p estimated, backtested
```

Both `devel/` scripts now use `pkgload::load_all(".")`. Do not use
`library(diseasenowcasting)` — the installed build is a different, older package
and will silently give you pre-fix numbers.

Data-preparation rules for FluSight, both properties of the file rather than the
model:

```r
START <- as.Date("2023-09-23")            # the first as_of
raw <- flusight |>
  filter(location_name == st, target_end_date >= START, as_of >= START) |>
  filter(as_of <= max(target_end_date))   # truncate to the last event date
```
