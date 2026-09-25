# diseasenowcasting — AI Agent Reference Guide

This document is a complete reference for AI assistants (and human contributors)
working in the `diseasenowcasting` codebase.  It is designed so that you can write correct
`diseasenowcasting` code from scratch without reading the source.

---

## 1. What diseasenowcasting does

`diseasenowcasting` is a standalone R package for **Bayesian epidemic nowcasting** using
the RTMB autodiff engine (CppAD + built-in Laplace approximation).  It
reimplements the `diseasenowcast2` Stan-based package with no Stan/cmdstanr
dependency, matching the same public API: `model(likelihood(), epidemic(),
delay())`.

**Core idea:** The reporting delay is a stochastic
process modelled jointly with the epidemic.  The likelihood depends only on
*observed* delays (not a reporting triangle), so censoring is handled cleanly.
The Laplace approximation marginalises over the latent epidemic coefficients;
posterior draws are sampled from N(mode, H⁻¹) where H is the joint Hessian.

**Default mode:** joint-mode Laplace (`use_random = FALSE`) — same as
`cmdstanr $laplace()`.  20–400× faster than the marginal (`random=`) Laplace.

**Two-stage cascade:** Stage 1 fits the delay only (`delay_only = TRUE`);
Stage 2 hard-fixes the delay at K imputed values and pools draws.  This
propagates delay uncertainty without sacrificing convergence.

---

## 2. The model menu

```r
model(likelihood, epidemic, delay)   # combine three components
```

### Likelihoods

| Constructor | Count distribution | Notes |
|---|---|---|
| `nb_likelihood()` | Negative-binomial (NB-2) | Default; handles overdispersion |
| `poisson_likelihood()` | Poisson | Simpler; good for low counts |

### Epidemic models

| Constructor | Key args | Description |
|---|---|---|
| `hsgp_epidemic(num_basis, gp_kernel=2, gp_basis=1, tmax_model=0, gp_boundary_frac=0.62)` | `num_basis` (int, 0=auto) | Hilbert-space GP; flexible smooth trend. Shared kernel (alpha, ell) across strata. |
| `ar1_epidemic()` | — | AR(1) trend; fast, per-stratum phi/sigma. |
| `sir_epidemic(N_pop=1e6, use_beta_rw_trend=TRUE)` | `N_pop` | Discrete-time SIR; coupled force of infection across strata. |
| `arima_epidemic(p=2, d=1, q=0, include_drift)` | `p`, `d`, `q` | ARIMA on log-incidence. AR/MA parameterised by partial autocorrelations -> stationary + invertible by construction. Drift only at `d >= 1`. Default has **no MA term** (see below). |
| `sts_epidemic(trend="semilocal")` | `trend` | Structural time series. `"semilocal"` = local level + slope reverting to a long-run `D` (does not extrapolate off the plot); also `"local_linear"`, `"local_level"`. |
| `ets_epidemic(trend="additive", damped=TRUE)` | `trend`, `damped` | Single-source-of-error damped local trend: level and slope share one innovation. |
| `random_walk_epidemic()` / `naive_epidemic()` | `include_drift` | Random walk on log-incidence; the baseline. `naive_epidemic()` is the named baseline slot (currently a RW). |
| `theta_epidemic()` | — | The Theta method's model form: random walk with an estimated drift. |
| `custom_epidemic(intensity_fn, priors, ...)` | `intensity_fn`, `priors` | **User-defined** `f(t)`. Any RTMB-traceable generator of `log_mean[T×S]`. See §2b. |

### Delay families

| Constructor | Distribution | Notes |
|---|---|---|
| `lognormal_delay()` | Log-normal | Best for COVID; fastest convergence. |
| `gamma_delay()` | Gamma (mean/SD) | Good for dengue/mpox. |
| `generalized_gamma_delay()` | Generalised Gamma | Most flexible; Q ∈ (0.05, 3) bounded. |
| `dirichlet_delay(bins=NA)` | Non-parametric simplex | Dirichlet prior + geometric tail; two-stage only. |
| `custom_delay(cdf_factory, priors, ...)` | **User-defined** | Any RTMB-traceable CDF. See §2b. |

### Combining

```r
mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
```

---

## 2a. Validation processes — confirmations and retractions

Use when a report is **provisional** and later resolved: confirmed (a real case)
or retracted (struck from the register). The nowcast target becomes the settled
count rather than the raw report count.

A missing outcome means **not resolved yet**, NOT "genuine". The lag is
right-censored at the report's age, so the model is a mixture-cure likelihood, not
a flat multiplication by a confirmed fraction.

**It is detected, not requested.** There are no `retraction_date=` /
`confirmation_date=` arguments — `nowcast()` reads the process off the data:

```r
# Record the outcome on the tbl_now, then just fit:
tn <- tbl.now::tbl_now(df, event_date = onset, report_date = reported,
                       validation_date = result, validation_type = outcome,
                       data_type = "linelist")
nowcast(tn, model())        # attaches a validation_process() and says so
```

`validation_type` values are `"confirmed"`, `"retracted"`, `"pending"`. A
**validation date with an `NA` type is an error** — the report resolved but its
sign is unknown, so it cannot enter either lag law.

| Mode | Lag support | rho(j) = P(counts \| pending, age j) |
|---|---|---|
| retraction only | `{1,2,...}` (same-period rows dropped) | rises with age |
| confirmation only | `{0,1,...}` (same-day is normal) | falls with age |
| both | `{0,1,...}` | flat at `p` (shared lag) |

**Mode inference.** From `unique(validation_type)` over the **full** data, not the
as-of view, so it is a stable property of the data source and cannot flip between
backtest dates. `validation_process(mode = )` asserts instead: an assertion the
data cannot support is an error, while inference with no resolved rows falls back
to the ordinary count model.

**Configuring** — via `validation_process()`, passed as `model(validation = )`:

```r
model(nb_likelihood(), hsgp_epidemic(), lognormal_delay(),
      validation = validation_process(
        validation_delay = dirichlet_validation(bins = 10),  # any delay family
        p                = beta_prior(20, 3),   # or a number to fix it
        stratified_p     = TRUE,                # one p per stratum
        mode             = "auto",              # or assert one
        negative_delay   = lognormal_validation()  # COMPETING RISKS, see below
      ))
```

Lag constructors: `lognormal_validation()`, `gamma_validation()`,
`generalized_gamma_validation()`, `dirichlet_validation()`. Prefer Dirichlet at
high counts — the correction applies to every pending report, so a wrong lag
*shape* biases more than noise.

**Competing risks** (`negative_delay`): positives and negatives come back on
different timescales, so a pending report's age becomes informative. Requires
`mode = "both"` — one outcome cannot identify two lag laws. Errors otherwise.

**Censored validation dates**: `nowcast(validation_censored = )` names a logical
column marking rows whose validation date is an upper bound. This is the **only**
surviving validation argument, because a `tbl_now` has no validation-censoring
attribute. Combines with `tbl.now`'s `is_censored` on the report side; all four
patterns are supported.

**Other data types**: `count-incidence` works identically (one row per distinct
`(event, report, validation)` with a case count) and gives bit-identical results
to the linelist form.

**`count-cumulative`** may carry a validation process — its down-revisions *are*
the retractions — but **confirmations there are an error**: a confirmation does
not change a cumulative count, so its delay parameters are unidentifiable. On a
cumulative stream `p` is **fixed** at the empirical down-revision rate by default,
because the stream cannot identify it (see `devel/P_IDENTIFIABILITY.md`); pass
`p = beta_prior(...)` to estimate it anyway.

**Reading the output**: `print()` states the mode and the fitted probability;
`parameters()` (NOT `tidy()` — that is tbl.now's, and returns the nowcast) has
`type == "resolution"` rows, with `prob_confirmed` / `prob_not_retracted` on the
natural scale. `backtest()` builds truth from the cases that settle positive,
automatically, with pending cases kept.

Gotchas:
- A `tbl_now` refuses to hold a validation dated after its own `now`, so let it
  infer `now` from the data and pass the analysis date to `nowcast(now = )`.
- Retractions dated after `now` are masked to "not yet retracted" automatically.
- `p = 1` with observed retractions errors — it says retractions are impossible.
- Nothing resolved at all -> falls back to the ordinary count model.

Full treatment: `vignette("Validation_processes")`; derivation in
`vignette("Mathematics")` section 8.

## 2b. Custom components (user-defined delays & epidemic processes)

Users can supply their **own** delay distribution or epidemic process as an
R function, instead of choosing from the built-in menu.  Both go into the
`model()` exactly like a built-in component.

**⚠️ `library(RTMB)` is REQUIRED.** RTMB is in `Imports` (not `Depends`), so
`library(diseasenowcasting)` does NOT attach it.  Built-in models work anyway
(their math lives in the package namespace), but a *user-written* function lives
in the global env and its arithmetic only dispatches to RTMB's autodiff methods
when RTMB is **attached**.  The fit/validate path calls `.assert_rtmb_attached()`
and aborts with a clear "Run `library(RTMB)`" message if it is missing.  This is
also why the `test-custom-components.R` tests `library(RTMB)` at the top and the
vignette does so in its setup chunk.

**The one rule (AD-traceability).** Every op inside the user function must be
RTMB-differentiable:
- ✅ `+ - * /`, `exp`, `log`, `sqrt`, `abs`, `sum`, `cumsum`, `pnorm`, `pgamma`,
  `lgamma`, matrix arithmetic, **fixed-length** `for` loops.
- ❌ `if`/`ifelse` on a *parameter value*, `pmax`/`pmin` on AD types
  (use `(x + abs(x))/2` for `pmax(x,0)`), RNG, external solvers (`deSolve`).
- **Index assignment in a loop** (`v[t] <- ...`) needs ```` `[<-` <- RTMB::ADoverload("[<-") ```` as the FIRST line of the function. Prefer `cumsum()` to avoid it.

Always run the **validator** first — it test-tapes the function and reports
finite `fn()`/`gr()`, turning a cryptic optimiser failure into a clear message.

### custom_delay() — num_id 5

```r
# Each of cdf / log_cdf / log_survival is `function(theta) -> function(d)`.
# Only `cdf` is required; log_cdf defaults to log(cdf), log_survival to log(1-cdf).
# Supply log_survival explicitly for heavy tails (default log(1-cdf) loses precision as F->1).
weibull_cdf      <- function(theta) { shape <- exp(theta[1]); scale <- exp(theta[2])  # log scale => unconstrained
                                      function(d) 1 - exp(-(d/scale)^shape) }
weibull_log_surv <- function(theta) { shape <- exp(theta[1]); scale <- exp(theta[2])
                                      function(d) -(d/scale)^shape }            # exact, stable in the tail
dly <- custom_delay(
  cdf          = weibull_cdf,
  log_survival = weibull_log_surv,
  priors       = list(normal_prior(0,1), normal_prior(log(7),1)),  # per-param: prior=free, number=fixed
  name = "Weibull", param_names = c("log_shape","log_scale"), inits = c(0, log(7))
)
# No n_params argument: it is inferred from priors / param_names / inits (must agree).
validate_custom_delay(dly)             # REQUIRES library(RTMB)
model(nb_likelihood(), ar1_epidemic(), dly)
```
Works in BOTH the joint and the two-stage Stage-1 delay-only fit.  Fitted
params land in `fit$parList$custom_delay_params`.  Internally the three functions
are assembled into `priors$cdf_factory`, carried downstream (reconstruct /
surprise / diagnostics).

### custom_epidemic() — num_id 4 (epidemic_model)

```r
# intensity_fn(theta) -> numeric matrix log_mean[max_time x n_strata] (the FULL log f(t),
# including any intercept; NOT just a trend). One column => unstratified.
max_time <- infer_max_time(tn)                   # event-times the model spans (see note)
rw_fn <- function(theta) {                       # pure random walk on log-incidence
  log_mu0 <- theta[1]; sigma <- exp(theta[2]); eps <- theta[3:(2L + max_time)]
  matrix(log_mu0 + cumsum(sigma * eps), max_time, 1L)
}
custom_epi <- custom_epidemic(
  intensity_fn = rw_fn,
  priors       = c(list(normal_prior(3,1), normal_prior(-2,0.5)),  # n_params inferred (2 + max_time)
                   rep(list(std_normal_prior()), max_time)),
  inits        = c(3, -2, rep(0, max_time))
)
validate_custom_epidemic(custom_epi)            # REQUIRES library(RTMB)
model(nb_likelihood(), custom_epi, lognormal_delay())
```
- **No `n_params` argument** — it is inferred from `priors` / `param_names` /
  `inits` (whichever are supplied must agree on the count).
- **For time-varying dims** (e.g. a RW with one innovation per event-time), get
  `max_time` first with `infer_max_time(tn)` (= `prepare_from_tbl_now(tn, model)$max_time`).
- v1 is **fixed-effects only** (no `random=` Laplace over custom latents); fine
  for short/medium series.  Custom processes are joint-fit only (not the
  two-stage Stage-1, which is delay-specific — unaffected).
- ODE example: write the RHS + a fixed-step RK4/Euler loop inside `intensity_fn`
  (with the `[<-` overload).  For stiff/adaptive needs, `RTMBode` is the
  production path (add to Suggests; not used by the package itself).
- Fitted params in `fit$parList$custom_epidemic_params`; `priors$intensity_fn`
  carries the function to `.joint_reconstruct()` / prior-only / diagnostics.

See `vignette("Custom_delays_and_processes")` for worked Weibull-delay,
random-walk, and SIR-ODE examples with built-in comparisons.

---

## 2c. Classical time-series epidemic processes

`arima_epidemic()`, `sts_epidemic()`, `ets_epidemic()`, `random_walk_epidemic()`,
`naive_epidemic()` and `theta_epidemic()` are latent trends for **log**
incidence.  They slot in exactly where HSGP/AR(1) do:

```
log_mean[t, s] = mu_intercept[s] + (X gamma[, s])[t] + trend[s](t)
```

so **covariates and temporal effects apply to them unchanged**, and every trend
coefficient is estimated **per stratum** (the AR(1) convention, not HSGP's
shared kernel).

**No seasonal states.**  There is no SARIMA `(P,D,Q)_s` and no Holt-Winters
seasonal vector.  Seasonality comes through the covariate path:
`temporal_effects(day_of_week = TRUE)` adds **reference-coded** weekday dummies
(6 columns, first level dropped -- see `.encode_design_column()` in
`R/06_process_design.R`, so no collinearity with `mu_intercept`) and
`temporal_effects(seasons = c(7, 52))` adds Fourier pairs.  A handful of
coefficients instead of `s` latent states per stratum.

**What is actually distinct.**  Under a count likelihood several classical
methods coincide, and the package does not pretend otherwise:

| Constructor | Latent process | Note |
|---|---|---|
| `random_walk_epidemic()` | RW | `naive_epidemic()` is the same fit with a different label |
| `theta_epidemic()` | RW + drift | Theta = SES + drift (Hyndman & Billah 2003); the SES smoothing is what the Kalman filter for a local level model already does, so only the drift is left to estimate |
| `ets_epidemic(trend="none")` | RW | documented as equal to `random_walk_epidemic()` |
| `ets_epidemic(trend="additive")` | one-source damped local trend | genuinely distinct: rank-one restriction of `sts_epidemic("local_linear")` |
| `sts_epidemic("semilocal")` | two-source level + mean-reverting slope | the one that keeps long-horizon trends bounded |
| `arima_epidemic(p,d,q)` | ARMA on the d-th difference | conditional (zero pre-sample) likelihood |

**Parameterisation gotchas**

- `alpha` is NOT a separate ETS parameter.  `(alpha, beta, sigma)` is identified
  only up to a common rescaling, so `sigma` **is** the level innovation SD
  (classical `alpha*sigma`) and `beta` is Hyndman's `beta* = beta/alpha` in (0,1).
- ARIMA `ar`/`ma` slots are priors on the **partial autocorrelations**, not on
  the coefficients.  `.pacf_to_coefficients()` (Levinson-Durbin) maps them.
  Default `normal_prior(0, 0.5)`, deliberately shrunk: a partial autocorrelation
  near 1 on a *differenced* series is an I(2) level, whose predictive variance
  grows like `h^2` over the unobserved tail.
- **The default order is `(2, 1, 0)`, not `(1, 1, 1)`.**  On a latent trend the
  AR and MA coefficients are only weakly separated, and it gets worse with
  series length.  The fitted `ar + ma` is nowhere near zero, so the point
  estimates do NOT sit at the common factor -- but the curvature does: on a
  1,000-week series the two correlate -0.79 to -0.84 in the Laplace covariance,
  and `(1,1,1)` scores within 1 nll unit of the matching `(2,1,0)`.  The
  likelihood cannot choose; the interval pays.  At 985 event-times `(1,1,1)`
  returned 90% bands of **175x to 691x the settled count** against about 7x for
  `(2,1,0)`, with `converged = 1.000` and a positive-definite Hessian throughout.
  On a short series the pair is fine (correlation 0.08 at 70 daily event-times).
  `fit_check()` reports `arma_ridge_correlation` and warns above 0.6 -- see §2g.
- **Long series are expensive; the slope variants of STS are the fragile ones.**
  One latent innovation per event-time (two for STS with a slope) means the
  parameter count is set by the data: 28 for HSGP on any series, 1,623 for STS
  on 1,095 weeks.  Measured on dengue at **985 event-times**, untruncated, 54
  fits (`devel/longrun_2008.R`):

  | process | converged | median min | max min | median band / truth |
  |---|---|---|---|---|
  | **STS** | **0.667** | 67.7 | **175.3** | 9.1 |
  | SIR | 1.000 | 28.5 | 105.8 | 11.8 |
  | ETS | 1.000 | 29.6 | 79.7 | 18.9 |
  | RW / AR(1) / ARIMA / Theta | 1.000 | 9.9-25.0 | 48.4-60.8 | 4.7-5.6 |
  | **HSGP** | 1.000 | **3.4** | **7.3** | **1.3** |

  **What STS's 0.667 actually is** (`devel/sts_longrun/`).  Not a failure to
  converge: those fits return `nlminb` code 0 after 2,141 of 50,000 permitted
  iterations, an accepted polish, a POSITIVE DEFINITE Hessian and no parameter
  at a box bound.  The one reason is `quadratic objective gap 0.019 exceeds
  0.01` -- 0.019 nll units unclaimed on an objective of 63,405.

  Three things about it are counter-intuitive, and each was measured:

  * **It is the 2T slope block, not mean reversion.**  `sts_epidemic(trend =
    "local_level")` (T innovations) passes at a gap of 8.8e-07;
    `"local_linear"`, which has neither `slope_phi` nor `slope_mean`, fails at
    0.109.  `slope_phi` is fitted at **-0.002**, nowhere near a boundary, and
    `slope_sigma` sits exactly at its `exponential_prior(100)` mean.
  * **It is not the Dirichlet delay.**  `type = "auto"` fits the Dirichlet
    delay ONE-stage and every parametric delay TWO-stage (§`.collect_nowcast_fits`),
    so the longrun grid confounded delay family with stage.  Fitted one-stage,
    `lognormal_delay()` fails identically (gap 0.0165).  Fitted two-stage --
    what validation scores -- STS passes at 985 event-times with a gap of
    6.5e-07.  "The failures are the fast fits" is the same artifact: 6 cold
    init rungs of one joint fit against 26 warm-started two-stage fits.
  * **The gap is an aggregate, and the big gradient is a decoy.**  The whole
    max gradient of 1.68 sits on `sts_slope_mean`, which with `phi ~ 0` acts as
    a DRIFT, so its curvature is `sum_t lambda_t (t-2)^2` -- order `T^3`, about
    1e10 here.  A gradient of 1.68 against that is a displacement of 1.6e-10
    and contributes 1.4e-10 to the gap.  The 0.019 is ~1,000 innovation
    coordinates each about 0.05 off against curvature of order 30.

  `fit()` now closes this where it can, by taking the Newton step the gap
  prices (§2h) -- but only where a step can be CERTIFIED, and on these cells
  that is **one of five**: `semilocal x dirichlet` on `dengue`.  On lognormal,
  on gengamma, on `dengue_strata`, and on `local_linear`, every init rung steps
  onto indefinite curvature, the refinement declines, and the fit comes back
  bit-identical.  **Do not rely on it to make a one-stage long-series STS fit
  adequate.**  Use `type = "two_stage"` (which passes at a gap of 6.5e-07),
  `trend = "local_level"`, or HSGP.

  HSGP is 20x cheaper at the median and 24x at the tail, AND has the tightest
  band and the second-best centre -- on a series this long it is not a
  compromise.  **This used to be a convergence cliff for everything** (ETS
  passed 15% of dengue fits, STS 39%); that cause was `fit()` hard-coding
  `iter.max = 500` regardless of problem size (§2e).
- `include_drift` defaults to `d >= 1` and **errors at `d = 0`** (the ARMA mean
  and `mu_intercept` are the same quantity).
- A **number in any parameter slot holds it at that value**, as on the delay
  side (see §2d).
- Latent-state cost: ARIMA/ETS/RW/Theta are `T` innovations per stratum (same as
  AR(1)); `sts_epidemic()` with a slope is `2T`.  On a 1,000+ step weekly series
  STS is the slowest of the set by a wide margin.

**Internals**: `R/13_epidemic_timeseries.R` holds the trend builders
(`arima_trend`, `sts_trend`, `ets_trend`) and the constrained-parameter helpers.
They are **dual-mode** -- one implementation serves both the RTMB tape and the
plain-R `.joint_reconstruct()` mirror, via `.trend_zeros()` -- so the objective
and `predict()` cannot drift apart.  Dispatch codes: `epidemic_model` 5 = ARIMA,
6 = ETS family (ETS/RW/Naive/Theta), 7 = STS.

**auto_nowcast()**: the default candidate grid is still `{SIR, AR1, HSGP}` --
widening it would triple the selection cost for every existing caller. Compare
the new processes by passing them through `models =`:

```r
auto_nowcast(tn, models = list(
  model(nb_likelihood(), sts_epidemic(),  lognormal_delay()),
  model(nb_likelihood(), arima_epidemic(), lognormal_delay())))
```

**Validation harness**: `devel/validate_epidemic_timeseries.R` backtests the
whole process menu across dengue, covid_us, mpox, mpox-as-cumulative and
FluSight (stratified and pooled) with an hourly-updating ETA log at
`devel/epidemic_timeseries/progress.log`.

---

## 2e. The optimiser's iteration budget scales with the problem

`fit(control = NULL)` (the default) sizes the `nlminb` budget from the tape:
`iter.max = max(500, min(50000, 25 * n_parameters))`, `eval.max` twice that.
`.scaled_nlminb_control()` in `R/12_fit.R`; the L-BFGS-B polish scales the same way.

**Why it matters.** The joint fit optimises one latent innovation per event-time,
so `n_parameters` is a property of the DATA, not the model spec.  A constant
`iter.max = 500` is generous for an HSGP (28 parameters) and nowhere near enough
for a structural trend on a 1,095-week series (1,623).

**The failure was silent.** `nlminb` returns code 1, the fit is *kept*, and the
only trace is a `fit_check()` warning about a non-positive-definite Hessian —
never an error.  On dengue this affected six of nine processes: at 500
iterations ETS stopped with a max gradient of 6.6 and Theta with 107; given room
they reach 0.32 and 0.036, and ETS finds a better mode (nll 53061.5 -> 53049.0).

**Raising a cap is free when it does not bind.** A short series converges in the
same number of steps and returns a bit-identical objective — verified to
`0.00e+00` on mpox for HSGP/ETS/STS, and across seven processes pooled and
stratified. Only fits that were previously stopping early cost more.

If you see `optimizer code 1` in `fit_check()$reasons`, the budget bound; that
is now expected only on genuinely pathological fits.

---

## 2f. The cap on the latent log-incidence

The objective caps `log_mean` with a softplus,
`ub - log1p(exp(ub - log_mean))`, applied **downstream of every epidemic
process**, so a binding cap truncates all of them identically.  The bound is
`min(max(6, log1p(casemax)) + log(100), 16)`, where `casemax` is the largest
count **reported so far** per (event time, stratum).

**Why the headroom.**  The latent incidence exceeds the reported count by
`1/Gstar` -- the reciprocal of the reporting fraction, i.e. the quantity a
nowcast exists to estimate.  Without the `log(100)` term the ceiling sits below
the answer on any stream that is growing while mostly unreported.  On `covid_us`
in March-April 2020 it was below the settled truth at six of seven as-of dates
and the median nowcast came out at 8.7% of it, for all nine processes.  `exp(16)`
(~8.9M) remains the hard overflow stop.

**It distorts before it saturates.**  `lambda` keeps exactly
`plogis(ub - log_mean)` of its value: 95.3% three log units below the ceiling,
88.1% two, 50% at it.  Once saturated the gradient vanishes, so the trend is
unidentified above the ceiling and `lambda` goes flat.

**The diagnostic.**  `fit_check()` carries `log_mean_upper_bound`,
`max_log_mean`, `log_mean_headroom` and `log_mean_cap_bound`, and warns when the
headroom drops below 3.  The check reads the **uncapped** `log_mean` (`fit$mu`,
not `fit$mu_safe` -- `mu_safe` approaches the bound asymptotically and never
reaches it, so a check written against it can never fire).  A cap-bound fit is
`fit_status == "warning"` but `optimizer_adequate == TRUE`: the optimiser has
converged, to a ceiling.

**Count-cumulative streams are exempt from the WARNING, not the check.**  Act on
`log_mean_cap_reportable`, which is `FALSE` there: the horizon-0 nowcast is built
by the cohort kernels from the observed cumulative rather than from `lambda`, so
a saturated cap has no predictive consequence.  Measured on flusight, lifting the
bound from ~12 to 20 moved the median 0.5% and -0.2% and the objective by noise,
while `lambda` peaked at t=62/102 and t=165/408 -- the interior of the series,
nowhere near the event-time being scored.  Left reportable it fired on 52-65% of
flusight fits and would have trained callers to ignore a warning that matters a
great deal on the count-incidence path.  `log_mean_cap_bound` still records it.

**The escape hatch.**  `nowcast(..., mu_log_upper_bound = )` (a `...`
pass-through to `prepare_data()`) sets the bound explicitly.

## 2g. `arma_ridge`: AR and MA trading against each other

Like the cap in §2f this leaves `optimizer_adequate` alone -- the optimizer
really has converged -- and only sets `fit_status` to `"warning"`, with the
detail in `reasons`.  Reported by `fit_check()` and warned about by `nowcast()`.

For `arima_epidemic(p, d, q)` with `p >= 1` and `q >= 1`, the largest absolute
correlation between an AR and an MA coordinate in the Laplace covariance.  A high
value means a flat ridge: the likelihood barely moves along the direction that
increases one coefficient and decreases the other, so the pair is only weakly
identified -- but the fit converges and the Hessian stays positive definite,
because the flatness is a 2x2 block, not a global near-singularity.  What it
costs is the INTERVAL, not the median.

Calibrated over 65 fits (`devel/calibrate_arima_ridge.R`) against the ratio of
the ARIMA(p,d,q) band to the ARIMA(p+q,d,0) band on the same cell:

| band ratio | n | min | median | max |
|---|---|---|---|---|
| <= 5x | 35 | 0.044 | 0.198 | 0.423 |
| > 5x | 30 | 0.157 | 0.834 | 0.921 |

Every cut in `[0.5, 0.7]` flags the same 24 fits with **zero false alarms**, so
the threshold is 0.6, the middle of the empty band.  It caught **20 of 20** at
`q = 1` (10/10 on `(1,1,1)`, 10/10 on `(2,1,1)`); those fits sit within 1 nll
unit of their pure-AR reference, which is the flat-ridge signature.

**It detects near-collinearity only.**  The 6 blow-ups it missed are all
`q = 2`, with correlations of 0.16-0.26 and objectives 8.6 to 37.0 nll units
BETTER than their reference -- that is overfitting, a different failure, and no
correlation statistic should be expected to flag it.  A quiet check is not a
promise that the interval is sound.

---

## 2h. The joint fit takes the Newton step the gap prices

`quadratic_gap` (§`.joint_fit_diagnostic`) is not just a score: it IS the
objective decrease a Newton step on the free subspace would buy, and the
diagnostic has already factorised the Hessian that computes it.  Where the gap
is what fails a fit and that Hessian is positive definite, `fit()` now takes
the step rather than reporting the shortfall.  `.refine_on_quadratic_gap()` in
`R/12_fit.R`.

**When it fires.**  Only on a fit that would otherwise be reported inadequate.
It declines with a recorded reason otherwise -- `already_adequate`,
`gap_not_binding`, `hessian_not_positive_definite`, `nonfinite_fit`,
`no_analytic_hessian` -- and a fit that already passed is unchanged to the last
digit.

**Why it exists.**  A joint fit optimises one latent innovation per event-time,
so on a long series the gap is an AGGREGATE over thousands of coordinates, none
of them individually bad.  `sts_epidemic("semilocal")` at 985 event-times left
~1,000 innovations about 0.05 from the mode against curvature of order 30:
0.019 of unclaimed objective, against a tolerance of 0.01, with `nlminb`
returning code 0.  A five-vector L-BFGS-B cannot find the direction that fixes
a thousand coordinates simultaneously.  One Newton step can.

**The three guards, and why each is load-bearing.**  A Newton step is a LOCAL
model.  At a condition number of 1e6-1e7 a full step can lower the objective
and land where the Hessian is indefinite; accepting on objective alone fixed
one of three STS-with-slope cells and broke two, with Laplace ridges of 4.7 and
478.5.  So:

1. a step whose landing point is not certified by a successful Cholesky is
   discarded (`no_certified_step`);
2. the refined point is a candidate, kept only where the re-run diagnostic is
   positive definite AND either adequate or strictly lower-gap
   (`rejected_by_diagnostic`);
3. `last.par.best` is restored on every decline.

**Guard 3 is the one that will catch you again.**  RTMB records
`last.par.best` whenever `fn()` sees a lower objective, and `.nowcast_draws()`
samples the Laplace posterior at THAT vector, not at `opt$par`.  A line search
evaluates better-but-rejected candidates, so "restore by re-evaluating the kept
point" does not work -- the kept point is by construction the higher-objective
one.  Symptom: every optimizer statistic bit-identical to the unrefined fit,
and a Laplace ridge out of nowhere.  `.restore_tape_best()` exists for this;
any code that trial-evaluates an RTMB tape needs it.

**Measured** (`devel/sts_longrun/`, dengue at 2008-11-10, 985 event-times):

| cell | before | after |
|---|---|---|
| STS semilocal x dirichlet, one_stage | warning, gap 0.0190 | **pass**, gap 9.9e-05 |
| ETS additive x dirichlet, one_stage | pass, 5/6 rungs | pass, **6/6**, gap 3.2e-04 -> 1.7e-05 |
| STS local_level x dirichlet | pass, gap 8.8e-07 | bit-identical |
| STS semilocal x lognormal, one_stage | warning, gap 0.0165 | unchanged -- no rung certifies |
| STS semilocal x gengamma, one_stage | warning, gap 0.0273 | unchanged -- no rung certifies |
| STS semilocal x dirichlet, dengue_strata (3,984 par) | warning, gap 0.0553 | unchanged -- no rung certifies |
| STS local_linear x dirichlet, one_stage | warning, gap 0.1094 | unchanged -- no rung certifies |
| STS semilocal x lognormal, two_stage | pass, gap 6.5e-07 | unchanged, declines |

Cost is a few seconds on a ten-minute fit (8 steps measured at 11 s on a
2,006-parameter tape), and zero on anything that already passed.

---

## 2d. Fixed parameters

A number in a parameter slot means **hold it here**; a `prior_class` means
estimate it.  This works for every delay, epidemic, likelihood, revision and
cumulative parameter:

```r
model(nb_likelihood(phi = 5),                 # NB overdispersion held at 5
      ar1_epidemic(phi = 0.9, sigma = 0.1),   # AR(1) coefficients held
      lognormal_delay(mu = log(3)))           # delay location held
```

**How it works.** The parameter is seeded at the unconstrained value that maps
to the number, mapped out of the optimisation with `factor(NA)`, and its prior
and Jacobian terms are skipped -- they are constants once it stops moving.
`.resolve_fixed_parameter()` plus the `.unconstrain_*()` maps in `R/01_utils.R`
do the seeding; `.fill_fixed_parameters()` puts held values back into a
parameter list before reconstruction (a posterior **draw** has no entry for a
pinned parameter, since it is not in the Laplace precision).

**Gotchas**

- Stratified fits: supply **one** value to share across strata, or **one per
  stratum**.  Anything else errors naming the stratum count.
- `coef()` reports a held parameter.  `parameters()` does **not** -- it reports
  estimates with credible intervals, and a held parameter has no width.  It is
  also absent from `fit$obj$par`.
- A held value must be inside its domain.  Statically-knowable domains (`phi`
  in (-1, 1), probabilities in (0, 1), scales > 0) error **at construction**;
  `sigma` is bounded by the engine's `ar_sigma_max`, so that one errors at fit
  time -- and is deliberately rethrown past the six-rung init ladder rather than
  becoming "failed to converge for all init attempts".  Those aborts carry class
  `diseasenowcasting_invalid_fixed_value`.
- `mu` cannot be held under `strata_pooling = "hierarchical"`: the pooling is a
  model *for* the intercept, so pinning it leaves nothing to pool.  This errors.
- `ar1_epidemic(error = )` is **read by nothing** -- a dead slot.  The AR(1)
  innovations are always standard normal (non-centred).

**History.** Before 2.5.0 the epidemic and likelihood slots read only the prior
entry's `$dist`, never its `$is_constant` / `$fixed`, so a supplied number was
replaced by a standard-normal prior and the parameter was estimated anyway --
silently, in nine slots that the roxygen examples advertised as working.

## 3. Data preparation

### The tbl_now workflow (recommended)

```r
library(tbl.now)

# From a linelist (one row per case):
tn <- tbl_now(df, event_date = onset, report_date = reported,
              strata = sex, data_type = "linelist", verbose = FALSE)

# From pre-aggregated counts:
tn <- tbl_now(df, event_date = event_col, report_date = report_col,
              strata = region, case_count = n,
              data_type = "count-incidence", verbose = FALSE)

# Add day-of-week covariates for daily data:
tn <- tn |>
  add_temporal_effects(temporal_effects(day_of_week = TRUE)) |>
  compute_temporal_effects()
```

### tbl.now companion package — full reference

`diseasenowcasting` consumes data as a `tbl_now` object from the companion
[`tbl.now`](https://github.com/RodrigoZepeda/tbl.now) package: a tibble that
carries **two time indices** (`event_date`, `report_date`) plus modelling
metadata (strata, covariates, temporal effects, `now`, units, data type) and is
fully dplyr-compatible.

**Create**

```r
tbl_now(data, event_date, report_date,
        strata = NULL, covariates = NULL, case_count = NULL, is_censored = NULL,
        now = NULL,                # Date; default max(report_date)
        event_units = "auto",      # "days"|"weeks"|"months"|"years"|"numeric"
        report_units = "auto",
        data_type = "auto",        # "linelist"|"count-incidence"|"count-cumulative"
        t_effects = character(0),  # temporal_effects() spec, stored LAZILY
        verbose = TRUE, align_weeks = FALSE)
```

Auto-added **protected** columns: `.event_num`, `.report_num`,
`.delay` (= `.report_num - .event_num`). Removing a protected column downgrades
the object back to a plain tibble (with a warning).

**Getters** (every attribute has one)

```r
get_event_date(x) / get_report_date(x)    # column NAMES (character), not the dates
get_event_units(x) / get_report_units(x)  # "days"|"weeks"|"months"|"years"|"numeric"
get_now(x)                                 # Date — the as-of date
get_strata(x) / get_num_strata(x)
get_covariates(x) / get_num_covariates(x)
get_case_count(x) / get_is_censored(x) / get_data_type(x)
get_temporal_effects(x)        # list of LAZY specs (length 0 = none attached)
get_temporal_effect_cols(x)    # computed column names (character(0) before compute)
get_latest_reported_cases(x)   # most-recent count per event_date -> the "truth" for scoring
get_initial_reported_cases(x)  # first-reported count per event_date
```

**Data types** — `"linelist"` (one row per case), `"count-incidence"` (count
reported *exactly* on `report_date`), `"count-cumulative"` (cumulative up to
`report_date`). Convert with `to_count(x, to = "count-incidence")`.

**Temporal effects (lazy, two-step).** `nowcast(temporal_effects = "auto")` does
this for you, but to control it manually:

```r
spec <- temporal_effects(day_of_week = TRUE, week_of_year = TRUE,
                         month_of_year = FALSE, seasons = integer(0))  # seasons = Fourier periods, e.g. c(7, 52)
x <- x |> add_temporal_effects(spec, date_type = "event_date")  # attaches spec; NO columns yet
x <- compute_temporal_effects(x)                                # materialises the columns
get_temporal_effect_cols(x)                                     # the created column names
```

dplyr verbs (`filter`/`select`/`mutate`/`group_by`/`rename`/…) **preserve the
spec** and never trigger computation; only `compute_temporal_effects()` adds
columns. Pre-attach + compute on the `tbl_now` and `nowcast()` will use the
covariates; otherwise pass `temporal_effects = "none"` to disable.

**Modify metadata** — changers replace, adders append, removers drop:

```r
change_now(x, as.Date("2023-06-01"))           # move the as-of date (re-censors)
change_strata(x, ...) / add_strata(x, ...) / remove_strata(x, ...)
change_covariates(...) / add_covariates(...) / remove_covariates(...)
add_is_censored(x, is_censored = my_logical)   # mark reports as right-censored delays
```

**Utilities**

```r
complete_zeroes(x)             # fill missing event/report/strata cells with 0
align_weeks(x, date_col)       # snap to a consistent epiweek day (integer .delay)
week_2_date(x, week, year)     # epiweek + year -> Date
update(x, new_data)            # bind new rows, preserving attributes
is_tbl_now(x) / tbl_now_attributes(x)
```

**tbl.now pitfalls**

- `get_event_date()` returns the column **name**, not the dates. For the calendar
  grid `diseasenowcasting` uses `nc@engine$min_event` + `nc@engine$event_unit`.
- Call `add_temporal_effects()` **before** `compute_temporal_effects()` (else no-op).
- `rowwise()` is **not** supported on a `tbl_now`.
- `get_temporal_effects()` returns specs; use `get_temporal_effect_cols()` for names.

### Missing strata

`NA` or `""` strata values are silently mapped to an explicit `"missing"` level
— they form their own cell in the K-way product, modelled like any other stratum
with its own intercept + trend, sharing the delay/phi/kernel.

### prepare_from_tbl_now()

```r
# Internal; called automatically by nowcast()
prep <- prepare_from_tbl_now(tn, model, now = as.Date("2023-01-01"))
# Returns list(data=engine, now, event_col, min_event, event_unit,
#              max_time, strata_cols, strata_levels)
```

### infer_max_time()

```r
# Exported convenience: the number of event-times the model spans (t = 0..max_time-1).
# Use it to size a custom_epidemic() intensity_fn whose loop runs over time.
max_time <- infer_max_time(tn)                  # = prepare_from_tbl_now(tn, model())$max_time
```

### prepare_data() — lower-level

```r
engine <- prepare_data(
  model, m,                 # m = [event_time, count, delay, cell_index] matrix
  X        = NULL,          # [max_time × P] covariate matrix
  d_star   = NULL,          # [max_time × num_strata] max-observable-delay matrix
  max_time = NULL,          # default max(m[,1])
  num_strata = NULL,        # default inferred from m[,4]
  gp_L = 1.5,
  gp_boundary_frac = 0.62,  # validated default
  ar_sigma_max = 1
)
```

Key output fields:
- `case_counts` — `[max_time × num_strata]` matrix of observed counts
- `d_star` — `[max_time × num_strata]` matrix of max-observable delays
- `num_strata`, `max_time`, `delay_family`, `epidemic_model`, `is_negative_binomial`

---

## 4. Fitting

### Main entry point: nowcast()

```r
nc <- nowcast(
  data,                        # tbl_now
  model,                       # model() object
  type    = "one_stage",       # or "two_stage"
  now     = NULL,              # as.Date(); default get_now(data)
  K       = 25,                # delay imputations (two_stage only)
  n_draws = 2000,
  delay_window = 120,          # days/weeks used for Stage-1 delay fit
  floor_mu     = 0.08,         # minimum delay-mean spread for imputation
  floor_sig_frac = 0.08,       # minimum sigma spread fraction
  np_spread    = 1,            # Dirichlet simplex imputation covariance scale
  temporal_effects = "auto",   # "auto" | "none"; auto-adds DOW/seasonality
  seed = NULL,
  ...                          # passed to prepare_from_tbl_now / prepare_data
)
```

Returns a `nowcast_class` S7 object.

**NB overdispersion `phi` is NOT a `nowcast()` argument.** Set it on the
likelihood: `model(nb_likelihood(phi = lognormal_prior(log(5), 0.5)), ...)`.
The default `nb_likelihood()` uses `lognormal_prior(log(20), 0.5)`.

### one_stage vs two_stage vs auto

- `one_stage`: single joint fit of delay + epidemic simultaneously.  Fast; can
  underestimate delay uncertainty.
- `two_stage`: Stage 1 = delay-only fit on a recent window; Stage 2 = K joint
  fits with delay hard-fixed at imputed values; draws are pooled.  Recommended
  for production.  Adds ~K× the cost of one fit.
- `auto`: resolves per delay in `.collect_nowcast_fits` — **dirichlet one-stage,
  every other delay two-stage** (dirichlet scored worse under two-stage simplex
  imputation in experiments).  Available on `nowcast()` and `backtest()`.

### auto_nowcast() — pick the best model automatically

```r
# Builds a candidate grid (epidemic process x delay) sized to the series length,
# backtests + scores it, refits the winner. Returns a nowcast_class with the
# scoreboard in @comparison = list(scores, chosen, metric, max_time).
nc <- auto_nowcast(
  data,                          # tbl_now
  metric = "wis",                # "wis"|"ape"|"mse"|"coverage_50"(|cov50-.5|)|"coverage_90"(|cov90-.9|)|"coverage"(both)
  type   = "auto",               # backtest + final fit strategy
  sir = NULL, ar = NULL, hsgp = NULL,   # pass a component to inject priors + force it in
  delays = NULL,                 # default {lognormal, gen-gamma, dirichlet}
  likelihood = nb_likelihood(),  # single, OR list(nb_likelihood(), poisson_likelihood())
  models = NULL,                 # extra model() objects (e.g. custom_delay/custom_epidemic) to compare
  n_dates = 6, n_draws_select = 500,    # fast selection backtest
  n_draws = 2000, K = 25,        # full final refit
  min_ar = 15, min_hsgp = 30)    # length thresholds
# Epidemic candidates by max_time: <min_ar -> {SIR}; [min_ar,min_hsgp) -> {SIR,AR1};
# >=min_hsgp -> {AR1,HSGP}. Any explicit sir=/ar=/hsgp= is force-included.
# Set future::plan(multisession) before calling for parallel backtesting.

# Accessors (prefer over reaching into @comparison):
best_model_name(nc)    # winning label, e.g. "SIR/nb/Dirichlet" (= @comparison$chosen)
best_model(nc)         # the winning model() object (= nc@model), reusable in nowcast()/backtest()
comparison_scores(nc)  # the ranked scoreboard data.frame (= @comparison$scores)
best_score(nc)         # the single scoreboard row for the winner
selection_metric(nc)   # the metric used (= @comparison$metric)
# best_model_name/comparison_scores/best_score/selection_metric error on a plain
# nowcast() (no @comparison); best_model() works on any nowcast. print(nc) shows
# the top of the scoreboard when @comparison is present.
```

### save_nowcast() / load_nowcast() — persist a fit to disk

```r
# RTMB tapes have external pointers that DON'T survive saveRDS (a reloaded tape
# crashes R). save_nowcast() drops the tape and stores: model() spec, input
# tbl_now, priors, engine, and per-fit params + Laplace MODE + PRECISION.
save_nowcast(nc, "fit.rds")          # works on a nowcast() or auto_nowcast() result
nc2 <- load_nowcast("fit.rds")       # predict()/coef()/parameters()/autoplot() all work
predict(nc2, n_draws = 500)          # any n_draws (re-sampled from stored precision; NOT frozen)
nowcast(nc2@data, nc2@model)         # re-fit from the loaded model + bundled data
nc3 <- load_nowcast("fit.rds", rebuild = TRUE)  # also re-tape obj (no re-opt); needs RTMB for custom
# predict() works with NO RTMB attached, even for custom delays/epidemics (cdf_factory/
# intensity_fn are plain R; only the Laplace mode+precision are needed -- see .nowcast_draws
# obj==NULL branch in R/15_nowcast.R, and the parameters() obj==NULL fallback in R/27_parameters.R).
```

### fit() — lower-level

```r
# Returns a list: par, parList, nll, convergence, obj, opt, random,
#   use_random, epi_model, is_nb, lambda [T×S], mu, mu_safe, Gstar [T×S],
#   delay_mu, delay_sigma, phi_nb, reconstruct, Bmat, freq, data, priors, model
result <- fit(model, engine, priors = NULL, init = NULL, control = list(...))
```

### default_priors()

```r
priors <- default_priors(model, engine, phi = lognormal_prior(log(20), 0.5))
# Returns named list of prior_class objects: delay_mu, delay_sigma, delay_Q,
# phi_nb, mu_intercept, gamma_cov, gp_alpha, gp_ell, ar_phi, ar_sigma,
# R0, gamma_sir, N_eff, delay_probs
```

---

## 5. Results

### Parameter estimates

```r
coef(nc)
# Named vector: delay_mu, delay_sigma, phi_nb, mu_intercept,
#               log_gp_alpha, log_gp_ell, ar_phi_unc, log_ar_sigma_unc, ...
# For two-stage: delay_mu/sigma averaged over imputations
```

### Posterior-predictive nowcast

```r
pred <- predict(nc, n_draws = NULL, summary = FALSE, seed = NULL)
# Returns nowcast_prediction_class with slot @draws [n_draws × max_time]

summary(pred)
# data.frame: mean, median, sd, mad, q2.5, q5, q10, q25, q50, q75, q90, q95,
#             q97.5, .event_num (0-indexed)

autoplot(pred)   # ggplot2: median + 50%/90% ribbons
```

### Reporting fraction and inflation

```r
rf <- reporting_fraction(nc)              # one row per (event-time, stratum)
# event_date, .event_num, stratum, horizon, observed,
# reporting_fraction, reporting_fraction_low/high, inflation
subset(rf, horizon == 0)$inflation        # what the nowcast multiplies by
reporting_fraction(nc, summary = FALSE)   # one row per retained fit too
```

`reporting_fraction` is the engine's `Gstar` = `F_D(d_star + 1)`: the fraction of
each cohort the fit believes has arrived. `inflation = 1 / reporting_fraction` is
**exactly the multiplier the nowcast applies to the observed count** — the whole
of its claim, as one dimensionless number per cohort. It is the quantity most
level errors travel through and it is invisible in `summary(predict(nc))`.

`reporting_fraction` is in (0, 1], `inflation` in [1, Inf). Both are *fitted*,
not observed — the delay law is extrapolated past the data.

**A very large inflation at horizon 0 is normal, not a fault.** Early-2020
`covid_us` genuinely needs 37–83x; a fit reporting ~1 there would be badly
wrong. There is no universally suspicious threshold, which is why nothing warns.
What is informative is comparing it against the retrospective settled inflation,
or watching its stability: `covid_colombia` runs 15.8 (Mar 2020), 103.2 (Jun),
5.1 (Oct), and a single stationary delay is then wrong in both directions at
different dates. With `summary = TRUE` the `_low`/`_high` columns are the range
over retained fits — under `type = "two_stage"` that spread is the delay
uncertainty the cascade propagates, and at horizon 0 it can span two orders of
magnitude.

### Latent incidence (lambda)

```r
mean(nc, seed = 42)       # numeric vector length max_time (posterior mean)
median(nc, seed = 42)     # posterior median
quantile(nc, probs = c(0.025, 0.5, 0.975), seed = 42)  # [max_time × probs]
summary(nc)               # prints params + latent incidence table
```

### Lower-level draw helper

```r
summarise_nowcast_matrix(draws_matrix)
# draws_matrix: [n_draws × max_time] → data.frame with q2.5..q97.5 + .event_num
```

---

## 6. Backtesting and scoring

### backtest()

```r
bt <- backtest(
  data,                          # tbl_now
  models = list(mdl1, mdl2),     # or a single model()
  dates  = NULL,                 # vector of Date; NULL = n_dates evenly spaced
  n_dates = 20,
  type   = "one_stage",
  max_delay = NULL,              # truth-completeness horizon (event units);
                                 #   NULL = 99th pct of observed delays. Dates
                                 #   within max_delay of the last report are
                                 #   dropped (truth not yet complete). Inf = keep all.
  n_draws = 1000,
  K = 25,
  return_simulations = FALSE,    # if TRUE, @simulations slot populated
  seed = NULL,
  ...
)
# Returns backtest_class; parallelised via future.apply::future_lapply()
# Set future::plan(multisession, workers=8) before calling for parallel execution
```

### score()

```r
score(bt, metric = c("wis", "ape", "mse"), report = TRUE)
# Returns data.frame: model, wis, ape, mse, coverage_50, coverage_90
# Sorted best-first by `metric`; cli report printed if report=TRUE
# Accepts a backtest_class or a predict() data.frame
```

### autoplot

```r
autoplot(bt)     # faceted by model; median + 50%/90% ribbons vs final truth
autoplot(pred)   # single nowcast
```

---

## 6b. Surprise (anomaly detection) and censoring

### surprise() — is new data surprising under the fit?

```r
# type "count": new_data has columns event_index (0-indexed), count
# type "delay": new_data has columns delay (event units), optional weight
# type "both" (default): supply both. level = credible level (default 0.99).
s <- surprise(nc, new_data, type = "both", level = 0.99, n_draws = 500)
s$count_surprise   # event_index, observed, ppp_right/left, direction (high/low), is_surprising
s$delay_surprise   # delay, mean_tail_prob (P(D>=d)), cdf_prob (P(D<=d)), direction (long/short), is_surprising
```

### update() computes surprise automatically + warns

```r
nc2 <- update(nc, new_rows, surprise_level = 0.99)   # one warning listing surprising delays
extreme_values(nc2)                                  # tidy data.frame of flagged surprises (or NULL)
update(nc, new_rows, compute_surprise = FALSE)        # silent
```

`update()` scores only **too-long reporting delays** (upper-censored) against the
previous fit and raises a single warning (in the data's time unit). It does NOT
score the epidemic/count process. `extreme_values(nc)` returns the flagged rows.

### prior_only: see what a prior implies

```r
# Draw the epidemic from the PRIORS only (no fitting); data just sets the grid.
nc <- nowcast(tn, model(nb_likelihood(), sir_epidemic(R0 = lognormal_prior(log(3), 0.1)), lognormal_delay()),
              prior_only = TRUE, n_draws = 300)
quantile(nc, probs = c(0.05, 0.5, 0.95))   # prior-predictive epidemic band
# Works for HSGP / AR1 / SIR; predict()/autoplot()/median() all apply.
```

### Default priors

Each component constructor documents its defaults in a **Default priors** roxygen
section: `?epidemic_process` (HSGP alpha~HalfNormal(0,1), ell~InvGamma(3,1);
AR1 phi~StdNormal, sigma~Exp(100); SIR R0~LogNormal(log2,0.5),
gamma~LogNormal(log(1/5),0.5), N_eff~Beta(2,5)), `?delay_process`, `?likelihood`
(phi~LogNormal(log20,0.5)). Delay means + the intercept are data-informed.
and **new report delays** against the previous fit, and raises a cli warning
naming exactly what was surprising (count too high/low, delay too long/short).

### Censoring an outlier delay (m_censored)

A report flagged `is_censored` contributes `log G_D(j)` (delay <= j, an upper
bound) instead of the exact-delay term — so an extreme outlier delay stops
distorting the fitted delay distribution.  `nowcast()` reads the tbl_now's
`is_censored` column automatically (both the delay-only and joint objectives
handle it).

```r
# Flag every report whose delay exceeds a bound as censored, then re-fit.
# censor_delays_above() lives in tbl.now (a Depends, so it is already attached):
tn2 <- censor_delays_above(tn, max_delay = 45)   # sets is_censored = TRUE for delay > 45
nc  <- nowcast(tn2, model())                      # uses the censored delays

# Or set is_censored yourself when building the tbl_now:
# tbl.now::tbl_now(df, ..., is_censored = my_logical_column)
```

The typical loop: fit -> `update()` warns "delay too long" -> `censor_delays_above()`
-> re-fit (better-calibrated delay distribution, often better WIS for one-stage fits).

---

## 7. Internal architecture (for contributors)

### build_joint_obj()

```r
built <- build_joint_obj(data, priors, init = NULL, use_random = FALSE)
# Returns list(obj, random, epi_model, is_nb, Bmat, freq, n_strata)
# obj is an RTMB::MakeADFun result; obj$fn/gr/he are the nll/gradient/hessian
```

**Parameter layout** (per-stratum where S = num_strata):

| Parameter | Shape | Shared? |
|---|---|---|
| `mu_intercept` | `[S]` | No (per-stratum) |
| `gamma` | `[P × S]` | No |
| `basis_coefs` (HSGP) | `[num_basis × S]` | No (but kernel alpha/ell shared) |
| `log_gp_alpha`, `log_gp_ell` | scalar | Yes (shared kernel) |
| `ar_innov` (AR1/SIR) | `[T × S]` | No |
| `ar_phi_unc`, `log_ar_sigma_unc` | `[S]` | No |
| `log_R0`, `u_gamma`, `u_neff` (SIR) | `[S]` | No |
| `log_phi_nb` | scalar | Yes |
| `delay_mu`, `log_delay_sigma_excess`, `delay_Q` | scalar | Yes |
| `delay_logits` (Dirichlet) | `[n_bins]` | Yes |
| `custom_delay_params` (custom delay, family 5) | `[n_params]` | Yes |
| `custom_epidemic_params` (custom epidemic, epidemic_model 4) | `[n_params]` | n/a (user owns full `log_mean[T×S]`) |
| `arima_ar_pacf_unc`, `arima_ma_pacf_unc` (ARIMA) | `[p × S]`, `[q × S]` | No |
| `log_arima_sigma_unc`, `arima_drift` (ARIMA) | `[S]` | No |
| `arima_innov` (ARIMA) | `[T × S]` | No |
| `log_ets_sigma_unc`, `ets_beta_unc`, `ets_damp_unc`, `ets_drift`, `ets_slope_init` (ETS family) | `[S]` | No |
| `ets_innov` (ETS family) | `[T × S]` | No |
| `log_sts_level_sigma_unc`, `log_sts_slope_sigma_unc`, `sts_slope_phi_unc`, `sts_slope_mean`, `sts_slope_init` (STS) | `[S]` | No |
| `sts_level_innov`, `sts_slope_innov` (STS) | `[T × S]` | No |

**Component dispatch codes.** `delay_family`: 1=LogNormal, 2=Gamma,
3=GenGamma, 4=Dirichlet, **5=Custom**.  `epidemic_model`: 1=HSGP, 2=AR1,
3=SIR, **4=Custom**, **5=ARIMA**, **6=ETS family**, **7=STS**.  Custom params are fixed via the per-element `priors` API
(a number fixes, a prior frees) → an RTMB `map` with `factor(NA)` entries.

**Smooth cap on log_mean** (prevents exp() overflow):
```r
log_mean_capped <- upper_bound - log1p(exp(upper_bound - log_mean_col))
```
where `upper_bound = min(max(6, log1p(casemax)) + log(100), 16)` and
`casemax` is the largest count REPORTED so far. See §2f.

### .joint_reconstruct()

Plain-R mirror of the objective (no AD tape).  Takes `parlist` (from
`obj$env$parList()` or `.split_named_vector(draw)`), returns list:
`mu`, `mu_safe`, `lambda [T×S]`, `Gstar [T×S]`, `delay_mu`, `delay_sigma`,
`phi_nb`, `log_loc`, `log_scale`.

### .nowcast_draws()

```r
draws <- .nowcast_draws(fit, target, n_draws, probs, seed)
# fit must have: $data, $priors, $obj (with last.par.best), $use_random,
#               $Bmat, $freq
# Returns list(M [n_draws×T], lambda_draws [n_draws×T],
#              M_strata [n_draws×T×S], lambda_strata [n_draws×T×S],
#              n_strata, nowcast, draws, quantiles, median, target, observed)
```

The total `M` and `lambda_draws` are `rowSums()` over strata — at S=1 they
equal the single-column matrices exactly.

### .pool_fit_draws()

```r
pooled <- .pool_fit_draws(fits_list, target, n_draws)
# Returns list(M [pooled_n×T], lambda [pooled_n×T])
# Used by both predict() and .nowcast_lambda_draws()
```

### Delay aggregation (prepare_data)

```r
aggregate_by_delay_and_time(obs_matrix)
# Pools ALL strata (ignores col 4) — correct because delay is shared
# Returns list(obs_delays, row_sums, col_sums)
# Fixed censoring: c_t = max_time - t + 1 (per-time, not per-season)
```

---

## 8. Common gotchas

### Per-stratum vector shapes

`ar_phi_unc` and `log_ar_sigma_unc` **must** have length `num_strata`, not 1.
If you warm-start with a scalar from a previous fit, `.adapt_init()` recycles it
to the right length — but if you build `init` by hand you must do this yourself:

```r
init$log_ar_sigma_unc <- rep(-2, engine$num_strata)
init$ar_phi_unc       <- rep(0,  engine$num_strata)
```

### Matrix init for basis_coefs / ar_innov / gamma

These are `[rows × num_strata]` matrices.  Always initialise as matrices:

```r
init$basis_coefs <- matrix(0, engine$num_basis, engine$num_strata)
init$ar_innov    <- matrix(0, engine$max_time,  engine$num_strata)
init$gamma       <- matrix(0, engine$P,         engine$num_strata)
```

### GenGamma Q bounds

The raw parameter `delay_Q` is the *unconstrained* value; the actual shape is:

```r
shape_Q <- 0.05 + 2.95 * plogis(delay_Q)   # ∈ (0.05, 3)
```

Never set `shape_Q` directly in `init`; set `delay_Q` (the raw unconstrained
value).  Raw `delay_Q = -2` ≈ `shape_Q = 0.27` (near log-normal).

### joint-mode vs random= Laplace

`use_random = FALSE` (default, via `getOption("diseasenowcasting.use_random", FALSE)`) uses
the joint Hessian — same as `cmdstanr $laplace()`.  Set
`options(diseasenowcasting.use_random = TRUE)` to switch to the marginal nested Laplace
(slower, sometimes more accurate for hierarchical models).

### HSGP num_basis: too many is worse than too few

Auto `num_basis` is `3` below 10 event-times, `8` below 20, then
`min(150, max(12, ceiling(1.5 * sqrt(max_time))))` — ~60 for a 1,500-day series,
which can cause an ill-conditioned Laplace.  For COVID-length series, cap at 20:

```r
model(nb_likelihood(), hsgp_epidemic(num_basis = 20L), lognormal_delay())
```

**But do not carry that 20 onto a short series.**  The basis count sets the
shortest resolvable wavelength (basis `j` ≈ `2 * gp_L * max_time / j`), and the
place an over-flexible basis bends is the right-hand edge, where reporting is
least complete.  On mpox at `now = 2022-08-09` (33 event-times, settled truth
64), one-stage NB + log-normal:

| num_basis | median | 90% band | covers? |
|---|---|---|---|
| 8 | 84 | [9, 484] | yes |
| 12 (auto) | 186 | [19, 1913] | yes |
| 20 | 1540 | [180, 13373] | **no** |

`prepare_data()` warns when an **explicitly supplied** `num_basis` exceeds
`max_time / 2` (`.warn_hsgp_basis_fraction()`).  An automatic count never warns:
the ladder's floor of 12 is itself more than half of a 20-step series, so
checking it would fire on the package's own default path for every short series.
Short series are handled better elsewhere — `auto_nowcast()` keeps HSGP out of
its grid below `min_hsgp = 30`.  The warning is also skipped in `delay_only`
mode, where no epidemic process is built.

This used to be invisible: the `log_mean` cap (§2f) clipped the runaway into
something plausible-looking.

### delay_only = TRUE skips fit() validity check

`fit()` blocks `delay_only` fits via its epidemic-GQ validity check.  Use
`fit_internal()` / `build_delay_only_obj()` + `nlminb()` directly, or call
`nowcast(..., delay_only = TRUE)` if that path is exposed.  For the two-stage
cascade, the delay-only Stage-1 is handled internally by `.collect_nowcast_fits()`
— you don't call it directly.

### Dirichlet two-stage: simplex dimension must match Stage-2

When `dirichlet_delay(bins = k)`, the simplex has `k+1` entries (including the
geometric tail).  The bins count must be consistent between Stage-1 and Stage-2.
The package handles this automatically when you use `nowcast()`.

---

## 9. Datasets

| Object | Package | Description | Key columns |
|---|---|---|---|
| `denguedat` | tbl.now | Weekly dengue linelist, Colombia | onset_week, report_week, gender |
| `mpoxdat` | tbl.now | Daily mpox counts, USA 2022 | dx_date, dx_report_date, race, n |
| `covidat` | tbl.now | Daily COVID counts (small demo) | date_of_symptom_onset, date_of_registry, sex, n |
| `covid_colombia` | **diseasenowcasting** | Daily COVID counts, Colombia 2020–2023 (37,600 rows) | notification_date, diagnosis_date, sex, n |

For `covid_colombia`: event = `notification_date`, report = `diagnosis_date`.

---

## 10. Typical session skeleton

```r
library(diseasenowcasting)
library(tbl.now)

# 1. Build tbl_now
tn <- tbl_now(my_data, event_date = onset, report_date = reported,
              strata = region, data_type = "linelist", verbose = FALSE) |>
  add_temporal_effects(temporal_effects(day_of_week = TRUE)) |>
  compute_temporal_effects()

# 2. Choose a model
mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())

# 3. Fit (production: type="two_stage", K=25)
nc <- nowcast(tn, mdl, type = "two_stage", K = 25, n_draws = 2000,
              now = as.Date("2023-06-01"), seed = 42)

# 4. Inspect
coef(nc)
summary(predict(nc, seed = 42))
autoplot(predict(nc, seed = 42))

# 5. Backtest multiple models
plan(future::multisession, workers = 8)
bt <- backtest(tn, list(mdl, model(nb_likelihood(), ar1_epidemic(), lognormal_delay())),
               dates = my_dates, type = "two_stage", K = 5, seed = 42)
score(bt)
autoplot(bt)

# 6. Update as new data arrive (warm-start)
nc2 <- update(nc, new_rows)
```
