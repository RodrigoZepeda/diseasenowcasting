# diseasenowcasting — AI Agent Reference Guide

This document is a complete reference for AI assistants (and human
contributors) working in the `diseasenowcasting` codebase. It is
designed so that you can write correct `diseasenowcasting` code from
scratch without reading the source.

------------------------------------------------------------------------

## 1. What diseasenowcasting does

`diseasenowcasting` is a standalone R package for **Bayesian epidemic
nowcasting** using the RTMB autodiff engine (CppAD + built-in Laplace
approximation). It reimplements the `diseasenowcast2` Stan-based package
with no Stan/cmdstanr dependency, matching the same public API:
`model(likelihood(), epidemic(), delay())`.

**Core idea:** The reporting delay is a stochastic process modelled
jointly with the epidemic. The likelihood depends only on *observed*
delays (not a reporting triangle), so censoring is handled cleanly. The
Laplace approximation marginalises over the latent epidemic
coefficients; posterior draws are sampled from N(mode, H⁻¹) where H is
the joint Hessian.

**Default mode:** joint-mode Laplace (`use_random = FALSE`) — same as
`cmdstanr $laplace()`. 20–400× faster than the marginal (`random=`)
Laplace.

**Two-stage cascade:** Stage 1 fits the delay only
(`delay_only = TRUE`); Stage 2 hard-fixes the delay at K imputed values
and pools draws. This propagates delay uncertainty without sacrificing
convergence.

------------------------------------------------------------------------

## 2. The model menu

``` r

model(likelihood, epidemic, delay)   # combine three components
```

### Likelihoods

| Constructor | Count distribution | Notes |
|----|----|----|
| [`nb_likelihood()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md) | Negative-binomial (NB-2) | Default; handles overdispersion |
| [`poisson_likelihood()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md) | Poisson | Simpler; good for low counts |

### Epidemic models

| Constructor | Key args | Description |
|----|----|----|
| `hsgp_epidemic(num_basis, gp_kernel=2, gp_basis=1, tmax_model=0, gp_boundary_frac=0.62)` | `num_basis` (int, 0=auto) | Hilbert-space GP; flexible smooth trend. Shared kernel (alpha, ell) across strata. |
| [`ar1_epidemic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/epidemic_process.md) | — | AR(1) trend; fast, per-stratum phi/sigma. |
| `sir_epidemic(N_pop=1e6, use_beta_rw_trend=TRUE)` | `N_pop` | Discrete-time SIR; coupled force of infection across strata. |

### Delay families

| Constructor | Distribution | Notes |
|----|----|----|
| [`lognormal_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md) | Log-normal | Best for COVID; fastest convergence. |
| [`gamma_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md) | Gamma (mean/SD) | Good for dengue/mpox. |
| [`generalized_gamma_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md) | Generalised Gamma | Most flexible; Q ∈ (0.05, 3) bounded. |
| `dirichlet_delay(bins=NA)` | Non-parametric simplex | Dirichlet prior + geometric tail; two-stage only. |

### Combining

``` r

mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
```

------------------------------------------------------------------------

## 3. Data preparation

### The tbl_now workflow (recommended)

``` r

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

`diseasenowcasting` consumes data as a `tbl_now` object from the
companion [`tbl.now`](https://github.com/RodrigoZepeda/tbl.now) package:
a tibble that carries **two time indices** (`event_date`, `report_date`)
plus modelling metadata (strata, covariates, temporal effects, `now`,
units, data type) and is fully dplyr-compatible.

**Create**

``` r

tbl_now(data, event_date, report_date,
        strata = NULL, covariates = NULL, case_count = NULL, is_censored = NULL,
        now = NULL,                # Date; default max(report_date)
        event_units = "auto",      # "days"|"weeks"|"months"|"years"|"numeric"
        report_units = "auto",
        data_type = "auto",        # "linelist"|"count-incidence"|"count-cumulative"
        t_effects = character(0),  # temporal_effects() spec, stored LAZILY
        verbose = TRUE, align_weeks = FALSE)
```

Auto-added **protected** columns: `.event_num`, `.report_num`, `.delay`
(= `.report_num - .event_num`). Removing a protected column downgrades
the object back to a plain tibble (with a warning).

**Getters** (every attribute has one)

``` r

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

**Data types** — `"linelist"` (one row per case), `"count-incidence"`
(count reported *exactly* on `report_date`), `"count-cumulative"`
(cumulative up to `report_date`). Convert with
`to_count(x, to = "count-incidence")`.

**Temporal effects (lazy, two-step).**
`nowcast(temporal_effects = "auto")` does this for you, but to control
it manually:

``` r

spec <- temporal_effects(day_of_week = TRUE, week_of_year = TRUE,
                         month_of_year = FALSE, seasons = integer(0))  # seasons = Fourier periods, e.g. c(7, 52)
x <- x |> add_temporal_effects(spec, date_type = "event_date")  # attaches spec; NO columns yet
x <- compute_temporal_effects(x)                                # materialises the columns
get_temporal_effect_cols(x)                                     # the created column names
```

dplyr verbs (`filter`/`select`/`mutate`/`group_by`/`rename`/…)
**preserve the spec** and never trigger computation; only
[`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/compute_temporal_effects.html)
adds columns. Pre-attach + compute on the `tbl_now` and
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
will use the covariates; otherwise pass `temporal_effects = "none"` to
disable.

**Modify metadata** — changers replace, adders append, removers drop:

``` r

change_now(x, as.Date("2023-06-01"))           # move the as-of date (re-censors)
change_strata(x, ...) / add_strata(x, ...) / remove_strata(x, ...)
change_covariates(...) / add_covariates(...) / remove_covariates(...)
add_is_censored(x, is_censored = my_logical)   # mark reports as right-censored delays
```

**Utilities**

``` r

complete_zeroes(x)             # fill missing event/report/strata cells with 0
align_weeks(x, date_col)       # snap to a consistent epiweek day (integer .delay)
week_2_date(x, week, year)     # epiweek + year -> Date
update(x, new_data)            # bind new rows, preserving attributes
is_tbl_now(x) / tbl_now_attributes(x)
```

**tbl.now pitfalls**

- [`get_event_date()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.html)
  returns the column **name**, not the dates. For the calendar grid
  `diseasenowcasting` uses `nc@engine$min_event` +
  `nc@engine$event_unit`.
- Call
  [`add_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.html)
  **before**
  [`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/compute_temporal_effects.html)
  (else no-op).
- `rowwise()` is **not** supported on a `tbl_now`.
- [`get_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.html)
  returns specs; use
  [`get_temporal_effect_cols()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.html)
  for names.

### Missing strata

`NA` or `""` strata values are silently mapped to an explicit
`"missing"` level — they form their own cell in the K-way product,
modelled like any other stratum with its own intercept + trend, sharing
the delay/phi/kernel.

### prepare_from_tbl_now()

``` r

# Internal; called automatically by nowcast()
prep <- prepare_from_tbl_now(tn, model, now = as.Date("2023-01-01"))
# Returns list(data=engine, now, event_col, min_event, event_unit,
#              max_time, strata_cols, strata_levels)
```

### prepare_data() — lower-level

``` r

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

Key output fields: - `case_counts` — `[max_time × num_strata]` matrix of
observed counts - `d_star` — `[max_time × num_strata]` matrix of
max-observable delays - `num_strata`, `max_time`, `delay_family`,
`epidemic_model`, `is_negative_binomial`

------------------------------------------------------------------------

## 4. Fitting

### Main entry point: nowcast()

``` r

nc <- nowcast(
  data,                        # tbl_now
  model,                       # model() object
  type    = "one_stage",       # or "two_stage"
  now     = NULL,              # as.Date(); default get_now(data)
  K       = 25,                # delay imputations (two_stage only)
  n_draws = 2000,
  delay_window = 120,          # days/weeks used for Stage-1 delay fit
  floor_mu     = 0.15,         # minimum delay-mean spread for imputation
  floor_sig_frac = 0.25,       # minimum sigma spread fraction
  np_spread    = 1,            # Dirichlet simplex imputation covariance scale
  temporal_effects = "auto",   # "auto" | "none"; auto-adds DOW/seasonality
  seed = NULL,
  ...                          # passed to prepare_from_tbl_now / prepare_data
)
```

Returns a `nowcast_class` S7 object.

**NB overdispersion `phi` is NOT a
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
argument.** Set it on the likelihood:
`model(nb_likelihood(phi = lognormal_prior(log(5), 0.5)), ...)`. The
default
[`nb_likelihood()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md)
uses `lognormal_prior(log(20), 0.5)`.

### one_stage vs two_stage

- `one_stage`: single joint fit of delay + epidemic simultaneously.
  Fast; can underestimate delay uncertainty.
- `two_stage`: Stage 1 = delay-only fit on a recent window; Stage 2 = K
  joint fits with delay hard-fixed at imputed values; draws are pooled.
  Recommended for production. Adds ~K× the cost of one fit.

### fit() — lower-level

``` r

# Returns a list: par, parList, nll, convergence, obj, opt, random,
#   use_random, epi_model, is_nb, lambda [T×S], mu, mu_safe, Gstar [T×S],
#   delay_mu, delay_sigma, phi_nb, reconstruct, Bmat, freq, data, priors, model
result <- fit(model, engine, priors = NULL, init = NULL, control = list(...))
```

### default_priors()

``` r

priors <- default_priors(model, engine, phi = lognormal_prior(log(20), 0.5))
# Returns named list of prior_class objects: delay_mu, delay_sigma, delay_Q,
# phi_nb, mu_intercept, gamma_cov, gp_alpha, gp_ell, ar_phi, ar_sigma,
# R0, gamma_sir, N_eff, delay_probs
```

------------------------------------------------------------------------

## 5. Results

### Parameter estimates

``` r

coef(nc)
# Named vector: delay_mu, delay_sigma, phi_nb, mu_intercept,
#               log_gp_alpha, log_gp_ell, ar_phi_unc, log_ar_sigma_unc, ...
# For two-stage: delay_mu/sigma averaged over imputations
```

### Posterior-predictive nowcast

``` r

pred <- predict(nc, n_draws = NULL, summary = FALSE, seed = NULL)
# Returns nowcast_prediction_class with slot @draws [n_draws × max_time]

summary(pred)
# data.frame: mean, median, sd, mad, q2.5, q5, q10, q25, q50, q75, q90, q95,
#             q97.5, .event_num (0-indexed)

autoplot(pred)   # ggplot2: median + 50%/90% ribbons
```

### Latent incidence (lambda)

``` r

mean(nc, seed = 42)       # numeric vector length max_time (posterior mean)
median(nc, seed = 42)     # posterior median
quantile(nc, probs = c(0.025, 0.5, 0.975), seed = 42)  # [max_time × probs]
summary(nc)               # prints params + latent incidence table
```

### Lower-level draw helper

``` r

summarise_nowcast_matrix(draws_matrix)
# draws_matrix: [n_draws × max_time] → data.frame with q2.5..q97.5 + .event_num
```

------------------------------------------------------------------------

## 6. Backtesting and scoring

### backtest()

``` r

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
# Returns backtest_class; parallelised via doFuture (%dofuture%)
# Set future::plan(multisession, workers=8) before calling for parallel execution
```

### score()

``` r

score(bt, metric = c("wis", "ape", "mse"), report = TRUE)
# Returns data.frame: model, wis, ape, mse, coverage_50, coverage_90
# Sorted best-first by `metric`; cli report printed if report=TRUE
# Accepts a backtest_class or a predict() data.frame
```

### autoplot

``` r

autoplot(bt)     # faceted by model; median + 50%/90% ribbons vs final truth
autoplot(pred)   # single nowcast
```

------------------------------------------------------------------------

## 6b. Surprise (anomaly detection) and censoring

### surprise() — is new data surprising under the fit?

``` r

# type "count": new_data has columns event_index (0-indexed), count
# type "delay": new_data has columns delay (event units), optional weight
# type "both" (default): supply both. level = credible level (default 0.99).
s <- surprise(nc, new_data, type = "both", level = 0.99, n_draws = 500)
s$count_surprise   # event_index, observed, ppp_right/left, direction (high/low), is_surprising
s$delay_surprise   # delay, mean_tail_prob (P(D>=d)), cdf_prob (P(D<=d)), direction (long/short), is_surprising
```

### update() computes surprise automatically + warns

``` r

nc2 <- update(nc, new_rows, surprise_level = 0.99)   # one warning listing surprising delays
extreme_values(nc2)                                  # tidy data.frame of flagged surprises (or NULL)
update(nc, new_rows, compute_surprise = FALSE)        # silent
```

[`update()`](https://rdrr.io/r/stats/update.html) scores only **too-long
reporting delays** (upper-censored) against the previous fit and raises
a single warning (in the data’s time unit). It does NOT score the
epidemic/count process. `extreme_values(nc)` returns the flagged rows.

### prior_only: see what a prior implies

``` r

# Draw the epidemic from the PRIORS only (no fitting); data just sets the grid.
nc <- nowcast(tn, model(nb_likelihood(), sir_epidemic(R0 = lognormal_prior(log(3), 0.1)), lognormal_delay()),
              prior_only = TRUE, n_draws = 300)
quantile(nc, probs = c(0.05, 0.5, 0.95))   # prior-predictive epidemic band
# Works for HSGP / AR1 / SIR; predict()/autoplot()/median() all apply.
```

### Default priors

Each component constructor documents its defaults in a **Default
priors** roxygen section:
[`?epidemic_process`](https://rodrigozepeda.github.io/diseasenowcasting/reference/epidemic_process.md)
(HSGP alpha~HalfNormal(0,1), ell~InvGamma(3,1); AR1 phi~StdNormal,
sigma~Exp(100); SIR R0~LogNormal(log2,0.5),
gamma~LogNormal(log(1/5),0.5), N_eff~Beta(2,5)),
[`?delay_process`](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md),
[`?likelihood`](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md)
(phi~LogNormal(log20,0.5)). Delay means + the intercept are
data-informed. and **new report delays** against the previous fit, and
raises a cli warning naming exactly what was surprising (count too
high/low, delay too long/short).

### Censoring an outlier delay (m_censored)

A report flagged `is_censored` contributes `log G_D(j)` (delay \<= j, an
upper bound) instead of the exact-delay term — so an extreme outlier
delay stops distorting the fitted delay distribution.
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
reads the tbl_now’s `is_censored` column automatically (both the
delay-only and joint objectives handle it).

``` r

# Flag every report whose delay exceeds a bound as censored, then re-fit:
tn2 <- censor_delays_above(tn, max_delay = 45)   # sets is_censored = TRUE for delay > 45
nc  <- nowcast(tn2, model())                      # uses the censored delays

# Or set is_censored yourself when building the tbl_now:
# tbl.now::tbl_now(df, ..., is_censored = my_logical_column)
```

The typical loop: fit -\>
[`update()`](https://rdrr.io/r/stats/update.html) warns “delay too long”
-\>
[`censor_delays_above()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/censor_delays_above.md)
-\> re-fit (better-calibrated delay distribution, often better WIS for
one-stage fits).

------------------------------------------------------------------------

## 7. Internal architecture (for contributors)

### build_joint_obj()

``` r

built <- build_joint_obj(data, priors, init = NULL, use_random = FALSE)
# Returns list(obj, random, epi_model, is_nb, Bmat, freq, n_strata)
# obj is an RTMB::MakeADFun result; obj$fn/gr/he are the nll/gradient/hessian
```

**Parameter layout** (per-stratum where S = num_strata):

| Parameter | Shape | Shared? |
|----|----|----|
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

**Smooth cap on log_mean** (prevents exp() overflow):

``` r

log_mean_capped <- upper_bound - log1p(exp(upper_bound - log_mean_col))
```

where `upper_bound = min(max(6, log1p(casemax)), 16)`.

### .joint_reconstruct()

Plain-R mirror of the objective (no AD tape). Takes `parlist` (from
`obj$env$parList()` or `.split_named_vector(draw)`), returns list: `mu`,
`mu_safe`, `lambda [T×S]`, `Gstar [T×S]`, `delay_mu`, `delay_sigma`,
`phi_nb`, `log_loc`, `log_scale`.

### .nowcast_draws()

``` r

draws <- .nowcast_draws(fit, target, n_draws, probs, seed)
# fit must have: $data, $priors, $obj (with last.par.best), $use_random,
#               $Bmat, $freq
# Returns list(M [n_draws×T], lambda_draws [n_draws×T],
#              M_strata [n_draws×T×S], lambda_strata [n_draws×T×S],
#              n_strata, nowcast, draws, quantiles, median, target, observed)
```

The total `M` and `lambda_draws` are
[`rowSums()`](https://rdrr.io/r/base/colSums.html) over strata — at S=1
they equal the single-column matrices exactly.

### .pool_fit_draws()

``` r

pooled <- .pool_fit_draws(fits_list, target, n_draws)
# Returns list(M [pooled_n×T], lambda [pooled_n×T])
# Used by both predict() and .nowcast_lambda_draws()
```

### Delay aggregation (prepare_data)

``` r

aggregate_by_delay_and_time(obs_matrix)
# Pools ALL strata (ignores col 4) — correct because delay is shared
# Returns list(obs_delays, row_sums, col_sums)
# Fixed censoring: c_t = max_time - t + 1 (per-time, not per-season)
```

------------------------------------------------------------------------

## 8. Common gotchas

### Per-stratum vector shapes

`ar_phi_unc` and `log_ar_sigma_unc` **must** have length `num_strata`,
not 1. If you warm-start with a scalar from a previous fit,
`.adapt_init()` recycles it to the right length — but if you build
`init` by hand you must do this yourself:

``` r

init$log_ar_sigma_unc <- rep(-2, engine$num_strata)
init$ar_phi_unc       <- rep(0,  engine$num_strata)
```

### Matrix init for basis_coefs / ar_innov / gamma

These are `[rows × num_strata]` matrices. Always initialise as matrices:

``` r

init$basis_coefs <- matrix(0, engine$num_basis, engine$num_strata)
init$ar_innov    <- matrix(0, engine$max_time,  engine$num_strata)
init$gamma       <- matrix(0, engine$P,         engine$num_strata)
```

### GenGamma Q bounds

The raw parameter `delay_Q` is the *unconstrained* value; the actual
shape is:

``` r

shape_Q <- 0.05 + 2.95 * plogis(delay_Q)   # ∈ (0.05, 3)
```

Never set `shape_Q` directly in `init`; set `delay_Q` (the raw
unconstrained value). Raw `delay_Q = -2` ≈ `shape_Q = 0.27` (near
log-normal).

### joint-mode vs random= Laplace

`use_random = FALSE` (default, via
`getOption("diseasenowcasting.use_random", FALSE)`) uses the joint
Hessian — same as `cmdstanr $laplace()`. Set
`options(diseasenowcasting.use_random = TRUE)` to switch to the marginal
nested Laplace (slower, sometimes more accurate for hierarchical
models).

### HSGP num_basis for long daily series

Auto `num_basis = ceiling(1.5 * sqrt(max_time))` → ~60 for a 1,500-day
series, which can cause an ill-conditioned Laplace. For COVID-length
series, cap at 20:

``` r

model(nb_likelihood(), hsgp_epidemic(num_basis = 20L), lognormal_delay())
```

### delay_only = TRUE skips fit() validity check

[`fit()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/fit.md)
blocks `delay_only` fits via its epidemic-GQ validity check. Use
`fit_internal()` / `build_delay_only_obj()` +
[`nlminb()`](https://rdrr.io/r/stats/nlminb.html) directly, or call
`nowcast(..., delay_only = TRUE)` if that path is exposed. For the
two-stage cascade, the delay-only Stage-1 is handled internally by
`.collect_nowcast_fits()` — you don’t call it directly.

### Dirichlet two-stage: simplex dimension must match Stage-2

When `dirichlet_delay(bins = k)`, the simplex has `k+1` entries
(including the geometric tail). The bins count must be consistent
between Stage-1 and Stage-2. The package handles this automatically when
you use
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md).

------------------------------------------------------------------------

## 9. Datasets

| Object | Package | Description | Key columns |
|----|----|----|----|
| `denguedat` | tbl.now | Weekly dengue linelist, Colombia | onset_week, report_week, gender |
| `mpoxdat` | tbl.now | Daily mpox counts, USA 2022 | dx_date, dx_report_date, race, n |
| `covidat` | tbl.now | Daily COVID counts (small demo) | date_of_symptom_onset, date_of_registry, sex, n |
| `covid_colombia` | **diseasenowcasting** | Daily COVID counts, Colombia 2020–2023 (37,600 rows) | notification_date, diagnosis_date, sex, n |

For `covid_colombia`: event = `notification_date`, report =
`diagnosis_date`.

------------------------------------------------------------------------

## 10. Typical session skeleton

``` r

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
