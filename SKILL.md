# dcast3 — AI Agent Reference Guide

This document is a complete reference for AI assistants (and human contributors)
working in the `dcast3` codebase.  It is designed so that you can write correct
`dcast3` code from scratch without reading the source.

---

## 1. What dcast3 does

`dcast3` is a standalone R package for **Bayesian epidemic nowcasting** using
the RTMB autodiff engine (CppAD + built-in Laplace approximation).  It
reimplements the `diseasenowcast2` Stan-based package with no Stan/cmdstanr
dependency, matching the same public API: `model(likelihood(), epidemic(),
delay())`.

**Core idea (Günther et al. 2021):** The reporting delay is a stochastic
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
| `spline_epidemic()` | — | B-spline trend (experimental, not baked into production models). |

### Delay families

| Constructor | Distribution | Notes |
|---|---|---|
| `lognormal_delay()` | Log-normal | Best for COVID; fastest convergence. |
| `gamma_delay()` | Gamma (mean/SD) | Good for dengue/mpox. |
| `generalized_gamma_delay()` | Generalised Gamma | Most flexible; Q ∈ (0.05, 3) bounded. |
| `dirichlet_delay(bins=NA)` | Non-parametric simplex | Dirichlet prior + geometric tail; two-stage only. |

### Combining

```r
mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
```

---

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
  floor_mu     = 0.15,         # minimum delay-mean spread for imputation
  floor_sig_frac = 0.25,       # minimum sigma spread fraction
  np_spread    = 1,            # Dirichlet simplex imputation covariance scale
  phi = lognormal_prior(log(20), 0.5),  # NB overdispersion prior
  seed = NULL,
  ...                          # passed to prepare_from_tbl_now / prepare_data
)
```

Returns a `nowcast_class` S7 object.

### one_stage vs two_stage

- `one_stage`: single joint fit of delay + epidemic simultaneously.  Fast; can
  underestimate delay uncertainty.
- `two_stage`: Stage 1 = delay-only fit on a recent window; Stage 2 = K joint
  fits with delay hard-fixed at imputed values; draws are pooled.  Recommended
  for production.  Adds ~K× the cost of one fit.

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
  n_draws = 1000,
  K = 25,
  return_simulations = FALSE,    # if TRUE, @simulations slot populated
  seed = NULL,
  ...
)
# Returns backtest_class; parallelised via doFuture (%dofuture%)
# Set plan(multisession, workers=8) before calling for parallel execution
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

**Smooth cap on log_mean** (prevents exp() overflow):
```r
log_mean_capped <- upper_bound - log1p(exp(upper_bound - log_mean_col))
```
where `upper_bound = min(max(6, log1p(casemax)), 16)`.

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

`use_random = FALSE` (default, via `getOption("dcast3.use_random", FALSE)`) uses
the joint Hessian — same as `cmdstanr $laplace()`.  Set
`options(dcast3.use_random = TRUE)` to switch to the marginal nested Laplace
(slower, sometimes more accurate for hierarchical models).

### HSGP num_basis for long daily series

Auto `num_basis = ceiling(1.5 * sqrt(max_time))` → ~60 for a 1,500-day series,
which can cause an ill-conditioned Laplace.  For COVID-length series, cap at 20:

```r
model(nb_likelihood(), hsgp_epidemic(num_basis = 20L), lognormal_delay())
```

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
| `covid_colombia` | **dcast3** | Daily COVID counts, Colombia 2020–2023 (37,600 rows) | notification_date, diagnosis_date, sex, n |

For `covid_colombia`: event = `notification_date`, report = `diagnosis_date`.

---

## 10. Typical session skeleton

```r
library(dcast3)
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
