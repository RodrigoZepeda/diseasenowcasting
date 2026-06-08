# Two-stage multiple-imputation nowcast

Two-stage multiple-imputation nowcast

## Usage

``` r
nowcast_twostage(
  model,
  m,
  X = NULL,
  d_star = NULL,
  max_time = NULL,
  target = NULL,
  delay_window = 120L,
  K = 25L,
  floor_mu = 0.15,
  floor_sig_frac = 0.25,
  np_spread = 1,
  n_draws_per = 200L,
  phi = lognormal_prior(log(20), 0.5),
  probs = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975),
  seed = sample.int(.Machine$integer.max, 1)
)
```

## Arguments

- model:

  A
  [`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
  object (LogNormal delay).

- m:

  Observation matrix `[event_time, count, delay, strata...]`.

- X:

  Optional covariate matrix (`max_time` rows).

- d_star:

  Optional max-observable-delay vector.

- max_time:

  Time-window length; defaults to `max(m[, 1])`.

- target:

  Event-time to nowcast (default newest).

- delay_window:

  Recent window length for the Stage-1 delay fit.

- K:

  Number of delay imputations.

- floor_mu:

  Floor on the log-mean imputation SD (parametric families).

- floor_sig_frac:

  Floor on the delay-SD imputation SD (fraction of sigma).

- np_spread:

  Dirichlet only: covariance-inflation factor for the simplex imputation
  (samples `delay_logits` from the Stage-1 Laplace posterior with
  covariance scaled by `np_spread`). Default 1 (the raw, well-informed
  full-series posterior); values \> 1 widen the simplex spread.

- n_draws_per:

  Posterior nowcast draws per imputation.

- phi:

  NB overdispersion prior (default `lognormal_prior(log(20), 0.5)`).

- probs:

  Quantile probabilities to report.

- seed:

  Optional RNG seed.

## Value

A list with `quantiles`, `median`, pooled `draws`, the `rung` used
(`"multi"`, `"anchored"`, or `"onestage"`), and `n_samp` (imputations
pooled).
