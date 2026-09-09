# Fit a nowcast model with the RTMB engine

Optimises the negative log-posterior built from `model` + `data`. For
`delay_only` data this fits the reporting-delay process alone (no
epidemic); the joint epidemic fit is added in later phases.

## Usage

``` r
fit(
  model,
  data,
  priors = NULL,
  init = NULL,
  control = list(iter.max = 500, eval.max = 1000, rel.tol = 1e-09),
  warn = TRUE
)
```

## Arguments

- model:

  A
  [`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
  object.

- data:

  Prepared-data list from
  [`prepare_data()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/prepare_data.md).

- priors:

  Optional prior bundle; defaults to
  [`default_priors()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/default_priors.md).

- init:

  Optional named init list.

- control:

  `nlminb` control list.

- warn:

  If `TRUE`, warn when the returned joint fit does not pass the
  optimizer adequacy checks. Internal warm-start and imputation fits set
  this to `FALSE` and report only diagnostics for the fits that affect
  the result.

## Value

A list with `par` (named estimates), `obj`, `opt`, `data`, `priors`,
`model`, `convergence`, and (delay-only) `delay_mu` / `delay_sigma`.
