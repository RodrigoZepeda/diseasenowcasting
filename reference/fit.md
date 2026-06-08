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
  control = list(iter.max = 500, eval.max = 1000, rel.tol = 1e-09)
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

## Value

A list with `par` (named estimates), `obj`, `opt`, `data`, `priors`,
`model`, `convergence`, and (delay-only) `delay_mu` / `delay_sigma`.
