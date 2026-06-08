# Prepare data for the RTMB nowcast engine

Prepare data for the RTMB nowcast engine

## Usage

``` r
prepare_data(
  model,
  m,
  m_censored = NULL,
  X = NULL,
  d_star = NULL,
  delay_only = FALSE,
  max_time = NULL,
  num_strata = NULL,
  gp_L = 1.5,
  gp_boundary_frac = 0.62,
  ar_sigma_max = 1,
  ...
)
```

## Arguments

- model:

  A
  [`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
  object.

- m:

  Observation matrix: columns `[event_time, count, delay, strata...]`,
  delays 1-indexed (single stratum supported in this version).

- m_censored:

  Optional censored-observation matrix (same layout).

- X:

  Optional covariate matrix (`max_time` rows, P columns).

- d_star:

  Optional max-observable-delay vector; if NULL, computed as
  `rev(seq_len(max_time)) - 1`.

- delay_only:

  If TRUE, only the delay process is prepared/fit.

- max_time:

  Time-window length; defaults to `max(m[, 1])`.

- num_strata:

  Number of stratum cells (the K-way product of the strata levels). If
  `NULL`, inferred from column 4 of `m`. The likelihood is summed over
  all `max_time x num_strata` (time, stratum) cells; `1` is
  unstratified.

- gp_L:

  HSGP boundary factor (\> 1). Default 1.5.

- gp_boundary_frac:

  Fraction of the HSGP domain placed left of the data. Default 0.62.

- ar_sigma_max:

  Upper bound on the AR/beta RW innovation SD. Default 1.

- ...:

  Reserved.

## Value

A named list of engine inputs.
