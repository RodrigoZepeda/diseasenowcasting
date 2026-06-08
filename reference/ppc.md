# Posterior predictive check for a fitted nowcast

Posterior predictive check for a fitted nowcast

## Usage

``` r
ppc(
  object,
  n_draws = 500L,
  seed = sample.int(.Machine$integer.max, 1),
  min_full_report_delay = NULL,
  ...
)
```

## Arguments

- object:

  A `nowcast_class` object.

- n_draws:

  Number of posterior draws for the check (default 500).

- seed:

  Optional RNG seed.

- min_full_report_delay:

  Events are considered "fully reported" when their maximum observable
  delay `d*` is at least this many units (default the 90th percentile of
  observed delays).

- ...:

  Unused.

## Value

A named list with slots `$plot` (a ggplot / patchwork), `$delay_ppc`
(data frame), and `$count_ppc` (data frame).
