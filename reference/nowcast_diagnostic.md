# Three-panel diagnostic plot for a fitted nowcast

Produces three stacked panels: (1) reporting-delay histogram with the
fitted density, (2) smoothed latent incidence lambda with 50/90% CI, and
(3) posterior-predictive nowcast with observed counts. Requires the
`patchwork` package for a combined output.

## Usage

``` r
nowcast_diagnostic(
  object,
  n_draws = NULL,
  seed = sample.int(.Machine$integer.max, 1)
)
```

## Arguments

- object:

  A `nowcast_class` object.

- n_draws:

  Number of posterior draws (default: min(`object@n_draws`, 500)).

- seed:

  Optional RNG seed.

## Value

A `patchwork` combined plot (or a named list of three ggplots).
