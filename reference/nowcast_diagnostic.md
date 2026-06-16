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
  seed = sample.int(.Machine$integer.max, 1),
  previous_times = 30
)
```

## Arguments

- object:

  A `nowcast_class` object.

- n_draws:

  Number of posterior draws (default: min(`object@n_draws`, 500)).

- seed:

  Optional RNG seed.

- previous_times:

  Number of most recent event-times to display in panels 2 (smoothed
  incidence) and 3 (nowcast): only the last `previous_times` dates
  (including the nowcast target) are shown, as in
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
  Panel 1 (the delay distribution) always uses all delays. Default `30`;
  use `Inf` (or `NULL`) to show every event-time.

## Value

A `patchwork` combined plot (or a named list of three ggplots).
