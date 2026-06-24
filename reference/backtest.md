# Backtest one or more nowcast models across a set of as-of dates

Backtest one or more nowcast models across a set of as-of dates

## Usage

``` r
backtest(
  data,
  models = diseasenowcasting::model(),
  dates = NULL,
  type = c("two_stage", "one_stage", "auto"),
  n_dates = 20L,
  max_delay = NULL,
  return_simulations = FALSE,
  n_draws = 1000L,
  K = 25L,
  np_spread = 1,
  recent = FALSE,
  seed = sample.int(.Machine$integer.max, 1),
  ...
)
```

## Arguments

- data:

  A `tbl_now` (the full data; its eventual counts are the truth).

- models:

  A
  [`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
  object or a list of them. With several models the backtest can rank
  them (see
  [`score()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/score.md)).

- dates:

  A vector of as-of dates. If `NULL`, a default grid spanning the
  observed range (interior points) is sampled.

- type:

  `"two_stage"` (default), `"one_stage"`, or `"auto"` (per delay:
  dirichlet one-stage, all other delays two-stage).

- n_dates:

  If `dates` is `NULL`, how many to sample. Default 20.

- max_delay:

  Truth-completeness horizon (in event units). Event dates within
  `max_delay` units of the last report do not yet have a fully observed
  eventual count, so the backtest **excludes** them (their "truth" would
  still be accruing). Default `NULL` uses the 99th percentile of the
  observed reporting delays. Pass a number to override, or `Inf` to
  evaluate every date regardless of completeness.

- return_simulations:

  If TRUE, also keep the pooled draw matrix per (date, model). Default
  FALSE (summaries only: mean/median/sd/quantiles).

- n_draws:

  Posterior draws per nowcast.

- K, np_spread:

  Two-stage controls passed through.

- recent:

  When `dates` is `NULL`, choose the **most recent** `n_dates`
  complete-truth as-of dates rather than spreading them across the whole
  history (default `FALSE`). Useful when the backtest is meant to judge
  how a model does on *recent* dynamics (e.g. for model selection ahead
  of a present-day nowcast).

- seed:

  Optional base RNG seed.

- ...:

  Passed to
  [`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md).

## Value

A `backtest_class` object.

## Details

`backtest()` evaluates one nowcast per (as-of date x model) cell and
these cells are **embarrassingly parallel**. The work is dispatched with
future.apply, so parallelism is controlled by the future plan you set
*before* calling `backtest()`:

    library(future)
    plan(multisession, workers = 4)   # 4 parallel R sessions
    bt <- backtest(data, models, dates = my_dates)
    plan(sequential)                  # back to serial when done

With the default plan (`sequential`) the cells run one at a time. For a
grid of many dates x models, `plan(multisession, workers = N)` (or
`plan(multicore)` on Linux/macOS) gives a near-linear speed-up up to the
number of physical cores. Each worker needs the package available, which
is automatic for an installed package; with `devtools::load_all()` use
`plan(multisession)` so workers re-load it.

## Examples

``` r
if (interactive() && requireNamespace("tbl.now", quietly = TRUE)) {
  # future::plan(future::multisession, workers = 4)   # opt in to parallelism
  # bt <- backtest(my_tbl_now, list(model_a, model_b), dates = my_dates)
  # future::plan("sequential")
}
```
