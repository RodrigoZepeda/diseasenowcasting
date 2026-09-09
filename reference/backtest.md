# Backtest one or more diseasenowcasting models

`backtest()` translates native
[`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
specifications into labelled
[`tbl.now::engine_diseasenowcasting()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.html)
specifications and delegates the full retrospective workflow to
[`tbl.now::nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.html).
The returned object is therefore the common `nowcast_backtest` result
used by `tbl.now` for tidying, forecast conversion, scoring, weighting,
and ensembling.

## Usage

``` r
backtest(
  data,
  models = diseasenowcasting::model(),
  dates = NULL,
  type = c("two_stage", "one_stage", "auto"),
  horizon = NULL,
  n_dates = 4L,
  n_draws = 1000L,
  K = 25L,
  np_spread = 1,
  seed = NULL,
  keep_draws = FALSE,
  on_error = c("warn", "abort"),
  verbose = TRUE,
  truth_axis = NULL,
  truth_type = NULL,
  quantile_levels = tbl.now::nowcast_quantile_levels(),
  ...
)
```

## Arguments

- data:

  A
  [tbl.now::tbl_now](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
  holding the full data, including observations that arrived after the
  retrospective nowcast dates.

- models:

  A
  [`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
  or list of models. Names on the list become the canonical method
  labels; unnamed models receive labels from their component names.
  Labels must be unique.

- dates:

  Retrospective nowcast origins, passed as `now_dates` to
  [`tbl.now::nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.html).

- type:

  `"two_stage"`, `"one_stage"`, or `"auto"`, passed to every
  diseasenowcasting engine.

- horizon:

  Number of time units of hindsight used by `tbl.now` when
  `dates = NULL`. `NULL` uses `4` for ordinary data and the largest
  model settlement horizon for count-cumulative data (26 for its
  automatic model).

- n_dates:

  Number of automatic retrospective origins. Ignored when `dates` is
  supplied. Default `4`.

- n_draws:

  Posterior draws per fit.

- K, np_spread:

  Native two-stage fitting controls passed to every engine.

- seed:

  Optional base seed. `tbl.now` derives a stable seed for each
  model/date fit from this value.

- keep_draws:

  Whether the canonical backtest retains posterior draws.

- on_error:

  Either `"warn"` to record and skip failed cells or `"abort"`.

- verbose:

  Whether to report progress.

- truth_axis, truth_type:

  Canonical scoring truth controls passed to
  [`tbl.now::nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.html).
  When both are `NULL`, they follow the native estimand: reported totals
  without a revision process, confirmed cases for confirmation/both
  modes, and still-standing cases for retraction-only mode.

- quantile_levels:

  Quantile probabilities requested from every model.

- ...:

  Additional arguments passed to every
  [`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
  fit through its engine specification.

## Value

A
[tbl.now::nowcast_backtest](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.html)
object.

## See also

[diseasenowcasting_workflows](https://rodrigozepeda.github.io/diseasenowcasting/reference/diseasenowcasting_workflows.md)
for the native/common ownership boundary;
[`tbl.now::score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.html),
[`tbl.now::nowcast_weights()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_weights.html),
[`tbl.now::nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.html),
[`fit_check()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/fit_check.md)
