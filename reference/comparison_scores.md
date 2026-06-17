# The model-selection scoreboard from `auto_nowcast()`

The ranked table of candidate models that
[`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md)
backtested, one row per model, best-first by the selection `metric`.
Columns include `model` (the label), `wis` (and its decomposition),
`ape`, `mse`, and `coverage_50` / `coverage_90` (see
[`score()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/score.md)).

## Usage

``` r
comparison_scores(nc)
```

## Arguments

- nc:

  A `nowcast_class` returned by
  [`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md).

## Value

A `data.frame`, one row per candidate model.

## See also

[`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md),
[`best_score()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/best_score.md),
[`score()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/score.md)
