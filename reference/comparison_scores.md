# The model-selection scoreboard from `auto_nowcast()`

The ranked table of candidate models that
[`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md)
backtested, one row per model, best-first by the selected raw or
relative scoringutils metric. In addition to scoringutils metrics, it
includes `selection_score`, median and total retrospective fit seconds,
successful-fit count, epidemic priority, and original grid order.

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
[`selection_timings()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/selection_timings.md),
[`scoringutils::score()`](https://epiforecasts.io/scoringutils/reference/score.html)
