# The scoreboard row for the model `auto_nowcast()` chose

The single
[`comparison_scores()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/comparison_scores.md)
row belonging to the winning model – its WIS, APE, MSE and interval
coverage – rather than the whole table.

## Usage

``` r
best_score(nc)
```

## Arguments

- nc:

  A `nowcast_class` returned by
  [`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md).

## Value

A one-row `data.frame`.

## See also

[`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md),
[`comparison_scores()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/comparison_scores.md),
[`best_model_name()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/best_model_name.md)
