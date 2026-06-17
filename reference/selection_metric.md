# The metric `auto_nowcast()` used to pick the winner

The metric
[`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md)
used to pick the winner

## Usage

``` r
selection_metric(nc)
```

## Arguments

- nc:

  A `nowcast_class` returned by
  [`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md).

## Value

A string: `"wis"`, `"ape"`, `"mse"`, `"coverage"`, `"coverage_50"`, or
`"coverage_90"`.

## See also

[`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md),
[`comparison_scores()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/comparison_scores.md)
