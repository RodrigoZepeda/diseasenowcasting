# Fitting times recorded by `auto_nowcast()`

Fitting times recorded by
[`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md)

## Usage

``` r
selection_timings(nc)
```

## Arguments

- nc:

  A result returned by
  [`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md).

## Value

A list with `backtest` (one row per attempted retrospective fit),
`refit` (the full-data refit attempts), and `total_seconds` for the
complete automatic-selection call.

## See also

[`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md),
[`comparison_scores()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/comparison_scores.md)
