# Score a backtest: WIS, APE, MSE per model (and rank them)

Score a backtest: WIS, APE, MSE per model (and rank them)

## Usage

``` r
score(object, metric = c("wis", "ape", "mse"), report = TRUE)
```

## Arguments

- object:

  A `backtest_class`.

- metric:

  Metric to RANK models by: `"wis"` (default), `"ape"`, or `"mse"`. All
  three are always reported; `metric` only chooses the ranking.

- report:

  If TRUE (default), print the ranked comparison via cli.

## Value

A data.frame, one row per model, with `wis`, `ape`, `mse`,
`coverage_50`, `coverage_90`, sorted best-first by `metric`.
