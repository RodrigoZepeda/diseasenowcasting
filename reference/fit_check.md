# Check RTMB optimizer diagnostics for a fitted nowcast

Predictive accuracy belongs to
[`tbl.now::score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.html)
and
[`tbl.now::nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.html).
`fit_check()` deliberately reports only diagnostics specific to the RTMB
optimization performed by diseasenowcasting.

## Usage

``` r
fit_check(object, warn = TRUE)
```

## Arguments

- object:

  A result from
  [`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
  or
  [`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md).
  Native diseasenowcasting operations unwrap the common result
  automatically.

- warn:

  If `TRUE`, warn when any retained fit fails the common optimizer
  adequacy predicate: finite objective and derivatives, optimizer code
  zero, box-constrained KKT residual, positive-definite curvature on the
  locally free subspace, and a quadratic objective-gap estimate no
  larger than `0.01`.

## Value

A data frame with one row per retained RTMB fit and columns `fit`,
`rung`, `convergence`, `objective`, raw and projected gradients,
quadratic objective gap, Hessian status, any Laplace-precision
regularization used for prediction, overall status, and diagnostic
reasons.

## See also

[diseasenowcasting_workflows](https://rodrigozepeda.github.io/diseasenowcasting/reference/diseasenowcasting_workflows.md)
for the distinction between native fit diagnostics and predictive
scoring;
[`nowcast_diagnostic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast_diagnostic.md),
[`tbl.now::score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.html),
[`tbl.now::nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.html)
