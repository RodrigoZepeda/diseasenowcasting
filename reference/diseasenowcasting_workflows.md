# Native and cross-engine workflows

`diseasenowcasting` owns the statistical model and RTMB fit;
[tbl.now::tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.html)
owns the common result grammar. The public result of
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
and
[`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md)
therefore supports both layers without an explicit conversion.

## Use the native layer for modelling

Use
[`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md),
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md),
and
[`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md)
when you need this package's epidemic, delay, likelihood, revision, or
cumulative-process components. Use
[`fit_check()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/fit_check.md)
and
[`nowcast_diagnostic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast_diagnostic.md)
for RTMB-specific convergence and fit diagnostics. These functions
automatically unwrap the retained native fit from the common result.

[`backtest()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/backtest.md)
is a native model-specification convenience, not a second evaluation
system: it translates each
[`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
into a
[`tbl.now::engine_diseasenowcasting()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.html)
and returns
[`tbl.now::nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.html)
directly.

## Use the common layer for results and comparison

Use
[`tbl.now::run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.html)
with
[`tbl.now::engine_diseasenowcasting()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.html)
when diseasenowcasting is one of several engines. Once a fit exists, use
the common methods for
[`tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
predictive scoring, forecast conversion, ensembling, and persistence.

In particular, a backtest can be passed directly to
[`scoringutils::as_forecast_quantile()`](https://epiforecasts.io/scoringutils/reference/as_forecast_quantile.html),
[`scoringutils::as_forecast_point()`](https://epiforecasts.io/scoringutils/reference/as_forecast_point.html),
or, when draws were retained,
[`scoringutils::as_forecast_sample()`](https://epiforecasts.io/scoringutils/reference/as_forecast_sample.html).
Relative WIS and additional scores then come from
[`scoringutils::score()`](https://epiforecasts.io/scoringutils/reference/score.html)
and
[`scoringutils::add_relative_skill()`](https://epiforecasts.io/scoringutils/reference/add_relative_skill.html),
rather than a package-local scorer.

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- nowcast(data, model = model())
autoplot(fit)                  # common result plot
fit_check(fit)                 # native RTMB diagnostics

bt <- backtest(data, models = list(default = model()))
relative <- bt |>
  scoringutils::as_forecast_quantile() |>
  scoringutils::score() |>
  scoringutils::add_relative_skill(metric = "wis")
} # }
```
