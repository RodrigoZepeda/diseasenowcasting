# Tidy a nowcast into the cross-package nowcast table

Returns the posterior nowcast as one row per event date per stratum, in
the column layout shared by every engine `tbl.now` normalises. Called on
a fitted
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
it first draws the posterior predictive via
[`predict()`](https://rdrr.io/r/stats/predict.html); called on the
result of [`predict()`](https://rdrr.io/r/stats/predict.html) it
summarises the draws it already holds.

## Arguments

- x:

  A `nowcast` (from
  [`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md))
  or a `nowcast_prediction` (from
  [`predict()`](https://rdrr.io/r/stats/predict.html) on one).

- probs:

  Optional numeric probabilities in `[0, 1]`. Each adds one column named
  `q<probs * 100>` (so `0.05` gives `q5`, `0.025` gives `q2.5`). Because
  the object keeps the draws, these quantiles are exact.

- conf.level:

  Width of the credible interval (default 0.95).

- ...:

  Passed to [`predict()`](https://rdrr.io/r/stats/predict.html) when `x`
  is a fitted nowcast (e.g. `n_draws`, `seed`); unused otherwise.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
sorted by `stratum` then `event_date`, with columns `event_date` (Date),
`stratum` (character, `"all"` when the fit is unstratified), `estimate`
(posterior median), `conf.low`, `conf.high`, `level` (the width the
interval actually has) and `engine`, plus one column per entry of
`probs`.

## See also

[`model_parameters()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model_parameters.md)
for the per-parameter table that
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) used to
return.

## Examples

``` r
if (requireNamespace("tbl.now", quietly = TRUE)) {
  # nc <- nowcast(data, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()))
  # tidy(nc)                                    # one row per event date x stratum
  # tidy(predict(nc), probs = c(0.05, 0.95))    # plus exact q5 / q95 columns
  # model_parameters(nc)                        # one row per estimated parameter
}
#> NULL
```
