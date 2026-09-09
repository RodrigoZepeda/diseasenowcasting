# Load a nowcast saved with [`save_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/save_nowcast.md)

Restores the common `tbl_nowcast` result from a bundle written by
[`save_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/save_nowcast.md).
The native fit is restored into `@fit`. The result works with
[`predict()`](https://rdrr.io/r/stats/predict.html),
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
[`coef()`](https://rdrr.io/r/stats/coef.html),
[`parameters()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/parameters.md),
[`mean()`](https://rdrr.io/r/base/mean.html)/[`median()`](https://rdrr.io/r/stats/median.html)/[`quantile()`](https://rdrr.io/r/stats/quantile.html)
straight away (sampling from the stored Laplace mode + precision). To
re-fit it – on the same or new data – pass the loaded object's `model`
to
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
(the saved `tbl_now` is in the `data` slot).

## Usage

``` r
load_nowcast(file, rebuild = FALSE)
```

## Arguments

- file:

  Path to a `.rds` bundle written by
  [`save_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/save_nowcast.md).

- rebuild:

  If `TRUE`, also re-tape the RTMB objective for each fit (no
  re-optimization). Needed only for `use_random` marginal fits or to
  inspect the live tape; custom delays/epidemics require
  [`library(RTMB)`](https://github.com/kaskr/RTMB). Default `FALSE` –
  the stored mode + precision already drive
  [`predict()`](https://rdrr.io/r/stats/predict.html).

## Value

A diseasenowcasting subclass of
[tbl.now::tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.html).

## See also

[`save_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/save_nowcast.md)
