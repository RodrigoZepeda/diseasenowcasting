# Infer `max_time` for custom epidemic processes

Infers `max_time` from a model and a `tbl_now`. `max_time` is the number
of event-time points the model spans: the epidemic process runs from
`t = 0` to `t = max_time - 1`.

## Usage

``` r
infer_max_time(data, model = diseasenowcasting::model())
```

## Arguments

- data:

  A `tbl_now` object.

- model:

  A
  [`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
  object (default
  [`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)).
  Only its dimensions matter here, so any model works as a placeholder.

## Value

The number of event-time points in the model (an integer).

## Details

This function is useful for building custom epidemic processes whose
[intensity
function](https://rodrigozepeda.github.io/diseasenowcasting/reference/custom_epidemic.md)
loops over time and therefore needs to know the number of event-times
*before* it is written (e.g. an SIR recursion or a random walk with one
innovation per time point).

## Examples

``` r
library(tbl.now)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
mpox_pooled <- mpoxdat |>
  ungroup() |>
  summarise(n = sum(n), .by = c(dx_date, dx_report_date))
mpox_tn <- tbl_now(mpox_pooled,
                   event_date  = dx_date,
                   report_date = dx_report_date,
                   case_count  = n,
                   data_type   = "count-incidence",
                   verbose     = FALSE)

# The result is 316: the model spans from time t = 0 (min event_num)
# to time t = 315 (max report_num).
infer_max_time(mpox_tn)
#> [1] 316
```
