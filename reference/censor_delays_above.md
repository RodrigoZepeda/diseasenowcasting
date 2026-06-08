# Flag reports with an implausibly long delay as censored

Marks every report whose reporting delay exceeds `max_delay` (in event
units) as *censored*: the recorded delay becomes an upper bound rather
than an exact value. This is the natural response to a "delay too long"
surprise (see
[`surprise()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/surprise.md)):
instead of letting a 300-day outlier drag the delay distribution, you
tell the model only that the case arrived *by* that delay.

## Usage

``` r
censor_delays_above(data, max_delay, quiet = FALSE)
```

## Arguments

- data:

  A `tbl_now` object.

- max_delay:

  Numeric. Reports with delay (report date minus event date, in the
  data's event units) strictly greater than this are flagged.

- quiet:

  If `TRUE`, suppress the informational message.

## Value

The `tbl_now` with its `is_censored` column updated (created if absent).
Re-fit with
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
to use it.

## Examples

``` r
if (requireNamespace("tbl.now", quietly = TRUE)) {
  df <- data.frame(onset = as.Date("2020-01-01") + c(0, 0, 1, 2),
                   reported = as.Date("2020-01-01") + c(1, 5, 2, 300))
  tn <- tbl.now::tbl_now(df, event_date = onset, report_date = reported,
                         data_type = "linelist", verbose = FALSE)
  tn <- censor_delays_above(tn, max_delay = 60)   # the 300-day report -> censored
}
#> ℹ Marked 1 report with delay > 60 event units as censored.
#> • Their delay is now an upper bound; re-fit with `nowcast()` to use it.
```
