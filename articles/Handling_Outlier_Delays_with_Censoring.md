# Handling Outlier Delays with Censoring

``` r

set.seed(248675)
library(diseasenowcasting)
library(tbl.now)
library(dplyr)
library(ggplot2)
```

## TL; DR

In general the workflow is:

1.  Fit a nowcast.

2.  When new data arrives use
    [`update()`](https://rdrr.io/r/stats/update.html) to get warnings
    about extreme values.

3.  A human with domain-knowledge identifies which ones correspond to
    outliers and which correspond to true values.

4.  The
    [`censor_reporting_delays_above()`](https://rodrigozepeda.github.io/tbl.now/reference/censoring.html)
    function turns extreme delays into upper bounds.

5.  Model is re-updated using the censored data consequently improving
    the delay distribution.

6.  Backtest to verify the fit improved.

## The problem: an extreme delay

Real surveillance data occasionally contains reports with extreme
reporting delays. This can be due to typos, healthcare-system hurdles or
other issues not related to the disease’s natural evolution. In
Colombia’s COVID-19 data (`covid_colombia`) the bulk of reports arrive
within a week or two, but a handful take **more than 100 days**:

``` r

data(covid_colombia)

tbl_covid <- covid_colombia |> 
  tbl_now(event_date  = notification_date,
          case_count  = n,
          data_type   = "count-incidence",
          report_date = diagnosis_date,
          t_effects   = temporal_effects(day_of_week = TRUE))

summary(tbl_covid$.delay)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>     0.0     4.0    10.0    11.6    17.0   330.0
```

![\*Reporting-delay distribution of COVID-19 Colombia (extremes
exagerated for illustration purposes). A few reports arrive hundreds of
days
late.\*](Handling_Outlier_Delays_with_Censoring_files/figure-html/delay-hist2-1.png)

*Reporting-delay distribution of COVID-19 Colombia (extremes exagerated
for illustration purposes). A few reports arrive hundreds of days late.*

When a parametric delay model (log-normal, gamma, …) is fit to data
containing such an outlier, the extreme value **drags the estimated
delay distribution to the right**. The model then believes delays are
longer than they really are, thus inflating the most recent nowcasts.

The `diseasenowcasting` framework offers a fix. It treats such reports
as **right-censored**. Instead of telling the model “this case had delay
exactly 330”, it tells it only “this case arrived *by* delay 330”
(*i.e.* its delay of 330 is an **upper bound** for the true delay).

In what follows we explain how to use the model to automatically detect
extreme delays and how to inform the model so that predictions are
improved.

## 1) Fit a model

The first step for a model to learn about *extreme delays* is to have an
initial model with historical data so that it learns what the usual
distribution is. In this case we’ll work with an early-pandemic window
and fit a
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md).
To play out the “new data arrives” story we first fit on the reports
available at an early date (`2020-08-31`):

``` r

#Initial data
initial_tbl <- tbl_covid |> 
  filter(
    notification_date <= as.Date("2020-08-31") & 
    diagnosis_date <= as.Date("2020-08-31")) |> 
  change_now() #Update the "now" of the nowcast to the latest date
```

We then fit a nowcast to this data (in this example, the next day,
`2020-09-01`):

``` r

initial_ncast <- nowcast(initial_tbl)
```

## 2) Update the model

We can then get new data:

``` r

new_data_tbl <- tbl_covid |> 
  filter(
    notification_date <= as.Date("2020-09-01") & 
    diagnosis_date <= as.Date("2020-09-01")) |> 
  change_now()
```

and [`update()`](https://rdrr.io/r/stats/update.html) the model. This
will automatically score the new data against the old fit and **warn**
that something is amiss:

``` r

nc_updated <- update(initial_ncast, new_data_tbl)
#> Warning: ! Surprising reporting delay of 114 days (1 report): longer than the model
#>   expects (P(D >= d) = 0.00051).
#> ! Surprising reporting delay of 112 days (1 report): longer than the model
#>   expects (P(D >= d) = 0.00055).
#> ! Surprising reporting delay of 99 days (1 report): longer than the model
#>   expects (P(D >= d) = 0.00087).
#> ℹ If these are outliers, treat them as censored with
#>   `tbl.now::censor_reporting_delays_above()` and re-fit.
#> ℹ See all flagged delays with `extreme_values(nc)`.
```

The warning tells us *exactly* what was unexpected (reporting delays far
longer than usual). The full table is available via
[`extreme_values()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/extreme_values.md):

``` r

extreme_values(nc_updated)
#>   delay weight mean_tail_prob cdf_prob      lpd relative_surprise direction
#> 1    99      1       0.000870 0.999130 -10.3354             3e-04      long
#> 2   112      1       0.000547 0.999453 -10.8867             2e-04      long
#> 3   114      1       0.000511 0.999489 -10.9672             1e-04      long
#>   surprise level
#> 1    delay  0.99
#> 2    delay  0.99
#> 3    delay  0.99
```

The `mean_tail_prob` expressess the probability of observing such a
value, The `cdf_prob` the probability of lying below that value.
Variable `level` shows the level of certainty to qualify something as an
outlier (default = `0.99`) and can be modified in
`update(..., level = 0.95)`. Column `delay` corresponds to the observed
delay and `weight` corresponds to how many times it was observed.
Finally `lpd` stands for the log pointwise predictive density value.

## 3) Censor the outliers and re-fit

We follow the warning’s advice: we **flag as censored** every report
whose delay exceeds a sensible bound (here 99 days as reported by
[`extreme_values()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/extreme_values.md)).
The function
[`censor_reporting_delays_above()`](https://rodrigozepeda.github.io/tbl.now/reference/censoring.html)
works by setting the report-censoring flag in the `tbl_now` for reports
greater than the `max_delay`. Extreme delays are thus turned into upper
bounds. The
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
then reads the `.is_censored_report` flag automatically.

``` r

new_data_tbl_censored <- tbl.now::censor_reporting_delays_above(new_data_tbl, max_delay = 99)

# Adds column `.is_censored_report`:
new_data_tbl_censored
```

    #> # A tibble:  7,798 × 8
    #> # Data type: "count-incidence"
    #> # Frequency: Event: `days` | Report: `days`
    #>    .is_censored_report  notification_date diagnosis_date sex        n .event_num
    #>    <lgl>                <date>            <date>         <chr>  <int>      <dbl>
    #>    [is_censored_report] [event_date]      [report_date]  [...]  [cas…      [...]
    #>  1 FALSE                2020-03-02        2020-03-06     Female     1          0
    #>  2 FALSE                2020-03-03        2020-03-14     Female     1          1
    #>  3 FALSE                2020-03-06        2020-03-09     Male       1          4
    #>  4 FALSE                2020-03-07        2020-03-09     Female     1          5
    #>  5 FALSE                2020-03-08        2020-03-11     Female     2          6
    #>  6 FALSE                2020-03-09        2020-03-11     Female     1          7
    #>  7 FALSE                2020-03-09        2020-03-11     Male       2          7
    #>  8 FALSE                2020-03-10        2020-03-11     Female     1          8
    #>  9 FALSE                2020-03-10        2020-03-12     Female     2          8
    #> 10 FALSE                2020-03-10        2020-03-13     Male       1          8
    #> # ────────────────────────────────────────────────────────────────────────────────
    #> # Now: 2020-09-01 | Event date: "notification_date" | Report date:
    #> # "diagnosis_date"
    #> # left-censored indicator: ".is_censored_report"
    #> # T. effects (lazy): [event_date] day_of_week
    #> # ────────────────────────────────────────────────────────────────────────────────
    #> # ℹ 7,788 more rows
    #> # ℹ 2 more variables: .report_num <dbl>, .delay <dbl>

We refit but this time using the censored data:

``` r

nc_updated_censored <- update(initial_ncast, new_data_tbl_censored)
```

The fitted values change once the outliers are no longer taken
literally.

``` r

#Previous
coef(nc_updated)
#>     delay_mu  delay_sigma       phi_nb mu_intercept log_gp_alpha   log_gp_ell 
#>   2.20810473   9.54898720   0.09713993   7.63780445   1.24492030  -1.65496852

#Updated
coef(nc_updated_censored)
#>     delay_mu  delay_sigma       phi_nb mu_intercept log_gp_alpha   log_gp_ell 
#>     2.268409     9.754582     0.114937     7.633782     1.250796    -1.641437
```

Which also affects predictions:

``` r

#Previous
pred_previous <- predict(nc_updated) 
summary(pred_previous) |> tail(6)
#>         mean  median       sd      mad     q2.5      q5    q10      q25     q50
#> 179 10623.74 10249.5 2477.538 1926.639 7142.600 7571.85 8075.0  9035.50 10249.5
#> 180 11023.44 10627.5 2657.515 2176.457 7045.700 7561.85 8227.8  9262.75 10627.5
#> 181 10694.82 10203.0 3117.712 2653.113 6161.000 6720.60 7365.0  8599.50 10203.0
#> 182 10623.61 10141.5 3387.235 2984.474 5521.725 6057.55 6863.3  8200.00 10141.5
#> 183 12737.19 12107.5 4056.112 3435.925 6793.700 7524.90 8328.7 10026.50 12107.5
#> 184 12123.50 11714.0 3929.586 3584.927 5699.500 6505.30 7493.9  9396.75 11714.0
#>          q75     q90      q95    q97.5 .event_num event_date
#> 179 11690.00 13593.7 14982.15 16450.82        178 2020-08-27
#> 180 12263.75 14234.9 15763.70 17286.30        179 2020-08-28
#> 181 12200.25 14456.3 16149.55 18757.77        180 2020-08-29
#> 182 12288.00 14971.8 16903.35 19062.32        181 2020-08-30
#> 183 14695.50 17707.9 20091.50 22264.57        182 2020-08-31
#> 184 14265.50 17311.2 19068.95 20790.15        183 2020-09-01

#Updated
pred_censored <- predict(nc_updated_censored)
summary(pred_censored) |> tail(6)
#>         mean  median       sd      mad     q2.5      q5    q10     q25     q50
#> 179 11272.00 10478.5 4217.186 2399.588 6292.375 7151.40 7889.8 9039.00 10478.5
#> 180 11774.33 11020.5 4276.613 2612.341 6071.725 6938.90 7992.4 9419.00 11020.5
#> 181 11051.35 10312.0 4852.537 3043.778 4349.925 5727.60 6716.2 8399.75 10312.0
#> 182 11091.11 10276.5 5385.175 3461.130 3295.700 4492.65 6056.0 8117.00 10276.5
#> 183 12991.14 12124.5 5866.257 4005.244 4746.625 5905.65 7461.5 9708.75 12124.5
#> 184 12410.25 11571.5 5915.530 4346.983 3561.100 4855.75 6511.8 8847.00 11571.5
#>          q75     q90      q95    q97.5 .event_num event_date
#> 179 12383.75 15107.5 17358.55 20376.32        178 2020-08-27
#> 180 13074.00 16011.7 18911.35 21798.15        179 2020-08-28
#> 181 12591.00 15635.3 18349.95 21430.15        180 2020-08-29
#> 182 12874.25 16816.6 19818.05 23032.02        181 2020-08-30
#> 183 15262.50 18848.3 21719.40 26309.70        182 2020-08-31
#> 184 14857.25 18800.0 21955.70 25850.10        183 2020-09-01
```

## 4) Does it nowcast better? Backtest

Finally we check if controlling the extreme values actually *improves
accuracy*. We backtest the same model on the plain data (`new_data_tbl`)
and on the censored data (`new_data_tbl_censored`) across a set of
dates, scoring the most recent nowcast (d^\* = 0) against the eventual
truth with the Weighted Interval Score (WIS; lower is better) and
coverage (closer to the expected coverage the better).

``` r

#dates to backtest
eval_dates <- as.Date(c("2020-04-15", "2020-05-01", 
                        "2020-05-15", "2020-06-01"))

#Backtest each model on the same dates
bt_plain   <- backtest(new_data_tbl, dates = eval_dates)
bt_cens    <- backtest(new_data_tbl_censored, dates = eval_dates)

score_backtest <- function(x) {
  x |>
    scoringutils::as_forecast_quantile() |>
    scoringutils::score() |>
    scoringutils::summarise_scores(by = "model")
}
rbind(
  plain = score_backtest(bt_plain),
  censored = score_backtest(bt_cens)
)
#>                model      wis overprediction underprediction dispersion
#>               <char>    <num>          <num>           <num>      <num>
#> 1: HSGP/nb/LogNormal 11.69163       1.131217        5.729060   4.831353
#> 2: HSGP/nb/LogNormal 12.71610       1.012515        6.856349   4.847233
#>           bias interval_coverage_50 interval_coverage_90 ae_median
#>          <num>                <num>                <num>     <num>
#> 1: -0.05018315            0.6630037            0.8901099  24.33150
#> 2: -0.07655678            0.6520147            0.8827839  25.58242
```

Censoring these outlier delays result in a **lower (better) WIS** and a
**similar coverage** in this example. Though in a real test we would
need to `backtest` through more dates to reach a conclusion.

## Summary – fit -\> update -\> identify outliers -\> censor -\> refit loop

In general the workflow is:

1.  Fit a nowcast.

2.  New data arrives -\>
    [`update()`](https://rdrr.io/r/stats/update.html) scores it and
    **warns** about potential outliers.

3.  Manually identify which ones correspond to outliers and which
    correspond to true values. This has to be done by a human as no
    automated system will know when something flagged as noise is real.

4.  Use `tbl.now::censor_reporting_delays_above(tn, bound)` to turn the
    delays into an upper bound. Or modify the `tbl_now` directly (column
    `is_censored`)

5.  Re-fit -\> the delay distribution is no longer distorted.

6.  Backtest to verify the fit improved.
