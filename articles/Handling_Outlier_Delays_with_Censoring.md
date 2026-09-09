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
#>    2.2381978    9.5144713    0.1305914    7.6287675    1.2503505   -1.6708021

#Updated
coef(nc_updated_censored)
#>     delay_mu  delay_sigma       phi_nb mu_intercept log_gp_alpha   log_gp_ell 
#>    2.2569348    9.4437376    0.1782137    7.6507759    1.2536978   -1.6326414
```

Which also affects predictions:

``` r

#Previous
pred_previous <- predict(nc_updated) 
summary(pred_previous) |> tail(6)
#>         mean  median       sd      mad     q2.5      q5    q10     q25     q50
#> 179 10911.50 10262.0 3690.802 2266.895 6301.775 7091.65 7791.8 8861.00 10262.0
#> 180 11482.56 10625.0 4722.973 2440.360 5905.600 7014.90 7953.7 9198.50 10625.0
#> 181 11076.26 10348.5 4650.133 2847.333 5007.275 6129.30 7084.9 8556.00 10348.5
#> 182 10919.67 10035.0 4990.094 3515.245 3967.950 5131.45 6149.0 7920.00 10035.0
#> 183 12796.34 12029.0 5440.402 3831.780 4408.675 6248.45 7821.3 9603.75 12029.0
#> 184 12316.40 11540.5 5531.517 4135.713 3748.300 5043.65 6605.8 8980.00 11540.5
#>          q75     q90      q95    q97.5 .event_num event_date
#> 179 12059.00 14531.8 16199.15 18978.55        178 2020-08-27
#> 180 12618.25 15411.5 17957.60 21523.35        179 2020-08-28
#> 181 12455.50 15647.1 18611.95 21697.40        180 2020-08-29
#> 182 12775.00 16201.2 19197.90 22314.20        181 2020-08-30
#> 183 14861.00 18535.1 21320.95 25276.92        182 2020-08-31
#> 184 14728.75 18271.3 21498.55 24715.05        183 2020-09-01

#Updated
pred_censored <- predict(nc_updated_censored)
summary(pred_censored) |> tail(6)
#>         mean  median       sd      mad     q2.5      q5    q10     q25     q50
#> 179 11163.12 10645.0 3014.853 2401.812 6760.375 7307.00 8060.5 9162.75 10645.0
#> 180 11645.36 10984.5 3523.133 2698.332 6617.375 7223.25 7967.5 9342.00 10984.5
#> 181 11522.69 10723.0 4272.170 3251.342 5664.125 6414.00 7158.0 8833.50 10723.0
#> 182 11170.18 10469.0 4520.236 3659.798 4334.375 5250.25 6346.5 8183.25 10469.0
#> 183 13175.90 12444.5 4908.323 4100.130 5668.250 6620.50 7733.5 9867.00 12444.5
#> 184 12435.13 11785.5 5029.302 4258.027 4637.500 5662.75 6901.0 9157.50 11785.5
#>          q75     q90      q95    q97.5 .event_num event_date
#> 179 12529.50 14876.5 16731.00 19208.62        178 2020-08-27
#> 180 13158.00 15904.5 18009.75 20751.62        179 2020-08-28
#> 181 13192.75 16863.5 19602.50 22400.75        180 2020-08-29
#> 182 13191.00 16624.0 19477.25 21686.50        181 2020-08-30
#> 183 15517.75 19632.0 22639.00 24697.38        182 2020-08-31
#> 184 14986.75 18343.5 20860.75 24125.00        183 2020-09-01
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
#> 1: HSGP/nb/LogNormal 11.68571       1.151506        5.775682   4.758518
#> 2: HSGP/nb/LogNormal 12.57092       1.042898        6.911783   4.616242
#>           bias interval_coverage_50 interval_coverage_90 ae_median
#>          <num>                <num>                <num>     <num>
#> 1: -0.06282051            0.6703297            0.8827839  23.78205
#> 2: -0.05531136            0.6593407            0.8901099  24.49817
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
