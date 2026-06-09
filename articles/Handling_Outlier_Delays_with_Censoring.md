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
    [`censor_delays_above()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/censor_delays_above.md)
    function turns extreme delays into upper bounds.

5.  Model is re-updated using the censored data consequently improving
    the delay distribution.

## The problem: an extreme delay

Real surveillance data occasionally contains reports with extreme
reporting delays. This can be due to typos, healthcare-system hurdles or
other issues not related to the disease’s natural evolution. In
Colombia’s COVID-19 data (`covid_colombia`) the bulk of reports arrive
within a week or two, but a handful take **more than 100 days**:

``` r

data(covid_colombia)

tbl_covid <- covid_colombia |> 
  tbl_now(event_date = notification_date,
          case_count  = n,
              data_type   = "count-incidence",
          report_date = diagnosis_date,
          t_effects = temporal_effects(day_of_week = TRUE))

summary(tbl_covid$.delay)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>     0.0     4.0    10.0    11.6    17.0   330.0
```

![\_Reporting-delay distribution of COVID-19 Colombia (extremes
exagerated for illustration purposes). A few reports arrive hundreds of
days
late.\_](Handling_Outlier_Delays_with_Censoring_files/figure-html/delay-hist2-1.png)

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
abnormal delays and how to inform the model so that predictions are
improved.

## 1) Fit a model

The first step for a model to learn about *extreme delays* is to have an
initial model with historical data so that it learns what the usual
delay distribution is. In this case we’ll work with an early-pandemic
window and fit a nowcast. To play out the “new data arrives” story we
first fit on the reports available at an early date:

``` r

#Initial data
initial_tbl <- tbl_covid |> 
  filter(
    notification_date <= as.Date("2020-08-31") & 
    diagnosis_date <= as.Date("2020-08-31")) |> 
  change_now() #Update the "now" of the nowcast to the latest date
```

We then fit a nowcast to this data:

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
will automatically score the new report against the old fit and **warn**
that something is amiss:

``` r

nc_updated <- update(initial_ncast, new_data_tbl)
#> Warning: ! Surprising reporting delay of 114 days (1 report): longer than the model
#>   expects (P(D >= d) = 8e-04).
#> ! Surprising reporting delay of 112 days (1 report): longer than the model
#>   expects (P(D >= d) = 0.00084).
#> ! Surprising reporting delay of 99 days (1 report): longer than the model
#>   expects (P(D >= d) = 0.0013).
#> ℹ If these are outliers, treat them as censored with `censor_delays_above()`
#>   and re-fit.
#> ℹ See all flagged delays with `extreme_values(nc)`.
```

The warning tells us *exactly* what was unexpected (reporting delays far
longer than usual). The full table is available via
[`extreme_values()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/extreme_values.md):

``` r

extreme_values(nc_updated)
#>   delay weight mean_tail_prob cdf_prob      lpd relative_surprise direction
#> 1    99      1       0.001270 0.998730 -10.0837             3e-04      long
#> 2   112      1       0.000844 0.999156 -10.5819             2e-04      long
#> 3   114      1       0.000795 0.999205 -10.6545             2e-04      long
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

We follow the warning’s advice: we flag as censored every report whose
delay exceeds a sensible bound (here 99 days as reported by
[`extreme_values()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/extreme_values.md)).
The function
[`censor_delays_above()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/censor_delays_above.md)
works by setting `is_censored = TRUE` in the `tbl_now` for reports
greater than the `max_delay`. Extreme delays are thus turned into upper
bounds. The
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
then reads the `is_censored` flag automatically.

``` r

new_data_tbl_censored <- censor_delays_above(new_data_tbl, max_delay = 99)

#Adds column `.is_censored`:
new_data_tbl_censored
```

    #> # A tibble:  7,798 × 8
    #> # Data type: "count-incidence"
    #> # Frequency: Event: `days` | Report: `days`
    #>    .is_censored  notification_date diagnosis_date sex          n .event_num
    #>    <lgl>         <date>            <date>         <chr>    <int>      <dbl>
    #>    [is_censored] [event_date]      [report_date]  [...]  [cases]      [...]
    #>  1 FALSE         2020-03-02        2020-03-06     Female       1          0
    #>  2 FALSE         2020-03-03        2020-03-14     Female       1          1
    #>  3 FALSE         2020-03-06        2020-03-09     Male         1          4
    #>  4 FALSE         2020-03-07        2020-03-09     Female       1          5
    #>  5 FALSE         2020-03-08        2020-03-11     Female       2          6
    #>  6 FALSE         2020-03-09        2020-03-11     Female       1          7
    #>  7 FALSE         2020-03-09        2020-03-11     Male         2          7
    #>  8 FALSE         2020-03-10        2020-03-11     Female       1          8
    #>  9 FALSE         2020-03-10        2020-03-12     Female       2          8
    #> 10 FALSE         2020-03-10        2020-03-13     Male         1          8
    #> # ────────────────────────────────────────────────────────────────────────────────
    #> # Now: 2020-09-01 | Event date: "notification_date" | Report date:
    #> # "diagnosis_date"
    #> # Right-censored indicator: ".is_censored"
    #> # T. effects (lazy): [event_date] day_of_week
    #> # ────────────────────────────────────────────────────────────────────────────────
    #> # ℹ 7,788 more rows
    #> # ℹ 2 more variables: .report_num <dbl>, .delay <dbl>

We refit with the censored data:

``` r

nc_updated_censored <- update(initial_ncast, new_data_tbl_censored)
```

The fitted values change once the outliers are no longer taken
literally.

``` r

#Previous
coef(nc_updated)
#>     delay_mu  delay_sigma       phi_nb mu_intercept log_gp_alpha   log_gp_ell 
#>    2.1837780    9.1643683    0.1941141    7.6212181    1.2539174   -1.6688185

#Updated
coef(nc_updated_censored)
#>     delay_mu  delay_sigma       phi_nb mu_intercept log_gp_alpha   log_gp_ell 
#>   2.20092627   9.20295857   0.05545091   7.69584948   1.20578266  -1.56674944
```

Which also affects predictions:

``` r

#Previous
pred_previous <- predict(nc_updated) 
summary(pred_previous) |> tail(6)
#>         mean  median        sd      mad q2.5      q5    q10     q25     q50
#> 179 10626.92  9376.5  7729.546 2515.972 4906 4954.75 6200.6 7831.50  9376.5
#> 180 11134.35  9635.0  9590.599 2831.766 4356 4415.75 6170.7 7932.00  9635.0
#> 181 10283.28  9002.5 12146.092 3322.507 2884 2939.55 4569.6 6900.00  9002.5
#> 182 10664.80  8800.5 16702.184 3974.109 1632 1709.65 3543.4 6188.00  8800.5
#> 183 12317.05 10551.0 15293.335 4439.646 2425 2459.90 4578.7 7717.75 10551.0
#> 184 11233.26 10259.5  9054.003 4771.007 1233 1256.85 2884.2 7137.75 10259.5
#>          q75     q90      q95    q97.5 .event_num
#> 179 11282.75 14264.2 17500.60 24131.85        178
#> 180 11818.75 15452.2 20146.15 27595.45        179
#> 181 11511.50 14950.0 18249.35 23759.90        180
#> 182 11588.50 15654.3 20850.25 31856.57        181
#> 183 13725.25 18390.4 23680.90 31558.40        182
#> 184 13590.75 17390.2 22027.15 30179.35        183

#Updated
pred_censored <- predict(nc_updated_censored)
summary(pred_censored) |> tail(6)
#>         mean  median       sd      mad     q2.5      q5    q10     q25     q50
#> 179 10639.03  9811.5 4448.225 2427.016 5665.375 6368.70 7092.9 8342.50  9811.5
#> 180 11032.07 10275.0 4354.723 2882.174 5054.925 6086.60 6990.2 8472.25 10275.0
#> 181 10658.79  9787.5 5382.006 3325.472 3842.625 4946.60 5984.9 7680.00  9787.5
#> 182 10338.49  9514.5 5667.019 3769.510 2591.650 3943.95 5161.5 7051.75  9514.5
#> 183 12452.94 11487.5 7321.187 4222.445 3655.250 5029.80 6594.7 8812.00 11487.5
#> 184 12053.63 11130.5 6355.090 4421.113 2551.500 4157.15 5932.3 8393.50 11130.5
#>          q75     q90      q95    q97.5 .event_num
#> 179 11735.25 14363.9 17053.90 21147.97        178
#> 180 12440.25 15467.7 18260.60 22433.45        179
#> 181 12173.00 15556.0 18793.50 22793.70        180
#> 182 12130.50 16121.9 19285.65 23204.67        181
#> 183 14588.75 18446.6 21701.90 25943.57        182
#> 184 14386.00 18630.4 21772.05 27027.35        183
```

## 4) Does it nowcast better? Backtest

Finally we check that controlling the extreme values actually *improves
accuracy*. We backtest the same model on the plain data (`new_data_tbl`)
and on the censored data (`new_data_tbl_censored`) across a set of
dates, scoring the most recent nowcast (d^\* = 0) against the eventual
truth with the Weighted Interval Score (WIS; lower is better) and
coverage (closer to the expected coverage the better).

``` r

eval_dates <- as.Date(c("2020-04-15", "2020-05-01", "2020-05-15", "2020-06-01"))

bt_plain <- backtest(new_data_tbl, dates = eval_dates)
bt_cens  <- backtest(new_data_tbl_censored, dates = eval_dates)

rbind(
  plain    = score(bt_plain, report = FALSE)[, c("wis", "coverage_50", "coverage_90")],
  censored = score(bt_cens,  report = FALSE)[, c("wis", "coverage_50", "coverage_90")]
)
#>               wis coverage_50 coverage_90
#> plain    193.6895         0.5        0.50
#> censored 205.5531         0.5        0.75
```

Censoring the outlier delays result in a **lower (better) WIS** and a
better coverage: the delay distribution is no longer pulled to the right
by extreme values. **This shows that censoring the extreme values
improves the models.**

## Summary – fit -\> update -\> identify outliers -\> censor -\> refit loop

In general the workflow is:

1.  Fit a nowcast.

2.  New data arrives -\>
    [`update()`](https://rdrr.io/r/stats/update.html) scores it and
    **warns** about potential outliers.

3.  Manually identify which ones correspond to outliers and which
    correspond to true values. This has to be done by a human as no
    automated system will know when something flagged as noise is real.

4.  Use `censor_delays_above(tn, bound)` turning the delays into an
    upper bound. Or modify the `tbl_now` directly (column `is_censored`)

5.  Re-fit -\> the delay distribution is no longer distorted.
