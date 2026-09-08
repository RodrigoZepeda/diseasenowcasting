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
#>    2.2038885    9.4014684    0.1163485    7.6320245    1.2492393   -1.6611300

#Updated
coef(nc_updated_censored)
#>     delay_mu  delay_sigma       phi_nb mu_intercept log_gp_alpha   log_gp_ell 
#>   2.21249391   9.39874394   0.07726209   7.65654583   1.23661412  -1.63708049
```

Which also affects predictions:

``` r

#Previous
pred_previous <- predict(nc_updated) 
summary(pred_previous) |> tail(6)
#>         mean  median       sd      mad     q2.5      q5    q10     q25     q50
#> 179 10632.45 10223.5 2451.417 2056.366 7111.925 7551.95 8106.9 8955.00 10223.5
#> 180 11104.54 10567.5 2792.014 2232.054 7222.675 7682.80 8250.8 9229.25 10567.5
#> 181 10835.98 10352.0 3281.240 2802.855 6046.950 6633.80 7341.0 8549.25 10352.0
#> 182 10658.49 10060.5 3698.273 3098.634 5267.475 5971.00 6760.6 8206.50 10060.5
#> 183 12617.48 11877.0 4194.532 3387.000 6598.775 7355.45 8278.5 9857.00 11877.0
#> 184 12244.73 11730.0 4173.515 3533.777 5590.325 6549.95 7596.6 9523.25 11730.0
#>          q75     q90      q95    q97.5 .event_num event_date
#> 179 11748.75 13756.3 15132.20 16788.00        178 2020-08-27
#> 180 12394.25 14631.9 16261.80 18112.65        179 2020-08-28
#> 181 12390.50 14957.3 16998.15 18558.55        180 2020-08-29
#> 182 12420.00 15278.2 17031.45 19580.02        181 2020-08-30
#> 183 14478.50 17914.6 20333.75 22721.05        182 2020-08-31
#> 184 14361.25 17349.7 19840.55 22059.10        183 2020-09-01

#Updated
pred_censored <- predict(nc_updated_censored)
summary(pred_censored) |> tail(6)
#>         mean  median       sd      mad     q2.5      q5    q10     q25     q50
#> 179 10758.04 10322.5 2436.242 2005.216 7307.425 7744.00 8199.8 9096.00 10322.5
#> 180 11200.58 10830.5 2680.059 2368.454 7100.000 7588.95 8308.4 9321.00 10830.5
#> 181 10877.98 10344.0 3138.320 2606.411 6196.375 6889.90 7557.8 8763.50 10344.0
#> 182 10805.81 10256.5 3574.682 3179.436 5285.550 6000.00 6798.7 8344.75 10256.5
#> 183 12700.48 12095.0 4143.878 3570.842 6673.975 7371.85 8233.1 9890.50 12095.0
#> 184 12197.07 11807.5 4016.545 3638.300 5571.950 6510.00 7564.6 9458.25 11807.5
#>          q75     q90      q95    q97.5 .event_num event_date
#> 179 11801.50 13948.0 15336.05 17065.00        178 2020-08-27
#> 180 12602.25 14624.3 16144.20 17511.67        179 2020-08-28
#> 181 12364.50 14791.9 16668.10 18438.08        180 2020-08-29
#> 182 12724.75 15318.3 17118.75 19327.42        181 2020-08-30
#> 183 14717.00 17994.7 19858.60 22015.00        182 2020-08-31
#> 184 14385.50 17544.0 19258.15 20883.05        183 2020-09-01
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

rbind(
  plain    = score(bt_plain, report = F)[,c("wis","coverage_50","coverage_90")],
  censored = score(bt_cens,  report = F)[,c("wis","coverage_50","coverage_90")]
)
#>               wis coverage_50 coverage_90
#> plain    180.3485         0.5         0.5
#> censored 197.0056         0.5         0.5
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
