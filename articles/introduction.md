# Introduction to diseasenowcasting: Real-Time Epidemic Nowcasting

`diseasenowcasting` is an R package for nowcasting time series of
epidemiological cases. Epidemiologic surveillance tools usually have an
intrinsic delay between the **true date of an event** (`event_date`) and
the **report date for that event** (`report_date`). Some examples
include the true date being symptom onset or testing time and the report
date corresponds to when the case was registered in the system.
`diseasenowcasting` uses censored Bayesian models (via R’s Template
Model Builder
[`RTMB`](https://cran.r-project.org/web/packages/RTMB/index.html)) to
infer the cases that have not yet been reported thus providing a
prediction of the final number of cases.

## Native modelling and the shared workflow

Use `diseasenowcasting`’s native interface when specifying the
statistical model:
[`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md),
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md),
and
[`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md)
expose its epidemic, delay, likelihood, revision, and cumulative-process
choices. Use
[`fit_check()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/fit_check.md)
and
[`nowcast_diagnostic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast_diagnostic.md)
for diagnostics that are specifically about the RTMB fit.

The returned object is already a
[`tbl.now::tbl_nowcast`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.html).
Plotting, tidying, predictive scoring, ensembling, saving, and loading
therefore use the common result directly. Likewise,
[`backtest()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/backtest.md)
is a convenience for translating native
[`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
specifications into `tbl.now` engines; it returns a canonical
`nowcast_backtest`, not a second native backtest class. That result can
be passed directly to the scoringutils `as_forecast_quantile()`,
[`as_forecast_point()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.html),
or `as_forecast_sample()` methods. See
[`?diseasenowcasting_workflows`](https://rodrigozepeda.github.io/diseasenowcasting/reference/diseasenowcasting_workflows.md)
for the full boundary and examples.

## Your data: the `tbl_now` format

`diseasenowcasting` works with data organised as a `tbl_now` object from
the companion [`tbl.now`](https://rodrigozepeda.github.io/tbl.now/)
package. A `tbl_now` is simply a data frame that has been annotated with
the roles of its columns:

- *event date* when the event happened (e.g. symptom onset)

- *report date* when the event was reported (e.g. date entered into the
  database)

- *strata* (optional) columns that defined all the strata (e.g. sex and
  region)

- *now* (optional) the date until which to nowcast (assumes all events
  and reports before the now have been observed and missing observations
  correspond to no observations - i.e.  if one day there were not cases
  the missingness can be translated into zero cases)

``` r

library(diseasenowcasting)
library(tbl.now)
library(dplyr)
library(ggplot2)
set.seed(27653)
```

As a quick example, here is how to build a `tbl_now` using the following
surveillance data for dengue in Puerto Rico:

``` r

data(denguedat)
```

    #>   onset_week report_week gender
    #> 1 1990-01-01  1990-01-01   Male
    #> 2 1990-01-01  1990-01-01 Female
    #> 3 1990-01-01  1990-01-01 Female
    #> 4 1990-01-01  1990-01-08 Female
    #> 5 1990-01-01  1990-01-08   Male
    #> 6 1990-01-01  1990-01-15 Female

We can transform the `data.frame` to a `tbl_now` by specifying the event
and report dates (`onset` and `report` weeks respectively) as well as
the `data_type` and the strata (in this case, `gender`).

``` r

dengue_tbl <- tbl_now(
  denguedat,
  event_date  = onset_week,    # symptom onset date
  report_date = report_week,   # when the record was reported
  data_type   = "linelist",    # another option is "count-incidence"  if data is aggregated
  now         =  as.Date("1991-01-01") #When is the now of the nowcast
)
dengue_tbl
#> # A tibble:  52,987 × 6
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   gender .event_num .report_num .delay
#>    <date>       <date>        <chr>       <dbl>       <dbl>  <dbl>
#>    [event_date] [report_date] [...]       [...]       [...]  [...]
#>  1 1990-01-01   1990-01-01    Male            0           0      0
#>  2 1990-01-01   1990-01-01    Female          0           0      0
#>  3 1990-01-01   1990-01-01    Female          0           0      0
#>  4 1990-01-01   1990-01-08    Female          0           1      1
#>  5 1990-01-01   1990-01-08    Male            0           1      1
#>  6 1990-01-01   1990-01-15    Female          0           2      2
#>  7 1990-01-01   1990-01-15    Female          0           2      2
#>  8 1990-01-01   1990-01-15    Female          0           2      2
#>  9 1990-01-01   1990-01-22    Female          0           3      3
#> 10 1990-01-01   1990-01-08    Female          0           1      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 1991-01-01 | Event date: "onset_week" | Report date: "report_week"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
```

Once your data is a `tbl_now`, a single call to
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
does the rest.

> For more information about `tbl_now` check [the package’s
> website](https://rodrigozepeda.github.io/tbl.now/index.html).

## Example 1 – Dengue fever (setting up a stratified nowcast)

We fit a nowcast stratified by gender to illustrate the basic workflow.
First we add the column `gender` as strata to the `tbl_now`:

``` r

dengue_tbl <-  dengue_tbl |>  add_strata(gender)
```

Notice that the `tbl_now` automatically prints the `strata`
specification below:

``` r

dengue_tbl
#> # A tibble:  52,987 × 6
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   gender   .event_num .report_num .delay
#>    <date>       <date>        <chr>         <dbl>       <dbl>  <dbl>
#>    [event_date] [report_date] [strata]      [...]       [...]  [...]
#>  1 1990-01-01   1990-01-01    Male              0           0      0
#>  2 1990-01-01   1990-01-01    Female            0           0      0
#>  3 1990-01-01   1990-01-01    Female            0           0      0
#>  4 1990-01-01   1990-01-08    Female            0           1      1
#>  5 1990-01-01   1990-01-08    Male              0           1      1
#>  6 1990-01-01   1990-01-15    Female            0           2      2
#>  7 1990-01-01   1990-01-15    Female            0           2      2
#>  8 1990-01-01   1990-01-15    Female            0           2      2
#>  9 1990-01-01   1990-01-22    Female            0           3      3
#> 10 1990-01-01   1990-01-08    Female            0           1      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 1991-01-01 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
```

We can also add temporal effects for example a weekly seasonality (52
seasons) as well as a holiday effect using the `almanac` package:

``` r

library(almanac)

#Specify 52 seasons (weekly) and holidays from the US
t_effects  <- temporal_effects(seasons = 52, holidays = cal_us_federal())

#Add the temporal effects
dengue_tbl <- dengue_tbl |> 
  add_temporal_effects(t_effects)
```

Finally we fit the model:

``` r

nc_dengue <- nowcast(dengue_tbl)
```

The fitted model can be visualized with
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
Note that the nowcast was already stratified by the strata specified in
the `tbl_now`:

``` r

autoplot(nc_dengue) 
```

![\*Nowcast for dengue example. The shaded bars show the median while
the errorbar has the 90% credible
intervals.\*](introduction_files/figure-html/dengue-plot-1.png)

*Nowcast for dengue example. The shaded bars show the median while the
errorbar has the 90% credible intervals.*

Values can be obtained via
[`predict()`](https://rdrr.io/r/stats/predict.html) and
[`summary()`](https://rdrr.io/r/base/summary.html):

``` r

# Full posterior-predictive nowcast at every event-time
pred_dengue <- predict(nc_dengue)

#This creates a summary of mean and quantiles
summary(pred_dengue) 
```

    #>         mean median        sd     mad q2.5  q5 q10 q25 q50 q75 q90    q95
    #> 154 108.6180    108  1.515990  1.4826  107 107 107 108 108 109 111 111.00
    #> 155  89.1175     89  2.426239  1.4826   86  86  87  87  89  90  92  93.00
    #> 156  68.2195     67  4.286083  2.9652   63  63  64  65  67  70  73  76.00
    #> 157  45.5855     44  7.797056  5.9304   36  37  38  41  44  49  55  58.05
    #> 158  40.4260     37 14.853681 10.3782   24  25  27  31  37  46  57  63.05
    #> 159  36.3720     32 18.817696 14.8260   12  14  17  24  32  44  59  70.00
    #>       q97.5 .event_num stratum event_date
    #> 154 112.000         47   Total 1990-11-26
    #> 155  95.000         48   Total 1990-12-03
    #> 156  78.025         49   Total 1990-12-10
    #> 157  64.000         50   Total 1990-12-17
    #> 158  71.000         51   Total 1990-12-24
    #> 159  80.000         52   Total 1990-12-31

Additionally the
[`nowcast_diagnostic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast_diagnostic.md)
shows the fitted distribution for the delay, the smoothed epidemic
process as well as the aggregated nowcast (for the sum of all strata):

``` r

nowcast_diagnostic(nc_dengue) 
```

![](introduction_files/figure-html/dengue-diagnostic-1.png)

## Example 2 – Mpox (modifying the nowcast model)

The `mpoxdat` dataset (also in `tbl.now`) covers the 2022 mpox outbreak
in New York City with daily case counts stratified by race.

``` r

data(mpoxdat)

mpox_tbl <- tbl_now(
  mpoxdat,
  event_date  = dx_date,
  report_date = dx_report_date,
  case_count  = n,
  data_type   = "count-incidence",
  now         =  as.Date("2022-08-15")
) 
```

A simple plot of the data shows that we should be taking into account
day-of-the-week effects:

``` r

autoplot(mpox_tbl)
```

![](introduction_files/figure-html/mpoxplot-1.png)

We can also set it again with
[`add_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.html):

``` r

mpox_tbl <- mpox_tbl |> 
  add_temporal_effects(temporal_effects(day_of_week = TRUE))
```

You can see that the `tbl_now` indicates its computation:

    #> # A tibble:  1,417 × 7
    #> # Data type: "count-incidence"
    #> # Frequency: Event: `days` | Report: `days`
    #>    dx_date      dx_report_date race              n .event_num .report_num .delay
    #>    <date>       <date>         <chr>         <int>      <dbl>       <dbl>  <dbl>
    #>    [event_date] [report_date]  [...]         [cas…      [...]       [...]  [...]
    #>  1 2022-07-08   2022-07-12     Asian             4          0           4      4
    #>  2 2022-07-08   2022-07-12     Black             6          0           4      4
    #>  3 2022-07-08   2022-07-12     Hispanic          6          0           4      4
    #>  4 2022-07-08   2022-07-12     Non-Hispanic…     6          0           4      4
    #>  5 2022-07-08   2022-07-13     Asian             2          0           5      5
    #>  6 2022-07-08   2022-07-13     Black             3          0           5      5
    #>  7 2022-07-08   2022-07-13     Hispanic          8          0           5      5
    #>  8 2022-07-08   2022-07-13     Non-Hispanic…     5          0           5      5
    #>  9 2022-07-08   2022-07-14     Black             1          0           6      6
    #> 10 2022-07-08   2022-07-14     Hispanic          3          0           6      6
    #> # ────────────────────────────────────────────────────────────────────────────────
    #> # Now: 2022-08-15 | Event date: "dx_date" | Report date: "dx_report_date"
    #> # T. effects (lazy): [event_date] day_of_week
    #> # ────────────────────────────────────────────────────────────────────────────────
    #> # ℹ 1,407 more rows

One can also choose between several likelihoods, epidemic processess and
delay distributions and feed it into the
[`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md).
Here we use a Susceptible-Infected-Recovered model (SIR) with a delay
that follows a lognormal distribution:

``` r

#Models can be modified via the model() 
mpox_model <- model(likelihood = nb_likelihood(),   #Negative binomial
                    epidemic   = sir_epidemic(),    #SIR model
                    delay      = lognormal_delay()) #Delay distribution

#We can then fit the  model
nc_mpox <- nowcast(mpox_tbl, model = mpox_model)

#And show the nowcast
autoplot(nc_mpox) 
```

![](introduction_files/figure-html/mpox-fit-1.png)

## Example 3 – Comparing models with a backtest

A **backtest** reruns nowcasts at multiple historical dates and scores
them against the eventually-observed totals. This lets you compare
between models before committing to one for real-time monitoring.

> The
> [`backtest()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/backtest.md)
> function fits one nowcast per `date` and `model` cell. Those cells run
> in parallel through the [future](https://future.futureverse.org/)
> framework.  
> By default they run sequentially; to use several CPU cores, set a
> [parallel plan](https://future.futureverse.org/reference/plan.html)
> *before* calling
> [`backtest()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/backtest.md):

``` r

library(future)
plan(multisession, workers = 4)   # 4 parallel R sessions
# ... run backtest() ...
plan(sequential)                  # restore serial execution when done
```

In what follows we run a backtest in sequential mode however we strongly
recommend using as many workers in a multisession plan as possible:

``` r

# Compare HSGP (flexible GP trend) vs AR1 (autoregressive trend) 
# and SIR (susceptible, infected, recovered) on mpox
models_to_compare <- list(
  model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
  model(nb_likelihood(), ar1_epidemic(),  lognormal_delay()),
  model(nb_likelihood(), sir_epidemic(),  lognormal_delay())
)

#Uncomment this line to use several of your cores
#plan(multisession, workers = 4) 

backtest_mpox <- backtest(
  mpox_tbl,
  models  = models_to_compare,
  dates = seq(as.Date("2022-08-01"), as.Date("2022-08-15"), by = "week")
)

#This closes the plan multisession opened above
#plan(sequential)   
```

The backtest is already in the common `tbl.now` format. Convert it
directly to a scoringutils quantile forecast and compute any supported
metrics:

``` r

backtest_scores <- backtest_mpox |>
  scoringutils::as_forecast_quantile() |>
  scoringutils::score()

relative_scores <- backtest_scores |>
  scoringutils::add_relative_skill(metric = "wis") |>
  scoringutils::summarise_scores(by = "model")
relative_scores
#> Key: <model>
#>                model      wis overprediction underprediction dispersion
#>               <char>    <num>          <num>           <num>      <num>
#> 1:  AR1/nb/LogNormal 2.810670     0.11111111        1.350000  1.3495587
#> 2: HSGP/nb/LogNormal 4.116668     0.48263889        1.693229  1.9408001
#> 3:  SIR/nb/LogNormal 3.463451     0.03009259        2.594734  0.8386241
#>          bias interval_coverage_50 interval_coverage_90 ae_median
#>         <num>                <num>                <num>     <num>
#> 1: -0.4333333            0.5416667            0.7395833  5.906250
#> 2: -0.4161458            0.5416667            0.7500000  8.520833
#> 3: -0.4723958            0.5104167            0.6666667  7.119792
#>    wis_relative_skill
#>                 <num>
#> 1:          0.8213374
#> 2:          1.2029778
#> 3:          1.0120938
```

The scoringutils output includes WIS and its decomposition, median
absolute error, interval coverage, and relative WIS. A well-calibrated
nowcast should have low WIS and coverage close to the nominal levels.

> Read the table by picking the model with the lowest WIS whose coverage
> is still close to nominal – a model that wins on WIS by being
> overconfident is not the one you want in production. Note that for the
> tutorial we only used 3 historical dates, which is far too few to
> reach a definite conclusion; a real comparison would use dozens.

## When a report is not yet a case: the revision process

Everything so far assumes a report **is** a case. Many registers work
provisionally instead: a report arrives, and is later **resolved** —
confirmed by a laboratory result, or retracted when it turns out not to
be a case at all.

Record that on the `tbl_now` and `diseasenowcasting` nowcasts the number
that actually settles rather than the raw report count. There is no
argument to pass: the process is **detected** from the data.

``` r

# One date, plus what the result was: "confirmed", "retracted" or "pending".
dat <- tbl_now(linelist, event_date = onset, report_date = reported,
               revision_date = result, revision_type = outcome,
               data_type = "linelist")

nowcast(dat)     # works out for itself which outcomes you record
```

Whether you record only retractions, only confirmations, or both, is
read from the values in `revision_type` — and the answer targets,
respectively, the cases never retracted, the cases eventually confirmed,
or the cases whose result comes back positive.

The key point, and the reason this is not just “multiply by the
confirmed fraction”: **a missing resolution date means *not resolved
yet*, not “fine”**. A report filed this morning has had no chance to be
retracted; one filed two months ago and still standing is almost
certainly genuine. The model weighs every report by how long it has had
to be contradicted, so recent event times — exactly the ones you care
about — are corrected properly.

[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
prints what it is modelling and the fitted probability, and
[`parameters()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/parameters.md)
gives its uncertainty
([`tidy()`](https://generics.r-lib.org/reference/tidy.html) on a nowcast
gives you the predicted counts instead):

``` r

parameters(nc) |> filter(type == "resolution")
#>                 term estimate conf.low conf.high       type
#>  prob_not_retracted    0.8512   0.8399    0.8619 resolution
```

See the [Revision processes
article](https://rodrigozepeda.github.io/diseasenowcasting/articles/Revision_processes.html)
for the full treatment, including per-stratum probabilities, censored
revision dates, count-incidence data, and the shared revision-delay
assumption used when both outcomes are recorded.

## Example 4 – Letting the package choose the model (`auto_nowcast()`)

Doing the backtest-and-compare loop by hand (Example 3) is exactly what
[`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md)
automates. Given a `tbl_now`, it builds a grid of candidate models
*sized to how much data you have* (which epidemic processes are even
feasible, crossed with the delay families), backtests them, scores them,
and **refits the single best one** on the full data. The result is an
ordinary nowcast, with the ranked comparison stored alongside it.

Here we use the dengue data up to January 1992:

``` r

#All dengue observed as of January 1992
dengue_92 <- denguedat |>
  filter(onset_week  <= as.Date("1992-01-01") &
         report_week <= as.Date("1992-01-01"))

dengue_tbl_92 <- tbl_now(
  dengue_92,
  event_date  = onset_week,
  report_date = report_week,
  data_type   = "linelist",
  now         = as.Date("1992-01-01")
)
```

Backtesting the whole grid is the expensive step: cost grows with the
number of candidates (`delays` x epidemic processes), the number of
backtest dates (`n_dates`), and – most steeply – the length of the
series. The settings below are deliberately small so this vignette
builds quickly; for a real selection keep the defaults (all three delay
families, `n_dates = 6`, `n_draws = 2000`, `K = 25`). To run the
candidates in parallel, set a
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
before the call and restore it afterwards (left commented here so the
vignette stays single-process):

``` r

# Uncomment to run candidates in parallel:
# library(future)
# plan(multisession, workers = max(parallel::detectCores() - 1, 1))
auto_ncast <- auto_nowcast(
  dengue_tbl_92,
  metric         = "wis",   # rank candidates by relative WIS (the default)
  relative_score = TRUE,
  delays         = list(lognormal_delay(), generalized_gamma_delay()),
  n_dates        = 3,       # backtest at 3 historical dates (raise for a firmer choice)
  n_draws_select = 100,     # draws while comparing (small => fast)
  n_draws        = 300,     # draws for the final fit of the winner
  K              = 5,       # delay imputations for the final fit
  K_select       = 5        # delay imputations while comparing
)
# plan(sequential)
```

We can show the scores of the models to see the best performer:

``` r

comparison_scores(auto_ncast)  # every candidate, ranked best-first
#> # A tibble: 6 × 16
#>   model                    wis overprediction underprediction dispersion    bias
#>   <chr>                  <dbl>          <dbl>           <dbl>      <dbl>   <dbl>
#> 1 HSGP/nb/LogNormal      0.360       0.00648            0.154      0.200 -0.0275
#> 2 HSGP/nb/GeneralizedGa… 0.410       0.0291             0.161      0.220 -0.0192
#> 3 AR1/nb/GeneralizedGam… 0.519       0.000741           0.318      0.200 -0.047 
#> 4 AR1/nb/LogNormal       0.568       0                  0.392      0.175 -0.0552
#> 5 SIR/nb/LogNormal       1.48        0                  1.36       0.120 -0.0702
#> 6 SIR/nb/GeneralizedGam… 1.49        0                  1.37       0.125 -0.0713
#> # ℹ 10 more variables: interval_coverage_50 <dbl>, interval_coverage_90 <dbl>,
#> #   ae_median <dbl>, wis_relative_skill <dbl>, median_fit_seconds <dbl>,
#> #   total_fit_seconds <dbl>, successful_fits <int>, epidemic_priority <int>,
#> #   grid_order <int>, selection_score <dbl>
selection_timings(auto_ncast)  # retrospective fits, refits, and total seconds
#> $backtest
#> # A tibble: 18 × 5
#>    .method                  .now       elapsed_seconds success error
#>    <chr>                    <date>               <dbl> <lgl>   <chr>
#>  1 SIR/nb/LogNormal         1991-11-18           2.04  TRUE    NA   
#>  2 SIR/nb/GeneralizedGamma  1991-11-18           3.96  TRUE    NA   
#>  3 AR1/nb/LogNormal         1991-11-18           1.15  TRUE    NA   
#>  4 AR1/nb/GeneralizedGamma  1991-11-18           3.15  TRUE    NA   
#>  5 HSGP/nb/LogNormal        1991-11-18           0.879 TRUE    NA   
#>  6 HSGP/nb/GeneralizedGamma 1991-11-18           2.69  TRUE    NA   
#>  7 SIR/nb/LogNormal         1991-11-25           2.10  TRUE    NA   
#>  8 SIR/nb/GeneralizedGamma  1991-11-25           3.49  TRUE    NA   
#>  9 AR1/nb/LogNormal         1991-11-25           1.52  TRUE    NA   
#> 10 AR1/nb/GeneralizedGamma  1991-11-25           3.50  TRUE    NA   
#> 11 HSGP/nb/LogNormal        1991-11-25           1.09  TRUE    NA   
#> 12 HSGP/nb/GeneralizedGamma 1991-11-25           2.58  TRUE    NA   
#> 13 SIR/nb/LogNormal         1991-12-02           1.96  TRUE    NA   
#> 14 SIR/nb/GeneralizedGamma  1991-12-02           3.49  TRUE    NA   
#> 15 AR1/nb/LogNormal         1991-12-02           1.17  TRUE    NA   
#> 16 AR1/nb/GeneralizedGamma  1991-12-02           3.04  TRUE    NA   
#> 17 HSGP/nb/LogNormal        1991-12-02           0.945 TRUE    NA   
#> 18 HSGP/nb/GeneralizedGamma 1991-12-02           3.20  TRUE    NA   
#> 
#> $refit
#> # A tibble: 1 × 4
#>   model             elapsed_seconds success error
#>   <chr>                       <dbl> <lgl>   <chr>
#> 1 HSGP/nb/LogNormal            1.18 TRUE    NA   
#> 
#> $total_seconds
#> [1] 44.954
```

[`best_model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/best_model.md)
hands back the winning
[`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
object, so you can reuse the same specification on other data (or feed
it to
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
/
[`backtest()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/backtest.md)):

``` r

winner <- best_model(auto_ncast)
winner
```

Because the result is a normal nowcast, everything else just works:

``` r

autoplot(auto_ncast)
```

![\_Nowcast from the model auto_nowcast()
selected.\_](introduction_files/figure-html/auto-plot-1.png)

*Nowcast from the model auto_nowcast() selected.*

### Updating the chosen model as new data arrive

The selected nowcast is an ordinary `nowcast_class`, so once
[`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md)
has picked a model you keep it and feed it the next batch of reports
with [`update()`](https://rdrr.io/r/stats/update.html) – no need to
re-run the selection. [`update()`](https://rdrr.io/r/stats/update.html)
warm-refits the *same* winning model and scores the incoming reports
against the fitted delay, warning if any arrive far later than expected:

``` r

# A few more weeks of dengue, observed as of 1992-04-01:
dengue_apr <- denguedat |>
  filter(onset_week  <= as.Date("1992-04-01") &
         report_week <= as.Date("1992-04-01"))

dengue_tbl_apr <- tbl_now(
  dengue_apr,
  event_date  = onset_week,
  report_date = report_week,
  data_type   = "linelist",
  now         = as.Date("1992-04-01")
)

auto_ncast_updated <- update(auto_ncast, dengue_tbl_apr)
#> Warning: ! Surprising reporting delay of 13 weeks (2 reports): longer than the model
#>   expects (P(D >= d) = 2.9e-05).
#> ! Surprising reporting delay of 12 weeks (1 report): longer than the model
#>   expects (P(D >= d) = 5.6e-05).
#> ! Surprising reporting delay of 11 weeks (5 reports): longer than the model
#>   expects (P(D >= d) = 0.00011).
#> ! Surprising reporting delay of 10 weeks (7 reports): longer than the model
#>   expects (P(D >= d) = 0.00023).
#> ! Surprising reporting delay of 8 weeks (1 report): longer than the model
#>   expects (P(D >= d) = 0.0011).
#> ! Surprising reporting delay of 7 weeks (14 reports): longer than the model
#>   expects (P(D >= d) = 0.0026).
#> ! Surprising reporting delay of 6 weeks (11 reports): longer than the model
#>   expects (P(D >= d) = 0.0064).
#> ℹ If these are outliers, treat them as censored with
#>   `tbl.now::censor_reporting_delays_above()` and re-fit.
#> ℹ See all flagged delays with `extreme_values(nc)`.
```

Any reports with surprising delays are collected by
[`extreme_values()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/extreme_values.md)
(it returns `NULL` when nothing looks off):

``` r

extreme_values(auto_ncast_updated)
#>   delay weight mean_tail_prob cdf_prob      lpd relative_surprise direction
#> 1     6     11       0.006445 0.993555  -5.0725            0.0162      long
#> 2     7     14       0.002609 0.997391  -6.0402            0.0062      long
#> 3     8      1       0.001111 0.998889  -6.9546            0.0025      long
#> 4    10      7       0.000230 0.999770  -8.6399            0.0005      long
#> 5    11      5       0.000111 0.999889  -9.4196            0.0002      long
#> 6    12      1       0.000056 0.999944 -10.1626            0.0001      long
#> 7    13      2       0.000029 0.999971 -10.8724            0.0000      long
#>   surprise level
#> 1    delay  0.99
#> 2    delay  0.99
#> 3    delay  0.99
#> 4    delay  0.99
#> 5    delay  0.99
#> 6    delay  0.99
#> 7    delay  0.99
```

See the article on [Handling Outlier Delays with
Censoring](https://rodrigozepeda.github.io/diseasenowcasting/articles/Handling_Outlier_Delays_with_Censoring.html)
for what to do when a delay *is* flagged.

## Example 5 – Count-cumulative revisions and a historical origin

FluSight publishes a running level for each target week. Keep three
dates distinct: `full_data` is the retrospective extract used only to
obtain scoring truth; `historical_now` is the information cutoff; and
`event_date` identifies the weeks being nowcast. Starting near September
2023 avoids interpreting the between-season gap as a reporting delay.

``` r

data(flusight, package = "tbl.now")
historical_now <- as.Date("2024-01-27")
full_data <- flusight |>
  filter(location_name == "California",
         target_end_date >= as.Date("2023-09-01"))

calendar_tbl <- tbl_now(
  full_data,
  event_date = target_end_date,
  report_date = as_of,
  case_count = observation,
  data_type = "count-cumulative",
  event_units = "weeks", report_units = "weeks",
  now = max(full_data$as_of), verbose = FALSE
)
calendar_rows <- full_data |>
  filter(target_end_date <= historical_now, as_of <= historical_now)
calendar_asof <- tbl_now(
  calendar_rows,
  event_date = target_end_date, report_date = as_of,
  case_count = observation, data_type = "count-cumulative",
  event_units = "weeks", report_units = "weeks",
  now = historical_now, verbose = FALSE
) |>
  tbl.now::complete_zeroes(max_delay = 26L, until = historical_now)

stopifnot(max(calendar_asof$target_end_date) <= historical_now,
          max(calendar_asof$as_of) <= historical_now)
```

The **calendar clock** keeps every intervening calendar week, and zero
completion makes missing cells inside the observable triangle explicit.
For the **compressed publication clock**, map the observed publication
weeks to consecutive synthetic weeks before constructing the `tbl_now`;
retain the real dates in separate columns for the as-of filter and
scoring join.

``` r

publication_weeks <- sort(unique(full_data$as_of))
publication_index <- setNames(seq_along(publication_weeks) - 1L,
                              as.character(publication_weeks))
compressed_data <- full_data |>
  filter(target_end_date %in% publication_weeks) |>
  mutate(event_num = publication_index[as.character(target_end_date)],
         report_num = publication_index[as.character(as_of)],
         event_model = as.Date("2000-01-01") + 7L * event_num,
         report_model = as.Date("2000-01-01") + 7L * report_num)
compressed_tbl <- tbl_now(
  compressed_data, event_date = event_model, report_date = report_model,
  case_count = observation, data_type = "count-cumulative",
  event_units = "weeks", report_units = "weeks", verbose = FALSE
)
# The final publication of one season is immediately followed by the first of
# the next on this model clock; the original dates still enforce historical_now.
```

Count-cumulative data use their own component. `settlement = 26L`
targets C_t(26), finite-horizon database retention. The cumulative-level
model and the two signed hurdle models share the same collapsed
retraction kernel; the zero-truncated Poisson magnitude has no
dispersion parameter.

``` r

level_model <- model(
  nb_likelihood(), ar1_epidemic(), lognormal_delay(),
  cumulative = cumulative_process(
    observation = "cumulative", settlement = 26L
  )
)
ztnb_model <- model(
  nb_likelihood(), ar1_epidemic(), lognormal_delay(),
  cumulative = cumulative_process(
    observation = "hurdle_ztnb", settlement = 26L
  )
)
ztp_model <- model(
  poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
  cumulative = cumulative_process(
    observation = "hurdle_ztpoisson", settlement = 26L
  )
)

calendar_fits <- lapply(
  list(level = level_model, ztnb = ztnb_model, ztp = ztp_model),
  nowcast, data = calendar_tbl, now = historical_now,
  temporal_effects = "none"
)
```

An empirical multiplier is a development comparator, not part of the
fitted model. At target age a, calibrate it only on earlier cohorts for
which both C_s(a) and C_s(26) were observable by `historical_now`;
discard zero denominators and report a fallback when too few pairs
remain. Retrospective terminal values after `historical_now` may be used
for scoring, never for this calibration. See
`devel/skellam_prototypes/flusight_asof_data.R` for the complete
calendar/compressed preparation and leakage-safe multiplier used by the
large backtest runner.

## Saving and loading a fitted nowcast

Fitting can take a while, so you will often want to **save** a fitted
nowcast and reload it later – in a report, a dashboard, or a scheduled
job – instead of re-fitting.
[`save_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/save_nowcast.md)
writes it to a single `.rds` file and
[`load_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/load_nowcast.md)
brings it back.

The autodiff engine (`RTMB`) cannot itself be written to disk, so what
is stored is everything needed to *reuse* the fit: the
[`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
specification, the input `tbl_now`, and each fit’s parameters together
with its Laplace mode and precision. That is all
[`predict()`](https://rdrr.io/r/stats/predict.html) needs, so a reloaded
nowcast behaves exactly like the original (any number of draws, no
re-fitting):

``` r

saved <- tempfile(fileext = ".rds")
save_nowcast(auto_ncast, saved)

restored <- load_nowcast(saved)
# predict() / autoplot() / coef() / parameters() all work just as before:
autoplot(restored)
```

![](introduction_files/figure-html/save-load-1.png)

Because the input data travels in the bundle, you can also **re-fit**
the saved model later (on the original data, or on a newer extract):

``` r

nowcast(restored@data, restored@model)   # re-runs the optimisation
```

## Combining diseasenowcasting models in an ensemble

Different epidemic processes can fail in different ways. For example, an
HSGP can follow a smooth epidemic curve while an AR(1) process reacts
more locally. Because
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
returns the common
[`tbl.now::tbl_nowcast`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.html)
result, its output can be passed **directly** to
[`tbl.now::nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.html);
there is no need to refit the models through
[`tbl.now::run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.html)
or convert their results.

Here both members use the same likelihood and reporting-delay model, and
differ only in their epidemic process:

``` r

hsgp_fit <- nowcast(
  dengue_tbl,
  model = model(
    likelihood = nb_likelihood(),
    epidemic   = hsgp_epidemic(),
    delay      = lognormal_delay()
  ),
  n_draws = 1000,
  seed = 101
)

ar1_fit <- nowcast(
  dengue_tbl,
  model = model(
    likelihood = nb_likelihood(),
    epidemic   = ar1_epidemic(),
    delay      = lognormal_delay()
  ),
  n_draws = 1000,
  seed = 102
)
```

The default ensemble averages matching predictive quantiles from the two
models. Naming the arguments also records useful member names in the
result:

``` r

ensemble <- tbl.now::nowcast_ensemble(
  HSGP = hsgp_fit,
  AR1  = ar1_fit
)

autoplot(ensemble)
```

Both `diseasenowcasting` fits retain posterior draws, so they can also
be combined as a mixture distribution. This preserves disagreement
between the members as part of the ensemble uncertainty and will
generally give wider intervals than averaging their quantiles:

``` r

pooled_ensemble <- tbl.now::nowcast_ensemble(
  HSGP = hsgp_fit,
  AR1  = ar1_fit,
  type = "linear_pool",
  n_draws = 4000
)
```

## Next steps

This vignette covered the basics: building a `tbl_now`, fitting a
nowcast with
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md),
inspecting results with
[`predict()`](https://rdrr.io/r/stats/predict.html) /
[`summary()`](https://rdrr.io/r/base/summary.html) /
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
comparing models with
[`backtest()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/backtest.md)
/ `score()`, and combining structurally different fits with
[`tbl.now::nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.html).

Depending on what you want to do next, check out the following vignettes
and website articles:

- **[Nowcasting at the Start of an
  Epidemic](https://rodrigozepeda.github.io/diseasenowcasting/articles/Nowcasting_at_the_start_of_an_Epidemic.md)**
  — A worked, end-to-end case study of monitoring an outbreak in real
  time: choosing a model when data are scarce, reading the nowcast as
  the epidemic grows, and experimenting with the prior to encode what
  you already believe about the epidemic before much data arrives.

- **[Understanding Priors in
  diseasenowcasting](https://rodrigozepeda.github.io/diseasenowcasting/articles/Understanding_Priors.md)**
  — *Make the model say what you mean.* Shows the package’s default
  priors, how to tighten or loosen them, and how the prior trades off
  against the data. Uses the prior-predictive tools
  (`nowcast(..., prior_only = TRUE)`) to *see* what a prior implies
  before fitting.

- **[Custom delays and epidemic
  processes](https://rodrigozepeda.github.io/diseasenowcasting/articles/Custom_delays_and_processes.md)**
  — Two examples on how to set **your own delays and epidemic
  processes**. Includes how to use ordinary differential equation
  models.

- **[Handling Outlier Delays with
  Censoring](https://rodrigozepeda.github.io/diseasenowcasting/articles/Handling_Outlier_Delays_with_Censoring.html)**
  — *Robustness to reporting glitches.* How the censored likelihood
  copes with unusually long reporting delays, and how to flag extreme
  delays in your surveillance stream.

- **[Using alongside an
  LLM](https://rodrigozepeda.github.io/diseasenowcasting/articles/LLM_Usage.html)**
  — *Use AI.* How to use the
  [`SKILL.md`](https://github.com/RodrigoZepeda/diseasenowcasting/blob/master/SKILL.md)
  to teach a Large Language Model how to you develop your nowcasts with
  `diseasenowcasting`.

- **[Benchmark (diseasenowcasting vs NobBS and
  epinowcast)](https://rodrigozepeda.github.io/diseasenowcasting/articles/Benchmark.html)**
  — *How does it compare?* A reproducible backtest comparing
  `diseasenowcasting` against the `NobBS` and `epinowcast` packages.

- **[Mathematical Foundations of
  diseasenowcasting](https://rodrigozepeda.github.io/diseasenowcasting/articles/Mathematics.html)**
  — *Under the hood.* The censored likelihood, the epidemic processes
  (HSGP, AR(1), SIR), the delay families, and the Laplace-approximation
  inference that powers `RTMB`.

See
[`?nowcast`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md),
[`?backtest`](https://rodrigozepeda.github.io/diseasenowcasting/reference/backtest.md),
`?score`, and
[`?model`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
for full documentation.
