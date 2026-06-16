
<!-- README.md is generated from README.Rmd. Please edit that file -->

# diseasenowcasting <a href="https://rodrigozepeda.github.io/diseasenowcasting/"><img src="man/figures/logo.png" align="right" height="139" alt="diseasenowcasting website" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/diseasenowcasting)](https://CRAN.R-project.org/package=diseasenowcasting)
[![Codecov test
coverage](https://codecov.io/gh/RodrigoZepeda/diseasenowcasting/graph/badge.svg)](https://app.codecov.io/gh/RodrigoZepeda/diseasenowcasting)
[![R-CMD-check](https://github.com/RodrigoZepeda/diseasenowcasting/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RodrigoZepeda/diseasenowcasting/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`diseasenowcasting` is an R package for nowcasting time series of
epidemiological cases.

## What is nowcasting?

Surveillance systems have a delay between the **true date of an event**
(`event_date`, e.g. symptom onset or testing) and the **report date**
for that event (`report_date`, e.g. when the case was entered into the
database). Because recent event-dates are still missing reports that
have not yet arrived, the latest case counts look artificially low. A
*nowcast* corrects for this reporting delay and estimates the eventual,
fully-observed totals.

To do this `diseasenowcasting` fits censored Bayesian models. The
reporting delay is modeled directly as a stochastic process, jointly
with the epidemic dynamics, through a censored likelihood. Inference
runs on R’s Template Model Builder
([`RTMB`](https://cran.r-project.org/package=RTMB)), so no Stan/JAGS
compilation is required. The package supports several [epidemic
processes](https://rodrigozepeda.github.io/diseasenowcasting/reference/epidemic_process.html),
[several delay
families](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.html),
stratified data, [extreme value
detection](https://rodrigozepeda.github.io/diseasenowcasting/reference/extreme_values.html)
and
[backtesting](https://rodrigozepeda.github.io/diseasenowcasting/reference/backtest.html).

> :warning: `diseasenowcasting` is currently under active development
> and parts of the interface might still change.

## Why `diseasenowcasting`?

How `diseasenowcasting` compares with other R nowcasting packages:

> | Feature | `diseasenowcasting` | [`baselinenowcast`](https://github.com/epinowcast/baselinenowcast) | \>[`NobBS`](https://cran.r-project.org/package=NobBS) | [`nowcaster`](https://github.com/covid19br/nowcaster) | \>[`epinowcast`](https://github.com/epinowcast/epinowcast) |
> |----|:--:|:--:|:--:|:--:|:--:|
> | Arbitrary delay distributions <sup>†</sup> | ✅ | ❌ | ❌ | ❌ | ❌ |
> | Arbitrary epidemic processes <sup>†</sup> | ✅ | ❌ | ❌ | ❌ | ❌ |
> | No per-model compilation | ✅ | ✅ | ✅ | ✅ | ❌ |
> | Pure R — no external engine (Stan/JAGS) <sup>‡</sup> | ✅ | ✅ | ❌ | ✅ | ❌ |
> | Stratified data | ✅ | ❌ | ❌ | ✅<sup>§</sup> | ✅ |
> | Calendar / day-of-week effects | ✅ | ❌ | ❌ | ❌ | ✅ |
> | Counts that can decrease (cases later un-confirmed) | 🚧 | ✅ | ❌ | ❌ | ❌ |
> | Effective reproductive number (Rₜ) | ❌ | ❌ | ❌ | ❌ | ✅ |
>
> <sub> <sup>†</sup> *Arbitrary* means you supply your own custom R
> function: any distribution for the delay, any function `f(t)` for the
> epidemic process (not just a choice from a built-in menu).
> `epinowcast` is very flexible through parametric families and model
> formulas, but does not take arbitrary user-defined functions.
>
> <sup>‡</sup> `NobBS` requires JAGS and `epinowcast` requires CmdStan
> (an external Stan toolchain); `nowcaster` runs entirely in R but
> depends on the (non-CRAN) `INLA` package.
>
> <sup>§</sup> `nowcaster` stratifies by age/region structure only, not
> arbitrary user-defined strata; `diseasenowcasting` allows any
> combination of strata columns.
>
> <sup>🚧</sup> In development for `diseasenowcasting` (counts that
> revise *downward*, e.g. a positive later re-classified as negative);
> `baselinenowcast` already supports this. </sub>

## Installing

You can install `diseasenowcasting` (and its companion data package
[`tbl.now`](https://github.com/RodrigoZepeda/tbl.now)) from GitHub with
[`pak`](https://pak.r-lib.org/):

``` r
# install.packages("pak") # <- uncomment if you do not have `pak`
pak::pkg_install("RodrigoZepeda/tbl.now")
pak::pkg_install("RodrigoZepeda/diseasenowcasting")
```

## Example

`diseasenowcasting` works with data organised as a `tbl_now` object from
the companion [`tbl.now`](https://rodrigozepeda.github.io/tbl.now/)
package: a data frame annotated with the **roles** of its columns (event
date, report date, and optionally the `strata` and the `now` date up to
which everything is observed).

``` r
set.seed(6728)
library(diseasenowcasting)
library(tbl.now)
library(dplyr)

# Example surveillance data (dengue in Puerto Rico) shipped with tbl.now
data(denguedat)

#Simulate a nowcast on January 1st 1991
#Only cases and reports before that date will be used
dengue_data <- denguedat |> 
  filter(onset_week <= as.Date("1991-01-01") & report_week <= as.Date("1991-01-01"))
```

We annotate the data with `tbl_now()`, pointing at the event and report
dates (here the symptom-onset and report weeks) and the strata
(`gender`):

``` r
dengue_tbl <- tbl_now(
  dengue_data,
  event_date  = onset_week,    # when symptoms started
  report_date = report_week,   # when the case was registered
  strata      = gender,
  data_type   = "linelist",    # use "count-incidence" for pre-aggregated counts
  now         = as.Date("1991-01-01"),
  t_effects   = temporal_effects(seasons = 52) #Weekly seasonal effect (Fourier)  
)
```

Once the data is a `tbl_now()`, a single call to `nowcast()` does the
rest (the nowcast is automatically stratified by the strata declared
above):

``` r
ncast <- nowcast(dengue_tbl, n_draws = 1000)
```

Results can be visualised with `autoplot()` (bars show the median, error
bars the 90% credible interval) and extracted with `predict()` /
`summary()`:

``` r
autoplot(ncast)
```

<div class="figure">

<img src="man/figures/README-plot-1.png" alt="_Nowcast for the dengue example, by gender._" width="100%" />
<p class="caption">

*Nowcast for the dengue example, by gender.*
</p>

</div>

``` r
pred <- predict(ncast)   # full posterior-predictive nowcast at every event-time
```

``` r
summary(pred)
```

    #> # A tibble: 6 × 16
    #>    mean median    sd   mad  q2.5    q5   q10   q25   q50   q75   q90   q95 q97.5
    #>   <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    #> 1 109.     108  2.09  1.48   107   107   107   107   108   110 112     113 114  
    #> 2  89.3     89  3.10  2.97    86    86    86    87    89    90  93      95  97  
    #> 3  69.0     68  5.74  4.45    62    63    64    65    68    71  76      79  82.0
    #> 4  48.7     45 13.7   8.90    35    36    37    40    45    53  65      73  83.0
    #> 5  47.8     41 26.3  17.8     22    23    26    31    41    56  77.1    95 111. 
    #> 6  43.1     35 29.3  22.2     11    13    17    23    35    55  79.1    98 116  
    #> # ℹ 3 more variables: .event_num <int>, stratum <chr>, event_date <date>

You can choose a different epidemic process, delay family or likelihood
by passing a `model()` to `nowcast()`:

``` r
model_1 <- model(
  likelihood = nb_likelihood(),          # negative binomial (recommended)
  epidemic   = ar1_epidemic(),           # AR(1) process 
  delay      = generalized_gamma_delay() # generalized gamma reporting delay
)
```

``` r
nowcast(dengue_tbl, model = model_1)
```

And `backtest` and `score` to evaluate how different models would have
performed in the past

``` r
model_2 <- model(
  likelihood = nb_likelihood(),  # negative binomial (recommended)
  epidemic   = hsgp_epidemic(),  # recommended for long-running epidemics
  delay      = lognormal_delay() # generalized gamma reporting delay
)

#Set ndates to the number of dates you want for the backtest
bt <- backtest(dengue_tbl, list(model_1, model_2), n_dates = 3)
```

And get the scores

``` r
autoplot(bt)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" alt="" width="100%" />
You can access these quantities directly with `score`

``` r
score(bt)
#>                     model      wis overprediction underprediction dispersion
#> 1 AR1/nb/GeneralizedGamma 12.10694              0        8.592593   3.514352
#> 2       HSGP/nb/LogNormal 12.30653              0        6.777778   5.528750
#>   coverage_50 coverage_90       ape  mse n
#> 1   0.0000000           1 0.7404989 1342 3
#> 2   0.3333333           1 0.7030839 1089 3
```

## Handling extreme delays

When new data arrives, `update()` re-scores every incoming report
against the previously fitted delay distribution and **warns** about
anything that seems implausibly delayed. The flagged reports are
available via `extreme_values()`:

``` r
#Simulate getting new data
dengue_update <- denguedat |> 
  filter(onset_week <= as.Date("1991-01-14") & report_week <= as.Date("1991-01-14"))

# level stands for when should we flag something is an extreme delay
# level = 0.99 means delays with probability 1  - 0.99 = 0.01 are considered extreme
nc_updated <- update(ncast, new_data = dengue_update, level = 0.99)

# The model identified two extreme (very unlikely) delays of 9 and 10 weeks: 
# A human should check them based on domain knowledge and decide whether to keep
# them or to censor them. 
extreme_values(nc_updated)  
```

A complete tutorial on handling extreme delays is available at the
[Handling Outlier Delays with
Censoring](https://rodrigozepeda.github.io/diseasenowcasting/articles/Handling_Outlier_Delays_with_Censoring.html)
vignette.

## See also

A good place to start is the **[Introduction to
diseasenowcasting](https://rodrigozepeda.github.io/diseasenowcasting/articles/introduction.html)**,
which walks through building a `tbl_now`, fitting with `nowcast()`,
inspecting results with `predict()` / `summary()` / `autoplot()`, and
comparing models with `backtest()` / `score()`.

From there, depending on what you want to do next:

- **[Nowcasting at the Start of an
  Epidemic](https://rodrigozepeda.github.io/diseasenowcasting/articles/Nowcasting_at_the_start_of_an_Epidemic.html)**
  — A worked, end-to-end case study of monitoring an outbreak in real
  time: choosing a model when data are scarce, reading the nowcast as
  the epidemic grows, and using the prior to encode what you already
  believe before much data has arrived.

- **[Understanding Priors in
  diseasenowcasting](https://rodrigozepeda.github.io/diseasenowcasting/articles/Understanding_Priors.html)**
  — *Make the model say what you mean.* The package’s default priors,
  how to tighten or loosen them, and how to *see* what a prior implies
  before fitting (`nowcast(..., prior_only = TRUE)`).

- **[Handling Outlier Delays with
  Censoring](https://rodrigozepeda.github.io/diseasenowcasting/articles/Handling_Outlier_Delays_with_Censoring.html)**
  — *Robustness to reporting glitches.* How the censored likelihood
  copes with unusually long delays, and how to flag and censor extreme
  delays in your surveillance stream.

- **[Custom delays and epidemic
  processes](https://rodrigozepeda.github.io/diseasenowcasting/articles/Custom_delays_and_processes.html)**
  — Two examples on how to set **your own delays and epidemic
  processes**. Includes how to use ordinary differential equation
  models.

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

See `?nowcast`, `?backtest`, `?score`, and `?model` for the full
function documentation.
