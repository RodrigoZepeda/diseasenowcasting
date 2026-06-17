# Automatically select and fit the best nowcasting model

Takes a `tbl_now` and **chooses a model for you**: it builds a grid of
candidate models (epidemic process x reporting-delay family) sized to
how much data you have,
[`backtest()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/backtest.md)s
them over several historical dates,
[`score()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/score.md)s
them, keeps the best one, and refits it on the full data. The returned
object is an ordinary
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
result (so
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
[`predict()`](https://rdrr.io/r/stats/predict.html), etc. work), with
the ranked scoreboard attached in its `comparison` slot.

## Usage

``` r
auto_nowcast(
  data,
  metric = c("wis", "ape", "mse", "coverage", "coverage_50", "coverage_90"),
  type = c("auto", "two_stage", "one_stage"),
  sir = NULL,
  ar = NULL,
  hsgp = NULL,
  delays = NULL,
  likelihood = nb_likelihood(),
  models = NULL,
  n_dates = 6L,
  n_draws_select = 500L,
  n_draws = 2000L,
  K = 25L,
  min_ar = 15L,
  min_hsgp = 30L,
  now = NULL,
  seed = sample.int(.Machine$integer.max, 1),
  verbose = TRUE,
  ...
)
```

## Arguments

- data:

  A `tbl_now` object
  ([`tbl.now::tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)).

- metric:

  Selection criterion. `"wis"` (default, lowest Weighted Interval
  Score), `"ape"` (lowest absolute percentage error of the median),
  `"mse"` (lowest mean squared error), or one of the calibration
  criteria, which pick the model whose empirical interval coverage is
  closest to nominal: `"coverage_50"` (smallest `|0.50 - coverage_50|`),
  `"coverage_90"` (smallest `|0.90 - coverage_90|`), or `"coverage"`
  (smallest `|0.50 - coverage_50| + |0.90 - coverage_90|`, i.e. both
  intervals jointly).

- type:

  Stage strategy used for *both* the backtest and the final fit:
  `"auto"` (default), `"two_stage"`, or `"one_stage"` (see
  [`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)).

- sir, ar, hsgp:

  Optional epidemic-process components (e.g. `sir_epidemic(R0 = ...)`)
  carrying your priors. If supplied, that process is forced into the
  candidate grid; otherwise the plain constructor is used when the
  series length calls for it.

- delays:

  A list of delay components to compare. Default:
  `list(lognormal_delay(), generalized_gamma_delay(), dirichlet_delay())`.

- likelihood:

  Either a single likelihood used for every candidate (default
  [`nb_likelihood()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md)),
  or a **list** of likelihoods to compare too, e.g.
  `list(nb_likelihood(), poisson_likelihood())`.

- models:

  Optional
  [`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
  object or list of them (e.g. carrying a
  [`custom_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/custom_delay.md)
  /
  [`custom_epidemic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/custom_epidemic.md))
  appended to the candidate grid so they compete in the same backtest.

- n_dates:

  Number of historical dates to backtest over (default 6).

- n_draws_select:

  Posterior draws during the selection backtest (default 500 – kept
  small for speed).

- n_draws:

  Posterior draws for the final fit of the winning model (default 2000).

- K:

  Delay imputations for two-stage fits (default 25).

- min_ar, min_hsgp:

  Series-length thresholds (in event-times) at which AR(1) and HSGP
  become candidates (defaults 15 and 30).

- now:

  As-of date for the final fit (default: the `tbl_now`'s `now`).

- seed:

  RNG seed.

- verbose:

  Print progress and the chosen model (default `TRUE`).

- ...:

  Passed through to
  [`backtest()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/backtest.md)
  and
  [`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
  (e.g. `temporal_effects`).

## Value

A `nowcast_class` (as from
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md))
for the selected model, with the model-selection scoreboard in its
`comparison` slot: `list(scores, chosen, metric, max_time)`.

## Details

**Candidate epidemic processes are chosen by series length**
(`max_time`, the number of event-times): the SIR process needs the least
data, the HSGP the most. With the default thresholds:

- `max_time < min_ar` -\> compares `{SIR}`;

- `min_ar <= max_time < min_hsgp` -\> compares `{SIR, AR(1)}`;

- `max_time >= min_hsgp` -\> compares `{AR(1), HSGP}`.

Any process you pass explicitly via `sir` / `ar` / `hsgp` is *always*
included (regardless of length), which is how you make a prior compete:
e.g. pass `sir = sir_epidemic(R0 = lognormal_prior(log(3), 0.2))` and
the SIR candidate will use that R0 prior throughout the comparison.

**Candidate delays** default to LogNormal, Generalized-Gamma and
Dirichlet; override with `delays`.

**Speed.** The grid is backtested with a fast configuration
(`n_draws_select` posterior draws over `n_dates` dates); only the
winning model is refit with the full `n_draws`. Backtesting is the
expensive step – set a
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
(e.g. `future::plan(multisession)`) for parallel speed-up.

## See also

[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md),
[`backtest()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/backtest.md),
[`score()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/score.md)

## Examples

``` r
# \donttest{
library(tbl.now)
data(denguedat)
# A short window keeps this example quick (auto_nowcast fits a whole grid):
dn <- subset(denguedat,
             onset_week >= as.Date("1990-06-01") & onset_week <= as.Date("1990-12-01"))
tn <- tbl_now(dn, event_date = onset_week, report_date = report_week,
              data_type = "linelist", verbose = FALSE)
# Backtesting the grid is the expensive step -- uncomment to run candidates
# in parallel (then restore sequential afterwards):
# future::plan(future::multisession, workers = 4)
# Compare a couple of delays; make the SIR candidate use a custom R0 prior:
nc <- auto_nowcast(tn,
                   sir    = sir_epidemic(R0 = lognormal_prior(log(2), 0.3)),
                   delays = list(lognormal_delay(), dirichlet_delay()),
                   n_dates = 2, n_draws_select = 150, n_draws = 300,
                   temporal_effects = "none")
#> ℹ auto_nowcast: comparing 6 candidate models (1 likelihood x 3 epidemic
#>   processes x 2 delays) over 2 backtest dates; max_time = 35.
#> ℹ Running 12 backtest cells sequentially.
#> • For a large grid, set a parallel plan first:
#>   `future::plan(future::multisession, workers = N)`.
#> ✔ auto_nowcast: selected HSGP/nb/Dirichlet (best wis).
# future::plan(future::sequential)
best_model_name(nc)    # the winning model's label
#> [1] "HSGP/nb/Dirichlet"
comparison_scores(nc)  # the ranked scoreboard
#>               model      wis overprediction underprediction dispersion
#> 1 HSGP/nb/Dirichlet 20.76903              0        11.55556   9.213472
#> 2  SIR/nb/Dirichlet 21.04875              0        10.88889  10.159861
#> 3  SIR/nb/LogNormal 22.49292              0        11.55556  10.937361
#> 4 HSGP/nb/LogNormal 23.38583              0        12.05556  11.330278
#> 5  AR1/nb/Dirichlet 24.23153              0        16.44444   7.787083
#> 6  AR1/nb/LogNormal 25.21764              0        16.55556   8.662083
#>   coverage_50 coverage_90       ape     mse n
#> 1           0           1 0.6287129 4032.25 1
#> 2           0           1 0.5693069 3306.25 1
#> 3           0           1 0.5594059 3192.25 1
#> 4           0           1 0.6732673 4624.00 1
#> 5           0           1 0.7128713 5184.00 1
#> 6           0           1 0.7029703 5041.00 1
best_score(nc)         # just the winner's row
#>               model      wis overprediction underprediction dispersion
#> 1 HSGP/nb/Dirichlet 20.76903              0        11.55556   9.213472
#>   coverage_50 coverage_90       ape     mse n
#> 1           0           1 0.6287129 4032.25 1
selection_metric(nc)   # which metric chose it
#> [1] "wis"
winner <- best_model(nc)  # the model() object, to reuse elsewhere
# }
```
