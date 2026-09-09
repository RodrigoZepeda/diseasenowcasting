# Automatically select and fit the best nowcasting model

Takes a `tbl_now` and **chooses a model for you**: it builds a grid of
candidate models (epidemic process x reporting-delay family) sized to
how much data you have,
[`backtest()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/backtest.md)s
them over several historical dates, converts the canonical backtest to a
scoringutils forecast, keeps the best one, and refits it on the full
data. The returned object is an ordinary
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
result (so
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
[`predict()`](https://rdrr.io/r/stats/predict.html), etc. work), with
the ranked scoreboard attached in its `comparison` slot.

## Usage

``` r
auto_nowcast(
  data,
  metric = "wis",
  relative_score = TRUE,
  tie_break = c("epidemic_priority", "fastest"),
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
  K_select = 10L,
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

  A single score column produced by
  [`scoringutils::score()`](https://epiforecasts.io/scoringutils/reference/score.html)
  to minimise. Default `"wis"`.

- relative_score:

  Logical. When `TRUE` (the default), select on the corresponding
  relative skill from
  [`scoringutils::add_relative_skill()`](https://epiforecasts.io/scoringutils/reference/add_relative_skill.html)
  rather than on the raw mean score. This makes comparisons fair when
  models are not all available for exactly the same targets.

- tie_break:

  How to break effectively equal selection scores. `"epidemic_priority"`
  (default) prefers HSGP, then AR(1), then SIR, then a custom epidemic
  process. `"fastest"` prefers the candidate with the lowest median
  elapsed time per successful retrospective fit. The unused rule is
  applied second, followed by candidate-grid order for determinism.

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

  Delay imputations for the **final** two-stage fit of the winning model
  (default 25).

- K_select:

  Delay imputations during the **selection** backtest (default 10 – kept
  small for speed, like `n_draws_select`). The selection backtest fits
  the whole grid over many dates, so its cost scales with `K_select`;
  ranking the candidates is robust to a coarser imputation than the
  final fit. Lower it (e.g. `5`) for a long series where selection
  dominates the runtime.

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

A diseasenowcasting subclass of
[tbl.now::tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.html)
(as from
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md))
for the selected model, with the model-selection scoreboard retained on
the diseasenowcasting subclass and its native fit:
`list(scores, chosen, metric, relative_score, tie_break, timings, max_time)`.

## Details

**Candidate epidemic processes are chosen by series length**
(`max_time`, the number of event-times): a process becomes a candidate
as soon as the series is long enough to support it (SIR needs the least
data, the HSGP the most) and is never dropped for being *too* long, so
the comparison always spans every process the data can support. With the
default thresholds:

- `max_time < min_ar` -\> compares `{SIR}`;

- `min_ar <= max_time < min_hsgp` -\> compares `{SIR, AR(1)}`;

- `max_time >= min_hsgp` -\> compares `{SIR, AR(1), HSGP}`.

Any process you pass explicitly via `sir` / `ar` / `hsgp` is *always*
included (regardless of length), which is how you make a prior compete:
e.g. pass `sir = sir_epidemic(R0 = lognormal_prior(log(3), 0.2))` and
the SIR candidate will use that R0 prior throughout the comparison.

**Robustness.** A candidate that fails to converge on a backtest date
simply drops out of the comparison there (it never aborts the search),
and candidates are scored on the common set of dates where they all
produced a forecast so a model cannot "win" on a lucky subset. The
winner is then refit on the full data; if that refit fails,
`auto_nowcast()` falls through to the next-best candidate (and so on),
so it converges whenever any candidate would.

**Candidate delays** default to LogNormal, Generalized-Gamma and
Dirichlet; override with `delays`.

**Speed.** The grid is backtested with a fast configuration
(`n_draws_select` posterior draws over `n_dates` dates spread across the
history); only the winning model is refit with the full `n_draws`.
Backtesting is the expensive step – set a
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
(e.g. `future::plan(multisession)`) for parallel speed-up.

## See also

[diseasenowcasting_workflows](https://rodrigozepeda.github.io/diseasenowcasting/reference/diseasenowcasting_workflows.md),
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md),
[`backtest()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/backtest.md),
[`scoringutils::score()`](https://epiforecasts.io/scoringutils/reference/score.html),
[`scoringutils::add_relative_skill()`](https://epiforecasts.io/scoringutils/reference/add_relative_skill.html)

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
#> ℹ Backtesting "SIR/nb/LogNormal" at 1990-12-17.
#> ℹ Backtesting "SIR/nb/Dirichlet" at 1990-12-17.
#> ℹ Backtesting "AR1/nb/LogNormal" at 1990-12-17.
#> ℹ Backtesting "AR1/nb/Dirichlet" at 1990-12-17.
#> ℹ Backtesting "HSGP/nb/LogNormal" at 1990-12-17.
#> ℹ Backtesting "HSGP/nb/Dirichlet" at 1990-12-17.
#> ℹ Backtesting "SIR/nb/LogNormal" at 1990-12-24.
#> ℹ Backtesting "SIR/nb/Dirichlet" at 1990-12-24.
#> ℹ Backtesting "AR1/nb/LogNormal" at 1990-12-24.
#> ℹ Backtesting "AR1/nb/Dirichlet" at 1990-12-24.
#> ℹ Backtesting "HSGP/nb/LogNormal" at 1990-12-24.
#> ℹ Backtesting "HSGP/nb/Dirichlet" at 1990-12-24.
#> ✔ auto_nowcast: selected HSGP/nb/LogNormal (best relative wis; ties by
#>   epidemic_priority) in 7.1 seconds.
# future::plan(future::sequential)
best_model_name(nc)    # the winning model's label
#> [1] "HSGP/nb/LogNormal"
comparison_scores(nc)  # the ranked scoreboard
#> # A tibble: 6 × 16
#>   model               wis overprediction underprediction dispersion     bias
#>   <chr>             <dbl>          <dbl>           <dbl>      <dbl>    <dbl>
#> 1 HSGP/nb/LogNormal 0.151         0.0132         0.0433      0.0941 -0.0424 
#> 2 HSGP/nb/Dirichlet 0.371         0.0433         0.00377     0.324   0.0864 
#> 3 SIR/nb/LogNormal  0.529         0.101          0.0580      0.370   0.00169
#> 4 SIR/nb/Dirichlet  0.719         0.139          0.00753     0.572   0.103  
#> 5 AR1/nb/Dirichlet  0.784         0.107          0.0179      0.659   0.0424 
#> 6 AR1/nb/LogNormal  0.788         0.105          0.0733      0.609  -0.0314 
#> # ℹ 10 more variables: interval_coverage_50 <dbl>, interval_coverage_90 <dbl>,
#> #   ae_median <dbl>, wis_relative_skill <dbl>, median_fit_seconds <dbl>,
#> #   total_fit_seconds <dbl>, successful_fits <int>, epidemic_priority <int>,
#> #   grid_order <int>, selection_score <dbl>
best_score(nc)         # just the winner's row
#> # A tibble: 1 × 16
#>   model               wis overprediction underprediction dispersion    bias
#>   <chr>             <dbl>          <dbl>           <dbl>      <dbl>   <dbl>
#> 1 HSGP/nb/LogNormal 0.151         0.0132          0.0433     0.0941 -0.0424
#> # ℹ 10 more variables: interval_coverage_50 <dbl>, interval_coverage_90 <dbl>,
#> #   ae_median <dbl>, wis_relative_skill <dbl>, median_fit_seconds <dbl>,
#> #   total_fit_seconds <dbl>, successful_fits <int>, epidemic_priority <int>,
#> #   grid_order <int>, selection_score <dbl>
selection_metric(nc)   # which metric chose it
#> [1] "wis"
winner <- best_model(nc)  # the model() object, to reuse elsewhere
# }
```
