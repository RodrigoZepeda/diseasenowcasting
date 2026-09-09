# Use diseasenowcasting

`diseasenowcasting` fits Bayesian epidemic nowcasts with censored
reporting-delay models and RTMB inference. It supports several
likelihoods, epidemic processes, delay distributions, strata,
covariates, report-level revisions, aggregate cumulative revision
streams, custom components, backtesting, and automatic model selection
and ensembling through `tbl.now`.

The companion `tbl.now` package is part of the workflow, not an
interchangeable preprocessing option. Read `tbl.now`’s `SKILL.md` for
complete guidance on building, validating, reshaping, diagnosing, and
converting `tbl_now` data and on using the common `tbl_nowcast` result.
This skill covers the statistical model and native RTMB operations.

If an exact signature or default matters, inspect `?function`, the
current vignettes, or the source. The package is experimental and
evolves.

## Division of responsibility with `tbl.now`

Use `tbl.now` to define and validate what the data mean:

- Declare event, report, and revision dates; data type; case counts;
  time units; analysis `now`; censoring; and revisions with
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html).
- Declare strata with `strata =` or the add/change/remove-strata
  helpers. Each distinct combination becomes a series fitted by
  `diseasenowcasting`.
- Declare covariates with `covariates =` or the
  add/change/remove-covariate helpers. `diseasenowcasting` uses those
  columns automatically; `covariate_prior` controls their coefficients,
  not which columns are inputs.
- Specify temporal effects with
  [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.html),
  attach them with
  [`add_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.html),
  and create their columns with
  [`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.html).
  `diseasenowcasting` consumes that specification.
- Use `tbl.now` for reshaping, validation, diagnostics, plotting, common
  `tbl_nowcast` results, scoring, backtesting, and ensembles.

Use `diseasenowcasting` for the likelihood, epidemic trajectory,
reporting delay, report-level revision or cumulative-revision process,
priors, strata pooling, RTMB fitting, and fit diagnostics. In
particular, `strata_pooling` controls how the strata declared in
`tbl.now` share information.

``` r

x <- tbl_now(
  cases,
  event_date = onset_date,
  report_date = report_date,
  strata = c(region, age_group),
  covariates = mobility,
  data_type = "linelist",
  now = as.Date("2026-09-01")
)

x <- x |>
  add_temporal_effects(temporal_effects(day_of_week = TRUE)) |>
  compute_temporal_effects()
```

Covariates and temporal effects must cover the fitted event-time grid.
For backtests, construct them from information available at each
historical origin to avoid leakage.

## Standard workflow

``` r

library(tbl.now)
library(diseasenowcasting)

x <- tbl_now(
  cases,
  event_date = onset_date,
  report_date = report_date,
  strata = region,
  data_type = "linelist",
  now = as.Date("2026-09-01"),
  verbose = FALSE
)

mdl <- model(
  likelihood = nb_likelihood(),
  epidemic = hsgp_epidemic(),
  delay = lognormal_delay()
)

fit <- nowcast(x, model = mdl, type = "auto", n_draws = 2000, seed = 12345)

autoplot(fit)
fit_check(fit)
parameters(fit)
tbl.now::tidy(fit)

#As well as:
pred <- predict(fit)
summary(pred)
```

For pre-aggregated arrivals, declare `case_count` and
`data_type = "count-incidence"`. Keep the as-of date and time units
explicit when ambiguity would alter the analysis. Use
[`tbl.now::validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.html)
and
[`tbl.now::diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.html)
before blaming a fit for malformed data.

[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
uses an existing `tbl.now` temporal-effect specification, or adds a
sensible default when `temporal_effects = "auto"`. Pass `"none"` to opt
out.

## Specify a model

``` r

model(
  likelihood = nb_likelihood(),
  epidemic = hsgp_epidemic(),
  delay = lognormal_delay(),
  covariate_prior = std_normal_prior(),
  strata_pooling = "independent"
)
```

Add `revision = revision_process(...)` or
`cumulative = cumulative_process(...)` only for the corresponding data
process;
[`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
supplies inert defaults when they are omitted.

### Likelihood

- [`nb_likelihood()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md)
  handles overdispersion and is the usual choice.
- [`poisson_likelihood()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md)
  is simpler when equidispersion is defensible.

The NB `phi` prior belongs on `nb_likelihood(phi = ...)`, not on
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md).
Constructor arguments accept a prior object to estimate a parameter or a
single number to fix it.

### Epidemic process

- [`hsgp_epidemic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/epidemic_process.md)
  gives a flexible smooth trajectory.
- [`ar1_epidemic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/epidemic_process.md)
  gives a fast autoregressive trajectory.
- [`sir_epidemic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/epidemic_process.md)
  gives a mechanistic susceptible-infectious-recovered process.
- [`custom_epidemic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/custom_epidemic.md)
  accepts an RTMB-traceable log-intensity function.

For multiple strata, the reporting delay and several hyperparameters are
shared while epidemic trajectories are stratum-specific. Use
`strata_pooling = "hierarchical"` to partially pool stratum intercepts;
the default is independent intercepts.

### Reporting delay

- [`lognormal_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md)
  and
  [`gamma_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md)
  are parsimonious.
- [`generalized_gamma_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md)
  is more flexible.
- [`dirichlet_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md)
  estimates a non-parametric delay simplex.
- [`custom_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/custom_delay.md)
  accepts an RTMB-traceable distribution.

Delay parameters left unspecified receive documented defaults, some
informed by the observed delay distribution. Inspect
`default_priors(mdl, prepared_data)` when prior behavior matters.

### Priors

Available constructors include normal, standard normal, Student-t,
Cauchy, double-exponential, flat, positive/half-distribution, gamma,
Weibull, inverse-gamma, lognormal, chi-square, exponential, logistic,
and beta priors. Use
[`fix_param()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/fix_param.md)
for a prepared prior bundle only when working at the lower level. Always
check whether a constructor argument is on natural, log, or
unconstrained scale.

## Fit strategy

``` r

fit <- nowcast(
  x,
  model = mdl,
  type = "auto",       # "one_stage", "two_stage", or "auto"
  now = NULL,
  K = 25,
  n_draws = 2000,
  delay_window = 120,
  temporal_effects = "auto",
  seed = 42
)
```

- `"one_stage"` fits epidemic and delay jointly.
- `"two_stage"` fits the delay first, refits the epidemic across `K`
  fixed-delay imputations, and pools retained draws.
- `"auto"` uses one-stage for Dirichlet and custom delays and two-stage
  for the supported parametric delays. This is the recommended one.

Count-cumulative models use one-stage regardless of the request. Fit
warnings and
[`fit_check()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/fit_check.md)
matter: inspect convergence, projected gradient, curvature, excluded
imputations, and any precision regularization before relying on a
result.

Use `prior_only = TRUE` to inspect prior-predictive epidemic
trajectories without fitting:

``` r

prior_fit <- nowcast(x, model = mdl, prior_only = TRUE, n_draws = 1000)
autoplot(prior_fit)
quantile(prior_fit, c(0.05, 0.5, 0.95))
```

## Results

The public result is a `diseasenowcasting` subclass of
[`tbl.now::tbl_nowcast`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.html).
It contains common quantile predictions and draws while retaining the
native fit for model-specific operations.

``` r

as_tibble(fit)                  # common long quantiles
as_tibble(fit, type = "draws") # common posterior draws
tbl.now::tidy(fit)              # common wide summary
autoplot(fit)                   # common result plot

predict(fit)                    # posterior-predictive complete counts
summary(predict(fit))
mean(fit); median(fit); quantile(fit) # latent epidemic incidence
parameters(fit)                 # parameter table with uncertainty
coef(fit)                       # compact point estimates
nowcast_diagnostic(fit)         # delay, latent epidemic, and nowcast panels
fit_check(fit)                  # RTMB optimizer/Laplace diagnostics
```

Do not confuse posterior-predictive counts with latent incidence. For
generic plotting, scoring, and ensembling, use the common `tbl.now`
layer. For RTMB fit quality and model parameters, use the native
functions above.

## Report-level confirmations and retractions

Use a `tbl_now` revision axis for provisional reports that later resolve
once:

``` r

x <- tbl_now(
  cases,
  event_date = onset,
  report_date = reported,
  revision_date = resolved,
  revision_type = outcome,
  is_censored_revision = revision_is_bound,
  data_type = "linelist"
)

mdl <- model(
  nb_likelihood(),
  hsgp_epidemic(),
  lognormal_delay(),
  revision = revision_process(
    revision_delay = dirichlet_revision(bins = 10),
    p = beta_prior(20, 3),
    stratified_p = FALSE,
    mode = "auto"
  )
)

fit <- nowcast(x, mdl)
```

The canonical outcomes are `"confirmed"`, `"retracted"`, and
`"pending"`. Missing revision dates represent unresolved reports, not
confirmed cases. Modes are `"confirmation_only"`, `"retraction_only"`,
`"both"`, or `"auto"`. Automatic inference uses the full data source so
a backtest’s mode does not change by date.

Revision-delay aliases are
[`lognormal_revision()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/revision_delay.md),
[`gamma_revision()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/revision_delay.md),
[`generalized_gamma_revision()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/revision_delay.md),
and
[`dirichlet_revision()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/revision_delay.md).
Reporting and revision censoring are stored separately on the `tbl_now`
and consumed automatically. Do not pass obsolete validation or
`revision_censored` arguments to
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md).

If the data carry a usable report-level revision process and the model
leaves it inactive,
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
attaches a documented default and informs the user. Specify
[`revision_process()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/revision_process.md)
when priors, mode, lag family, or stratification of `p` matter.

## Aggregate count-cumulative streams

Count-cumulative data re-publish a level for each event time across
report dates, possibly with downward corrections. Declare that
representation in `tbl.now` and configure the separate cumulative
observation process:

``` r

x <- tbl_now(
  snapshots,
  event_date = target_end_date,
  report_date = as_of,
  case_count = observation,
  data_type = "count-cumulative",
  align_weeks = TRUE
)

mdl <- model(
  nb_likelihood(),
  ar1_epidemic(),
  lognormal_delay(),
  cumulative = cumulative_process(
    observation = "hurdle_ztnb",
    retraction_delay = lognormal_delay(),
    settlement = 26
  )
)

fit <- nowcast(x, mdl)
```

Observation options are `"hurdle_ztnb"`, `"hurdle_ztpoisson"`, and
`"cumulative"`. If omitted,
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
installs the documented default signed hurdle-ZTNB process. `settlement`
defines the finite target horizon.

Do not use report-level
[`revision_process()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/revision_process.md)
for aggregate cumulative data. These streams cannot identify a separate
confirmation probability and conditional revision lag. Confirmed
revision records are therefore invalid; the informative changes are
signed updates, especially downward revisions.

## Custom components

Attach RTMB when defining or fitting user functions. Importing
`diseasenowcasting` alone is enough for built-ins, but not for method
dispatch inside a function defined in the global environment.

``` r

library(RTMB)

weibull_cdf <- function(theta) {
  shape <- exp(theta[1])
  scale <- exp(theta[2])
  function(d) 1 - exp(-(d / scale)^shape)
}

weibull_log_survival <- function(theta) {
  shape <- exp(theta[1])
  scale <- exp(theta[2])
  function(d) -(d / scale)^shape
}

dly <- custom_delay(
  cdf = weibull_cdf,
  log_survival = weibull_log_survival,
  priors = list(normal_prior(0, 1), normal_prior(log(7), 1)),
  param_names = c("log_shape", "log_scale"),
  inits = c(0, log(7)),
  name = "Weibull"
)

validate_custom_delay(dly)
```

Custom delay factories take `theta` and return a function of delay.
Supply a stable `log_survival` for heavy tails. A custom epidemic
function takes `theta` and returns the complete time x stratum log-mean
matrix, including its intercept. Use `infer_max_time(x)` when its
parameter dimension depends on the event grid, then run
[`validate_custom_epidemic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/validate_custom_epidemic.md).

Inside custom functions use RTMB-traceable arithmetic and fixed-length
loops. Avoid RNG, external solvers, branching on parameter values, and
[`pmax()`](https://rdrr.io/r/base/Extremes.html) /
[`pmin()`](https://rdrr.io/r/base/Extremes.html) on AD values. Prefer
vectorized operations. If index assignment is unavoidable, use the
documented `RTMB::ADoverload("[<-")` pattern.

Each `priors` entry is either a prior (free parameter) or a number
(fixed parameter). The lengths of priors, parameter names, and initial
values must agree. Custom delays fit one-stage. A custom epidemic can be
used in a one-stage fit or in the epidemic stage of a supported
two-stage delay fit.

## Backtest and select models

``` r

models <- list(
  ar1 = model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
  hsgp = model(nb_likelihood(), hsgp_epidemic(), dirichlet_delay())
)

bt <- backtest(
  x,
  models = models,
  n_dates = 8,
  type = "auto",
  n_draws = 1000,
  seed = 42
)

scores <- bt |>
  scoringutils::as_forecast_quantile() |>
  scoringutils::score()

autoplot(bt)
```

[`backtest()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/backtest.md)
is a convenience over
[`tbl.now::nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.html)
and returns its common result. Use unique model names when comparing
settings of the same component families. Historical covariates must
contain only information that was available at each origin. Set
`keep_draws = TRUE` only when sample-based scoring is needed.

Automatic selection backtests a candidate grid and refits the winner:

``` r

selected <- auto_nowcast(x, n_dates = 8, type = "auto", seed = 42)

best_model_name(selected)
best_model(selected)
comparison_scores(selected)
best_score(selected)
selection_metric(selected)
selection_timings(selected)
```

Tune candidate likelihoods, delay families, epidemic models, and
selection sample sizes explicitly for consequential analyses. Use
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
before the call if optional parallel execution is desired, then restore
the prior plan.

## Ensemble models

Fit named members to the same `tbl_now` data, then combine their common
`tbl_nowcast` results with `tbl.now`:

``` r

members <- list(
  ar1 = nowcast(x, models$ar1, type = "auto", n_draws = 2000, seed = 41),
  hsgp = nowcast(x, models$hsgp, type = "auto", n_draws = 2000, seed = 42)
)

equal_ensemble <- tbl.now::nowcast_ensemble(
  members,
  type = "quantile",
  weights = "equal"
)

weighted_ensemble <- tbl.now::nowcast_ensemble(
  members,
  type = "linear_pool",
  weights = "inverse_score",
  backtest = bt,
  n_draws = 4000,
  name = "weighted ensemble"
)

autoplot(weighted_ensemble)
tbl.now::score_nowcast(weighted_ensemble, truth = complete_data)
```

Quantile averaging works whenever every member has compatible quantiles.
Linear pooling samples member draws and therefore requires draws from
every member; native `diseasenowcasting` fits provide them. Learn
weights with `weights = "inverse_score"` or `"optim"` plus a matching
backtest, or call
[`tbl.now::nowcast_weights()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_weights.html)
explicitly. Learned weights exclude each member’s current `now` by
default; use `include_now = TRUE` only deliberately.

Only ensemble forecasts of the same epidemiological target, estimand,
event grid, strata, forecast origin, and quantile levels. Member names
must match the backtest model labels when learning weights. Because
`tbl_nowcast` is the common result type, an ensemble may also combine
compatible models from other `tbl.now` engines.

## Update, surprise, and censor

``` r

updated <- update(
  fit,
  new_data = new_rows,
  compute_surprise = TRUE,
  surprise_level = 0.99
)

extreme_values(updated)
```

[`update()`](https://rdrr.io/r/stats/update.html) merges through
`tbl.now::update()`, preserves the fitted temporal effect specification,
and warm-starts the refit. Its automatic surprise check currently
reports unusually long new reporting delays. Treat a flag as a prompt
for domain review, not permission to alter data automatically.

For a manual posterior-predictive check, use
[`surprise()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/surprise.md)
with `event_index` and `count` columns for counts, or `delay` and
optional `weight` for delays. If a date is known only as an upper bound,
use `tbl.now` censoring helpers and refit:

``` r

x2 <- tbl.now::censor_reporting_delays_above(x, max_delay = 45)
fit2 <- nowcast(x2, mdl)
```

## Save and load

``` r

save_nowcast(fit, "fit.rds")
restored <- load_nowcast("fit.rds")
predict(restored, n_draws = 500)

# Rebuild the RTMB tape without re-optimizing when needed
restored_live <- load_nowcast("fit.rds", rebuild = TRUE)
```

Do not use plain [`saveRDS()`](https://rdrr.io/r/base/readRDS.html) as
the persistence workflow for a live fit: RTMB tapes contain external
pointers.
[`save_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/save_nowcast.md)
stores the model, data, modes, and precision needed for prediction
without that unsafe pointer.

## Lower-level API

Use
[`infer_max_time()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/infer_max_time.md),
[`prepare_data()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/prepare_data.md),
[`default_priors()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/default_priors.md),
[`fit()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/fit.md),
[`summarise_nowcast_matrix()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/summarise_nowcast_matrix.md),
and
[`sample()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/sample.md)
only when implementing or studying the engine below
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md).
Low-level prepared arrays use different indexing and shape conventions
from user data; consult current documentation and the development skill
before using them.

For changes to package code, read `DEVELOPMENT_SKILL.md` before editing.
