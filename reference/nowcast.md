# Fit a nowcast model to censored reporting data

The main entry point. Takes a `tbl_now` (from the tbl.now package) and a
[`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md),
fits the latent-epidemic + reporting-delay model as of a given date, and
returns a `nowcast_class` object. Fitting only – the
posterior-predictive nowcast is produced lazily by
[`predict()`](https://rdrr.io/r/stats/predict.html); the latent
incidence by
[`mean()`](https://rdrr.io/r/base/mean.html)/[`median()`](https://rdrr.io/r/stats/median.html)/[`quantile()`](https://rdrr.io/r/stats/quantile.html);
the parameter estimates by
[`coef()`](https://rdrr.io/r/stats/coef.html).

## Usage

``` r
nowcast(
  data,
  model = diseasenowcasting::model(),
  type = c("two_stage", "one_stage", "auto"),
  now = NULL,
  K = 25L,
  n_draws = 2000L,
  delay_window = 120L,
  np_spread = 1,
  floor_mu = 0.15,
  floor_sig_frac = 0.25,
  temporal_effects = "auto",
  prior_only = FALSE,
  seed = sample.int(.Machine$integer.max, 1),
  ...
)
```

## Arguments

- data:

  A `tbl_now` object
  ([`tbl.now::tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)).

- model:

  A
  [`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
  object. Default:
  [`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
  (NB + HSGP + Dirichlet).

- type:

  `"two_stage"` (default; delay-imputation pooling), `"one_stage"` (a
  single joint fit), or `"auto"` (per delay: dirichlet one-stage, all
  other delays two-stage – the better choice for each in our
  experiments).

- now:

  As-of date; only events/reports up to `now` are used. Default:
  `tbl.now::get_now(data)`, falling back to the latest report date.

- K:

  Number of delay imputations for the two-stage path.

- n_draws:

  Default number of posterior draws used by
  [`predict()`](https://rdrr.io/r/stats/predict.html) and the
  latent-incidence summaries.

- delay_window:

  Recent window length for the parametric Stage-1 delay fit.

- np_spread:

  Dirichlet simplex imputation covariance inflation (default 1).

- floor_mu, floor_sig_frac:

  Imputation-spread floors (parametric families).

- temporal_effects:

  Controls automatic seasonal / day-of-week covariates. `"auto"`
  (default) adds sensible effects based on the data's time unit (weekly
  -\> 52-period seasonality; daily -\> day-of-week + 52-period
  seasonality; monthly -\> 12-period seasonality) **only if the
  `tbl_now` does not already carry computed temporal effects**. Use
  `"none"` (or `"None"`) to disable, or pre-attach your own effects to
  the `tbl_now` with
  [`tbl.now::add_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.html) +
  [`tbl.now::compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/compute_temporal_effects.html).

- prior_only:

  If `TRUE`, ignore the likelihood and draw the epidemic parameters from
  their **priors** only, returning the prior-predictive latent
  incidence. Useful for understanding what a prior implies *before*
  seeing data (e.g. how the SIR `R0` prior or the AR(1) `phi` prior
  reshapes the epidemic). The result is a normal `nowcast_class`, so
  [`predict()`](https://rdrr.io/r/stats/predict.html) /
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  / [`median()`](https://rdrr.io/r/stats/median.html) /
  [`quantile()`](https://rdrr.io/r/stats/quantile.html) all work; `data`
  only supplies the time grid. Default `FALSE`.

- seed:

  Optional RNG seed (imputation draws).

- ...:

  Passed to
  [`prepare_data()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/prepare_data.md)
  (e.g. `gp_boundary_frac`).

## Value

A `nowcast_class` object.

## Overdispersion (`phi`)

The negative-binomial overdispersion prior is **not** an argument of
`nowcast()`. Set it on the likelihood instead, e.g.
`model(nb_likelihood(phi = lognormal_prior(log(5), 0.5)), ...)`. The
default
[`nb_likelihood()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md)
already uses `lognormal_prior(log(20), 0.5)`.

## Examples

``` r
if (requireNamespace("tbl.now", quietly = TRUE)) {
  # data <- tbl.now::tbl_now(my_linelist, event_date = onset, report_date = reported)
  # nc <- nowcast(data, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()))
  # predict(nc); median(nc); coef(nc)
}
#> NULL
```
