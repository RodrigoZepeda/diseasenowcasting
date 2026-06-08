# Package index

## All functions

- [`backtest()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/backtest.md)
  : Backtest one or more nowcast models across a set of as-of dates

- [`censor_delays_above()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/censor_delays_above.md)
  : Flag reports with an implausibly long delay as censored

- [`covid_colombia`](https://rodrigozepeda.github.io/diseasenowcasting/reference/covid_colombia.md)
  : COVID-19 Notifications – Colombia 2020-2023

- [`default_priors()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/default_priors.md)
  : Build the default prior bundle for an RTMB nowcast model

- [`lognormal_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md)
  [`gamma_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md)
  [`generalized_gamma_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md)
  [`dirichlet_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md)
  : Delay distribution for the Bayesian Nowcast

- [`dn_palette()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/dn_palette.md)
  : diseasenowcasting colour palette

- [`hsgp_epidemic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/epidemic_process.md)
  [`ar1_epidemic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/epidemic_process.md)
  [`sir_epidemic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/epidemic_process.md)
  : Epidemic process for the Bayesian Nowcast

- [`extreme_values()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/extreme_values.md)
  :

  Surprising (extreme) values flagged during the last
  [`update()`](https://rdrr.io/r/stats/update.html)

- [`fit()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/fit.md)
  : Fit a nowcast model with the RTMB engine

- [`fix_param()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/fix_param.md)
  : Hard-fix a parameter in a prior bundle (treat as data, drop from
  estimation)

- [`poisson_likelihood()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md)
  [`nb_likelihood()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md)
  : Likelihood for the Bayesian Nowcast

- [`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
  : Bayesian Nowcast Model

- [`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
  : Fit a nowcast model to censored reporting data

- [`nowcast_diagnostic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast_diagnostic.md)
  : Three-panel diagnostic plot for a fitted nowcast

- [`nowcast_twostage()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast_twostage.md)
  : Two-stage multiple-imputation nowcast

- [`ppc()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/ppc.md)
  : Posterior predictive check for a fitted nowcast

- [`prepare_data()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/prepare_data.md)
  : Prepare data for the RTMB nowcast engine

- [`std_normal_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  [`normal_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  [`cauchy_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  [`student_t_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  [`double_exponential_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  [`flat_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  [`positive_flat_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  [`half_std_normal_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  [`half_normal_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  [`half_cauchy_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  [`half_student_t_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  [`half_double_exponential_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  [`gamma_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  [`weibull_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  [`inv_gamma_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  [`lognormal_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  [`chi_square_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  [`exponential_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  [`logistic_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  [`beta_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  : Priors for model parameters

- [`sample()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/sample.md)
  :

  Draw random samples from a prior (or fall back to
  [`base::sample()`](https://rdrr.io/r/base/sample.html))

- [`score()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/score.md)
  : Score a backtest: WIS, APE, MSE per model (and rank them)

- [`summarise_nowcast_matrix()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/summarise_nowcast_matrix.md)
  : Quantile-table summary of a pooled nowcast draws matrix

- [`surprise()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/surprise.md)
  : Compute surprise scores for new observations

- [`surprise(`*`<list>`*`)`](https://rodrigozepeda.github.io/diseasenowcasting/reference/surprise.list.md)
  : Surprise score on a raw fit() result

- [`theme_diseasenowcasting()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/theme_diseasenowcasting.md)
  : ggplot2 theme matching the diseasenowcasting visual identity

- [`tidy()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/tidy.md)
  : Tidy parameter estimates from a fitted nowcast
