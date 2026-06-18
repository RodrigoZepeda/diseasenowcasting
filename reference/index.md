# Package index

## All functions

- [`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md)
  : Automatically select and fit the best nowcasting model

- [`backtest()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/backtest.md)
  : Backtest one or more nowcast models across a set of as-of dates

- [`best_model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/best_model.md)
  :

  The winning
  [`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
  object from a nowcast

- [`best_model_name()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/best_model_name.md)
  :

  Name of the model chosen by
  [`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md)

- [`best_score()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/best_score.md)
  :

  The scoreboard row for the model
  [`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md)
  chose

- [`comparison_scores()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/comparison_scores.md)
  :

  The model-selection scoreboard from
  [`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md)

- [`covid_colombia`](https://rodrigozepeda.github.io/diseasenowcasting/reference/covid_colombia.md)
  : COVID-19 Notifications – Colombia 2020-2023

- [`custom_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/custom_delay.md)
  **\[experimental\]** : User-defined delay distribution

- [`custom_epidemic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/custom_epidemic.md)
  : User-defined epidemic process

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

- [`infer_max_time()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/infer_max_time.md)
  :

  Infer `max_time` for custom epidemic processes

- [`poisson_likelihood()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md)
  [`nb_likelihood()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md)
  : Likelihood for the Bayesian Nowcast

- [`load_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/load_nowcast.md)
  :

  Load a nowcast saved with
  [`save_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/save_nowcast.md)

- [`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
  : Bayesian Nowcast Model

- [`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
  : Fit a nowcast model to censored reporting data

- [`nowcast_diagnostic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast_diagnostic.md)
  : Three-panel diagnostic plot for a fitted nowcast

- [`nowcast_twostage()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast_twostage.md)
  : Two-stage multiple-imputation nowcast

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

- [`save_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/save_nowcast.md)
  : Save a fitted nowcast to disk

- [`score()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/score.md)
  : Score a backtest: WIS, APE, MSE per model (and rank them)

- [`selection_metric()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/selection_metric.md)
  :

  The metric
  [`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md)
  used to pick the winner

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

- [`validate_custom_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/validate_custom_delay.md)
  **\[experimental\]** : Validate a custom delay distribution for RTMB
  traceability

- [`validate_custom_epidemic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/validate_custom_epidemic.md)
  **\[experimental\]** : Validate a user-defined epidemic process for
  RTMB traceability
