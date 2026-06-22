# 1.3.0

* Added `save_nowcast()` / `load_nowcast()` to persist a fitted `nowcast()` (or
  `auto_nowcast()`) to a single `.rds`. The RTMB autodiff tape cannot be
  serialized, so the bundle stores the `model()` spec, the input `tbl_now`, and
  each fit's parameters plus its Laplace mode and precision. A loaded nowcast
  works with `predict()` / `coef()` / `tidy()` / `autoplot()` straight away (any
  `n_draws`, no RTMB needed -- even for custom delays/epidemics), and can be
  re-fit from its `model` + bundled data. `load_nowcast(file, rebuild = TRUE)`
  also re-tapes the objective (no re-optimization) when a live tape is needed.
* Removed the `censor_delays_above()` function as it now lives in `tbl.now`.
  It remains available with the same signature because `tbl.now` is a dependency
  (attached whenever `diseasenowcasting` is).
* Improved documentation and badges with `lifecycle`. 
* Updated dependency on tbl.now to latest version (0.7.8)
* Removed `rlang` dependency
* Added `RTMBode` as a remote repository and to suggests. 
* The advanced ODE example in the *Custom Delays and Epidemic Processes* vignette
  now integrates the SIR system with the `RTMBode` solver instead of a
  hand-written RK4 scheme.

# 1.2.0

## Automatic model selection

* Added `auto_nowcast()`: give it a `tbl_now` and it builds a grid of candidate
  models (epidemic process x delay family) sized to how much data you have,
  backtests them over several dates, scores them, and refits the winner on the
  full data. The returned object is an ordinary `nowcast()` result with the
  ranked scoreboard in its `comparison` slot. You can
  - pass priors to the candidates (e.g. `sir = sir_epidemic(R0 = ...)`);
  - compare likelihoods (`likelihood = list(nb_likelihood(), poisson_likelihood())`);
  - add your own `custom_delay()`/`custom_epidemic()` models via `models = ...`;
  - select on `metric = "wis"` (default), `"ape"`, `"mse"`, or a calibration
    criterion: `"coverage_50"`, `"coverage_90"`, or `"coverage"` (both intervals'
    miss from nominal, summed).
* Accessors for an `auto_nowcast()` result: `best_model_name()` (the winning
  label), `best_model()` (the winning `model()` object, to reuse elsewhere),
  `comparison_scores()` (the ranked scoreboard), `best_score()` (the winner's
  row), and `selection_metric()` (the criterion used). Printing an
  `auto_nowcast()` result now also shows the top of the scoreboard.
* `nowcast()` / `backtest()` gain a `type = "auto"` option: the Dirichlet delay is
  fit one-stage and every other delay two-stage (the better choice for each in
  our experiments).
  
## Miscelaneous

* Updated `roxygen2` to 8.0.0
* Added the S7 `@` to `NAMESPACE`. 
* Improved test coverage. 
  
# 1.1.0  

## Custom (user-defined) components

* `diseasenowcasting` is now a **model-agnostic framework**: every nowcast is
  built from a likelihood, an epidemic process, and a reporting-delay
  distribution, and each can be a built-in *or* one you write yourself.
* Added **custom reporting-delay distributions** via `custom_delay()` (supply any
  RTMB-traceable CDF as `cdf`, with optional `log_cdf` / `log_survival`) with a
  `validate_custom_delay()` checker.
* Added **custom epidemic processes** via `custom_epidemic()` (supply any
  RTMB-traceable `intensity_fn(theta)` returning `log_mean[max_time x n_strata]`)
  with a `validate_custom_epidemic()` checker. Random walks, ODE/SIR models,
  regression surfaces, etc. all work.
* `custom_delay()` / `custom_epidemic()` no longer take an `n_params` argument —
  the number of parameters is inferred from `priors`, `param_names`, or `inits`.
* Added `infer_max_time()` to read off the number of event-times a model spans,
  for sizing a custom epidemic whose intensity function loops over time.
* New vignette *Custom Delays and Epidemic Processes* with worked examples on the
  `denguedat` and `mpoxdat` datasets, including an advanced RK4-integrated SIR ODE.

## Other changes

* `nowcast_diagnostic()` gains a `previous_times` argument (default 30) to limit
  the incidence and nowcast panels to the most recent event-times.
* Custom components require `library(RTMB)` to be attached; a clear error is
  raised otherwise.
* Removed the experimental `ppc()` posterior-predictive-check function.
* Internal modernisation (no change in results): non-standard evaluation now uses
  the rlang `.data` pronoun, backtesting uses `future.apply`.

#  1.0.0

* Changed the structure of the package from Stan to RTMB
* Added the gaussian process (HGSP) and SIR models
* Added the lognormal and generalized gamma delay
* Added surprise factors for extreme values
* Deprecated previous package