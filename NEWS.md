# 1.1.0

## Custom (user-defined) components

* `diseasenowcasting` is now a **model-agnostic framework**: every nowcast is
  built from a likelihood, an epidemic process, and a reporting-delay
  distribution, and each can be a built-in *or* one you write yourself.
* Added **custom reporting-delay distributions** via `custom_delay()` (supply any
  RTMB-traceable CDF factory) with a `validate_custom_delay()` checker.
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