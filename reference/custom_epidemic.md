# User-defined epidemic process

Lets you supply any RTMB-traceable function `intensity_fn(theta)` that
returns the log expected-incidence trajectory
`log_mean[n_time x n_strata]` as the latent epidemic process for the
nowcast. This makes the framework epidemic-process agnostic: random
walks, ODE models, regression surfaces, and anything else that can be
written in AD-safe arithmetic are all valid.

## Usage

``` r
custom_epidemic(
  intensity_fn,
  priors = list(),
  name = "Custom",
  param_names = character(0),
  inits = numeric(0)
)
```

## Arguments

- intensity_fn:

  A function `function(theta)` that takes a numeric parameter vector and
  returns a numeric matrix of dimensions `[n_time x n_strata]`
  containing log expected incidence (the full `log_mean`, not just a
  trend — include any intercept inside the function). Must use only
  RTMB-traceable operations: `+`, `-`, `*`, `/`, `exp`, `log`, `sqrt`,
  `abs`, `sum`, `for` loops of *fixed* length (not data-dependent).
  Never use `if`/`ifelse` on parameter values, `pmax`/`pmin` on AD
  types, or external solvers. Call
  [`validate_custom_epidemic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/validate_custom_epidemic.md)
  to check traceability before fitting.

- priors:

  A list with one element per parameter in `theta`. Each element is
  either a `prior_class` object such as
  [`normal_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  (free parameter, estimated) or a single numeric scalar (fixed
  parameter, held constant during optimisation). The number of
  parameters is inferred from the length of this list (or from
  `param_names` / `inits` if `priors` is omitted). For time-varying
  processes (e.g. a random walk with one innovation per event-time), get
  the number of event-times with
  [`infer_max_time()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/infer_max_time.md)
  first.

- name:

  Character label shown in print and diagnostic output.

- param_names:

  Character vector naming the parameters; its length sets the number of
  parameters if `priors` is empty. Defaults to `"theta1"`, …

- inits:

  Numeric vector of starting values; its length sets the number of
  parameters if `priors` and `param_names` are empty. Defaults to zeros.

## Value

A `custom_epidemic_class` object (subclass of `epidemic_process_class`).

## Details

**\[experimental\]**

## See also

[`validate_custom_epidemic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/validate_custom_epidemic.md),
[epidemic_process](https://rodrigozepeda.github.io/diseasenowcasting/reference/epidemic_process.md)

## Examples

``` r
# Pure random walk on log-incidence (max_time = 20)
# Uses cumsum() — no [<- assignment needed, so fully AD-safe.
max_time <- 20L
rw_fn <- function(theta) {
  log_mu0  <- theta[1]
  sigma_rw <- exp(theta[2])
  eps      <- theta[3:(2L + max_time)]
  lm       <- log_mu0 + cumsum(sigma_rw * eps)
  matrix(lm, max_time, 1L)
}
# n_params is inferred (here 2 + max_time) from the priors list:
custom_epi <- custom_epidemic(
  rw_fn,
  priors   = c(list(normal_prior(2, 1), normal_prior(-2, 0.5)),
               rep(list(std_normal_prior()), max_time)),
  name     = "RandomWalk",
  param_names = c("log_mu0", "log_sigma", paste0("eps_", seq_len(max_time))),
  inits    = c(2, -2, rep(0, max_time))
)
```
