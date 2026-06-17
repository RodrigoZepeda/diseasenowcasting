# User-defined delay distribution

**\[experimental\]**

## Usage

``` r
custom_delay(
  cdf,
  log_cdf = function(theta) function(d) log(cdf(theta)(d)),
  log_survival = function(theta) function(d) log(1 - cdf(theta)(d)),
  priors = list(),
  name = "Custom",
  param_names = NULL,
  inits = NULL,
  num_delay_seasons = 1L,
  season_distribution = std_normal_prior()
)
```

## Arguments

- cdf:

  A function `cdf(theta)` returning a function of the delay `d` that
  evaluates the CDF `F(d)` (values in (0, 1)). This is the only required
  piece.

- log_cdf:

  Optional function `log_cdf(theta)` returning a function of `d` for
  `log F(d)`. Defaults to
  `function(theta) function(d) log(cdf(theta)(d))`.

- log_survival:

  Optional function `log_survival(theta)` returning a function of `d`
  for `log(1 - F(d))`. Defaults to
  `function(theta) function(d) log(1 - cdf(theta)(d))`. Supplying it
  explicitly is recommended for heavy-tailed delays, where the default
  `log(1 - F)` loses precision as `F -> 1` (e.g. for a Weibull,
  `log_survival = -(d / scale)^shape` is exact and stable).

- priors:

  A list with one element per parameter. Each element is either a
  `prior_class` object (free parameter, assigned that prior) or a
  length-1 numeric (parameter is fixed to that value and not estimated).
  The number of parameters is inferred from the length of this list (or
  from `param_names` / `inits` if `priors` is omitted).

- name:

  A display name for the distribution (used in print output).

- param_names:

  Optional character vector naming each parameter (used in diagnostics);
  its length sets the number of parameters if `priors` is empty.

- inits:

  Numeric vector of initial values for the parameters (on the
  unconstrained scale passed to the functions); its length sets the
  number of parameters if `priors` and `param_names` are empty. Defaults
  to zeros.

- num_delay_seasons:

  Number of periodic delay seasons. Default 1.

- season_distribution:

  Prior for the delay-season effects.

## Value

A `custom_delay_class` object (a `delay_process_class`).

## Details

Allows any reporting-delay distribution that can be expressed with
RTMB-traceable functions. You supply the cumulative distribution
function `cdf`; `log_cdf` and `log_survival` are optional and default to
the obvious transforms of `cdf`. Each is given as a function of the
parameter vector `theta` that **returns a function of the delay** `d`:

    cdf(theta)          -> function(d) returning F(d)
    log_cdf(theta)      -> function(d) returning log F(d)
    log_survival(theta) -> function(d) returning log(1 - F(d))

Inside the RTMB tape `theta` is an `advector`, so every operation must
be AD-traceable (see the *RTMB traceability* section).

## RTMB traceability

All operations inside `cdf` / `log_cdf` / `log_survival` must be
differentiable under RTMB's CppAD tape:

- Allowed: `+`, `-`, `*`, `/`, `exp`, `log`, `sqrt`, `pnorm`, `pgamma`,
  `lgamma`, `abs`, `sum`, scalar multiplication, fixed-length `for`
  loops (length must not depend on parameter values).

- Not allowed: `if`/`ifelse` on parameter values, `pmax`/`pmin` on AD
  types (use `(x + abs(x)) / 2` for `pmax(x, 0)`), external solvers,
  anything non-differentiable.

Use
[`validate_custom_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/validate_custom_delay.md)
to check your functions before fitting.

## See also

[`validate_custom_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/validate_custom_delay.md),
[delay_process](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md)

## Examples

``` r
# Weibull delay: theta = c(log_shape, log_scale).
# Only `cdf` is required; we also supply `log_survival` (exact in the tail).
weibull_cdf <- function(theta) {
  shape <- exp(theta[1]); scale <- exp(theta[2])
  function(d) 1 - exp(-(d / scale)^shape)
}
weibull_log_survival <- function(theta) {
  shape <- exp(theta[1]); scale <- exp(theta[2])
  function(d) -(d / scale)^shape
}
custom_delay(
  cdf          = weibull_cdf,
  log_survival = weibull_log_survival,
  priors       = list(normal_prior(0, 1), normal_prior(log(7), 1)),
  name         = "Weibull",
  param_names  = c("log_shape", "log_scale"),
  inits        = c(0, log(7))
)
#> <diseasenowcasting::custom_delay_class>
#>  @ name               : chr "Weibull"
#>  @ num_id             : int 5
#>  @ num_delay_seasons  : int 1
#>  @ season_distribution: <diseasenowcasting::prior_class>
#>  .. @ name       : chr "StdNormal"
#>  .. @ num_id     : int 0
#>  .. @ stan_params: num(0) 
#>  @ cdf_factory        : function (theta)  
#>  @ n_params           : int 2
#>  @ priors             :List of 2
#>  .. $ : <diseasenowcasting::prior_class>
#>  ..  ..@ name       : chr "Normal"
#>  ..  ..@ num_id     : int 1
#>  ..  ..@ stan_params: num [1:2] 0 1
#>  .. $ : <diseasenowcasting::prior_class>
#>  ..  ..@ name       : chr "Normal"
#>  ..  ..@ num_id     : int 1
#>  ..  ..@ stan_params: num [1:2] 1.95 1
#>  @ param_names        : chr [1:2] "log_shape" "log_scale"
#>  @ inits              : num [1:2] 0 1.95
```
