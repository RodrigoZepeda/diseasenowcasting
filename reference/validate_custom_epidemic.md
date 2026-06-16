# Validate a user-defined epidemic process for RTMB traceability

Tapes `intensity_fn` through `RTMB::MakeADFun` at the supplied (or
default) initial values and confirms that the objective value and
gradient are both finite. Emits an informative error if the function is
not AD-safe.

## Usage

``` r
validate_custom_epidemic(epidemic, test_theta = NULL)
```

## Arguments

- epidemic:

  A `custom_epidemic_class` object from
  [`custom_epidemic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/custom_epidemic.md).

- test_theta:

  Optional numeric vector of length `n_params` to use as the test point.
  Defaults to `epidemic@inits`.

## Value

`epidemic`, invisibly. Emits a success message if the check passes.

## Examples

``` r
# Custom components tape USER functions, so RTMB must be attached
# (it is kept in Imports, not Depends, so attach it yourself):
library(RTMB)
max_time <- 15L
rw_fn <- function(theta)
  matrix(theta[1] + cumsum(exp(theta[2]) * theta[3:(2L + max_time)]), max_time, 1L)
custom_epi <- custom_epidemic(
  rw_fn,
  priors = c(list(normal_prior(2, 1), normal_prior(-2, 0.5)),
             rep(list(std_normal_prior()), max_time)),
  inits  = c(2, -2, rep(0, max_time))
)
validate_custom_epidemic(custom_epi)
#> ✔ Custom epidemic `Custom` passes RTMB traceability check.
```
