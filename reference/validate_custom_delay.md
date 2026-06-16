# Validate a custom delay distribution for RTMB traceability

Test-tapes the user-supplied `cdf` / `log_cdf` / `log_survival` on a
dummy numeric vector, checking that `obj$fn()` and `obj$gr()` are
finite. Emits a clear error (including common causes) on failure.

## Usage

``` r
validate_custom_delay(delay, test_theta = NULL, test_delays = 1:14)
```

## Arguments

- delay:

  A `custom_delay_class` object from
  [`custom_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/custom_delay.md).

- test_theta:

  Optional numeric vector of length `n_params` to use as the test point.
  Defaults to the `inits` slot.

- test_delays:

  Integer vector of delay values to evaluate (default 1:14).

## Value

Invisibly returns `TRUE` on success.

## Examples

``` r
# Custom components tape USER functions, so RTMB must be attached
# (it is kept in Imports, not Depends, so attach it yourself):
library(RTMB)
weibull_cdf <- function(theta) {
  shape <- exp(theta[1]); scale <- exp(theta[2])
  function(d) 1 - exp(-(d / scale)^shape)
}
dly <- custom_delay(weibull_cdf,
                    priors = list(normal_prior(0, 1), normal_prior(log(7), 1)),
                    name = "Weibull", inits = c(0, log(7)))
validate_custom_delay(dly)
#> ✔ Custom delay `Weibull` passes RTMB traceability check.
```
