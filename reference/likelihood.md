# Likelihood for the Bayesian Nowcast

Count observation model for the (truncation-corrected) case counts.

## Usage

``` r
poisson_likelihood(mu = numeric(0))

nb_likelihood(mu = numeric(0), phi = lognormal_prior(log(20), 0.5))
```

## Arguments

- mu:

  Log-scale mean intercept prior (or fixed numeric).

- phi:

  Negative-binomial overdispersion prior (or fixed numeric); NB only.
  Defaults to `lognormal_prior(log(20), 0.5)`. This is the *only* place
  to set the overdispersion prior —
  [`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
  reads it from the model and does not accept its own `phi` argument.

## Value

A `likelihood_class` object.

## Examples

``` r
poisson_likelihood()
#> Poisson(mu)
nb_likelihood()
#> NegBin(mu, phi ~ LogNormal(2.996, 0.500))
# Wider overdispersion (heavier-tailed counts) -- set via the likelihood:
nb_likelihood(phi = lognormal_prior(log(5), 0.5))
#> NegBin(mu, phi ~ LogNormal(1.609, 0.500))
```
