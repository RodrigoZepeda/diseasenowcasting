# Delay distribution for the Bayesian Nowcast

Specify the reporting-delay distribution. Parameter slots accept a fixed
numeric, a `prior_class` (e.g.
[`normal_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)),
or `numeric(0)` for the default prior.

## Usage

``` r
lognormal_delay(
  mu = numeric(0),
  sigma = numeric(0),
  num_delay_seasons = 1L,
  season_distribution = std_normal_prior()
)

gamma_delay(
  shape = numeric(0),
  rate = numeric(0),
  num_delay_seasons = 1L,
  season_distribution = std_normal_prior()
)

generalized_gamma_delay(
  mu = numeric(0),
  sigma = numeric(0),
  Q = numeric(0),
  num_delay_seasons = 1L,
  season_distribution = std_normal_prior()
)

dirichlet_delay(alpha = numeric(0), bins = numeric(0))
```

## Arguments

- mu:

  Log-mean intercept (`delay_mu`).

- sigma:

  Log-scale / SD parameter \> 0.

- num_delay_seasons:

  Number of periodic delay seasons. Default 1.

- season_distribution:

  Prior for the delay-season effects.

- shape, rate:

  Gamma delay parameters (the `shape` slot is the log-mean, the `rate`
  slot the delay SD; see the original parameterisation).

- Q:

  GenGamma shape (`delay_Q`); `Q = 0` recovers lognormal.

- alpha:

  Dirichlet concentration (scalar broadcast to all bins).

- bins:

  Dirichlet: number of explicit delay bins (geometric tail beyond).

## Value

A `delay_process_class` object.

## Default priors

When a prior argument is left empty,
[`default_priors()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/default_priors.md)
supplies these defaults. The delay *mean* prior is **data-informed**: a
[`normal_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
on the log scale, centred at the log of the median observed delay.

- **LogNormal**: `mu` ~ data-informed `normal_prior(log median delay)`;
  `sigma` ~ `gamma_prior(2, 2)`.

- **Gamma**: `shape` ~ data-informed `normal_prior(...)`; `rate` ~
  `gamma_prior(2, 2 / sd)` (data-informed SD).

- **Generalised Gamma**: `mu` ~ data-informed `normal_prior(...)`;
  `sigma` ~ `gamma_prior(2, 0.1)`; `Q` (shape) ~ `normal_prior(0, 0.5)`.

- **Dirichlet**: `alpha` ~ a per-bin concentration vector, data-informed
  from the empirical delay pmf (`0.05 + (bins+1) * pmf`), else
  `rep(1, bins + 1)`.

## Examples

``` r
lognormal_delay()
#> LogNormal(mu, sigma)
lognormal_delay(mu = normal_prior(log(7), 0.5))
#> LogNormal(mu ~ Normal(1.946, 0.500), sigma)
gamma_delay()
#> Gamma(shape, rate)
generalized_gamma_delay(Q = 1)
#> GeneralizedGamma(mu, sigma, Q = "1")
dirichlet_delay(alpha = 1, bins = 21)
#> Dirichlet(alpha = "1"; bins = "21")
```
