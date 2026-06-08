# Bayesian Nowcast Model

Combines a likelihood, an epidemic process, and a delay distribution
into a model object. Arguments are positional: the first is the
likelihood, the second the epidemic process, the third the delay. Any
argument can be omitted to use its default.

## Usage

``` r
model(
  likelihood = nb_likelihood(),
  epidemic = hsgp_epidemic(),
  delay = lognormal_delay(),
  covariate_prior = std_normal_prior(),
  strata_pooling = "independent"
)
```

## Arguments

- likelihood:

  A `likelihood_class`
  ([`poisson_likelihood()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md)
  /
  [`nb_likelihood()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md)).
  Default:
  [`nb_likelihood()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md).

- epidemic:

  An `epidemic_process_class`. Default:
  [`hsgp_epidemic()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/epidemic_process.md).

- delay:

  A `delay_process_class`. Default:
  [`lognormal_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md).

- covariate_prior:

  A `prior_class` applied to all covariate coefficients. Default:
  [`std_normal_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md).

- strata_pooling:

  `"independent"` (default) fits fully separate intercepts per stratum.
  `"hierarchical"` pools intercepts via a shared mean and a half-normal
  prior on the between-stratum SD: \\\mu_0^{(s)} =
  \mu\_{\text{global}} + \tau \cdot \delta^{(s)}\\, \\\delta^{(s)} \sim
  \mathcal{N}(0,1)\\, \\\tau \sim \text{HalfNormal}(0,1)\\. Only
  relevant when `num_strata > 1`.

## Value

A `model_class` object.

## Examples

``` r
model()
#> 
#> ── Bayesian Nowcast Model ──────────────────────────────────────────────────────
#> 
#> ── Likelihood 
#> NegBin(mu, phi ~ LogNormal(2.996, 0.500))
#> 
#> ── Epidemic process 
#> HSGP(alpha, ell ; kernel = "matern32", num_basis = "auto", tmax = "auto")
#> 
#> ── Delay process 
#> LogNormal(mu, sigma)
#> 
#> ── Covariate prior 
#> StdNormal()
#> Strata pooling: "independent"
#> ────────────────────────────────────────────────────────────────────────────────
model(poisson_likelihood(), hsgp_epidemic(gp_kernel = "matern52"))
#> 
#> ── Bayesian Nowcast Model ──────────────────────────────────────────────────────
#> 
#> ── Likelihood 
#> Poisson(mu)
#> 
#> ── Epidemic process 
#> HSGP(alpha, ell ; kernel = "matern52", num_basis = "auto", tmax = "auto")
#> 
#> ── Delay process 
#> LogNormal(mu, sigma)
#> 
#> ── Covariate prior 
#> StdNormal()
#> Strata pooling: "independent"
#> ────────────────────────────────────────────────────────────────────────────────
model(nb_likelihood(), ar1_epidemic(), lognormal_delay())
#> 
#> ── Bayesian Nowcast Model ──────────────────────────────────────────────────────
#> 
#> ── Likelihood 
#> NegBin(mu, phi ~ LogNormal(2.996, 0.500))
#> 
#> ── Epidemic process 
#> AR(1)(phi, sigma | error)
#> 
#> ── Delay process 
#> LogNormal(mu, sigma)
#> 
#> ── Covariate prior 
#> StdNormal()
#> Strata pooling: "independent"
#> ────────────────────────────────────────────────────────────────────────────────
model(nb_likelihood(), hsgp_epidemic(), lognormal_delay(),
      strata_pooling = "hierarchical")
#> 
#> ── Bayesian Nowcast Model ──────────────────────────────────────────────────────
#> 
#> ── Likelihood 
#> NegBin(mu, phi ~ LogNormal(2.996, 0.500))
#> 
#> ── Epidemic process 
#> HSGP(alpha, ell ; kernel = "matern32", num_basis = "auto", tmax = "auto")
#> 
#> ── Delay process 
#> LogNormal(mu, sigma)
#> 
#> ── Covariate prior 
#> StdNormal()
#> Strata pooling: "hierarchical"
#> ────────────────────────────────────────────────────────────────────────────────
```
