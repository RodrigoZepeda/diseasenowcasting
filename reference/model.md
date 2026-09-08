# Bayesian Nowcast Model

Combines a likelihood, an epidemic process, a reporting-delay
distribution, and an optional revision process into a model object.
Arguments are positional: likelihood, epidemic process, reporting delay,
then revision process. Any argument can be omitted by naming the later
components.

## Usage

``` r
model(
  likelihood = nb_likelihood(),
  epidemic = hsgp_epidemic(),
  delay = lognormal_delay(),
  revision = no_revision(),
  covariate_prior = std_normal_prior(),
  strata_pooling = "independent",
  cumulative = no_cumulative()
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

- revision:

  A `revision_process_class`
  ([`revision_process()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/revision_process.md))
  for report-level confirmation/retraction outcomes in linelist or
  count-incidence data. Default: inert. Count-cumulative revisions use
  the separate `cumulative` component.

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

- cumulative:

  Dedicated count-cumulative observation configuration from
  [`cumulative_process()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/cumulative_process.md).
  It is inert by default for linelist and count-incidence data;
  count-cumulative data use the hurdle–ZTNB default unless configured
  explicitly.

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
model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
      revision_process())
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
#> ── Revision process 
#> Revision(p)
#> Shared revision delay: LogNormal
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
