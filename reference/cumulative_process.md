# Count-cumulative observation process

Configures models for revision streams that publish cumulative levels.
The target is finite-horizon database retention `C_t(H)`, not biological
truth. The retraction mechanism is the collapsed kernel
`h_R(l) = retraction_mass * g_R(l)`; it does not separately identify a
truth probability and a conditional revision-delay law.

## Usage

``` r
cumulative_process(
  observation = c("hurdle_ztnb", "hurdle_ztpoisson", "cumulative"),
  retraction_delay = lognormal_delay(),
  settlement = 26L,
  retraction_mass = beta_prior(1.5, 20),
  movement_intercept = normal_prior(-1, 2),
  movement_age = normal_prior(0, 1),
  movement_previous = normal_prior(0, 1),
  magnitude_size = NULL
)
```

## Arguments

- observation:

  Observation composite likelihood. `"cumulative"` uses cumulative
  Poisson or negative-binomial marginals according to the model's
  [likelihood](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md).
  `"hurdle_ztnb"` uses signed hurdle updates with a
  zero-truncated-negative-binomial magnitude. `"hurdle_ztpoisson"` uses
  the corresponding zero-truncated-Poisson magnitude.

- retraction_delay:

  Parametric delay family for retraction ages `1:H`. Lognormal, gamma,
  and generalized gamma are supported.

- settlement:

  Positive integer settlement horizon `H`, in model steps.

- retraction_mass:

  Prior or fixed value in `[0, 1]` for the finite-horizon mass of `h_R`.
  A Beta prior is used by default.

- movement_intercept, movement_age, movement_previous:

  Priors or fixed values for the bounded movement-probability
  regression. Set `movement_previous = 0` to disable previous-movement
  dependence.

- magnitude_size:

  Positive prior or fixed value for the ZTNB magnitude size. It is used
  only by `"hurdle_ztnb"`.

## Value

A `cumulative_process_class` object for `model(cumulative = )`.

## Examples

``` r
cumulative_process()
#> Observation: Signed hurdle--ZTNB update composite
#> Settlement horizon: H = 26 model steps
#> Retraction kernel: h_R(l) = mass * LogNormal(l)
#> Retraction mass: mass ~ Beta( 1.5, 20.0)
#> Movement: intercept ~ Normal(-1, 2), age ~ Normal(0, 1), previous ~ Normal(0,
#> 1)
#> Magnitude size: size ~ LogNormal(0.0, 1.5)
cumulative_process(observation = "cumulative", settlement = 52L)
#> Observation: Cumulative-level composite
#> Settlement horizon: H = 52 model steps
#> Retraction kernel: h_R(l) = mass * LogNormal(l)
#> Retraction mass: mass ~ Beta( 1.5, 20.0)
cumulative_process(observation = "hurdle_ztpoisson", settlement = 6L)
#> Observation: Signed hurdle--ZTPoisson update composite
#> Settlement horizon: H = 6 model steps
#> Retraction kernel: h_R(l) = mass * LogNormal(l)
#> Retraction mass: mass ~ Beta( 1.5, 20.0)
#> Movement: intercept ~ Normal(-1, 2), age ~ Normal(0, 1), previous ~ Normal(0,
#> 1)
```
