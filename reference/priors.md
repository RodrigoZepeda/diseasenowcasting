# Priors for model parameters

Specify a prior distribution for any estimated parameter. The result is
a `prior_class` object that can be passed to any parameter slot in
[`poisson_likelihood()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md),
[`nb_likelihood()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md),
the delay process constructors, or the epidemic process constructors.

## Usage

``` r
std_normal_prior()

normal_prior(mean, sd)

cauchy_prior(location, scale)

student_t_prior(df, location, scale)

double_exponential_prior(mean, sd)

flat_prior()

positive_flat_prior()

half_std_normal_prior()

half_normal_prior(location, scale)

half_cauchy_prior(location, scale)

half_student_t_prior(df, scale)

half_double_exponential_prior(mean, sd)

gamma_prior(shape, rate)

weibull_prior(shape, scale)

inv_gamma_prior(shape, scale)

lognormal_prior(meanlog, sdlog)

chi_square_prior(df)

exponential_prior(rate)

logistic_prior(location, scale)

beta_prior(alpha, beta)
```

## Arguments

- mean, location:

  Location parameter.

- sd, scale:

  Scale parameter (must be \> 0).

- df:

  Degrees of freedom (must be \> 0).

- shape:

  Shape parameter (must be \> 0).

- rate:

  Rate parameter (must be \> 0).

- meanlog:

  Mean on the log scale (lognormal).

- sdlog:

  SD on the log scale (lognormal, must be \> 0).

- alpha, beta:

  Beta distribution shape parameters (must be \> 0).

## Value

A `prior_class` object.

## Examples

``` r
normal_prior(0, 1)
#> Normal(0, 1)
gamma_prior(2, 0.1)
#> Gamma(2.0, 0.1)
exponential_prior(1)
#> Exponential(1)
flat_prior()
#> Flat()
```
