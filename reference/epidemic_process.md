# Epidemic process for the Bayesian Nowcast

Specify the latent epidemic process. Parameter slots accept a fixed
numeric, a `prior_class`, or `numeric(0)` for the default prior. The
log-incidence mean intercept is inherited from the likelihood (`mu`).

## Usage

``` r
hsgp_epidemic(
  alpha = numeric(0),
  ell = numeric(0),
  gp_kernel = "matern32",
  gp_basis = "dirichlet",
  num_basis = 0,
  tmax_model = 0
)

ar1_epidemic(phi = numeric(0), sigma = numeric(0), error = numeric(0))

sir_epidemic(
  R0 = numeric(0),
  gamma = numeric(0),
  N_eff = numeric(0),
  N_pop = 10000,
  use_beta_rw_trend = TRUE
)
```

## Arguments

- alpha:

  HSGP GP amplitude prior (\> 0).

- ell:

  HSGP GP length-scale prior (\> 0).

- gp_kernel:

  HSGP kernel: `"sq_exp"`, `"matern32"` (default), `"matern52"`.

- gp_basis:

  HSGP eigenbasis: `"dirichlet"`/`"sine"` (default) or
  `"neumann"`/`"cosine"`.

- num_basis:

  HSGP basis count; `numeric(0)`/`0` = auto from series length.

- tmax_model:

  HSGP time normalisation; `0` = auto (newest point at the right
  boundary).

- phi:

  AR(1) autocorrelation prior in (-1, 1).

- sigma:

  AR(1) innovation SD prior (\> 0).

- error:

  AR(1) standardised innovation prior.

- R0:

  SIR basic reproduction number prior (\> 0).

- gamma:

  SIR recovery rate prior in (0, 1).

- N_eff:

  SIR effective susceptible fraction prior in (0, 1).

- N_pop:

  SIR total population (default 10000).

- use_beta_rw_trend:

  SIR: beta follows an AR(1) walk if TRUE (default).

## Value

An `epidemic_process_class` object.

## Default priors

When a prior argument is left empty,
[`default_priors()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/default_priors.md)
supplies these defaults (see also [nowcast(prior_only =
TRUE)](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
to visualise them):

**HSGP** (`hsgp_epidemic`):

- `alpha` (GP amplitude): `half_normal_prior(0, 1)`

- `ell` (GP length-scale): `inv_gamma_prior(3, 1)`

**AR(1)** (`ar1_epidemic`):

- `phi` (autocorrelation):
  [`std_normal_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)

- `sigma` (innovation SD): `exponential_prior(100)`

- innovations:
  [`std_normal_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)

**SIR** (`sir_epidemic`):

- `R0`: `lognormal_prior(log(2), 0.5)`

- `gamma` (recovery rate): `lognormal_prior(log(1/5), 0.5)`

- `N_eff` (susceptible fraction): `beta_prior(2, 5)`

The log-incidence intercept comes from the likelihood (`mu`), defaulting
to a data-informed
[`normal_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
centred at the log median daily count.

## Examples

``` r
hsgp_epidemic()
#> HSGP(alpha, ell ; kernel = "matern32", num_basis = "auto", tmax = "auto")
hsgp_epidemic(gp_kernel = "sq_exp", num_basis = 20)
#> HSGP(alpha, ell ; kernel = "sq_exp", num_basis = "20", tmax = "auto")
ar1_epidemic(phi = 0.9)
#> AR(1)(phi = "0.9", sigma | error)
sir_epidemic(R0 = 2.5, use_beta_rw_trend = FALSE)
#> SIR(R0 = "2.5", gamma, N_eff ; N_pop = "10,000", beta_rw = "FALSE")
```
