# Build the default prior bundle for an RTMB nowcast model

Build the default prior bundle for an RTMB nowcast model

## Usage

``` r
default_priors(mod, data = NULL, ...)
```

## Arguments

- mod:

  A
  [`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
  object.

- data:

  Optional prepared-data list from
  [`prepare_data()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/prepare_data.md)
  (used for the data-informed location/scale defaults). May also be a
  bare list with an `m` matrix.

- ...:

  Per-key overrides (e.g. `phi = lognormal_prior(log(20), 0.5)`,
  `delay_mu = normal_prior(log(5), 0.3)`).

## Value

A named list of prior specs.

## Details

The default prior for each parameter is documented on the component
constructor: see the **Default priors** section of
[epidemic_process](https://rodrigozepeda.github.io/diseasenowcasting/reference/epidemic_process.md)
(HSGP / AR(1) / SIR),
[delay_process](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md)
(LogNormal / Gamma / GenGamma / Dirichlet) and
[likelihood](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md)
(the NB overdispersion `phi`). To *see* what a set of priors implies for
the epidemic curve before fitting, use [nowcast(prior_only =
TRUE)](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md).

## See also

[epidemic_process](https://rodrigozepeda.github.io/diseasenowcasting/reference/epidemic_process.md),
[delay_process](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md),
[likelihood](https://rodrigozepeda.github.io/diseasenowcasting/reference/likelihood.md)
