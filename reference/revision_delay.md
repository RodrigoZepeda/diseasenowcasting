# Revision-lag distributions

The distribution `g_C` of the **revision lag**: how long after a case is
reported its result comes back. Pass one to
[`revision_process()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/revision_process.md)
as `revision_delay`.

## Usage

``` r
lognormal_revision(mu = numeric(0), sigma = numeric(0))

gamma_revision(shape = numeric(0), rate = numeric(0))

generalized_gamma_revision(mu = numeric(0), sigma = numeric(0), Q = numeric(0))

dirichlet_revision(alpha = numeric(0), bins = numeric(0))
```

## Arguments

- mu:

  Log-mean intercept (`delay_mu`).

- sigma:

  Log-scale / SD parameter \> 0.

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

A `delay_process_class` object, for the `revision_delay` slot of
[`revision_process()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/revision_process.md).

## Details

These are aliases of the corresponding
[delay_process](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md)
constructors – a revision lag is an ordinary non-negative delay, only
measured from the *report* rather than from the event – so the
parameters, priors and **\[experimental\]** behaviour are identical. The
one difference is the support, and it depends on the mode: under
`retraction_only` the lag lives on `{1, 2, ...}` (a retraction lands
strictly after the report it withdraws, so a case retracted in the same
period is dropped by
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
– it was never visible in any data vintage), while under
`confirmation_only` and `both` it lives on `{0, 1, ...}`, since a test
can come back the day it was ordered.

## Which one to use

`dirichlet_revision()` is the safest default when the counts are large.
The correction applied to a pending report of age `j` is
`rho(j) = p / (p + (1 - p) * (1 - G_C(j)))`, so at high counts a *shape*
error in `g_C` biases the nowcast by more than its Monte-Carlo noise: on
a COVID series of ~8000 cases/day a lognormal `g_C` fitted to a
`1 + Poisson(2)` lag left a ~0.9% bias and lost nominal coverage, while
the Dirichlet recovered `rho` to four decimals. At low counts the
parametric families are fine and estimate fewer parameters.

## See also

[`revision_process()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/revision_process.md),
[delay_process](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md),
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)

## Examples

``` r
revision_process(lognormal_revision())
#> <diseasenowcasting::revision_process_class>
#>  @ revision_delay: <diseasenowcasting::lognormal_delay_class>
#>  .. @ name               : chr "LogNormal"
#>  .. @ num_id             : int 1
#>  .. @ num_delay_seasons  : int 1
#>  .. @ season_distribution: <diseasenowcasting::prior_class>
#>  .. .. @ name       : chr "StdNormal"
#>  .. .. @ num_id     : int 0
#>  .. .. @ stan_params: num(0) 
#>  .. @ mu                 : num(0) 
#>  .. @ sigma              : num(0) 
#>  @ p             : num(0) 
#>  @ stratified_p  : logi FALSE
#>  @ mode          : chr "auto"
#>  @ active        : logi TRUE
revision_process(dirichlet_revision(bins = 10))
#> <diseasenowcasting::revision_process_class>
#>  @ revision_delay: <diseasenowcasting::dirichlet_delay_class>
#>  .. @ name               : chr "Dirichlet"
#>  .. @ num_id             : int 4
#>  .. @ num_delay_seasons  : num 1
#>  .. @ season_distribution: <diseasenowcasting::prior_class>
#>  .. .. @ name       : chr "StdNormal"
#>  .. .. @ num_id     : int 0
#>  .. .. @ stan_params: num(0) 
#>  .. @ alpha              : num(0) 
#>  .. @ bins               : int 10
#>  @ p             : num(0) 
#>  @ stratified_p  : logi FALSE
#>  @ mode          : chr "auto"
#>  @ active        : logi TRUE

# Held-fixed revision lag, e.g. from an external study
revision_process(gamma_revision(shape = log(3), rate = 2))
#> <diseasenowcasting::revision_process_class>
#>  @ revision_delay: <diseasenowcasting::gamma_delay_class>
#>  .. @ name               : chr "Gamma"
#>  .. @ num_id             : int 2
#>  .. @ num_delay_seasons  : int 1
#>  .. @ season_distribution: <diseasenowcasting::prior_class>
#>  .. .. @ name       : chr "StdNormal"
#>  .. .. @ num_id     : int 0
#>  .. .. @ stan_params: num(0) 
#>  .. @ shape              : num 1.1
#>  .. @ rate               : num 2
#>  @ p             : num(0) 
#>  @ stratified_p  : logi FALSE
#>  @ mode          : chr "auto"
#>  @ active        : logi TRUE
```
