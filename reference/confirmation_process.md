# Confirmation / retraction process

Describes reports that are later retracted (count-cumulative streams
that revise downward as well as upward). Attach it to a
[`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
via the `confirmation` argument;
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
then uses the signed-increment Skellam / SkNB likelihood when the data
are count-cumulative.

## Usage

``` r
confirmation_process(retract_delay = lognormal_delay(), p = numeric(0))
```

## Arguments

- retract_delay:

  A `delay_process_class` (e.g.
  [`lognormal_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md),
  [`gamma_delay()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md))
  describing the retraction delay `g_C` (onset of a report to its
  retraction). Default a geometric-like short lognormal.

- p:

  The confirmation probability (probability that a report is genuine and
  never retracted), specified the same way as a delay parameter: either
  a `prior_class` (estimated with that prior) or a single numeric in
  `(0, 1]` (held fixed). **Left unset (the default)**,
  [`default_priors()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/default_priors.md)
  builds a *data-informed, strongly-concentrated* Beta prior centred at
  the empirical retraction rate – exactly as the lognormal delay's `mu`
  default is data-informed. The strong prior is deliberate: retractions
  are empirically rare (~1-2% of reports), and with a weak prior the
  Skellam variance abuses the retraction stream as an overdispersion
  knob and `p` collapses. Pass a fixed value (e.g. `p = 0.98`) to hold
  it constant, or your own
  [`beta_prior()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/priors.md)
  to estimate it under a prior of your choosing.

## Value

A `confirmation_process_class` object.

## Details

**Default priors.** `retract_delay` inherits the default priors of its
delay family (see
[delay_process](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md));
`p` gets a strongly-concentrated Beta centred at the empirical
retraction rate (see
[`default_priors()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/default_priors.md)).
At `p = 1` the confirmation layer is inert and the model is the ordinary
count model.

## See also

[`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md),
[delay_process](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md),
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)

## Examples

``` r
# A confirmation process with a lognormal retraction delay
confirmation_process(retract_delay = lognormal_delay())
#> <diseasenowcasting::confirmation_process_class>
#>  @ retract_delay: <diseasenowcasting::lognormal_delay_class>
#>  .. @ name               : chr "LogNormal"
#>  .. @ num_id             : int 1
#>  .. @ num_delay_seasons  : int 1
#>  .. @ season_distribution: <diseasenowcasting::prior_class>
#>  .. .. @ name       : chr "StdNormal"
#>  .. .. @ num_id     : int 0
#>  .. .. @ stan_params: num(0) 
#>  .. @ mu                 : num(0) 
#>  .. @ sigma              : num(0) 
#>  @ p            : num(0) 
#>  @ active       : logi TRUE

# Attach to a model for a count-cumulative stream
model(nb_likelihood(), ar1_epidemic(), lognormal_delay(),
      confirmation = confirmation_process())
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
```
