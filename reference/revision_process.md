# Revision process: reports that are later confirmed or retracted

A report is rarely a case outright. It is provisional, and **resolved
exactly once**: a test comes back, and the report is either *confirmed*
(a real case) or *retracted* (removed from the register). A revision
process models that second step, so the nowcast targets the settled
count rather than the raw report count.

## Usage

``` r
revision_process(
  revision_delay = lognormal_delay(),
  p = numeric(0),
  stratified_p = FALSE,
  mode = c("auto", "confirmation_only", "retraction_only", "both")
)
```

## Arguments

- revision_delay:

  A `delay_process_class` describing the revision lag `g_C` (report to
  result). Any delay family works; the `*_revision()` constructors
  ([`lognormal_revision()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/revision_delay.md)
  and friends) are aliases that read more naturally in this slot.
  Default a short lognormal.

- p:

  Probability that a report resolves **positive** – that it is a real
  case and is never retracted. Either a `prior_class` (estimated under
  that prior) or a single numeric in `(0, 1]` (held fixed). **Left unset
  (the default)** the behaviour depends on the data, because the two
  cases identify `p` through the report-level cure block:

  - **linelist / count-incidence** – a weak data-informed Beta. The cure
    block pins `p` directly: a report standing unresolved for a long
    time is evidence about the cure fraction, so the likelihood is
    informative and the prior only has to keep `p` on the interval.

- stratified_p:

  If `TRUE`, estimate a **separate** `p` per stratum instead of one
  shared value. Only meaningful for per-report revision data with more
  than one stratum; the revision lag `g_C` stays shared either way (it
  is usually a property of the verification workflow, whereas `p`
  reflects how often a given group is misclassified). Each stratum's `p`
  gets the same prior. Default `FALSE` – with sparse strata the shared
  `p` is safer.

- mode:

  Which outcomes the data record. `"auto"` (the default) infers it from
  the `revision_type` column; the others assert it. See **Modes**.

## Value

A `revision_process_class` object, for `model(revision = )`.

## Details

Attach it with `model(revision = revision_process(...))`.
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
switches it on automatically when the data carry it – see **Detection**
below.

## Modes

What differs between surveillance systems is only which resolutions get
recorded, and all three possibilities share one likelihood:

|  |  |  |  |
|----|----|----|----|
|  | `retraction_only` | `confirmation_only` | `both` |
| outcomes recorded | the negatives | the positives | both signs |
| a missing outcome means | not retracted **yet** | not confirmed **yet** | not resolved **yet** |
| target | reports never retracted | reports eventually confirmed | reports resolving positive |
| lag support | `{1, 2, ...}` | `{0, 1, ...}` | `{0, 1, ...}` |

The lag support differs only because a *retraction* in the same period
as its report describes a case never visible in any data vintage,
whereas a test coming back the day it was ordered is ordinary.

In `mode = "both"`, `revision_delay` is one shared law for positive and
negative resolutions. This first prototype deliberately does not fit
separate competing-risk lag laws. In a one-outcome mode, the same
argument denotes the lag for the outcome that is recorded: confirmation
or retraction respectively.

`"auto"` reads `unique(revision_type)` over the **full** data, not the
as-of view, so the mode is a stable property of the data source and does
not flip between backtest dates.

## Detection

[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
attaches a revision process when the `tbl_now` carries `revision_date` /
`revision_type` (see
[`tbl.now::add_revision_date()`](https://rodrigozepeda.github.io/tbl.now/reference/add.html)).

Count-cumulative data instead use
[`cumulative_process()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/cumulative_process.md),
whose primitive retraction object is the finite-age kernel `h_R`; it
does not estimate `p`.

## Count-cumulative data

Do not use this component for an aggregate cumulative stream. Configure
its down-revisions with
[`cumulative_process()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/cumulative_process.md).
Without individual report outcomes, `p` and a conditional revision-delay
law are not separately identified.

## Default priors

`revision_delay` inherits the default priors of its delay family (see
[delay_process](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md)).
`p` is described under its argument above. At `p = 1` the revision layer
is inert and the model is the ordinary count model.

## See also

[`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md),
[revision_delay](https://rodrigozepeda.github.io/diseasenowcasting/reference/revision_delay.md),
[delay_process](https://rodrigozepeda.github.io/diseasenowcasting/reference/delay_process.md),
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)

## Examples

``` r
# Results come back on one shared timescale, whatever the answer:
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

# Attach to a model:
model(nb_likelihood(), hsgp_epidemic(), lognormal_delay(),
      revision = revision_process(
        revision_delay = dirichlet_revision(bins = 10),
        p                = beta_prior(20, 3),
        stratified_p     = TRUE))
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
#> ── Revision process 
#> Revision(p ~ Beta(20, 3))
#> Shared revision delay: Dirichlet
#> p: estimated separately per stratum
#> 
#> ── Covariate prior 
#> StdNormal()
#> Strata pooling: "independent"
#> ────────────────────────────────────────────────────────────────────────────────
```
