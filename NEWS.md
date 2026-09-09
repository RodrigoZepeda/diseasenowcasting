# 2.4.1

This release removes three things `diseasenowcasting` was duplicating from
`tbl.now`. All three were invisible in normal use and none change results.

## `covid_colombia` moved to tbl.now

The `covid_colombia` dataset is gone from this package; it now lives in
`tbl.now`, alongside the other example datasets (`denguedat`, `mpoxdat`,
`flusight`, ...). The two copies were byte-identical, and shipping the same
35,501-row data frame from two packages that are always attached together only
made `?covid_colombia` and `data(covid_colombia)` ambiguous.

Nothing changes for users: `diseasenowcasting` depends on `tbl.now`, so
`library(diseasenowcasting)` still puts `covid_colombia` on the search path.
Code that qualified the name as `diseasenowcasting::covid_colombia` must now
say `tbl.now::covid_colombia`.

`LazyData` was dropped from `DESCRIPTION` along with the now-empty `data/`
directory.

## `?revision_delay` pointed at the wrong package

`diseasenowcasting` and `tbl.now` both documented a help topic named
`revision_delay`, meaning different things: the revision-lag *distributions*
here (`lognormal_revision()`, `dirichlet_revision()`, ...) and the
confirmed-vs-retracted *diagnostic* there (`diagnose_revision_delay()`,
`plot_revision_delay()`). With both packages attached -- which is always, since
one depends on the other -- `?revision_delay` prompted for a disambiguation and
then resolved to `tbl.now`, so a user who had just called `lognormal_revision()`
was shown the wrong page.

This package's topic is now `revision_distributions`; `?revision_delay`
unambiguously means `tbl.now`'s. No function was renamed, and
`?lognormal_revision` and its siblings still land on the right page. Only a
literal `?revision_delay` or a `[revision_delay]` doc link needs updating.

## `dn_palette()` no longer keeps its own copy of the colours

All eight `dn_palette()` colours were the `tbl.now::tbl_now_palette()` defaults
hard-coded a second time under different role names (`reported` for `epidemic`,
`accent` for `reporting`, and so on). Nowcast plots are routinely drawn beside
`tbl_now` plots in one document, so the copy would have stopped matching the
moment `tbl.now` retuned a colour -- silently, with no error to notice.

`dn_palette()` now reads `tbl.now::tbl_now_palette()` and renames the roles.
The returned values, names, order and `n` behaviour are unchanged, so plots
render identically. `theme_diseasenowcasting()` and the `autoplot()` bar
colours, which had their own hard-coded copies of the same two hexes, now go
through `dn_palette()` as well; the `color` argument still accepts any colour.

# 2.4.0

## Documentation: vignettes split into CRAN vignettes and website articles

Building the vignettes took 13.4 minutes, almost all of it re-fitting models.
Five of them are now pkgdown-only **articles** under `vignettes/articles/`
(`Benchmark`, `Handling_Outlier_Delays_with_Censoring`, `LLM_Usage`,
`Mathematics`, `Revision_processes`); they remain on the package website but no
longer ship in the tarball or run during `R CMD check`. Links to them from the
remaining vignettes now point at the website.

The four vignettes that still ship (`introduction`, `Understanding_Priors`,
`Nowcasting_at_the_start_of_an_Epidemic`, `Custom_delays_and_processes`) run the
same code on smaller inputs: `auto_nowcast()` in `introduction` selects over the
dengue data up to 1992 with three backtest dates instead of ten dates on data up
to 1994, its mpox backtest uses three dates instead of five, and
`Nowcasting_at_the_start_of_an_Epidemic` fits four observation windows instead of
seven.

The `Benchmark` article now shows the four tables per disease that its text
promises. Dengue previously collapsed the NobBS, epinowcast and baselinenowcast
views into a single de-duplicated table, the `diseasenowcasting`-only view was
never rendered for any disease, and the dengue heading said Colombia instead of
Puerto Rico.


## Breaking: fitted nowcasts now use the common `tbl.now` result grammar

`nowcast()` and `auto_nowcast()` now return a diseasenowcasting subclass of
`tbl.now::tbl_nowcast`. Predictive draws and requested quantiles are materialised
in the public result, while the complete native model fit is retained in `@fit`.
Native operations such as `predict()`, `coef()`, `parameters()`, `update()`,
`surprise()`, and model diagnostics unwrap that fit automatically.

The same object now works directly with the common `tbl.now` workflow:
`tidy()`, `as_tibble()`, `autoplot()`, `score_nowcast()`, forecast conversion,
weighting, and ensembling need no explicit adapter. `nowcast()` gains
`quantile_levels` so direct calls and `tbl.now::run_nowcast()` preserve the same
requested quantile grid. Event axes retain their original numeric, daily, or
weekly representation, and declared strata are reconstructed from the
authoritative input data rather than from lossy display labels where possible.

The result's standard `tbl_nowcast` properties are the source of truth for the
data, event axis, strata, and analysis date. Namespaced metadata is reserved for
diseasenowcasting prediction semantics and fit diagnostics. Model type, fitting
rung, revision mode, and automatic-selection evidence remain available on the
diseasenowcasting subclass and its retained native fit.

## Breaking: backtesting and predictive scoring delegate to `tbl.now`

`backtest()` now translates one or more native `model()` specifications into
labelled `tbl.now::engine_diseasenowcasting()` specifications and returns
`tbl.now::nowcast_backtest()` directly. This replaces the package-local
backtest class and makes canonical tidying, plotting, forecast conversion,
scoring, weighting, and ensembling available immediately.

The revised interface supports canonical `horizon`, `keep_draws`, `on_error`,
`verbose`, `truth_axis`, `truth_type`, and `quantile_levels` controls. Explicit
model-list names become method labels; inferred labels describe the model
components, and duplicate labels are rejected. Default truth semantics match
the fitted estimand: confirmed cases for confirmation/both revision modes,
still-standing cases for retraction-only mode, and reported totals otherwise.
Automatic backtest dates for count-cumulative models respect their settlement
horizon.

The exported package-local `score()` has been removed. Predictive evaluation
belongs to `tbl.now::score_nowcast()` and `scoringutils`; the new `fit_check()`
reports only RTMB optimizer and Laplace diagnostics.

## Optimizer and Laplace diagnostics

Joint fits now use one structured adequacy predicate throughout low-level
fitting, the two-stage collector, final warnings, prediction metadata, and
`fit_check()`. It requires a finite objective and derivatives, a successful
optimizer path, the box-constrained KKT conditions, positive-definite curvature
on the locally free subspace, finite reconstructed incidence, and a
curvature-scaled estimate of remaining objective improvement
`0.5 * r' H^(-1) r <= 0.01`. The raw maximum gradient remains available for
debugging but is no longer treated as a parameterization-independent
convergence criterion.

L-BFGS-B refinement now optimizes the centered objective
`Q(theta) - Q(theta_start)`. Its stopping and acceptance rules therefore do not
depend on an inferentially irrelevant additive constant in the negative log
posterior. A successful base `nlminb` solve is not invalidated by a later
line-search termination after an accepted improvement, and a successful polish
can rehabilitate the optimizer path. Both solver codes, centered objective
change, acceptance tolerance, and before/after gradients are retained for
auditability.

Cold initialization attempts are first classified by the common adequacy
predicate; the adequate candidate with the lowest negative log posterior is
then selected. If none is adequate, the lowest-objective candidate is retained
only as an explicitly degraded fit or internal initializer. This replaces
selection by the smallest unscaled gradient.

Internal warm, Stage-1, and discarded imputation fits no longer emit end-user
warnings. In two-stage fitting, only adequate Stage-2 fits contribute draws,
and at most one aggregate warning describes exclusions or a degraded retained
fit. `fit()` gains `warn` for controlling warnings from direct low-level joint
fits. The legacy matrix interface `nowcast_twostage()` now uses the same fitting
cascade and adequacy policy as `nowcast(type = "two_stage")`.

Laplace sampling records whether the original precision admitted a Cholesky
factorization and whether a diagonal ridge, non-finite repair, or eigenvalue
floor was required. Strictly complementary active box coordinates are held
fixed and sampling uses the certified free-coordinate precision. Any altered
precision is visible in result metadata and causes `fit_check()` to report a
warning rather than an unqualified pass.

Cross-platform numerical handling is now deterministic. Non-finite sparse
precision matrices are rejected before CHOLMOD factorization and sent directly
to the documented repair path, avoiding non-finite predictive draws on some
Linux builds. When an RTMB Laplace-marginal objective does not expose an
analytic Hessian, the adequacy check computes observed curvature by centered
finite differences of its analytic gradient; the source is recorded in the
fit diagnostic.

Two-stage results now expose an auditable `fit_diagnostics` record containing
the requested and resolved fitting type; requested, attempted, retained, and
excluded imputation counts; exclusion reasons; warm-fit use and status;
Stage-1 Hessian/Laplace status; fallback information; retained-fit diagnostics;
and Laplace-sampling regularization. The same record is available at
`@fit_diagnostics` and `@metadata$diseasenowcasting$fit_diagnostics`.

## Automatic model selection

`auto_nowcast()` keeps native model-grid construction and full-data refit
fallbacks but now evaluates its canonical backtest through `scoringutils`.
Selection defaults to pairwise relative skill for the requested metric, which
avoids rewarding a model merely because it succeeded on an easier subset of
targets. Set `relative_score = FALSE` to use the raw mean score.

Effectively tied scores are deterministic. The default
`tie_break = "epidemic_priority"` prefers HSGP, then AR(1), SIR, and custom
epidemic processes; `tie_break = "fastest"` instead prefers the smallest median
successful retrospective fit time. The unused rule and original candidate-grid
order provide secondary tie-breaks. Failed retrospective cells are skipped,
and a failed full-data winner falls through to the next-ranked candidate.

Backtest cell durations, full-data refit attempts, and total selection time are
recorded and exposed by the new `selection_timings()` function.
`comparison_scores()`, `best_score()`, and `selection_metric()` now report the
canonical scoringutils-based selection evidence.

## Updating, persistence, dependencies, and documentation

`update()` preserves the common public result while warm-starting the retained
native model through the same one- or two-stage collector. `save_nowcast()` now
accepts the public common result, and `load_nowcast()` restores that result
grammar together with its event axis, strata, quantile levels, fit diagnostics,
and automatic-selection evidence. The stored native mode and precision still
support prediction without rebuilding the RTMB tape.

The minimum `tbl.now` version is now 0.35.3. `future` is now suggested rather
than imported because parallel execution is owned by the canonical backtest
workflow; `future.apply` is no longer imported, and `generics` is suggested for
interoperability tests and workflows.

New `?diseasenowcasting_workflows` documentation explains the boundary between
native modelling/diagnostics and common cross-engine result operations. The
README, introductory material, model-selection documentation, and
outlier-censoring vignette have been updated for the common result, canonical
backtest, scoringutils relative-skill, and fit-diagnostic workflows.

# 2.3.0

## Breaking: tbl.now revision vocabulary is now the package vocabulary

The report-level post-report component is now called a revision process
everywhere. The exported API is `revision_process()`, `model(revision = )`,
`revision_delay =`, and the `*_revision()` delay aliases. Older spellings are
removed rather than deprecated.

`diseasenowcasting` now depends on `tbl.now (>= 0.35.0)` and reads the current
tbl.now revision metadata directly: `revision_date`, `revision_type`, and
`is_censored_revision`.

## Breaking: `model_parameters()` is now `parameters()`, and `tidy()` belongs to tbl.now

Version 2.1.0 moved the per-parameter table to `model_parameters()` and defined
a `tidy()` here that returned the nowcast. Both halves of that are now revised.

* `model_parameters()` is renamed **`parameters()`**. It is the same table
  (`term`, `estimate`, `std.error`, `conf.low`, `conf.high`, `type`) and the
  same `conf.level` argument; only the name changed. The old name is removed
  rather than deprecated.
* Standard errors are fixed. The interval was computed with `base::diag()` on
  the sparse solve of the precision matrix, which errors out and left
  `std.error` `NA` for **every** parameter. It now uses `Matrix::diag()`, and
  falls back to marginal SDs estimated from Laplace draws when the precision
  matrix is too large or too ill-conditioned to invert.
* **`tidy()` is no longer defined in this package.** `tbl.now (>= 0.35.0)`
  exports the shared generic and registers the `diseasenowcasting` method
  itself, so `tidy()` on a fit still returns the cross-package nowcast table
  described under 2.1.0 -- it is simply no longer our code, and we no longer
  register a method on a generic we do not own.

## Breaking: cumulative model configuration is now `cumulative_process()`

The count-cumulative model component is now configured with
`cumulative_process()` and passed as `model(cumulative = )`. Automatic
count-cumulative model selection remains inside `nowcast()`: when a
`tbl_now` has `data_type = "count-cumulative"` and no explicit cumulative
component, `nowcast()` selects the default signed hurdle--ZTNB cumulative model.

Count-cumulative data consume signed changes in the cumulative trajectory only.
Any `revision_date` metadata on such objects is treated as data provenance, not
as an individual report-level revision likelihood.

## Default model selection

`nowcast(type = "auto")` continues to select a fitting strategy automatically,
but the user-facing default remains `type = "two_stage"`. All automatic
selection for the diseasenowcasting backend lives in
`diseasenowcasting::nowcast()`; `tbl.now::run_nowcast()` passes the `tbl_now`
and engine arguments through without injecting model components.

# 2.2.0

## Breaking: the revision process is now the revision process

The optional component describing what happens to a report *after* it is filed --
a laboratory result comes back, and the report is either confirmed or retracted --
is called a **revision** process throughout, matching `tbl.now` 0.28.0. The old
spellings are gone, not deprecated.

| was | is |
|---|---|
| `revision_process()`, `resolution_process()` | `revision_process()` |
| `model(revision = )` | `model(revision = )` |
| `retract_delay = `, `resolution_delay = ` | `revision_delay = ` |
| `lognormal_retraction()` / `_confirmation()` / `_resolution()` | `lognormal_revision()` |
| `gamma_*`, `generalized_gamma_*`, `dirichlet_*` (three spellings each) | one `*_revision()` each |

Twelve lag constructors become four. The **outcome values are unchanged**: a case
is still `"confirmed"`, `"retracted"` or `"pending"`. Revision is what the
process does; confirmed is one of the things it can conclude.

## Breaking: the revision process is detected, not requested

`nowcast()` no longer takes `retraction_date`, `confirmation_date`,
`retraction_censored` or `confirmation_censored`. It reads the process off the
data instead, in the two places it can be:

* the `tbl_now` carries `revision_date` / `revision_type` (see
  `tbl.now::add_revision_date()`).

```r
# before
nowcast(data, model(), retraction_date = "retracted")

# now
data <- tbl.now::add_revision_date(data, retracted, revision_type = outcome)
nowcast(data, model())
```

Revision censoring is also read from the `tbl_now` object's
`is_censored_revision` attribute. There is no parallel column-name argument in
`nowcast()`; event, report, revision, outcome, and censoring metadata all have
one source of truth.

**The mode is inferred from the data.** `unique(revision_type)` over the
**full** data -- not the as-of view -- decides between `confirmation_only`,
`retraction_only` and `both`, so the mode is a stable property of the data source
and cannot flip between backtest dates. Assert it with
`revision_process(mode = )` when you want the check rather than the inference:
an assertion the data cannot support is an error, whereas inference with no
evidence falls back to the ordinary count model.

A revision **date with an `NA` `revision_type`** is now an error. The report
has resolved but its sign is unknown, so it cannot enter either lag law.

`backtest()` needs nothing extra either: the truth is built from the cases that
settle positive (confirmed, or never retracted), with pending cases kept --
"not resolved yet" is not "not a case".

The `nowcast_class` stores the resolved `@revision_mode` (`"none"`,
`"confirmation_only"`, `"retraction_only"`, or `"both"`). Date and censoring
column metadata remain on its `tbl_now` data.

## Documentation

`vignette("Retractions_and_confirmations")` is now
**`vignette("Revision_processes")`**, rewritten around the detected process
rather than the old column arguments. The old name is gone, like the old API.

## Breaking: `tidy()` is now `parameters()`

`tidy()` on a nowcast gives you the **nowcast** -- the predicted counts, through
`tbl.now`'s method for the broom generic. The parameter table is
`parameters()`. This package used to own the generic and answer the second
question under the first one's name; `tbl.now`'s `.onLoad()` takes it over now
that the method is gone.

```r
coef(nc)          # unchanged
parameters(nc)    # was tidy(nc)
tidy(nc)          # now tbl.now's: the nowcast itself
```

## count-cumulative data

The old fixed-`p` Skellam/SkNB implementation is replaced by a dedicated
`cumulative_process()` component. A cumulative stream identifies the
unconditional finite-age kernel `h_R`, not a biological truth probability and a
conditional revision-delay law separately. An old cumulative specification
using `revision_process()` is translated once to the collapsed kernel with a
targeted deprecation warning; the old likelihood is not used.

Three observation composites are selectable:

* `"cumulative"`: Poisson or negative-binomial cumulative-level marginals;
* `"hurdle_ztnb"`: signed updates with a structural zero and a
  zero-truncated-NB magnitude indexed by its own mean; and
* `"hurdle_ztpoisson"`: the same hurdle/sign construction with a
  zero-truncated-Poisson magnitude indexed by `(alpha + omega) / pi` and no
  magnitude dispersion.

The settlement horizon `H` is configurable and defaults to 26 model steps.
Predictions target `C_t(H)`, finite-horizon database retention, and are anchored
to the level observed at the analysis origin. Completed zeroes inside the as-of
triangle are scored; future cells are masked. The products over levels or signed
updates are composite likelihoods, and the current pseudo-posterior intervals do
not include sandwich/Godambe calibration.

## Fixed: `prior_only = TRUE` returned all-`NA` draws for cumulative data (#129)

The prior sampler now supplies the collapsed-kernel and model-specific hurdle
parameters. `hurdle_ztpoisson` deliberately supplies no magnitude dispersion.
A sampler that cannot reconstruct any draw aborts with the captured first error
instead of returning a correctly shaped but entirely missing result.

# 2.1.0

## `tidy()` now returns the nowcast, and uses the shared `generics` generic

**Breaking change.** `tidy()` on a fitted nowcast used to return one row per
estimated *parameter*. It now returns the **nowcast** — one row per event date
per stratum — matching the contract every other nowcasting engine returns, so
downstream code (plotting, scoring, cross-engine comparison via `tbl.now`) does
not have to special-case this package.

* `tidy()` is now **re-exported from `generics`** instead of being a generic
  defined here. The old package-local generic masked `generics::tidy` after
  `library(diseasenowcasting)`, which made every method other packages register
  on the shared generic invisible — including `tbl.now`'s.
* `tidy()` works on both a fitted `nowcast()` and on `predict(fit)`. On a fit it
  draws the posterior predictive first (pass `n_draws` / `seed` through `...`).
  It returns a tibble sorted by `stratum` then `event_date`, with columns
  `event_date`, `stratum` (`"all"` when unstratified), `estimate` (posterior
  **median**), `conf.low`, `conf.high`, `level` (the width the interval actually
  has) and `engine`. Event dates are reported on the model's own grid — never
  re-gridded.
* A `probs` argument appends one exact quantile column per probability, named
  `q5`, `q50`, `q95` (`probs * 100`, so `0.025` gives `q2.5`).
* Stratified fits get one block of rows per stratum, read from the per-stratum
  draws rather than the pooled ones.
* **New `model_parameters()`** returns the old `tidy()` table (`term`,
  `estimate`, `std.error`, `conf.low`, `conf.high`, `type`). Calling `tidy()` on
  a fit warns once per session and names `model_parameters()`.
* `tidy.default` is gone: registering a default method on the shared generic
  would have changed `tidy()`'s behaviour for every other package in the
  session. `model_parameters()` keeps a default method that errors clearly.

# 2.0.0

## Resolution processes: confirmation as well as retraction

A report is rarely a case outright — it is provisional, and **resolved exactly
once**: a test comes back, and it is either positive (the case is *confirmed*) or
negative (the case is *retracted*). Nothing is confirmed and then later retracted.
What differs between surveillance systems is only which resolutions get a date
column, and `diseasenowcasting` now handles all three possibilities under one
likelihood:

| | **retraction only** | **confirmation only** | **both** |
|---|---|---|---|
| dates recorded | the negatives | the positives | both signs |
| a missing date means | not retracted **yet** | not confirmed **yet** | not resolved **yet** |
| target | reports never retracted | reports eventually confirmed | reports resolving positive |
| lag support | `{1, 2, ...}` | `{0, 1, ...}` | `{0, 1, ...}` |
| argument | `retraction_date =` | `confirmation_date =` | both |

The lag support differs only because a *retraction* in the same period as its
report describes a case never visible in any data vintage, whereas a test coming
back the day it was ordered is ordinary.

* `nowcast(confirmation_date = , confirmation_censored = )` mirrors the retraction
  pair, with the same censoring, stratified-`p` and `g_C`-family support.
* The sign of the evidence flips. Under retraction the longer a report stands
  unretracted the more likely it is genuine (`rho(j)` rises); under confirmation
  the longer it sits unconfirmed the more likely it never will be (`rho(j)`
  falls). Confirmed rows enter the nowcast with certainty; retracted rows with
  probability zero.
* New lag constructors `lognormal_confirmation()`, `gamma_confirmation()`,
  `generalized_gamma_confirmation()`, `dirichlet_confirmation()`.
* **`count-incidence` data are supported for both modes**: one row per distinct
  `(event, report, resolution)` with a case count, `NA` marking the unresolved.
  Every statistic is a weighted tally, so the aggregated form and the linelist
  give bit-identical engines and log-likelihoods.
* **`count-cumulative` data are not row-level revision data.** As of 2.2.0
  they use `cumulative_process()` and the collapsed finite-age retraction
  kernel described above.
* **Both outcome values in one revision column** fit the full process: a report
  is resolved exactly once and the resolution is either positive (confirmed) or negative (retracted),
  as when a test comes back. Seeing the sign is strictly more informative than
  inferring it from the censoring: `p` collapses to a plain binomial on the
  resolved rows (`N+ / (N+ + N-)`, no censoring correction), an unresolved row
  contributes only `1 - G_C(j)`, free of `p`, and it enters the nowcast with
  probability `p` **flat in its age** — with a shared lag law the age says nothing
  about which way a pending test will go.
* The first prototype uses one revision-delay law. In a single-outcome mode it
  is the lag of that outcome; with both outcomes recorded it is shared by positive
  and negative resolutions. Separate competing-risk lag laws are outside this
  prototype.
* Not implemented, and named as such in the vignette: the **two published
  streams** count-cumulative model (a source publishing the reported *and*
  confirmed cumulatives, which does identify `p` and `g_K` apart).
* New vignette: `vignette("Retractions_and_confirmations")`, written for
  practitioners; the derivation stays in `vignette("Mathematics")` section 8.

## Verified for the negative binomial

The resolution blocks were derived by Poisson colouring, which raises the fair
question of whether they hold under `nb_likelihood()`. They do, and it is now
tested rather than asserted: conditional on the shared gamma frailty the
trajectory-type counts are independent Poisson, so the split of the rows across
types is multinomial and **free of the frailty size `r`** — checked by Monte Carlo
across `r` from 0.5 to 1e6. The count block is then the ordinary negative-binomial
`S_k`, and no quadrature is needed. `p` comes out identical under Poisson and NB
(it lives entirely in the frailty-free block), which is also tested.

### The negative-binomial predictive is wider than nominal — diagnosed, fix opt-in

On data simulated with a genuine per-origin gamma frailty, NB predictive
intervals cover far more than they should at recent horizons (50% intervals
covering ~0.77, 95% covering ~1.00, against Poisson's ~0.49 / ~0.94). It
reproduces identically on a plain nowcast with no retractions, so it is
package-wide and predates this work.

**Cause, now confirmed rather than suspected.** `predict()` draws the count still
to come from the *prior* frailty `Gamma(r, r)`. That gives the correct *marginal*
spread — which is why it survives casual checks — but a predictive must be
conditional on what has already been seen, and the `k_t` reports already in hand
pin that origin's frailty down. Simulated exactly, with true parameters and
conditioning on `k`: the true future SD is 11.6, the prior draw gives 32.6, and
the conjugate posterior `Gamma(r + k_t, r + E[observed])` gives 11.6.

**The fix is implemented but off by default**, behind
`options(diseasenowcasting.conditional_frailty = TRUE)`. Switching it on fixes the
calibration on data simulated from the model (50% coverage 0.77 → 0.51, intervals
~40% narrower). But on the real dengue / mpox / COVID benchmark it made mean WIS
*worse* on every disease (COVID 11.1 → 160.4) at essentially unchanged coverage:
the epidemic mean is always somewhat misspecified on real data, and the prior
draw's extra width was quietly absorbing that. Sharper intervals around a
slightly-off centre score worse. The default therefore reproduces the published
benchmarks exactly, and the option is there for anyone who wants the theoretically
correct predictive. Resolving the underlying misfit is separate work.

## `tidy()` reports uncertainty again

`tidy()` returned `NA` for every standard error on any joint AR(1) / HSGP fit —
`diag()` was being called on a sparse `Matrix`, which errors, and the surrounding
`tryCatch` turned the whole column into `NA`s. Fixed, with a Laplace-sampling
fallback for precision matrices too large or ill-conditioned to invert. `tidy()`
also now reports `confirm_p` (or `confirm_p[<stratum>]`) on the **natural scale**,
with the exact transformed credible interval rather than a logit-scale one.

## Linelist data with retractions

A linelist can now carry a **retraction date** — the date a reported case was
withdrawn from the register (reclassified, corrected, a duplicate) — and
`diseasenowcasting` will nowcast the **settled** count: cases that are reported
and never retracted.

* `nowcast(retraction_date = "<column>")` switches the observation model on. A
  missing retraction date is read as *not retracted yet*, not as *genuine*: the
  retraction lag of a standing row is right-censored at the age of its report, so
  the retraction block is a Berkson–Gage **mixture-cure** likelihood with cure
  fraction `p`. Retractions dated after `now` are masked out automatically.
* The row-level retraction structure uses `p` (the probability a report is
  genuine) and a revision lag. Left alone, `nowcast()` attaches a sensible
  default. A linelist identifies `p` directly from resolved and standing rows,
  so it gets a **weak** data-informed Beta prior. Count-cumulative data no longer
  use this decomposition; see the 2.2.0 migration entry.
* New retraction-delay constructors — `lognormal_retraction()`,
  `gamma_retraction()`, `generalized_gamma_retraction()`,
  `dirichlet_retraction()` — so a model reads as what it is. They are aliases of
  the `*_delay()` constructors; the difference is the support, `g_C` living on
  `{1, 2, ...}`. **Prefer `dirichlet_retraction()` when counts are large**: the
  correction `rho(j)` is applied to every standing case, so a *shape* error in
  `g_C` biases the nowcast by more than its Monte-Carlo noise. On a COVID series
  of ~8000 cases/day a lognormal `g_C` fitted to a `1 + Poisson(2)` lag left a
  0.9% bias and lost nominal coverage; the Dirichlet recovered `rho` to four
  decimals.
* **Per-stratum `p`** via `revision_process(stratified_p = TRUE)`: each
  stratum gets its own confirmation probability under a shared prior, while `g_C`
  stays shared. Use it when strata plausibly differ in data quality and each has
  enough retractions; with sparse strata the shared `p` is safer.
* **Partially observed rows are supported**, in every combination. A censored
  report date (`tbl.now`'s `is_censored`) and/or a censored retraction date (a new
  `retraction_censored =` column of flags, marking rows whose retraction date is
  an upper bound) contribute the log of a sum over the appearance delays they are
  compatible with. Note that an exact retraction *also bounds* a censored report —
  a case cannot be withdrawn before it is filed — so the two constraints combine.
  Each kernel collapses to the exact-row term when its interval is a single point.
* The posterior predictive thins the standing rows one by one: a row whose report
  is `j` periods old survives into the nowcast with probability
  `rho(j) = p / (p + (1 - p) * (1 - G_C(j)))`. Already-retracted rows contribute
  nothing.
* Negative-binomial over-dispersion needs **no quadrature** here — the gamma
  frailty cancels out of the row-level split, so the count block is the ordinary
  NB and the retraction block is closed form.
* With no retraction column, an all-`NA` one, or no retraction observed by `now`,
  the fit is bit-for-bit the ordinary count model.
* `backtest()` builds its truth from the cases that were never retracted when
  `retraction_date` is passed through — scoring against every reported row would
  make an unbiased model look biased low by the retraction rate.
* Cases retracted in the *same period* as their report are dropped: they were
  never visible in any data vintage, and `g_C` lives on `{1, 2, ...}`.
* See the new section 8 of the *Mathematical Foundations* vignette for the
  derivation, and `devel/benchmark_retraction.R` for the dengue / mpox / covid
  benchmark against ignoring or naively dropping the retracted rows.

## Count-cumulative data: counts that can revise *up and down*

Version 2.0.0 introduced the initial count-cumulative experiment. Its original
fixed-`p` Skellam/SkNB formulation is superseded by the 2.2.0 migration entry
above. Current code uses `cumulative_process()`, targets finite-horizon
retention, and does not estimate or report cumulative `p`.

# 1.3.2

* Lowered the two-stage delay-imputation spread floors `floor_mu` and
  `floor_sig_frac` to `0.08` (from `0.15` / `0.25`) in `nowcast()` and
  `nowcast_twostage()`. The previous defaults over-dispersed low-count daily
  data; `0.08` is the value the package is backtested at.
* Made `auto_nowcast()` more robust:
  - Candidate epidemic processes are now gated by a lower bound only -- a process
    is compared whenever the series is long enough to support it (and is no
    longer dropped for being *too* long), so the comparison always spans every
    process the data can support (e.g. `{SIR, AR(1), HSGP}` for long series).
  - Candidates are scored on the common set of as-of dates where they all
    produced a forecast, so a model can no longer "win" on a lucky subset.
  - The selection backtest is now wrapped: if it cannot run (e.g. the series is
    too short for any complete-truth date) `auto_nowcast()` refits the grid
    directly instead of erroring.
  - The winner is refit on the full data with a fallback: if it fails to
    converge, `auto_nowcast()` falls through to the next-best candidate, so it
    now converges whenever any candidate would.
  - New `K_select` argument: the selection backtest fits the whole grid over many
    dates, so it now uses a smaller delay-imputation count (default 10) than the
    final fit's `K` (default 25).  This removes a runtime cliff on long series
    (selection cost scales with `K_select`) while leaving the winning model fit at
    full `K`.
* `backtest()` gains a `recent` argument: when `dates` is `NULL`, choose the most
  recent complete-truth as-of dates instead of spreading them across the history.
  
# 1.3.0

* Added `save_nowcast()` / `load_nowcast()` to persist a fitted `nowcast()` (or
  `auto_nowcast()`) to a single `.rds`. The RTMB autodiff tape cannot be
  serialized, so the bundle stores the `model()` spec, the input `tbl_now`, and
  each fit's parameters plus its Laplace mode and precision. A loaded nowcast
  works with `predict()` / `coef()` / `tidy()` / `autoplot()` straight away (any
  `n_draws`, no RTMB needed -- even for custom delays/epidemics), and can be
  re-fit from its `model` + bundled data. `load_nowcast(file, rebuild = TRUE)`
  also re-tapes the objective (no re-optimization) when a live tape is needed.
* Removed the old `censor_delays_above()` spelling. The function now lives in
  `tbl.now` as `censor_reporting_delays_above()`.
  It remains available with the same signature because `tbl.now` is a dependency
  (attached whenever `diseasenowcasting` is).
* Improved documentation and badges with `lifecycle`. 
* Updated dependency on tbl.now to latest version (0.7.8)
* Removed `rlang` dependency
* Added `RTMBode` as a remote repository and to suggests. 
* The advanced ODE example in the *Custom Delays and Epidemic Processes* vignette
  now integrates the SIR system with the `RTMBode` solver instead of a
  hand-written RK4 scheme.

# 1.2.0

## Automatic model selection

* Added `auto_nowcast()`: give it a `tbl_now` and it builds a grid of candidate
  models (epidemic process x delay family) sized to how much data you have,
  backtests them over several dates, scores them, and refits the winner on the
  full data. The returned object is an ordinary `nowcast()` result with the
  ranked scoreboard in its `comparison` slot. You can
  - pass priors to the candidates (e.g. `sir = sir_epidemic(R0 = ...)`);
  - compare likelihoods (`likelihood = list(nb_likelihood(), poisson_likelihood())`);
  - add your own `custom_delay()`/`custom_epidemic()` models via `models = ...`;
  - select on `metric = "wis"` (default), `"ape"`, `"mse"`, or a calibration
    criterion: `"coverage_50"`, `"coverage_90"`, or `"coverage"` (both intervals'
    miss from nominal, summed).
* Accessors for an `auto_nowcast()` result: `best_model_name()` (the winning
  label), `best_model()` (the winning `model()` object, to reuse elsewhere),
  `comparison_scores()` (the ranked scoreboard), `best_score()` (the winner's
  row), and `selection_metric()` (the criterion used). Printing an
  `auto_nowcast()` result now also shows the top of the scoreboard.
* `nowcast()` / `backtest()` gain a `type = "auto"` option: the Dirichlet delay is
  fit one-stage and every other delay two-stage (the better choice for each in
  our experiments).
  
## Miscelaneous

* Updated `roxygen2` to 8.0.0
* Added the S7 `@` to `NAMESPACE`. 
* Improved test coverage. 
  
# 1.1.0  

## Custom (user-defined) components

* `diseasenowcasting` is now a **model-agnostic framework**: every nowcast is
  built from a likelihood, an epidemic process, and a reporting-delay
  distribution, and each can be a built-in *or* one you write yourself.
* Added **custom reporting-delay distributions** via `custom_delay()` (supply any
  RTMB-traceable CDF as `cdf`, with optional `log_cdf` / `log_survival`) with a
  `validate_custom_delay()` checker.
* Added **custom epidemic processes** via `custom_epidemic()` (supply any
  RTMB-traceable `intensity_fn(theta)` returning `log_mean[max_time x n_strata]`)
  with a `validate_custom_epidemic()` checker. Random walks, ODE/SIR models,
  regression surfaces, etc. all work.
* `custom_delay()` / `custom_epidemic()` no longer take an `n_params` argument —
  the number of parameters is inferred from `priors`, `param_names`, or `inits`.
* Added `infer_max_time()` to read off the number of event-times a model spans,
  for sizing a custom epidemic whose intensity function loops over time.
* New vignette *Custom Delays and Epidemic Processes* with worked examples on the
  `denguedat` and `mpoxdat` datasets, including an advanced RK4-integrated SIR ODE.

## Other changes

* `nowcast_diagnostic()` gains a `previous_times` argument (default 30) to limit
  the incidence and nowcast panels to the most recent event-times.
* Custom components require `library(RTMB)` to be attached; a clear error is
  raised otherwise.
* Removed the experimental `ppc()` posterior-predictive-check function.
* Internal modernisation (no change in results): non-standard evaluation now uses
  the rlang `.data` pronoun, backtesting uses `future.apply`.

#  1.0.0

* Changed the structure of the package from Stan to RTMB
* Added the gaussian process (HGSP) and SIR models
* Added the lognormal and generalized gamma delay
* Added surprise factors for extreme values
* Deprecated previous package
