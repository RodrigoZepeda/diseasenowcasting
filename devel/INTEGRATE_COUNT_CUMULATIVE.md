# Instructions for integrating the revised count-cumulative model

## Objective

Replace the package's current count-cumulative implementation, documentation, and
examples with the models prototyped in `devel/skellam_prototypes/`. Do not change
the statistical meaning or behavior of the linelist and count-incidence paths.

The integration must provide two explicitly selectable count-cumulative
observation models:

1. the cumulative-level Poisson or negative-binomial composite likelihood; and
2. the signed hurdle--zero-truncated-negative-binomial update composite
   likelihood.

The second model may be described informally as zero-inflated negative binomial,
but the implementation and mathematical documentation must call it what it is:
a hurdle model with a structural zero and a zero-truncated NB magnitude. It is
not a conventional ZINB count regression.

The settled target is the count at a configurable finite settlement horizon
`H`. Use `H = 26` model steps as the default. The user must be able to decrease
or increase it, including to 6 or 52 steps, without editing internal code.

## Sources of truth

Read these files before editing package code, in this order:

1. `main_identifiability_update.tex`, especially the count-cumulative section,
   the cumulative marginal, the signed-update marginals, and the hurdle--ZTNB
   construction.
2. `devel/skellam_prototypes/rtmb_identifiability_models.R`.
3. `devel/skellam_prototypes/flusight_asof_data.R`.
4. `devel/skellam_prototypes/README.md`.
5. `devel/skellam_prototypes/OPTIMIZATION_FINDINGS.md`.
6. `devel/skellam_prototypes/ALL_LOCATIONS_FINDINGS.md`.

Treat `main_journal_revised.tex` as background for the package's existing
linelist and count-incidence models. For the revised count-cumulative model,
`main_identifiability_update.tex` supersedes the old `p`/`g_C` formulation.

Do **not** integrate the legacy prototype files
`rtmb_count_cumulative.R`, `run_flusight.R`, or
`run_settled_flusight.R`. They use the superseded fixed-`p` model and a
conventional ZINB experiment.

## Statistical contract

### Primitive retraction object

For count-cumulative data, do not estimate or report a biological truth
probability `p` and a conditional retraction-delay distribution separately.
The data identify the unconditional, possibly defective finite-age retraction
kernel

```text
h_R(l) = Pr(R = l),  l = 1, ..., H
S_R(a) = 1 - sum_{l <= a} h_R(l),  a = 0, ..., H.
```

The prototype's parsimonious implementation is

```text
h_R(l) = retraction_mass * g_R(l),
```

where `g_R` uses one of the package's existing lognormal, gamma, or generalized
gamma delay families. This is a finite-horizon tail restriction and a convenient
factorization of `h_R`; it must not be presented as separate identification of
`p` and a biological validation-delay law. Report `h_R`, `S_R`, `q_C`, the
finite-horizon retraction mass, and the terminal retention. Do not label
`1 - retraction_mass` as an identified `p`.

The eventual target is database retention. Calling it biological truth requires
the extra assumption that a report is true if and only if it is never withdrawn.
State that distinction in help pages and vignettes.

### Cumulative-level composite likelihood

For delay `d`, implement

```text
q_C(d) = sum_{r=0}^d g_D(r) S_R(d-r)
E[C_t(d)] = mu_t q_C(d).
```

With `poisson_likelihood()`, use

```text
C_t(d) ~ Poisson(mu_t q_C(d)).
```

With `nb_likelihood()`, use the corresponding NB marginal with the same mean and
the package's mean/size convention. A product over delays is a composite
likelihood because levels from one event time are dependent. Never document it
as the exact joint likelihood.

### Signed hurdle--ZTNB update composite

Construct signed updates from the cumulative levels:

```text
Delta_t(0) = C_t(0)
Delta_t(d) = C_t(d) - C_t(d-1), d >= 1
alpha_t(d) = mu_t g_D(d)
omega_t(d) = mu_t sum_{r=0}^{d-1} g_D(r) h_R(d-r).
```

Let `total = alpha + omega`. The probability of a non-null update must satisfy

```text
0 < pi_t(d) <= min(1, total).
```

Use the prototype's bounded parameterization, including age and optional
previous-nonzero effects. Conditional on movement,

```text
Pr(positive | movement) = alpha / total
E[magnitude | movement] = total / pi_t(d).
```

The magnitude is a zero-truncated NB indexed by its **own** mean. Numerically
invert

```text
Psi_size(m_parent) = m_parent / (1 - P_NB(0; m_parent, size))
```

on the RTMB tape to obtain the parent NB mean. Passing `total / pi` directly to
the ordinary NB density as its parent mean is a serious error: it breaks
`E[Delta] = alpha - omega` and the cumulative mean identity. Test the inversion,
the limiting cases, and its derivatives.

The product of update marginals over `d` is also a composite likelihood. Preserve
the exact first moment

```text
E[Delta_t(d)] = alpha_t(d) - omega_t(d)
```

for every admissible movement probability.

### Epidemic and delay mechanisms

Reuse the package's existing `mu_t` implementations without forking their
mathematics:

- HSGP;
- AR;
- SIR; and
- their current covariate and strata behavior.

Reuse the existing report-delay and retraction-delay calculations for
lognormal, gamma, and generalized gamma. Keep unsupported delay families out of
the first integration unless they can satisfy the same normalization, support,
AD, and prediction tests. Reject unsupported combinations with a specific error,
not a failed optimization.

## Public interface

Introduce a dedicated count-cumulative configuration in `model()` rather than
continuing to overload the linelist `validation_process()` semantics. The exact
class/function name may follow the package's S7 conventions, but the interface
must satisfy all of these requirements:

- explicit selection of `"cumulative"` versus `"hurdle_ztnb"`;
- configurable settlement horizon, default `26L`;
- retraction-kernel delay family and mass prior/fixed value;
- hurdle movement and magnitude-dispersion priors;
- an unambiguous printed model summary;
- serialization through `save_nowcast()`/`load_nowcast()`; and
- a clear error if the component is used with linelist or count-incidence data.

Recommended shape, subject to matching the package's naming style:

```r
model(
  nb_likelihood(),
  ar1_epidemic(),
  lognormal_delay(),
  count_cumulative = count_cumulative_process(
    observation = "hurdle_ztnb",
    retraction_delay = lognormal_delay(),
    settlement = 26L
  )
)
```

Keep `validation_process()` unchanged for linelist and count-incidence data. For
backward compatibility, an old count-cumulative specification using
`validation_process()` may be translated once into the new collapsed kernel,
with a targeted deprecation warning. It must not retain the old likelihood or
continue reporting `p` as identified. Decide and document whether the hurdle
model or cumulative-level NB model is the count-cumulative default; based on the
prototype backtests, prefer hurdle--ZTNB unless a compatibility constraint
requires otherwise.

## Data preparation and the as-of contract

`tbl_now` is the package boundary. Preserve its class and metadata throughout
preparation.

At an origin `now`, the likelihood may see only rows satisfying

```r
event_date <= now & report_date <= now
```

No retrospective terminal value may enter data completion, parameter
initialization, priors, the empirical baseline, fitting, or prediction. Terminal
values are scoring data only.

For count-cumulative data, construct the complete event/report clock and add
explicit zero cells before de-accumulating. Use `tbl.now::complete_zeroes()` or
an exactly equivalent `tbl_now`-preserving operation. Do not apply this new rule
blindly to linelist/count-incidence paths, whose existing censoring behavior must
remain unchanged.

Support and demonstrate both clock definitions:

1. **Calendar clock:** keep every calendar week and backfill missing cells with
   zeroes.
2. **Compressed publication clock:** first make observed publication weeks
   consecutive, construct `event_num` and `delay` on that compressed clock, then
   construct/restore the `tbl_now` metadata and trim to `now`.

For FluSight, begin the usable publication series around September 2023. Do not
turn the surveillance-season gap into an enormous delay. In the compressed arm,
the last observed week of one season is followed numerically by the first
observed week of the next season. In the calendar arm, the omitted calendar
weeks remain real weeks and are explicitly zero-filled.

Make observation masks explicit. A zero that was observed or completed inside
the as-of triangle is data; a cell beyond the origin-specific observable horizon
is missing and contributes no likelihood. Never initialize an array to zero and
then accidentally score unobserved future cells as structural zeroes.

## Package code to replace or audit

Work through these files deliberately. Do not perform a broad mechanical rename.

### Core implementation

- `R/02_likelihood_class.R`: add the public model selector/component required by
  the chosen API, with validation and priors.
- `R/04_validation_class.R`: remove count-cumulative claims from the linelist
  validation API; retain its linelist/count-incidence behavior.
- `R/05_model_class.R`: carry and print the new count-cumulative configuration.
- `R/07_default_priors.R`: build priors for `h_R`/retraction mass and hurdle
  parameters. Do not route cumulative data through `confirm_p`.
- `R/09_prepare_data.R`: replace ambiguous `is_confirmation` and
  `increment_array` plumbing with explicit cumulative levels, updates,
  observation masks, ages, previous-movement indicators, and `H`.
- `R/12_fit.R`: make the optimizer strategy model-specific and expose diagnostic
  status.
- `R/14_objective_joint.R`: remove the current cumulative `p`, `g_C`,
  Skellam/SkNB path and dispatch to the two revised composite likelihoods. Leave
  ordinary count and linelist validation blocks unchanged.
- `R/15_nowcast.R`: replace the old future-genuine-minus-erroneous reconstruction
  with the finite-horizon reconstruction below.
- `R/17_prepare_from_tblnow.R`: implement the as-of and zero-completion rules,
  including both an observed-cell mask and correct `tbl_now` metadata handling.
- `R/18_collect_fits.R`, `R/20_nowcast_class.R`, `R/21_nowcast_methods.R`,
  `R/23_backtest.R`, `R/26_nowcast_diagnostic.R`, and `R/27_parameters.R`: audit
  all names, summaries, scores, diagnostics, and exposed parameters for old
  confirmation semantics.
- `R/28_confirmation_likelihood.R`: replace or split this file. No production
  count-cumulative call may reach the old Skellam/SkNB likelihood after the
  migration. A better name is `R/28_count_cumulative_likelihood.R`.
- `R/32_prior_only.R`: produce valid finite-horizon prior predictions under both
  new cumulative models.
- `R/34_save_load.R`: persist the observation-model choice, settlement horizon,
  model-specific parameters, random-effect state, and any schema version change.

Search the whole repository for `is_confirmation`, `increment_array`,
`confirmation`, `Skellam`, `SkNB`, `confirm_p`, `g_C`, `addition_mean`, and
`retraction_mean`. Classify every hit as cumulative-only, linelist validation,
shared, or documentation before changing it.

### Reconstruction and prediction

The primary operational reconstruction must be anchored to what was actually
known at the origin:

```text
C_t(H) = C_t(d*) + sum_{d=d*+1}^H Delta_t(d).
```

For the hurdle model, simulate future hurdle--ZTNB updates sequentially and
carry the previous-nonzero state. For the cumulative-level model, retain a direct
terminal marginal draw only as an explicitly labelled secondary approximation;
it is not conditioned on the current cumulative level. If an anchored
update-based approximation is exposed for the level model, label its assumptions
and test its mean against `mu_t q_C(H)`.

Do not silently hide negative reconstructed totals. If values are projected to
zero for a count-valued public result, count and expose how often projection was
needed in diagnostics.

Prediction output must name the estimand `C_t(H)` or finite-horizon settled
retention. Do not call it `N_t^+` unless the additional truth-equals-retention
assumption is requested and stated.

### Empirical baseline

Keep the empirical multiplier baseline as a development/backtest comparator.
For a target currently at age `a`, estimate its terminal/current multiplier only
from cohorts for which both age `a` and age `H` were observable by that same
origin. Use all such information available at `now`, and nothing after `now`.
Handle zero denominators explicitly and report the fallback. Do not make this
baseline part of the fitted package model unless separately requested.

## Optimization requirements

Carry over the optimization lessons from the prototype rather than relying on
the current global option.

- For cumulative-level Poisson/NB fits, integrate the latent AR innovations,
  HSGP coefficients, or SIR transmission innovations as RTMB random effects via
  Laplace.
- Keep bounded parameter transforms and conservative outer bounds for delay
  scales, epidemic hyperparameters, retraction mass, count dispersion, movement
  coefficients, and magnitude size.
- Start with bounded `nlminb` and the package's initialization ladder.
- Compute the maximum absolute gradient at the returned solution. Optimizer code
  zero alone is not a pass.
- If a Laplace fit has a maximum gradient above `0.05`, run the guarded bounded
  L-BFGS-B polish from the prototype. Accept it only if the objective is no worse
  within numerical tolerance and the gradient improves.
- Use `0.1` as the hard stability gate. Return a structured warning/diagnostic
  for a finite fit above that threshold rather than silently calling it converged.
- Keep the hurdle MAP strategy initially because it was faster and stable in the
  prototype. Re-evaluate Laplace for the hurdle only with measured evidence.
- Use monotonic elapsed time (`proc.time()` or equivalent) for long-run ETA
  calculations.

The previous full sweep covered 53 locations, five origins, and both clocks. Its
cumulative Laplace rerun produced finite fits throughout and passed the gradient
gate after polishing. Reproduce that gate after integration; do not assume the
package refactor preserved it.

Composite-likelihood curvature is not automatically calibrated posterior
uncertainty. The first integration may preserve the package's pseudo-posterior
draw mechanism for continuity, but documentation and diagnostics must call out
that limitation. Do not claim interval calibration until a sandwich/Godambe or
cluster-bootstrap correction has been implemented and tested.

## Documentation to substitute

Replace the old cumulative material instead of appending contradictory text.

- `vignettes/Mathematics.Rmd`: replace the current count-cumulative
  confirmation/Skellam section with the collapsed `h_R`/`S_R` model, cumulative
  Poisson/NB marginals, hurdle--ZTNB update law, mean-preservation proof,
  composite-likelihood caveat, and finite-horizon estimand.
- `vignettes/Validation_processes.Rmd`: restrict `p` and validation-delay
  interpretation to linelist/count-incidence data. Point cumulative users to the
  collapsed retraction kernel and explain why the two decomposed quantities are
  not identified.
- `vignettes/introduction.Rmd`: replace the FluSight example. Construct a
  count-cumulative `tbl_now`, show zero completion, fit both calendar and
  compressed clocks at a historical `now`, select both revised models, set
  `settlement = 26L`, and compare with the leakage-safe empirical multiplier.
- `NEWS.md`: replace existing count-cumulative Skellam/SkNB and fixed-`p` claims
  with one migration entry. State any API deprecation clearly.
- Roxygen in all affected `R/` files and regenerated `man/*.Rd`: use the same
  terminology and equations as the vignettes.

The vignette example must make these dates visually and textually distinct:

- the full retrospective data used to create a scoring truth;
- the historical `now` used to trim the fitting data; and
- the event weeks being nowcast.

Include an assertion in the example that the prepared fitting data contain no
event or report date after `now`.

## Tests and acceptance gates

### Mathematical unit tests

Add a focused test file for the new count-cumulative likelihoods. At minimum,
test:

- `h_R(l) >= 0`, `sum(h_R) <= 1`, `S_R(0) = 1`, and non-increasing `S_R`;
- `q_C(d)` against a plain-R convolution reference;
- the telescoping identity between cumulative levels and signed updates;
- `omega_t(0) = 0` and no negative delay-zero update;
- Poisson and NB cumulative log densities against base-R references;
- ZTNB normalization over a sufficiently wide support;
- inversion of `Psi_size` across small/large own means and sizes;
- finite RTMB values and gradients for sparse, large, positive, negative, and
  zero updates;
- `pi <= min(1, alpha + omega)` at every evaluated cell;
- Monte Carlo `E[Delta] = alpha - omega` within simulation error;
- the hurdle law's `Delta = 0`, positive, and negative branches; and
- `H` values 6, 26, and 52.

Do not delete useful numerical Skellam tests merely to make the new suite pass;
move them to a clearly labelled legacy/helper test if the Skellam routines remain
for research, or remove both routines and their tests together after confirming
there are no callers.

### Data and leakage tests

Test that:

- count-cumulative zero completion adds the expected cells and preserves
  `tbl_now` metadata;
- future cells are masked, not treated as observed zeroes;
- both `event_date` and `report_date` are at or before `now` in every fitting
  frame;
- the compressed clock makes consecutive observed publication weeks consecutive;
- the calendar clock retains real gaps;
- 2023 flows continuously into 2024 and 2024 into 2025 on the compressed clock;
- changing a post-`now` terminal value cannot change fitted inputs, priors,
  baseline multipliers, estimates, or predictions at that origin; and
- `update()`, `backtest()`, and save/load reconstruct the same clock and horizon.

### Regression tests for unaffected data types

Run the full existing linelist and count-incidence suites. Add targeted regression
tests proving that their `validation_process()` behavior, `p`, confirmation and
retraction modes, delay likelihoods, and prediction targets have not changed.

### Integration and optimization tests

Use the FluSight data by location, including `US`:

1. Run a fast smoke matrix crossing AR/HSGP/SIR with lognormal/gamma/generalized
   gamma for both revised observation models.
2. Run five historical origins for all 53 locations, including US, at `H = 26`,
   using both calendar and compressed clocks.
3. Record for every fit: error, objective, optimizer code, maximum gradient,
   elapsed time, all finite-parameter checks, `q_C` range, terminal retention,
   projection count, and prediction finiteness.
4. Fail the gate for any execution/domain failure, leakage assertion failure,
   non-finite objective/parameter/prediction, or maximum gradient above `0.1`
   after polishing.
5. Compare both models with the empirical multiplier using only origin-safe
   calibration cohorts. Report WIS/MAE/coverage as diagnostics, not as proof of
   calibrated uncertainty.

Keep the large all-location runner in `devel/`; do not put a 1,000-fit sweep in
ordinary CRAN tests. Add a small deterministic subset to `tests/testthat/` and
document the command for the full gate.

## Implementation order

Make the migration in small, reviewable stages:

1. Record the pre-existing dirty-worktree state and do not overwrite unrelated
   edits.
2. Add plain-R math helpers and mathematical tests.
3. Add RTMB densities/objective blocks and AD tests.
4. Add the dedicated public configuration and priors.
5. Replace count-cumulative preparation and masks.
6. Replace fitting, reconstruction, and prediction.
7. Update parameter summaries, diagnostics, backtesting, prior-only, and
   serialization.
8. Replace vignettes, roxygen, generated help, and NEWS.
9. Run focused tests, then the full package test suite and `R CMD check`.
10. Run the FluSight smoke matrix and full all-location/five-origin/two-clock
    stability gate.
11. Create the independent Claude verification file described below.

At each stage, search for old cumulative terminology and callers. Do not leave
both old and new objective paths reachable for the same data/model combination.

## Required independent verification file

After the code and documentation changes are complete, create a **separate**
file:

```text
devel/CLAUDE_VERIFY_COUNT_CUMULATIVE_INTEGRATION.md
```

This is not a copy of this plan and must not be written before the implementation
results exist. It is an evidence packet and adversarial review prompt for Claude.
Populate it with actual values, paths, commands, and outcomes; do not leave
placeholders.

The verification file must contain:

1. **Scope and diff inventory:** every added, modified, renamed, and deleted file,
   plus a short reason for each. Distinguish pre-existing user changes from the
   integration changes.
2. **Equation-to-code map:** manuscript equation/section, implementation function,
   and test name for `h_R`, `S_R`, `q_C`, `alpha`, `omega`, the cumulative
   marginals, the ZTNB inverse-mean parameterization, the hurdle pmf, and
   reconstruction.
3. **Old-path removal audit:** results of repository searches for the old
   cumulative `p`/`g_C`, Skellam/SkNB, `is_confirmation`, `confirm_p`, and old
   reconstruction names. Explain every remaining hit.
4. **Data-leakage audit:** exact assertions and tests showing that fitting and the
   empirical baseline read no post-`now` information. Include one mutation test
   where retrospective truth is changed after `now` and the origin result remains
   unchanged.
5. **Optimization evidence:** commands and summary tables for the smoke matrix and
   the 53-location/five-origin/two-clock run, including optimizer codes, gradient
   distribution, failures, timing, and any refits.
6. **Test evidence:** exact commands and outcomes for focused tests, the full
   testthat suite, vignette builds, and `R CMD check`. Include warnings and skips;
   do not summarize a warning-producing run as simply passing.
7. **Prediction comparison:** origin-safe empirical baseline versus both revised
   models, separated by clock and with the finite horizon stated.
8. **Compatibility evidence:** targeted results for linelist and count-incidence,
   save/load, `update()`, `backtest()`, and prior-only behavior.
9. **Known limitations:** composite-likelihood dependence, uncertainty
   calibration, finite-horizon tail assumption, unsupported delays, negative-total
   projection, and any remaining numerical warnings.
10. **Instructions to Claude:** independently inspect the diff and rerun a
    representative subset; do not trust the reported conclusions without tracing
    equations to code. Ask Claude to return `PASS`, `PASS WITH REQUIRED CHANGES`,
    or `FAIL`, with file/line references and reproducible evidence for every
    objection.

End that file with this checklist for Claude:

```text
[ ] No post-now data leakage
[ ] h_R is primitive in the cumulative likelihood
[ ] p is not separately estimated or reported for cumulative data
[ ] ZTNB is indexed by its own mean through Psi^{-1}
[ ] E[Delta] = alpha - omega is preserved
[ ] H is configurable and serialized
[ ] current C_t(d*) anchors the operational reconstruction
[ ] unobserved cells are masked rather than scored as zero
[ ] AR, HSGP, and SIR tape and differentiate
[ ] linelist and count-incidence behavior is unchanged
[ ] old count-cumulative likelihood is unreachable
[ ] optimization and gradient gates pass
[ ] docs describe composite rather than exact likelihoods
```

Claude should review the implementation, not edit it in the same pass. Put any
subsequent fixes in a new commit/diff and update the evidence file with the rerun
results so the audit trail remains legible.

## Definition of done

The integration is complete only when:

- both revised count-cumulative models are selectable through `model()` and work
  through `nowcast()`, `predict()`, `update()`, `backtest()`, prior-only, and
  save/load;
- the old cumulative fixed-`p` Skellam/SkNB path is unreachable;
- the finite settlement horizon is configurable, defaults to 26 steps, and is
  present in printed output and serialized objects;
- as-of preparation and the empirical baseline pass mutation-based leakage tests;
- calendar and compressed FluSight examples are documented with zero completion;
- the mathematical, AD, regression, and package checks pass;
- all locations including US complete at five origins under the required
  stability gate; and
- `devel/CLAUDE_VERIFY_COUNT_CUMULATIVE_INTEGRATION.md` contains the completed,
  reproducible evidence packet for independent review.
