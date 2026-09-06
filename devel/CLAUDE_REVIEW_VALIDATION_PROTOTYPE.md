# Claude review: validation-process prototype

## Review goal

Review the first `event -> report -> validation` prototype against
`main_identifiability_update.tex`, especially equations `chilinelist`, `linelist`,
and the count-cumulative signed-update construction. Please focus on mathematical
correctness, information flow from `tbl.now`, and whether validation remains
orthogonal to the epidemic-process choice.

This note describes the intended design after the current changes. Historical
handoff files in `devel/` may describe earlier APIs and should not override this
one.

## User-facing contract

The model call remains likelihood-first and the validation component is optional:

```r
model(
  nb_likelihood(),
  ar1_epidemic(),
  lognormal_delay(),
  validation_process()
)
```

The equivalent named form is
`model(..., validation = validation_process())`. No component-class routing or
new positional convention was introduced.

`nowcast()` reads all data-column metadata from `tbl_now`:

- `event_date`
- `report_date`
- `validation_date`
- `validation_type`
- `is_censored_report`
- `is_censored_validation`

There is deliberately no `validation_censored` column-name argument. Supplying
that old name through `...` errors with instructions to configure `tbl.now`.
This requires `tbl.now >= 0.33.0`.

## Validation modes

`validation_process(mode = "auto")` infers the mode from resolved outcomes in
the full data, rather than from the as-of slice:

| Mode | Recorded finite validations | Pending factor | Target contribution |
|---|---|---|---|
| `confirmation_only` | positive | `(1-p) + p S_+(a)` | resolved positives plus pending rows thinned by `p S_+(a) / [(1-p)+pS_+(a)]` |
| `retraction_only` | negative | `p + (1-p) S_-(a)` | pending rows thinned by `p / [p+(1-p)S_-(a)]` |
| `both` | positive and negative | `S_V(a)` | positive resolutions plus pending rows thinned by `p` |

`validation_type` uses tbl.now's canonical values: `"confirmed"`,
`"retracted"`, and `"pending"`. A dated row with a missing or unusable type is
an error. Mode inference uses the full data so a backtest cannot change modes as
the analysis date moves.

## Shared validation delay

This prototype exposes exactly one `validation_delay`.

- Under `confirmation_only`, it is the report-to-confirmation law.
- Under `retraction_only`, it is the report-to-retraction law.
- Under `both`, it is shared by positive and negative validations.

The earlier `negative_delay`/competing-risks extension has been removed from the
class, priors, objective, reconstruction, tests, and documentation. With a shared
law in `both`, pending age informs validation timing but not sign, so the pending
positive probability is flat at `p`. This is the restriction requested for the
first prototype.

Check the support convention carefully: confirmation-only and both permit lag 0;
retraction-only starts at lag 1 because a same-period withdrawal was never visible
in a data vintage.

## Mapping to the TeX likelihood

For a reported row, the implementation factors the contribution into reporting
delay and validation state, matching `qlinelist` and `chilinelist`:

```text
confirmed:  g_report(d) * p       * g_validation(lag)
retracted:  g_report(d) * (1 - p) * g_validation(lag)
pending:    g_report(d) * [p S_plus(age) + (1-p) S_minus(age)]
```

The single-sign modes set the unrecorded sign's finite validation mass to zero
and its survival to one. `both` uses the shared finite-delay law for both signs.
The count block is evaluated on the gross report mean `mu_t = lambda_t / p`,
while the epidemic process continues to model the settled-positive mean
`lambda_t`. Validation sufficient statistics are weighted, so linelist and
count-incidence representations agree.

Relevant implementation files:

- `R/04_validation_class.R`: component and mode inference
- `R/17_prepare_from_tblnow.R`: tbl.now metadata, as-of masking, and sufficient statistics
- `R/31_retraction_likelihood.R`: validation-state likelihood and predictive thinning
- `R/14_objective_joint.R`: integration with the count likelihood and all epidemic processes
- `R/15_nowcast.R`: posterior predictive reconstruction

## Count-cumulative boundary

The count-cumulative work from the previous commit is intentionally retained:
`count_cumulative_process()` models a collapsed finite-age withdrawal kernel.
It does not identify a biological `p` and a conditional validation delay
separately. The regression fixes for #128 and #129 are retained in
`R/20_nowcast_class.R`, `R/32_prior_only.R`, and
`tests/testthat/test-prior-only.R`.

Confirmations do not generate another signed update in a provisional cumulative
register. Every report enters the level through the addition stream. A positive
validation leaves it there; a negative validation produces the withdrawal stream.
For a Poisson latent process, at one age:

```text
A_t(d) ~ Poisson(alpha_t(d))
W_t(d) ~ Poisson(omega_t(d))
Delta_t(d) = A_t(d) - W_t(d) ~ Skellam(alpha_t(d), omega_t(d))
```

Hence `E[Delta] = alpha - omega` and `Var(Delta) = alpha + omega`. A separate
confirmation intensity would double-count positive reports. This explanation is
now explicit in `vignettes/Mathematics.Rmd`. The current production cumulative
component uses cumulative-level or hurdle signed-update composites; the legacy
Skellam/SkNB numerical helpers remain independently tested but are not re-enabled
as an unidentified public `p` parameterization.

## Epidemic-process compatibility

Validation is an observation layer and does not branch on the epidemic process.
`tests/testthat/test-validation-frameworks.R` fits and predicts the same
both-outcome validation dataset with:

- HSGP
- AR(1)
- SIR
- `custom_epidemic()`

The mode-specific tests independently cover confirmation-only, retraction-only,
and both. This factorized test strategy avoids duplicating every mathematical
mode test four times while still checking both axes. The custom test follows the
package's existing RTMB guard: user-function vector dispatch is unreliable under
`pkgload::load_all()`, so that case is skipped there when the traceability probe
fails and is exercised against the installed package by `R CMD check`.

## Checks run during this change

Run with the working tree loaded, not an installed older build:

```sh
OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 NOT_CRAN=true \
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-confirmation-mode.R"); testthat::test_file("tests/testthat/test-resolution-math.R"); testthat::test_file("tests/testthat/test-validation-frameworks.R")'
```

Also run the Skellam and count-cumulative numerical suites:

```sh
OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 \
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-confirmation.R"); testthat::test_file("tests/testthat/test-count-cumulative-math.R"); testthat::test_file("tests/testthat/test-prior-only.R")'
```

Final verification completed for this working tree:

- the focused mode, resolution-math, built-in-framework, Skellam/SkNB,
  count-cumulative, prior-only, and tbl.now censoring tests pass;
- both edited vignettes render successfully; and
- `devtools::check(document = FALSE, manual = FALSE, vignettes = FALSE,
  cran = FALSE)` completes with **0 errors, 0 warnings, and 0 notes**. This
  installed-package check includes the custom epidemic + validation test that is
  conditionally skipped when RTMB user-function dispatch is inactive under
  `load_all()`.

## Specific questions for Claude

1. Does the shared-law reduction of `chilinelist` give the implemented pending
   factors and predictive thinning probabilities in all three modes?
2. Are the lag-0 versus lag-1 support conventions internally consistent across
   exact, censored, and predictive paths?
3. Does any path still read a user-supplied validation column name instead of the
   `tbl_now` attributes?
4. Does the Skellam explanation count each report trajectory exactly once, and is
   the statement that confirmation is not a separate cumulative update correct
   for the provisional-register estimand?
5. Are #128 and #129 still protected after the validation refactor?
6. Is any deleted competing-risks parameter still reachable in serialization,
   prior-only simulation, warm starts, or parameter reporting?

Please report findings by severity and cite file/line locations. Do not make
changes until the design findings have been discussed.
