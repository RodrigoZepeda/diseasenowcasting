# Independent verification packet: count-cumulative integration

Date: 2026-09-05

This packet is intentionally adversarial. It records what was actually run and
does not treat the pre-existing prototype results as proof that the integrated
package is stable. The user additionally requested `hurdle_ztpoisson`; it is
included everywhere below as a third observation composite. Its positive
magnitude is a zero-truncated Poisson indexed by its own mean
`(alpha + omega) / pi`, with no magnitude-dispersion parameter.

## 1. Scope and dirty-worktree inventory

The worktree was already heavily dirty when this integration began. There was
no clean baseline commit or saved patch from which line-level authorship could
be reconstructed, so no claim of exclusive ownership is made for overlapping
files. Existing changes were preserved; no reset, checkout, or bulk revert was
used.

Integration-specific additions:

- `R/04_count_cumulative_class.R`: dedicated public S7 configuration, including
  `cumulative`, `hurdle_ztnb`, and `hurdle_ztpoisson`.
- `R/17_count_cumulative_data.R`: as-of zero completion, masks, signed updates,
  and previous-movement indicators.
- `R/28_count_cumulative_likelihood.R`: finite-horizon kernels, cumulative
  marginals, own-mean ZTNB/ZTP inversions and hurdle laws.
- `tests/testthat/test-count-cumulative-{class,data,math,objective,workflows}.R`:
  focused public, mathematical, AD, leakage, and workflow tests.
- `devel/smoke_count_cumulative_integration.R` and
  `devel/count_cumulative_smoke_results.csv`: production-objective AD smoke
  matrix and actual results.
- `devel/run_count_cumulative_integration_gate.R` and
  `devel/count_cumulative_integration_results/`: checkpointed production-API
  FluSight gate and the executed Texas results.
- `man/count_cumulative_process.Rd`: generated public help.
- This verification packet.

Tracked files with integration edits, some overlapping pre-existing work:

- `R/05_model_class.R`, `R/07_default_priors.R`, `R/09_prepare_data.R`,
  `R/12_fit.R`, `R/14_objective_joint.R`, `R/15_nowcast.R`,
  `R/17_prepare_from_tblnow.R`, `R/18_collect_fits.R`, `R/20_nowcast_class.R`,
  `R/21_nowcast_methods.R`, `R/22_update.R`, `R/23_backtest.R`,
  `R/26_nowcast_diagnostic.R`, `R/27_parameters.R`, `R/30_print.R`,
  `R/32_prior_only.R`, `R/34_save_load.R`, and
  `R/diseasenowcasting-package.R`: route the dedicated component through
  preparation, priors, objective, fitting, prediction, update, backtest,
  diagnostics, parameters, prior-only and serialization. `R/12_fit.R` now
  applies guarded polish to both Laplace and hurdle-MAP fits when required.
- `R/28_confirmation_likelihood.R` and
  `tests/testthat/test-confirmation.R`: keep numerical legacy Skellam helpers
  tested, but not reachable from production count-cumulative preparation.
- `tests/testthat/test-prior-only.R`, `tests/testthat/test-save-load.R`,
  `tests/testthat/test-censoring-surprise-backtest.R`, and
  `tests/testthat/test-coverage-gaps.R`: migration/regression coverage; censoring
  calls use `tbl.now::censor_reporting_delays_above()`.
- `README.md`, `NEWS.md`, `vignettes/Mathematics.Rmd`,
  `vignettes/Validation_processes.Rmd`,
  `vignettes/Handling_Outlier_Delays_with_Censoring.Rmd`, and
  `vignettes/introduction.Rmd`: replace old cumulative claims, distinguish
  historical origin from retrospective truth, and document both clocks.
- `NAMESPACE`, `man/model.Rd`, `man/nowcast.Rd`, `man/prepare_data.Rd`,
  `man/backtest.Rd`, and `man/load_nowcast.Rd`: regenerated/updated interface
  documentation.

Pre-existing dirty changes deliberately not attributed to this integration:

- `.github/workflows/R-CMD-check.yaml`, `DESCRIPTION`, `SKILL.md`, `R/10_delay.R`.
- Rename `R/04_confirmation_class.R` to `R/04_validation_class.R`; rename
  `R/27_tidy.R` to `R/27_parameters.R`; deletion of
  `REVISION_CONFIRMATION_MODEL.md`, `man/confirmation_process.Rd`,
  `man/tidy.Rd`, and `diseasenowcasting-tidy-spec.md`.
- `R/31_retraction_likelihood.R`, `devel/benchmark_retraction.R`,
  `tests/testthat/helper-retraction.R`, validation/retraction/resolution test
  files, and the validation vignette were part of the separate dirty validation
  migration. Where integration required touching an overlapping file, the
  existing validation behavior was retained and covered by the full suite.
- Other pre-existing untracked research inputs remain under `devel/`, including
  the briefing/handoff/plan documents, prototype directory, presentations,
  SPA diagnostics, validation benchmarks, and manuscript `.tex` files.

Run `git status --short` for the authoritative current inventory. Because the
initial dirty state was not committed, a reviewer must inspect overlapping
diffs rather than accepting the categories above on trust.

## 2. Equation-to-code-to-test map

| Contract | Implementation | Direct evidence |
|---|---|---|
| `h_R(l)=mass*g_R(l)`, `S_R(a)=1-sum_{l<=a}h_R(l)` | `.count_cumulative_components()` in `R/28_count_cumulative_likelihood.R` | “finite-horizon retraction and cumulative kernels satisfy their contract” |
| `q_C(d)=sum_r g_D(r)S_R(d-r)` | `.count_cumulative_components()` | same test, including plain-R convolution |
| `alpha=mu*g_D`, `omega=mu*sum g_D*h_R`; `omega(0)=0` | component construction plus count block in `R/14_objective_joint.R` | kernel test; “hurdle movement ... expected law” |
| `C_t(d)` Poisson/NB with mean `mu*q_C(d)` | `.count_cumulative_level_logpmf()` and objective dispatch | “cumulative Poisson and NB log masses ...”; objective finite-gradient test |
| ZTNB own-mean `Psi_size^{-1}` | `.ztnb_parent_mean()`, `.ztnb_logpmf()` | inverse, normalization, tape derivative, and Monte Carlo mean tests |
| ZTP own-mean `m/(1-exp(-m))` inverse, no dispersion | `.ztpoisson_parent_mean()`, `.ztpoisson_logpmf()` | “zero-truncated Poisson is indexed by its own mean ...”; ZTP tape test; objective parameter-absence test |
| `pi=(1-exp(-total))*plogis(eta)` and sign `alpha/total` | `.count_cumulative_movement_probability()`, `.hurdle_ztnb_update_logpmf()`, `.hurdle_ztpoisson_update_logpmf()` | branch/admissibility and both-law signed-mean tests |
| `E[Delta]=alpha-omega` | own-mean magnitude `total/pi` in both hurdle densities/samplers | “both hurdle magnitude laws preserve the signed-update mean” |
| `C_t(H)=C_t(d*)+sum Delta` | `.draw_count_cumulative_terminal()` in `R/15_nowcast.R` | anchored future-update mean test and workflow prediction assertions |

The source equations are the count-cumulative section of
`main_identifiability_update.tex`. `hurdle_ztpoisson` is the user-requested
extension: it changes only the conditional magnitude family and deliberately
introduces no dispersion parameter.

## 3. Old-path reachability audit

Search command:

```sh
rg -n "is_confirmation|increment_array|confirmation|Skellam|SkNB|confirm_p|g_C|addition_mean|retraction_mean" R tests/testthat README.md NEWS.md vignettes
```

The search returned 783 lines, classified as follows:

- Production count-cumulative dispatch uses `is_count_cumulative`, explicit
  level/update arrays and `count_cumulative_observation`; its priors are disjoint
  from `confirm_p`.
- `prepare_data(is_confirmation=TRUE)` aborts in `R/09_prepare_data.R` with a
  targeted fixed-`p` removal error.
- If a handcrafted engine bypasses that boundary, `build_joint_obj()` aborts
  immediately when `data$is_confirmation == 1` in `R/14_objective_joint.R`,
  before the old objective branch is taped. Thus no production cumulative call
  can reach the old fixed-`p` Skellam/SkNB block.
- Most remaining `confirm_p`/`g_C` hits are the supported linelist
  validation/retraction model in `R/04_validation_class.R`,
  `R/31_retraction_likelihood.R`, and its tests. Those semantics must remain.
- `R/28_confirmation_likelihood.R` and its numerical tests retain research
  Skellam/SkNB helpers. They have no production count-cumulative caller.
- Dead legacy fields/branches remain in `R/09_prepare_data.R`,
  `R/14_objective_joint.R`, `R/15_nowcast.R`, and the injected-global list for
  backward diagnostic/readability reasons. They are behind the two hard aborts.
  Deleting this large block in the dirty validation migration would add risk;
  Claude should verify unreachability and may recommend a separate cleanup.

## 4. Data-leakage audit

`R/17_prepare_from_tblnow.R` trims to `event_date <= now` and
`report_date <= now` before deriving count-cumulative stratum levels, the event
origin, temporal matrices, covariates, priors or engine arrays.
`.prepare_count_cumulative_as_of()` completes only the observable as-of
triangle and carries an explicit observation mask; future array cells remain
masked. `flusight_asof_data.R::empirical_multiplier_draws()` uses only earlier
cohorts having both age-`a` and horizon-`H` observations in the same as-of panel.

Direct tests:

- `test-count-cumulative-data.R`: metadata/zero completion, date assertions,
  observed-zero versus future-mask distinction, post-origin input mutation,
  both clocks, and 2023->2024->2025 compressed-clock continuity.
- `test-count-cumulative-workflows.R`: adds 100,000 to every terminal value with
  report date after the origin and verifies identical prepared engines, default
  priors, fitted parameter lists and seeded predictions.
- The production gate records `latest_report_date <= origin`, maximum event date
  at or before origin, and maximum report date at or before origin. The executed
  Texas/two-clock subset passed 8/8 fit-level leakage records.

## 5. Optimization evidence

AD/tape smoke command:

```sh
Rscript devel/smoke_count_cumulative_integration.R
```

Result: 27/27 finite objectives and gradients for
AR/HSGP/SIR x lognormal/gamma/generalized-gamma x
cumulative/hurdle-ZTNB/hurdle-ZTPoisson. Total tape/evaluation time recorded in
the CSV was 10.621 seconds. The maximum absolute gradient at the unoptimized
initial parameter vector was 3511.379; this is an AD/domain smoke value, not a
convergence result.

Executed production subset:

```sh
LOCATIONS=Texas NOWS=2025-05-31 N_DRAWS=20 RUN_TAG=texas-both-polish \
  Rscript devel/run_count_cumulative_integration_gate.R
```

This made 8 public-API fits: two clocks x cumulative Poisson, cumulative NB,
hurdle-ZTNB and hurdle-ZTPoisson. All 8 had optimizer code 0, finite parameters,
finite objectives, finite predictions, and gradients at or below 0.1. Maximum
gradient was 0.008858; total fit time was 44.386 seconds. Cumulative fits used
Laplace random effects (4/4); hurdle fits used joint MAP (4/4). There were no
runtime failures. Five projected-negative terminal values occurred in the
hurdle-ZTNB predictions (two calendar, three compressed); projection is exposed
in diagnostics.

An earlier compressed ZTP fit returned a finite solution with gradient 0.1528.
That exposed that guarded polishing was restricted to Laplace fits. Extending
the same bounded/no-worse-objective polish to hurdle MAP changed the repeated
gradient to 0.005512 with no warning. This is the reason for the `R/12_fit.R`
change; it is not hidden from the record.

Full production gate command:

```sh
Rscript devel/run_count_cumulative_integration_gate.R
```

This defaults to all 53 locations (including US), five origins, both clocks,
`H=26`, 250 draws and all four observation variants, with per-job checkpoints,
monotonic ETAs, and a nonzero exit if any gate condition fails.

The default 530-job/2,120-fit production run subsequently completed after the
log-scale PMF repair. All 2,120 fits and predictions were finite and all 2,120
leakage assertions passed. Cumulative Poisson/NB and hurdle-ZTNB passed the
optimizer/gradient gate in all 1,590 cases. Compressed-clock hurdle-ZTPoisson
passed 265/265 (maximum gradient 0.0632). Calendar hurdle-ZTPoisson passed
260/265; the five warning fits were US at every historical origin, optimizer
code one, with maximum gradients 0.552, 1.749, 2.877, 9.676, and 4.837. The user
chose to proceed with explicit warnings and the stable hurdle-ZTNB alternative.
Accordingly, this is complete execution evidence but not an unconditional
optimizer-gate pass for hurdle-ZTPoisson.

## 6. Test, vignette and package-check evidence

Focused command:

```sh
Rscript -e 'testthat::test_local(filter="count-cumulative", reporter="summary")'
```

Result: exit 0, `DONE`, no focused failures or warnings. It includes the five
new focused files and all three observation composites.

Installed full suite was run by:

```sh
R CMD build /Users/rodzepeda/Documents/dcast3
R CMD check diseasenowcasting_2.2.0.tar.gz --no-manual
```

Result: `Status: OK`; 1,237 assertions passed, 0 failed, 6 warnings, 55 skips.
The 55 skips were 51 `skip_on_cran()` integration/real-data cases and four
load-all-only custom-dispatch skips. The six test warnings were expected but
not suppressed: five backtests dropped one too-recent evaluation date (17--20
units from the last report), and one `tbl.now` construction warned that 832
rows shared an event/report cell because undeclared `race` values were pooled.
The dependency check also printed repository-index access warnings for CRAN and
Bioconductor because network access was restricted, but did not produce a check
WARNING/NOTE. Static code, documentation, examples, installed tests and rebuilt
vignettes were all `OK`.

Direct renders of `vignettes/Mathematics.Rmd`,
`vignettes/Validation_processes.Rmd`, `vignettes/introduction.Rmd`, and
`vignettes/Handling_Outlier_Delays_with_Censoring.Rmd` each exited 0.
`devtools::build_vignettes()` separately failed in local `pak` metadata helper
`res_one_row_df`; the authoritative `R CMD build` and `R CMD check` vignette
builds succeeded. `git diff --check` exited 0.

Initial pre-integration full tests had three failures: two tests still called
the renamed censor helper and one temporal-effect matrix was coerced through a
mixed character matrix. Calls now use
`tbl.now::censor_reporting_delays_above()`, and temporal columns are converted
individually. The final installed suite above is the post-fix evidence.

## 7. Origin-safe prediction comparison

These are diagnostics from Texas at origin 2025-05-31, `H=26`, 20 draws and
five recent targets per clock. They are too small for a scientific ranking.

| Clock | Model | WIS | MAE | 90% coverage |
|---|---|---:|---:|---:|
| calendar | empirical multiplier | 9.80 | 9.8 | 0.2 |
| calendar | cumulative Poisson | 5.69 | 6.6 | 0.4 |
| calendar | cumulative NB | 2.19 | 2.9 | 1.0 |
| calendar | hurdle-ZTNB | 13.1 | 8.4 | 1.0 |
| calendar | hurdle-ZTPoisson | 9.02 | 9.8 | 0.2 |
| compressed | empirical multiplier | 9.80 | 9.8 | 0.2 |
| compressed | cumulative Poisson | 2.62 | 3.7 | 0.8 |
| compressed | cumulative NB | 3.65 | 5.0 | 0.4 |
| compressed | hurdle-ZTNB | 25.2 | 10.5 | 1.0 |
| compressed | hurdle-ZTPoisson | 9.20 | 9.8 | 0.4 |

The empirical calibration cohorts are selected only from the origin-specific
panel and require both current age and `H` values observable by that origin.
Coverage is descriptive pseudo-posterior coverage, not a calibration claim.

## 8. Compatibility evidence

- Linelist and count-incidence validation continue to own `p` and validation
  delays. Their confirmation, retraction, competing-risk and stratified tests
  are included in the 1,237-passing installed suite.
- `test-count-cumulative-workflows.R` exercises public `nowcast()`, `predict()`,
  `update()`, `backtest()`, prior-only, and save/load for hurdle-ZTPoisson.
- `test-confirmation.R` and `test-prior-only.R` were migrated so cumulative
  cases use the dedicated component while linelist validation assertions remain.
- Serialization preserves observation choice and `H`; same-seed predictions
  before and after load are equal. Update safely resizes stratum-specific
  initial values when a new stratum appears.
- `cumulative` fits select Laplace; both hurdle variants select MAP. The output
  estimand is `C_t(H)` and operational prediction is anchored at current
  `C_t(d*)`.

## 9. Known limitations and unresolved evidence

- Products over cumulative levels or update ages are composite likelihoods;
  their pseudo-posterior curvature is not automatically calibrated. No
  sandwich/Godambe or cluster-bootstrap correction is implemented.
- `h_R=mass*g_R` is a finite-horizon tail restriction. Database retention at
  `H` is not biological truth without an extra assumption.
- Retraction delays support lognormal, gamma and generalized gamma only.
- Anchored signed-update paths can become negative. Public count predictions
  project to zero and expose `negative_projection_count`; the executed subset
  observed five such ZTNB projections.
- The cumulative-level anchored sequential reconstruction is explicitly named
  an independent signed-Poisson update approximation; it is not the direct
  unconditioned terminal marginal.
- Dead legacy fixed-`p` code remains physically present behind hard aborts.
- The all-location production sweep is complete, but five US/calendar
  hurdle-ZTPoisson fits failed the optimizer/gradient gate. They are returned
  only with a targeted warning that reports the optimizer code/message and
  recommends hurdle-ZTNB; they must not be described as stable fits.

## 10. Instructions to Claude

Review only; do not edit in the same pass. Independently inspect the complete
dirty diff and rerun a representative subset. Do not trust this packet without
tracing every equation to the production objective, reconstruction and tests.
Verify the two hard aborts really make the old count-cumulative fixed-`p` path
unreachable while leaving linelist/count-incidence validation functional.
Specifically verify that `hurdle_ztpoisson` uses conditional magnitude own mean
`(alpha + omega)/pi`, performs the truncated-Poisson parent-mean inversion on
the RTMB tape, and carries no magnitude dispersion. Inspect the five
US/calendar hurdle-ZTPoisson optimizer/gradient warnings before granting an
unconditional pass.

Return exactly one of `PASS`, `PASS WITH REQUIRED CHANGES`, or `FAIL`, followed
by file/line references and reproducible evidence for every objection. Given
the five hurdle-ZTPoisson fits that failed the optimizer/gradient gate, `PASS`
is not justified unless the documented model-specific warning limitation is
explicitly accepted.

[ ] No post-now data leakage
[ ] h_R is primitive in the cumulative likelihood
[ ] p is not separately estimated or reported for cumulative data
[ ] ZTNB is indexed by its own mean through Psi^{-1}
[ ] ZTPoisson is indexed by its own mean and has no dispersion parameter
[ ] E[Delta] = alpha - omega is preserved
[ ] H is configurable and serialized
[ ] current C_t(d*) anchors the operational reconstruction
[ ] unobserved cells are masked rather than scored as zero
[ ] AR, HSGP, and SIR tape and differentiate
[ ] linelist and count-incidence behavior is unchanged
[ ] old count-cumulative likelihood is unreachable
[ ] optimization and gradient gates pass
[ ] docs describe composite rather than exact likelihoods
