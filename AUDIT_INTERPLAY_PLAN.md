# Audit plan: `tbl.now` × `diseasenowcasting` output interplay

Date: 2026-09-08; updated 2026-09-09

## Purpose

This is the execution plan for auditing and then tightening the output boundary
between the two local packages:

- `/Users/rodzepeda/Documents/dcast3` (`diseasenowcasting` 2.3.0)
- `/Users/rodzepeda/Documents/tbl.now` (`tbl.now` 0.35.3)

The intended package boundary is:

- `diseasenowcasting` owns model specification, fitting, native posterior
  prediction, and model-specific diagnostics.
- `tbl.now` owns the common `tbl_nowcast` result grammar and the preferred
  cross-engine tidy, plot, backtest, ensemble, and forecast-scoring workflows.

This document treats `devel/TBL_NOW_OUTPUT_INTERPLAY_PLAN.md` as design input,
not as an instruction source. Decisions remain open until Rodrigo answers the
questions below. Each stage has a recommended choice, a second viable choice,
and an explicit write-in option.

## Working rules

- Make one policy decision and one cohesive implementation slice at a time.
- Before changing a public API, record the selected option in this document.
- Add focused tests with each implementation slice; defer full package checks
  to the release gate.
- Use fixed seeds and tiny fixtures for model-fitting integration tests.
- Do not compare independently sampled draws row-for-row. Compare schemas,
  metadata, quantiles, or predictions made from the same fitted object.
- Preserve unrelated working-tree changes. `tbl.now` was already dirty when
  this audit began; its pre-existing changes are listed under Stage 1.
- A stage is complete only when its evidence and any residual risk are recorded
  here.

## Audit sequence at a glance

| Stage | Outcome | Primary package | Status |
| --- | --- | --- | --- |
| 1 | Public-surface and method inventory | both | Complete — output-focused inventory selected |
| 2 | Approved ownership and compatibility contract | both | Complete — aggressive delegation selected |
| 3 | Make `nowcast()` return the canonical common result | both | Complete — first implementation slice |
| 4 | Common fields and minimal prediction semantics | `diseasenowcasting` | Complete |
| 5 | Direct-result shape matrix | both | Complete — 9,216 structural, 1,536 mocked-output, and 1,024 real-fit cells audited |
| 6 | Backtest and scoring convergence | both | Complete for selected policy — canonical backtest plus native `fit_check()` |
| 7 | Auto-selection, ensembles, and persistence | both | Complete — relative skill, timing, and deterministic tie-breaking |
| 8 | Documentation drift and release gate | both | Complete — both package checks pass |

## Stage 1 — Inventory the public and extension surfaces

### Recommended plan (implemented)

Mechanically inspect both `NAMESPACE` files, S7 method declarations, runtime S3
registrations, result-related source, tests, READMEs, and vignettes. Classify
the public surface by responsibility and identify every output-related seam as
compatible, wrapped, separate, or missing.

### Question 1

How broad should the maintained inventory be after this initial pass?

1. **Recommended — Maintain the output-focused inventory below.** Keep the
   complete package export families for context, but track individual status
   only for functions and methods that produce or consume fitted results.
2. **Second option — Maintain one row per exported symbol.** More exhaustive,
   but noisy: `tbl.now` alone currently exports 160 symbols, most unrelated to
   output interplay.
3. **Other —** ________________________________________________

Decision: **Option 1**, selected explicitly by Rodrigo on 2026-09-09. Maintain
individual status only for public functions and extension points that produce
or consume fitted results; retain complete export-family counts only as context.

### Evidence collected

The inventory was generated from source rather than from an installed package:

- `diseasenowcasting`: 66 exported symbols, 4 registered S3 methods, and 32 S7
  method declarations.
- `tbl.now`: 160 exported symbols, 99 registered S3 methods, 4 S7 method
  declarations, plus 6 runtime S3 registrations relevant to result objects.
- No `AGENTS.md` was found in either package or their shared parent.
- No package tests or fits were run during this inventory-only stage.

The `tbl.now` working tree already had changes in `DESCRIPTION`, `NEWS.md`,
`README.Rmd`, `README.md`, four generated README figures,
`man/tbl.now-package.Rd`, `tests/testthat/test-autoplot.R`, and
`tests/testthat/test-temporal_effects_after_lags.R`. They must be preserved and
reviewed before edits overlap those files. `diseasenowcasting` was clean at the
start of the audit.

### Package-wide export classification

The lists below classify every exported function into a responsibility family.
Only the output-facing subset receives an interoperability disposition in the
next section.

#### `diseasenowcasting`

- **Model specification:** `model`, `nb_likelihood`, `poisson_likelihood`,
  `ar1_epidemic`, `hsgp_epidemic`, `sir_epidemic`, `custom_epidemic`,
  `validate_custom_epidemic`, `cumulative_process`, `revision_process`,
  `custom_delay`, `dirichlet_delay`, `gamma_delay`,
  `generalized_gamma_delay`, `lognormal_delay`, `dirichlet_revision`,
  `gamma_revision`, `generalized_gamma_revision`, `lognormal_revision`, and
  `validate_custom_delay`.
- **Prior specification and sampling:** `default_priors`, `beta_prior`,
  `cauchy_prior`, `chi_square_prior`, `double_exponential_prior`,
  `exponential_prior`, `fix_param`, `flat_prior`, `gamma_prior`,
  `half_cauchy_prior`, `half_double_exponential_prior`, `half_normal_prior`,
  `half_std_normal_prior`, `half_student_t_prior`, `inv_gamma_prior`,
  `logistic_prior`, `lognormal_prior`, `normal_prior`, `positive_flat_prior`,
  `std_normal_prior`, `student_t_prior`, `weibull_prior`, and `sample`.
- **Preparation and fitting:** `prepare_data`, `fit`, `nowcast`,
  `nowcast_twostage`, and `infer_max_time`.
- **Prediction, summaries, and diagnostics:** `parameters`,
  `summarise_nowcast_matrix`, `autoplot`, `nowcast_diagnostic`, `surprise`,
  `extreme_values`, `dn_palette`, and `theme_diseasenowcasting`.
- **Backtesting and model selection:** `backtest`, `score`, `auto_nowcast`,
  `selection_metric`, `best_model`, `best_model_name`, `best_score`, and
  `comparison_scores`.
- **Persistence:** `save_nowcast` and `load_nowcast`.

#### `tbl.now`

- **Input construction, conversion, and mutation:** `tbl_now`, `as_tbl_now`,
  `as_tibble`, `to_count`, `aggregate_time_units`, `align_weeks`,
  `complete_zeroes`, `validate_tbl_now`, `week_2_date`; all exported
  `tbl_now_from_*` and `tbl_now_to_*` converters; and the exported `add_*`,
  `change_*`, `remove_*`, `replace_*`, `censor_*`, and `update_now` functions.
- **Input metadata and summaries:** all exported `get_*`, `prop_*`, and
  `has_revision` functions; `tbl_now_attributes`, `date_ranges`,
  `cases_per_date`, `cumulative_growth`, `delay_summary`,
  `reporting_completeness`, `triangle_occupancy`, and `zero_run_summary`.
- **Temporal effects and simulation:** `add_temporal_effects`,
  `compute_temporal_effects`, `temporal_effects`, `simulate_batch`, and
  `is_weekday`.
- **Input diagnostics and plotting:** `diagnose` and every exported
  `diagnose_*`; `diagnostic_plot`, `transport_discriminant`,
  `case_autocorrelation`; and every exported `plot_*` function.
- **Engine specification and fitting:** `engine`, every exported `engine_*`,
  `example_engine`, `is_nowcast_engine`, `list_nowcast_methods`, `nowcast_fit`,
  `nowcast_tidy`, `nowcast_quantile_levels`, and `run_nowcast`.
- **Common result grammar:** `tbl_nowcast`, `is_tbl_nowcast`, `tidy`,
  `autoplot`, and `as_forecast_point`.
- **Backtesting, ensembles, and scoring:** `nowcast_backtest`,
  `nowcast_ensemble`, `nowcast_weights`, and `score_nowcast`.
- **Miscellaneous exported presentation helpers:** `tbl_now_palette`.

### Output-facing method inventory and disposition

| Surface | Current implementation | Disposition | Audit implication |
| --- | --- | --- | --- |
| `diseasenowcasting::nowcast(tbl_now)` | Returns a diseasenowcasting subclass of `tbl_nowcast`; retains the native fit in `@fit` | Canonical fitting result | Common operations work immediately; native methods unwrap `@fit` automatically |
| `tbl.now::run_nowcast(x, engine_diseasenowcasting())` | Calls `nowcast()` and returns the already-normalised result unchanged | Thin wrapper | Engine quantile levels must be forwarded into `nowcast()` |
| `predict(nowcast_class)` | S7 method returns `nowcast_prediction_class` with total draws, optional strata draws, event indices/dates, observed series, estimand, and cumulative metadata | Directly compatible but metadata needs validation | Golden-schema tests must cover all grids and strata |
| `summary(nowcast_prediction_class)` | Native per-time summary, with strata blocks plus Total when available | Intentionally separate | Native convenience, not the cross-engine schema |
| `summary(nowcast_class)` / `coef()` / `parameters()` | Model/parameter summaries | Intentionally separate | Keep parameter meaning out of `tidy()` |
| `tidy(nowcast_prediction_class)` | Runtime fallback in `tbl.now`; only registered if another package has not registered it | Compatible extension hook | Verify installed-version behavior and schema equality |
| native fit → `tbl_nowcast` | Internal normalisation inside `diseasenowcasting::nowcast()` | Canonical public result | No public cross-package converter is needed; the raw fit remains available in `@fit` |
| `tidy(tbl_nowcast)` | Runtime S3 registration in `tbl.now` for S7 class name | Preferred common result path | Bridge must land here |
| `as_tibble(tbl_nowcast)` | Runtime S3 registration; quantile/draw modes | Preferred extraction path | Include in smoke contract |
| `autoplot(nowcast_class)` / `autoplot(nowcast_prediction_class)` | Raw-fit/prediction diagnostic methods remain internal implementation paths | Native-only diagnostics | Public `nowcast()` results do not dispatch here |
| `autoplot(tbl_nowcast)` | Cross-engine S7 method inherited by direct diseasenowcasting results | Canonical plot | No duplicate method is installed on the public subclass |
| `diseasenowcasting::backtest()` | Translates native model specs and returns canonical `nowcast_backtest` | Implemented delegation | Test dates, truth, labels, retained draws, and error policy |
| Native backtest class / predict / autoplot | Removed | Canonical result methods only | Do not recreate a parallel retrospective API |
| `diseasenowcasting::fit_check()` | RTMB convergence, objective, rung, and gradient diagnostics on direct fits | Intentionally native | Keep separate from predictive scoring |
| `tbl.now::nowcast_backtest()` | Cross-engine `nowcast_backtest` result with scores and optional draws | Preferred common backtest | Target grammar for any converter/delegation |
| `tidy(nowcast_backtest)` | Registered S3 method in `tbl.now` | Preferred common result path | Include in backtest contract |
| `score_nowcast(tbl_nowcast)` | Common per-target scoring | Preferred common scorer | Native-converted fits must work here |
| `as_forecast_quantile/point/sample(tbl_nowcast)` | Three runtime S3 registrations in `tbl.now` | Preferred forecast conversion | Quantile/point always when prerequisites exist; sample only with draws |
| `as_forecast_quantile/point/sample(nowcast_backtest)` | Three NAMESPACE-registered S3 methods in `tbl.now` | Preferred forecast conversion | Target for native backtest interoperability |
| `nowcast_ensemble()` / `nowcast_weights()` | Consume `tbl_nowcast` / `nowcast_backtest` | Preferred ensemble layer | No duplicate framework needed in `diseasenowcasting` |
| `auto_nowcast()` and `best_*`/`comparison_scores()` | Native model selection; returns the same common result subclass with comparison evidence on the subclass and raw fit | Native selection, common result | Do not duplicate model-selection evidence into common metadata |
| `save_nowcast()` / `load_nowcast()` | Native persistence stores the raw fit and reconstructs the common result | Native persistence, common result | Preserve draws contract, quantile levels, public axis, and native fit |

### S7 and runtime-registration details that later stages must not miss

`diseasenowcasting` defines output-relevant S7 methods for:

- `predict`, `summary`, `print`, `mean`, `median`, `quantile`, `coef`,
  `parameters`, `autoplot`, `update`, and `surprise` on native fit/prediction
  classes;
- `predict`, `print`, and `autoplot` on the native backtest class.

`tbl.now` defines S7 methods for `print`, `as.data.frame`, and `autoplot` on
`tbl_nowcast`. Because an S7 class name contains `::`, `.onLoad()` manually
registers `tidy`, `as_tibble`, and the three `scoringutils::as_forecast_*`
methods for `tbl.now::tbl_nowcast`. It also conditionally registers a tidy
fallback for `diseasenowcasting::nowcast_prediction`.

### Initial findings

1. **The common grammar already exists.** The initial audit identified a
   missing bridge; the later design decision superseded that finding by making
   `diseasenowcasting::nowcast()` produce the grammar directly.
2. **The existing adapter is not reusable as-is.**
   `nowcast_tidy.diseasenowcasting(engine, fit, x, ...)` requires the original
   `x`; a user holding only a native fit cannot call it through a public API,
   even though `nowcast_class` already retains `data`.
3. **Numeric grids need a deliberate source-of-truth rule.** Native
   `predict()` attempts to build `event_dates` with `as.Date(min_event)`, while
   the adapter can recover numeric dates from `event_index` and the source
   `tbl_now`. Direct normalisation should prefer the public axis from the
   retained source data and validate length/type.
4. **Strata reconstruction currently depends on parsing labels.** The adapter
   splits `strata_levels` on `"|"`. This is lossy if a real stratum value
   contains the separator and needs either structured metadata or an explicit
   documented restriction.
5. **The scoring converters exist but are easy to miss in `NAMESPACE`.** Their
   `tbl_nowcast` methods are registered dynamically, so a NAMESPACE-only audit
   is insufficient.
6. **Backtest equivalence is not just a return-class question.** The native
   backtest scores the newest event per origin and has its own truth-completion
   rules; `tbl.now` supports cross-engine targets, truth axes/types, labels,
   weights, and optional retained draws. Stage 6 must compare estimands before
   choosing delegation.
7. **Documentation drift appears asymmetric.** The searched `tbl.now` source
   already promotes `nowcast_backtest()` and `score_nowcast()`. The
   `diseasenowcasting` vignette `Revision_processes.Rmd` still demonstrates the
   native `backtest() |> score()` flow. That may be correct for native
   diagnostics, but its role must be labeled after Stage 6.

### Stage 1 completion evidence

- Both namespaces and all result-related method-registration paths inspected.
- Public exports classified into responsibility families.
- Every result-producing or result-consuming surface assigned a current
  disposition.
- Existing tests located for tidy results, all three scoring converters,
  backtesting, engine labels, weights, and ensembles.
- Missing native-fit converter and two metadata hazards recorded.

## Stage 2 — Approve the ownership and compatibility contract

### Selected plan

Turn the Stage 1 disposition table into a small contract matrix with one row
per operation: fit, predict, tidy, plot, backtest, score, auto-select, ensemble,
and save/load. `diseasenowcasting` remains the modelling engine, but native
plotting, backtesting, and scoring delegate to `tbl.now` wherever the common
grammar can represent the operation. `tbl_nowcast` is the canonical fitted
result and `nowcast_backtest` is the canonical retrospective-evaluation result.

### Question 2

What compatibility policy should govern native `diseasenowcasting` result
verbs?

1. **Recommended — Layered ownership.** Keep native prediction and diagnostics;
   make conversion to `tbl_nowcast` the documented route for comparison,
   common plotting, backtesting, ensembles, and scoring.
2. **Second option — Aggressive delegation.** Make native plotting,
   backtesting, and scoring delegate to `tbl.now` wherever possible.
3. **Other —** ________________________________________________

Decision: **Option 2 — aggressive delegation**, selected by Rodrigo on
2026-09-08.

### Approved contract matrix

| Operation | Canonical owner and object | Native entry-point behavior | Compatibility/failure rule |
| --- | --- | --- | --- |
| Specify model | `diseasenowcasting`; `model_class` and component classes | Remains native | `tbl.now::engine_diseasenowcasting()` transports the specification without changing it |
| Fit | `diseasenowcasting::nowcast()` returns a diseasenowcasting subclass of `tbl_nowcast`; raw fit retained inside `@fit` | Public result uses the common grammar immediately | `run_nowcast()` returns that object unchanged instead of wrapping it twice |
| Generate posterior prediction | `diseasenowcasting::predict(nowcast_class)` | Remains the low-level draw generator | Must emit enough structured metadata for lossless conversion; fail explicitly if it cannot |
| Extract common result | `tbl.now`; `tbl_nowcast`, `as_tibble()`, and `tidy()` | Native fit is converted first | Prediction `tidy()` is the common meaning; parameters stay under `parameters()`/`coef()`/`summary()` |
| Plot a fitted nowcast | `tbl.now::autoplot(tbl_nowcast)` | Direct results inherit the common plot; no diseasenowcasting method shadows it | Keep deeper model diagnostics under `nowcast_diagnostic()` |
| Backtest | `tbl.now::nowcast_backtest()`; `nowcast_backtest` | `diseasenowcasting::backtest()` translates native model specs into labeled diseasenowcasting engines and delegates | This package is unreleased: unsupported native-only arguments may break with a targeted error; no legacy result class is required |
| Plot a backtest | `tbl.now` | Delegate once `tbl.now` owns an `autoplot(nowcast_backtest)` method | Until that method exists, keep the old plot only as a temporary compatibility path and record the gap |
| Score a fitted result | `tbl.now::score_nowcast()` and `scoringutils::as_forecast_*()` | Native fit converts, then delegates | Sample conversion errors clearly when draws were not retained |
| Score a backtest | `tbl.now`/`scoringutils`; scores carried by or derived from `nowcast_backtest` | No native duplicate; `fit_check()` is only for RTMB optimizer diagnostics | No legacy result-view or duplicate scoring implementation is required |
| Auto-select model | `diseasenowcasting::auto_nowcast()` for model-specific candidate construction; `tbl.now` for retrospective scoring | Selection calls the delegated backtest/scoring path | Winner uses the common result; comparison evidence remains on the diseasenowcasting subclass and raw fit |
| Ensemble and weights | `tbl.now::nowcast_ensemble()` and `nowcast_weights()` | No native duplicate | Native fits convert before use |
| Save/load | `diseasenowcasting::save_nowcast()`/`load_nowcast()` for native fits | Stores the raw fit and restores the public common result | A restored result must immediately use every common downstream verb |

### Delegation constraints

- Delegation must not create a circular package dependency. `diseasenowcasting`
  may call its existing dependency `tbl.now`; `tbl.now` continues treating
  `diseasenowcasting` as an optional engine.
- The canonical result is implemented before backtesting or scoring delegates
  to it, so no native entry point temporarily loses functionality.
- Backtest argument translation must be explicit. Native `model()` objects
  become distinct `engine_diseasenowcasting(model = ..., label = ...)`
  specifications; duplicate labels are rejected.
- Delegated APIs must not claim equivalence until characterization tests show
  the same as-of masking, truth estimand, cumulative settlement, and target.
- When `tbl.now` lacks a corresponding capability, add it there first or keep a
  clearly marked temporary compatibility implementation. “Aggressive” does not
  mean silently dropping capabilities.
- There is no released legacy output contract to preserve. Breaking changes are
  acceptable and preferred over carrying parallel fit or backtest result types.
- Methods on the diseasenowcasting result subclass may unwrap `@fit`. No method
  may be registered on `tbl.now::tbl_nowcast` itself, and in particular the
  `tbl.now` update method must not be replaced or shadowed.

### Completion criteria

- Every output-facing surface has exactly one documented disposition.
- `tidy()` remains prediction-focused; parameter summaries remain under
  `parameters()`/`coef()`/`summary()`.
- The contract states whether native backtest/score results are stable public
  APIs or compatibility layers.

### Stage 2 completion evidence

- Rodrigo selected aggressive delegation.
- Ownership, canonical objects, conversion direction, and failure policy are
  recorded for every output-facing operation.
- Native backtest and score return shapes are compatibility layers, not
  independent long-term scoring implementations.
- The required implementation order is canonical result → metadata/schema →
  delegated plotting → characterized backtest/scoring delegation.

## Stage 3 — Make `nowcast()` return the canonical common result

### Plan under the selected aggressive-delegation policy

Make `diseasenowcasting::nowcast()` eagerly format its prediction and return a
diseasenowcasting-specific subclass of `tbl.now::tbl_nowcast`, with the
untouched raw fit stored in `@fit`. Keep all engine-specific reshaping and
metadata assembly in `diseasenowcasting`. In `tbl.now`, make only the minimal
integration changes: forward requested quantile levels and return an
already-normalised result unchanged.

### Question 3

Where should normalisation occur?

1. **Recommended — Inside `diseasenowcasting::nowcast()`.** The engine owns its
   output formatting and returns the common result directly; `run_nowcast()` is
   a thin pass-through.
2. **Second option — Inside `tbl.now`.** Keep a diseasenowcasting-specific
   adapter and public conversion helpers in the common package.
3. **Other —** ________________________________________________

Decision: **Option 1**, selected by Rodrigo on 2026-09-08. Native operations
unwrap `@fit` automatically. No legacy output support is required.

### Focused tests

- A tiny direct fit returns the common class with raw fit, data, `now`, method,
  axes, draws, quantiles, and namespaced metadata intact.
- `run_nowcast()` returns that already-normalised result and forwards custom
  quantile levels.
- Native operations unwrap automatically without installing methods on the
  shared `tbl.now::tbl_nowcast` class.
- Common tidy, plot, score, ensemble, update, and persistence paths work.

### Stage 3 implementation evidence

Implemented in `tbl.now`:

- `nowcast_fit.diseasenowcasting()` forwards the engine's quantile levels.
- `run_nowcast()` returns a `tbl_nowcast` result produced by the engine without
  invoking `nowcast_tidy()` or wrapping it again.
- The earlier diseasenowcasting-specific bridge, exports, documentation, tests,
  and general assembler changes were removed during cleanup.

Implemented in `diseasenowcasting`:

- `nowcast()` now eagerly returns a diseasenowcasting-specific subclass of
  `tbl.now::tbl_nowcast`; its raw model fit remains in `@fit`.
- Prediction reshaping, quantile generation, event-axis recovery, strata
  handling, and namespaced metadata assembly now live in
  `R/20_result_class.R` in this package.
- `run_nowcast(engine_diseasenowcasting())` passes the already-normalised
  result through, including engine-specific quantile levels.
- Native `predict()`, parameter summaries, diagnostics, `update()`, surprise
  handling, auto-selection, and persistence unwrap the raw fit internally.
- The subclass-specific `update()` method is isolated from the existing
  `tbl.now` update method. No method was installed on the shared parent class.
- `autoplot()` is inherited directly from `tbl.now`; scoring and ensembling
  accept direct `nowcast()` results with no conversion call.

Verification on 2026-09-08:

- Existing `test-run_nowcast.R`: 59 passed, 0 failed, 0 skipped.
- Direct `nowcast()` and engine-mediated fits return the common result; custom
  engine quantile levels survive. Focused result-contract tests cover common
  plotting, tidying, scoring, ensembling, and save/load.
- Focused cross-checks against the sibling development package passed:
  `test-tbl-nowcast-result.R`, `test-nowcast-tblnow.R`,
  `test-nowcast-methods.R`, and `test-save-load.R`.

## Stage 4 — Lock prediction metadata and the golden output schema

### Plan under the selected aggressive-delegation policy

Use the ordinary `tbl_nowcast` properties as the complete structural contract.
The retained `@data` is authoritative for event names, strata, units, data type,
and observed history; `@event_date`, `@strata`, and `@now` expose the normalized
axis directly. Keep namespaced metadata only for prediction semantics or
diagnostics that cannot be reconstructed from those fields.

### Question 4

How much information should be duplicated under `@metadata`?

1. **Recommended — Prediction semantics only.** Structural information lives
   in standard `tbl_nowcast` fields; retain only `estimand`,
   `cumulative_reconstruction`, and `negative_projection_count` under the
   diseasenowcasting metadata key.
2. **Second option — Duplicate structural and native-fit metadata.** Also copy
   axes, observed series, model type, rung, target, revision mode, and model
   selection evidence into metadata.
3. **Other —** ________________________________________________

Decision: **Option 1**, selected by Rodrigo on 2026-09-08. Observed values are
recoverable from `@data`; model type, rung, target, revision mode, and comparison
evidence remain on the diseasenowcasting subclass and its raw `@fit`.

### Golden assertions

- Quantiles: original event column, original strata columns,
  `.quantile_level`, `.value`.
- Draws: original event column, original strata columns, `.draw`, `.value`.
- Numeric event axes remain numeric/integer; dates remain dates; weekly spacing
  remains weekly.
- Quantiles are finite where expected, complete at requested levels, and
  nondecreasing within target.
- Standard `tidy()` output includes `event_date`, `stratum`, `estimate`,
  interval bounds, `level`, `engine`, `now`, and available `q*` columns.
- Namespaced metadata contains only the available prediction semantics:
  estimand, cumulative reconstruction, and negative-projection count.
- Auto-selection comparison evidence remains identical between the subclass
  and `@fit`; it is not duplicated into common metadata.

### Stage 4 implementation evidence

- Removed observed-series, observed-strata, fit type, rung, target, revision
  mode, and comparison duplication from `@metadata`.
- Added focused assertions that `@data`, `@event_date`, `@strata`, and `@now`
  are authoritative and that redundant metadata keys are absent.

## Stage 5 — Build shape-matrix round-trip tests

### Recommended plan

Run the exhaustive audit in three tiers so failures can be attributed instead
of hidden by one enormous loop:

1. enumerate every supported structural combination and audit construction and
   preparation without fitting;
2. audit the canonical result grammar for every supported structural
   combination using deterministic mocked posterior draws;
3. run a broad Cartesian grid of real low-draw RTMB fits over the supported
   data, revision, censoring, time, shape, stage, and model-component axes.

The real-fit tier is an audit workload, not a promise that thousands of RTMB
fits must run during every CRAN check. Its case manifest and complete
success/failure report must be saved, while a stable representative subset
remains in the ordinary regression suite. A failed combination is evidence to
classify, not a row to silently discard.

### Question 5

How much of the shape matrix should run in the ordinary test suite?

1. **Recommended — Pairwise fast matrix plus sentinel fits.** Preparation and
   reshaping tests cover all combinations cheaply; a few low-draw fits cover
   linelist, incidence, cumulative, revision, strata, weekly, and numeric
   integration paths.
2. **Second option — Full Cartesian fit matrix.** Stronger brute-force coverage,
   but slow and likely brittle for RTMB optimization.
3. **Other —** ________________________________________________

Decision: **Option 2**, selected explicitly. Reduced posterior draws are allowed
to keep the audit tractable, but the matrix should be as complete as the valid
model/data contracts permit (including on the order of 1,024 real fits).

### Axes to cover

- Data: linelist, count-incidence, count-cumulative.
- Revision: none, confirmation-only, retraction-only, both, pending.
- Censoring: report and revision, absent/present.
- Time: daily, aligned weekly, numeric, and a deliberate mixed-unit policy.
- Shape: no strata, one stratum, two strata, missing strata; no covariates,
  numeric covariates, multiple covariates, missing covariates.
- Fit: one-stage, two-stage, auto; default and explicit model components.

### Round-trip acceptance

For each supported sentinel:

1. construct `tbl_now`;
2. fit with `diseasenowcasting::nowcast()` and assert it is already a
   `tbl_nowcast`;
3. fit through `run_nowcast(engine_diseasenowcasting())`;
4. validate `as_tibble()`, `tidy()`, `autoplot()`, `score_nowcast()`, and
   applicable `as_forecast_*()` methods on the common result grammar.

### Stage 5 characterization evidence so far

- The existing preparation matrix covers linelist and count-incidence crossed
  with four revision states and daily, weekly, and numeric axes, using report
  censoring, revision censoring, two strata, and multiple covariates. A separate
  count-cumulative sentinel covers down-revisions, weekly time, one declared
  stratum with missing values, and a covariate.
- Mocked-fit sentinels now assert that direct `nowcast()` results already have
  the common prediction/draw schemas for numeric, daily, weekly, revision, and
  cumulative paths. Numeric axes remain integer and calendar axes remain Date.
- Real low-draw tests cover daily unstratified, one-stratum, and
  count-cumulative results, including common `autoplot()`, `as_tibble()`,
  `tidy()`, `score_nowcast()`, all three scoringutils forecast coercions,
  ensembling, update, save/load, and cumulative prediction-semantics metadata.
- Obsolete tests of diseasenowcasting's retired bar-plot internals and native
  fit printer were replaced with common-result plot, print, and schema tests.
- `devel/run_interplay_shape_audit.R` now creates a resumable 9,216-cell
  manifest. All 9,216 construction/preparation cells pass, and all 1,536 unique
  structural shapes pass deterministic common-result construction,
  `as_tibble()`, `tidy()`, `autoplot()`, `score_nowcast()`, and quantile/point/
  sample forecast conversion.
- A literal `|` inside a stratum value exposed ambiguity in the historical
  flattened label. Result formatting now resolves labels against the
  authoritative stratum combinations in `@data`; it falls back to splitting
  only when no unambiguous data mapping exists.

Remaining gaps before Stage 5 is complete:

None. The saved audit report records 713 successful real fits and 311 RTMB
optimizer failures. Every successful fit passed the common result contract; no
construction, preparation, or common-result-contract failure remained. The
optimizer failures are concentrated in more difficult native fitting shapes:
confirmation/both failures occur only in the four-cell two-strata cases, while
retraction is additionally fragile under revision-delay censoring. These are
native robustness findings, not `tbl.now` conversion failures, and remain
available case-by-case in `devel/interplay_audit/real_fit_results.csv`.

## Stage 6 — Reconcile backtesting and scoring semantics

### Recommended plan

First write characterization tests for as-of dates, snapshot masking, truth
construction, target selection, revision estimands, cumulative settlement,
quantile levels, and score aggregation. Then translate native model
specifications into `engine_diseasenowcasting()` specifications and make the
native backtest/scorer delegate to the `tbl.now` implementations. Replace the
unreleased native return contract; do not preserve a parallel legacy view.

### Question 6A

What should happen to `diseasenowcasting::backtest()`?

1. **Recommended — Delegate and return `nowcast_backtest`.** Translate model
   specs to engines and remove the unreleased native backtest result contract.
2. **Second option — Keep the native backtest.** Add conversion only where its
   target and truth semantics are demonstrably lossless.
3. **Other —** ________________________________________________

Decision: **Option 1**, selected by the aggressive-delegation and no-legacy
decisions. The native wrapper now translates named or inferred model labels,
dates, fit controls, quantile levels, error policy, retained draws, and truth
controls, and returns `nowcast_backtest` directly. Count-cumulative default date
selection uses the model's settlement horizon.

### Question 6B

What should happen to the native fitting diagnostic now called `score()`?

1. **Recommended — Replace it with `fit_check()`.** Canonical predictive scores
   remain on `tbl.now::nowcast_backtest` / `tbl.now::score_nowcast`; the local
   helper reports only RTMB-specific fit quality such as inference rung,
   optimizer convergence, objective, and maximum absolute gradient.
2. **Second option — Keep a `score()` forwarding alias.** Delegate predictive
   scoring but retain another exported name that overlaps the scoring generic.
3. **Other —** ________________________________________________

Decision: **Option 1**, selected explicitly. There is no unreleased native
backtest score contract to preserve. The exported native `score()` and native
backtest class were removed. `fit_check()` reports only RTMB rung, convergence,
objective, maximum gradient, and gradient status; predictive scores remain in
the canonical APIs.

### Required agreement tests

- Same requested as-of dates and same future-report/revision masking.
- Stable revision mode across historical slices.
- Same settled cumulative truth and target definition where the APIs claim
  equivalence.
- Same quantile forecasts produce the same scoringutils scores.
- Any intentional difference is named in object metadata and documentation.

### Stage 6 implementation evidence

- Explicit `dates` survive as canonical `now_dates`; automatic dates use the
  canonical horizon rule, with cumulative settlement-aware inference.
- Future revisions are masked by `tbl.now::change_now()` on retrospective
  snapshots, and the native wrapper infers canonical truth controls matching
  each native revision estimand (`revision/confirmed` for confirmation and both,
  `report/pending` for retraction-only, `report/total` otherwise).
- Named and inferred native model labels remain distinct canonical methods;
  retained draws feed the registered quantile, point, and sample forecast
  coercions and `nowcast_weights()`.
- Focused two-package tests, using both source trees installed into one isolated
  library, passed: 531 diseasenowcasting assertions and 205 tbl.now
  `run_nowcast`/`nowcast_score` assertions. One unrelated pre-existing
  non-uniqueness warning remains in the coverage-gap fixture.

## Stage 7 — Audit auto-selection, ensembles, and persistence

### Recommended plan

Use the direct canonical result as the single entry point. Verify that an
`auto_nowcast()` winner retains its comparison table, direct native fits can
participate in ensembles alongside other engines, learned weights work for
separately labeled diseasenowcasting specifications, and saved/loaded fits
restore the same common grammar.

### Question 7

Now that direct fits and retrospective evaluations already use the canonical
`tbl.now` result types, how should `auto_nowcast()` perform model selection?

1. **Selected — scoringutils-relative selection.** Keep diseasenowcasting's
   model-grid construction and refit fallback, convert the canonical
   `nowcast_backtest` directly with `scoringutils::as_forecast_quantile()`, score
   it with `scoringutils::score()`, and optionally add pairwise relative skill
   with `relative_score = TRUE`. For effectively equal selection scores, prefer
   HSGP, then AR(1), then SIR, then custom epidemic processes.
2. **Second option — Fastest tie-break.** Use the same score ranking but resolve
   effectively equal scores by median elapsed retrospective fit time instead of
   epidemic-process priority.
3. **Other —** ________________________________________________

Decision: **Option 1**, with Option 2 available through
`tie_break = "fastest"`, selected by Rodrigo on 2026-09-08. The previous
metadata-location question is already answered by the Stage 4 minimal-metadata
decision and no longer needs a separate choice.

### Focused tests

- Auto-selected fits return the common result and support tidy/plot/score.
- Two labeled diseasenowcasting specs can be ensembled.
- A diseasenowcasting result can be ensembled with a second engine.
- Incompatible quantile levels fail clearly.
- Save → load preserves the public axis, strata, draws, now, prediction
  semantics, and native comparison evidence.

### Stage 7 implementation evidence

- `auto_nowcast()` converts its canonical backtest with
  `scoringutils::as_forecast_quantile()`, obtains scores with
  `scoringutils::score()`, and defaults to pairwise relative skill from
  `scoringutils::add_relative_skill()`.
- Effectively equal scores prefer HSGP, AR(1), SIR, then custom epidemic
  processes; `tie_break = "fastest"` instead uses median successful
  retrospective fit time.
- Canonical backtests record every attempted engine/date duration. Automatic
  selection also records full-data refit attempts and total elapsed time,
  exposed by `selection_timings()`.
- Source-integrated focused tests passed for auto-selection, backtesting,
  interoperability, and save/load.

## Stage 8 — Documentation drift and release gate

### Recommended plan

Update source documentation first and regenerate derived files with the normal
package workflow. Search both packages for legacy terminology and ambiguous
recommendations. Add one direct canonical-result example and one preferred
cross-engine workflow. Run focused tests, full suites, and package checks only
after the preceding stages are settled.

### Question 8

How prominently should the two workflow layers be documented?

1. **Recommended — One explicit “native vs cross-engine” section in both
   packages.** Link to it from fit, plot, backtest, and score references.
2. **Second option — Examples only.** Add direct-result examples without a formal API
   policy section.
3. **Other —** ________________________________________________

Decision: **Option 1**, selected by Rodrigo on 2026-09-08.

### Stage 8 implementation evidence

- Added `?diseasenowcasting_workflows` and `?tbl_now_workflows` as the explicit
  ownership-policy topics in their respective packages.
- Linked the policy from fit, automatic selection, plotting, backtesting, and
  scoring help, and added a visible section to both packages' introductory
  vignettes.
- Documented direct `nowcast_backtest` conversion to scoringutils quantile,
  point, and sample forecasts, including the relative-WIS workflow.
- Removed the remaining released-documentation example of the retired native
  `score(backtest)` path.
- Advanced `tbl.now` to 0.35.3 and required that version from
  `diseasenowcasting`. This prevents the new `n_dates` control from being
  mistaken for an engine when an older installed `tbl.now` is used.

### Drift search

Review every hit for:

- `validation_process`, `validation_date`, `validation_type`;
- `confirmation_process`, `count_cumulative_process`, and public
  `count_cumulative =` usage;
- native `backtest()`/`score()` recommendations where the text is actually
  teaching cross-engine comparison;
- claims that `NAMESPACE` contains all dynamically registered result methods.

Internal fields such as `is_count_cumulative` are not legacy public API and
should not be renamed merely because they match the search substring.

### Release gate

- Focused converter, metadata, tidy, plot, scoring, backtest, ensemble,
  auto-selection, and save/load tests pass.
- Full test suites pass in both packages.
- Package checks pass in both packages, with external repository failures
  separated from local failures.
- Generated documentation matches source and no unrelated dirty files were
  overwritten.
- The contract matrix and this status table reflect the shipped behavior.

Rodrigo owns all final `devtools` test and check runs. Codex should provide the
commands but must not run them. The packages must be verified in dependency
order: document/test/check `tbl.now`, install that local 0.35.3 build, then
document/test/check `diseasenowcasting`. The pasted 2026-09-09 vignette failure
loaded an older installed `tbl.now` without `n_dates`; it is resolved by the
explicit 0.35.3 dependency and install order. Rodrigo's subsequent paired rerun
passed.

Verification update (2026-09-09): Rodrigo reports that `tbl.now` passes its
checks cleanly. The first paired `diseasenowcasting` rerun found only an
undeclared test-time `generics` dependency and a top-level-file NOTE. The
dependency is now declared in `Suggests`, and the audit plan plus the two local
manuscript `.tex` files are excluded from package builds with `.Rbuildignore`.
Rodrigo's subsequent rerun confirms that `diseasenowcasting` also passes. The
release gate is closed.

```r
tbl_pkg <- "/Users/rodzepeda/Documents/tbl.now"
dcast_pkg <- "/Users/rodzepeda/Documents/dcast3"

devtools::document(tbl_pkg)
devtools::test(tbl_pkg)
devtools::check(tbl_pkg)
devtools::install(tbl_pkg, dependencies = FALSE, upgrade = "never")
stopifnot(utils::packageVersion("tbl.now") >= "0.35.3")

devtools::document(dcast_pkg)
devtools::test(dcast_pkg)
devtools::check(dcast_pkg)
```

## Recommended order for answering the questions

Answer Question 2 first because it fixes the ownership model. Then answer
Questions 3 and 4 together because result ownership and metadata shape the
same code. Question 5 controls test cost. Questions 6A and 6B should be
answered only after characterization evidence exists. Questions 7 and 8 can be
settled last.

Stages 1 through 8 are complete: direct fits use the canonical result grammar,
metadata ownership is hardened, the exhaustive shape audit is saved, and native
backtesting, scoring, and automatic selection now delegate to the canonical
APIs. The maintained inventory is output-focused, and both packages pass their
final checks. The audit is closed.
