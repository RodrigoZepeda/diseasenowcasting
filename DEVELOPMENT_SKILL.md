---
name: diseasenowcasting-development
description: Develop and maintain the diseasenowcasting R package, including tbl.now integration, RTMB objectives, model components, fitting, prediction, revisions, cumulative streams, diagnostics, documentation, and tests. Use only for source changes; use SKILL.md for package usage.
---

# Develop `diseasenowcasting`

Read this package's `SKILL.md` and the companion `tbl.now` package's `SKILL.md`
before changing code. If a change touches `tbl_now`, `tbl_nowcast`, converters,
cross-engine scoring, or ensembling, also read `tbl.now/DEVELOPMENT_SKILL.md`.

`diseasenowcasting` owns the Bayesian model, RTMB objective, optimization,
posterior reconstruction, and native fit diagnostics. `tbl.now` owns input data
semantics and the common result/evaluation grammar. Extend the correct layer;
do not duplicate `tbl.now` data manipulation inside this package.

## Dependency policy

Core package behavior may use the companion `tbl.now` package plus
CRAN-available dependencies only. In particular:

- use `tbl.now` for nowcasting data/results and the CRAN package `RTMB` for
  autodiff and Laplace inference;
- other than the required companion `tbl.now`, do not add a GitHub-, R-universe-,
  Bioconductor-, or system-only dependency to `Depends` or `Imports` for the
  main API;
- prefer base R or an existing dependency before adding another package;
- keep optional integrations in `Suggests`, guard them with
  `requireNamespace()`, and ensure installation and core tests work without
  them;
- keep experimental code requiring a non-CRAN package outside the core path, or
  discuss the exception with the maintainer before changing `DESCRIPTION`.

Before adding or upgrading any other dependency, verify that the required
version is on CRAN and that its license and compiled requirements remain
suitable for CRAN. Coordinate the required `tbl.now` version with that
companion package's release policy.

## Start from the repository

Search the current exports, implementation, documentation, and tests before
adding a function or argument:

```sh
rg '^export|^S3method' NAMESPACE
rg '^[A-Za-z.][A-Za-z0-9._]*\s*<-\s*function' R
rg -n 'concept|synonym' R tests/testthat vignettes README.Rmd NEWS.md
```

Prefer extending an existing component or generic. Do not rely on this guide for
exact signatures: confirm them in current roxygen/source.

## Public architecture

The public workflow is:

```text
tbl.now::tbl_now
        |
        v
model(likelihood, epidemic, delay, revision, cumulative)
        |
        v
nowcast() / auto_nowcast()
        |
        v
diseasenowcasting subclass of tbl.now::tbl_nowcast
```

`model()` and its components are S7 objects:

- likelihood: negative binomial or Poisson;
- epidemic process: HSGP, AR(1), SIR, or custom;
- reporting delay: lognormal, gamma, generalized gamma, Dirichlet, or custom;
- optional report-level `revision_process()`;
- optional aggregate `cumulative_process()`;
- covariate prior and strata-pooling policy.

The input adapter `prepare_from_tbl_now()` handles dates, units, strata,
covariates, temporal effects, censoring, and revisions. Use exported `tbl.now`
getters and transformations rather than inspecting attributes or reproducing
their rules.

The low-level path is `prepare_data()` -> `default_priors()` -> `fit()`. Keep it
usable for tests and advanced callers, but design ordinary features through
`tbl_now` and `nowcast()`.

The public fit retains the native object in `@fit` while exposing the common
`tbl_nowcast` predictions and draws. Native operations unwrap the fit;
`tbl.now` operations consume the common fields.

## Data and shape invariants

Prepared ordinary observations use matrix `m` with 1-indexed columns:
`event_time`, `count`, `delay`, and optional stratum-cell index. Public event
indices such as `.event_num` remain 0-indexed. Do not mix these conventions.

Important shapes are:

- `case_counts`, `d_star`, `lambda`, and `Gstar`: time x stratum;
- covariate matrix `X`: time x covariate;
- HSGP coefficients: basis x stratum;
- AR/SIR innovations: time x stratum;
- count-cumulative arrays: time x age x stratum, always paired with an
  observation mask.

When adding a parameter, define its dimension, transformed/natural scale,
whether it is shared or stratum-specific, default prior, initialization,
bounds, reconstruction, print/parameter output, serialization, and tests.
Audit `.adapt_init()` so `update()` can warm-start changed time or stratum
dimensions.

The reporting-delay law, NB overdispersion, and HSGP kernel are shared across
strata by default; epidemic levels/trends and covariate coefficients are
stratum-specific. Preserve the exact one-stratum reduction. Hierarchical
pooling currently affects stratum intercepts and must not silently change other
parameters.

Missing or empty stratum values are an explicit `"missing"` level. Preserve the
full set of strata cells even if some are empty at a historical `now`.

## RTMB objective and reconstruction

`build_joint_obj()` defines the taped negative log posterior.
`.joint_reconstruct()` is its plain-R mirror used for prediction and
diagnostics. Any model change must update both and test numerical agreement;
duplicated formulas are acceptable only where the tape and reconstruction need
different execution contexts.

Within taped code:

- use RTMB-traceable arithmetic and distributions;
- avoid branching on parameter values, RNG, external solvers, and unregistered
  mutation of AD vectors;
- keep dimensions and loop lengths determined by data, not parameter values;
- preserve stable log-CDF/log-survival calculations in tails;
- use smooth transforms and explicit bounds where optimization requires them;
- test finite objective, gradient, Hessian/curvature, and reconstructed values.

Use `RTMB::` qualification in package internals. User-defined custom functions
are the exception: users must attach `RTMB` so arithmetic dispatches on AD
types. `validate_custom_delay()` and `validate_custom_epidemic()` must tape-test
functions before fitting. If index assignment is required in a user function,
document the existing `RTMB::ADoverload("[<-")` pattern; prefer vectorized
operations such as `cumsum()`.

Do not substitute another autodiff, Bayesian, ODE, or optimization framework for
core functionality. Optional custom examples may use a suggested CRAN package,
but the built-in models must remain `tbl.now` + RTMB based.

## Fitting and prediction invariants

`nowcast(type = )` supports one-stage, two-stage, and automatic selection:

- one-stage estimates epidemic and delay jointly;
- two-stage fits the delay, imputes fixed delay values, fits the epidemic for
  each accepted imputation, and pools draws;
- automatic mode uses one-stage for Dirichlet and custom delays and two-stage
  for supported parametric delays;
- count-cumulative models are one-stage because their delay, retraction kernel,
  and epidemic process are jointly identified.

Keep fit-selection and fallback decisions in `.collect_nowcast_fits()`. Excluded
fits must not contribute prediction draws. Surface retained/excluded fit and
Laplace-precision diagnostics through `fit_check()` and result metadata; do not
turn failed diagnostics into silent success.

Prediction distinguishes:

- posterior-predictive complete counts from `predict()`;
- latent incidence summaries from `mean()`, `median()`, and `quantile()`;
- parameter estimates from `parameters()` and `coef()`.

Preserve stratum-specific draws plus their total. Avoid ambiguous string parsing
of compound strata; prefer source-data keys. Quantile levels must be valid,
sorted, and recorded in the common result.

Use `save_nowcast()` / `load_nowcast()` for persistence. Raw RTMB tapes contain
external pointers and cannot be trusted through plain `saveRDS()`. When changing
fit fields, update serialization, tape rebuild, prediction without a live tape,
and round-trip tests together.

## Revision and cumulative models

These are distinct observation processes.

`revision_process()` applies to line-list or count-incidence data with
report-level outcomes. The supported modes are confirmation-only,
retraction-only, both, or automatic inference. Infer mode from the full source
data so it does not change across backtest snapshots. Pending means unresolved,
not confirmed. Reporting and revision censoring are separate bounds.

`cumulative_process()` applies to aggregate count-cumulative streams. It models
cumulative levels or signed updates with a finite-horizon retraction kernel. Do
not introduce a report-level confirmation probability `p`: aggregate cumulative
streams cannot separately identify truth probability and conditional revision
delay. Confirmed revision rows on cumulative data remain an error.

If revision-bearing line-list/incidence data are passed to `nowcast()` without
an active revision component, the package promotes a default
`revision_process()`. If cumulative data lack an explicit cumulative component,
it applies the documented default. Preserve informative messages and explicit
errors for incompatible model/data combinations.

Backtest truth must match the estimand: ordinary reported totals, confirmed
cases for confirmation/both modes, standing cases for retraction-only, and the
documented finite-horizon cumulative target.

## Priors and custom components

Component constructors accept a prior object for an estimated parameter or a
numeric value to fix it. Keep validation consistent across constructors,
`default_priors()`, parameter maps, `fix_param()`, and `parameters()`.

Defaults that depend on observed data are resolved only after preparation.
Document the parameter scale. Never silently reinterpret a natural-scale value
as unconstrained or log scale.

Custom delays supply factories for CDF, log-CDF, and log-survival. Custom
epidemics return the complete time x stratum log-intensity matrix, including
their intercept. Parameter count is inferred consistently from priors, names,
and initial values. A numeric entry fixes a parameter; a prior frees it.

## `tbl.now` integration

Use `tbl.now` for:

- building and validating inputs;
- conversions between line-list, incidence, and cumulative representations;
- zero completion, time aggregation, and temporal effects;
- revision metadata and censoring helpers;
- the `tbl_nowcast` result schema;
- cross-engine backtesting, scoringutils coercion, and ensembling.

`backtest()` in this package is a convenience that translates native model
specifications to `tbl.now::engine_diseasenowcasting()` and returns
`tbl.now::nowcast_backtest()`. Do not create a second scoring or backtest result
system. Use `scoringutils` through the common result for predictive metrics;
reserve `fit_check()` for RTMB optimization quality.

Any change to the integration boundary needs tests for the current minimum
`tbl.now` version and the corresponding `tbl.now` skill/docs update when the
user-facing workflow changes.

## Style and documentation

- Use native `|>`, snake case, `<-`, roxygen2, and the repository's existing
  file-number/module structure.
- Use S7 methods for package classes and the shared generics already in use.
- Use `cli` for user-facing conditions and messages.
- Qualify non-base calls in package code unless they are deliberately imported.
- Keep comments about mathematical intent and invariants, not line-by-line code.
- Never hand-edit `NAMESPACE` or `man/*.Rd`; run `devtools::document()`.
- Update `README.Rmd`, vignettes, `NEWS.md`, `_pkgdown.yml`, `SKILL.md`, and
  `tbl.now` documentation only when their public contract changes.
- Examples should be reproducible and proportionate; do not hide broken code
  with `\dontrun{}`, `if (FALSE)`, or broad warning suppression.

Use `dn_palette()` and semantic role names for package plots. A native fit
diagnostic and a common-result plot answer different questions; keep both
surfaces coherent without duplicating their data preparation.

## Testing

Add the narrowest test that exposes the behavior, then cover the cross-product
that can change the result:

- likelihood x epidemic x delay component;
- one-stage x two-stage/automatic path;
- unstratified x stratified, including missing levels;
- line-list x incidence x cumulative where supported;
- exact x censored report/revision delays;
- no revision x each revision mode x cumulative process;
- fresh fit x warm `update()` x save/load;
- native fit methods x shared `tbl_nowcast` output;
- fixed parameters x estimated priors x custom components.

For mathematical code, use small deterministic fixtures and compare to direct R
calculations. For stochastic output, set seeds and test invariants/calibration
rather than exact Monte Carlo draws unless exact reproducibility is the contract.
Assert the destination result and numerical values, not only that a function was
called.

Run targeted tests while iterating, then the appropriate package checks:

```r
devtools::test(filter = "relevant-file")
devtools::document()
devtools::test()
devtools::run_examples(run_donttest = TRUE, run_dontrun = TRUE)
devtools::check()
pkgdown::check_pkgdown()
```

Before finishing, confirm that:

- objective and reconstruction paths agree;
- fit diagnostics remain visible and excluded fits contribute no draws;
- `tbl.now` metadata, strata, units, censoring, and estimand survive the result;
- serialization and `update()` still work when fit structure changes;
- no new non-CRAN core dependency was added beyond the required companion
  `tbl.now` package;
- generated documentation, tests, `NEWS.md`, and both package skills agree;
- unrelated files and the user's existing worktree changes were preserved.
