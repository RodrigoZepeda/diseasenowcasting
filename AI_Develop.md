# AI_Develop.md — how to *develop* `diseasenowcasting`

This file is for an AI (or human) **modifying** the package internals. For how to
**use** the package, read `SKILL.md` and the vignettes — do not duplicate that here.

> **Stack:** R, **RTMB** (CppAD autodiff + Laplace approximation) for inference,
> **S7** for the object system, **tbl.now** for the input data object, **ggplot2**
> for plots, **scoringutils** for WIS. There is **no Stan / cmdstanr** anymore —
> the package was ported from a cmdstanr backend to RTMB. If you see references to
> Stan, `instantiate`, `.stan` files, or `cmdstanr` in older notes (e.g. parts of
> `CLAUDE.md` or `devel/`), treat them as **stale**.

---

## 1. The pipeline in one breath

```
tbl_now  ──prepare_from_tbl_now()──▶  engine (a plain list)
engine   ──default_priors(model, engine)──▶  priors (named list)
engine+priors+model ──build_joint_obj()──▶  RTMB MakeADFun `obj`
obj      ──fit()/fit_internal()──▶  fit list (optimise + Laplace)
fit(s)   ──nowcast() / collect_fits──▶  nowcast_class (S7)
nowcast  ──predict()/summary()/quantile()/autoplot()/surprise()/backtest()──▶  results
```

Everything downstream of `engine` is numeric/AD; `tbl_now` and dates live only at
the edges (`prepare_from_tbl_now`, and date reconstruction in `predict`).

---

## 2. Repo layout (`R/`, loaded in numeric order)

| File | Role |
|------|------|
| `00_prior_class.R` | `prior_class` (S7) + `*_prior()` constructors + `num_id` codes |
| `01_utils.R` | small helpers, `.valid_*` validators, constants |
| `02_likelihood_class.R` | `poisson_likelihood()`, `nb_likelihood()` |
| `03_delay_class.R` | delay S7 classes + `lognormal/gamma/generalized_gamma/dirichlet_delay()` |
| `04_epidemic_class.R` | epidemic S7 classes + `hsgp/ar1/sir_epidemic()` |
| `05_model_class.R` | `model()` — bundles likelihood + epidemic + delay |
| `06_prior_density.R` | `prior_lpdf()` — log-density of each prior (used in the objective) |
| `07_default_priors.R` | `default_priors(mod, data)` → the priors list |
| `08_sample.R` | `sample()` S7 generic (priors; `class_any` → `base::sample`) |
| `09_prepare_data.R` | `prepare_data()` — builds the engine list from matrices |
| `10_delay.R` | delay CDF/pmf builders `.delay_distribution_functions`, `.nonparametric_delay_functions` |
| `11_objective.R` | delay-only objective pieces |
| `12_fit.R` | `fit()` / `fit_internal()` — optimise + Laplace, retry ladder |
| `13_epidemic.R` | epidemic-process reconstruction helpers |
| `14_objective_joint.R` | **the censored joint likelihood** + `.joint_reconstruct()` |
| `15_nowcast.R` | `.nowcast_draws()` — posterior-predictive draw machinery |
| `16_multisample.R` | two-stage multiple-imputation pooling |
| `17_prepare_from_tblnow.R` | `tbl_now` → engine (dates, grid, covariates) |
| `18_collect_fits.R` | `.collect_nowcast_fits()` — one/two-stage orchestration |
| `20_nowcast_class.R` | `nowcast_class` (S7) + `nowcast()` user entry point |
| `21_nowcast_methods.R` | `predict`/`summary`/`mean`/`median`/`quantile` methods |
| `22_update.R` | `update()` a nowcast with new data |
| `23_backtest.R` | `backtest()` (foreach + doFuture) |
| `24_score.R` | `score()` — WIS/APE/MSE via scoringutils |
| `25_autoplot.R`, `26_autoplot_nowcast.R` | `autoplot()`, `nowcast_diagnostic()` |
| `27_tidy.R` | `tidy()` parameter table |
| `28_ppc.R` | posterior predictive checks |
| `29_surprise.R` | `surprise()` — delay/count anomaly scores |
| `30_print.R` | `print` methods |
| `31_censoring.R` | missing-report-date censoring helpers |
| `32_prior_only.R` | `nowcast(prior_only = TRUE)` prior-predictive engine |

---

## 3. Key data structures

### The `engine` (a.k.a. `data`, `stan_data` in old notes)
A plain list produced by `prepare_data()` / `prepare_from_tbl_now()` and consumed by
`build_joint_obj()`. Important fields:

- `max_time` (int), `num_strata`, `case_counts` `[max_time × num_strata]`
- `m` `[nrow × 4]` observation matrix: `(.event_num, count, delay, 1)` (1-indexed inside)
- `epidemic_model` ∈ `{1=HSGP, 2=AR1, 3=SIR}`; `delay_family` ∈ `{1=LogNormal, 2=Gamma, 3=GenGamma, 4=Dirichlet}`
- `is_negative_binomial` (0/1), `num_basis`, `gp_*` (HSGP geometry), `np_model_length` (Dirichlet bins)
- `d_star` per event-time (max observable delay), `X` covariate matrix, `P` (#covariates)

> **Gotcha:** the time grid is derived from real `Date`s. Passing integer
> "dates" makes `prepare_from_tbl_now` collapse to `max_time = 1`. Map day `d`
> to `ORIGIN + d` before building a `tbl_now`.

### A `priors` entry
`default_priors()` returns a named list; **each entry** is
`list(dist = <num_id>, params = <padded length-3 numeric>, is_constant = 0L, fixed = numeric(0))`.
`is_constant = 1L` fixes the parameter at `fixed` (this is how two-stage pins the
delay). The `num_id → name` map lives in `00_prior_class.R` (and is mirrored in
`32_prior_only.R::.PRIOR_CODE_NAME`).

### A `fit` list (from `fit()` / `fit_internal()`)
`par, parList, nll, convergence, obj, opt, random, lambda [T×S], mu, Gstar [T×S],
delay_mu, delay_sigma, phi_nb, reconstruct, Bmat, freq, data, priors, model`.

- `obj` is the **RTMB `MakeADFun`** object: `obj$fn/gr/he` are nll/grad/hessian,
  `obj$env$last.par.best` is the mode. **It holds external pointers and does NOT
  survive serialization to another R process** — see §6.
- `reconstruct` / `.joint_reconstruct(engine, priors, parList, Bmat, freq)` maps the
  unconstrained parameter list back to constrained quantities (`lambda`, `Gstar`,
  `delay_mu`, `delay_sigma`, `phi_nb`, …). This is the single source of truth used by
  `surprise()`, `nowcast_diagnostic()`, and `prior_only`.

### S7 classes
`prior_class`, `likelihood_class`/(poisson|nb), `*_delay_class`, `*_epidemic_class`,
`model_class`, `nowcast_class`, `nowcast_prediction_class`, `backtest_class`. Methods
are registered with `S7::method(generic, class) <- function(...)`. The package defines
its **own** S7 generic `sample` (08_sample.R); a `class_any` method falls back to
`base::sample` so non-prior objects keep base behaviour.

---

## 4. The objective (`14_objective_joint.R`)

`build_joint_obj(engine, priors, model)` constructs the parameter list, then returns
`RTMB::MakeADFun(nll, parameters, random = ..., silent = TRUE)`. Inside `nll`:

1. Reconstruct constrained params (delay, epidemic, overdispersion) from the
   unconstrained ones (same math as `.joint_reconstruct`).
2. Build the delay CDF `G_D` for each event-time (or constant — delays are
   **time-constant**; vestigial `num_delay_seasons` slots exist but are unused).
3. Add the **censored point-process log-likelihood**: per event-time, sum over
   observed delays of `log ΔG_D` plus the latent term `S_k` (closed-form for Poisson
   and NB — see the Mathematics vignette).
4. Add `prior_lpdf` for every free parameter.

Epidemic processes branch on `epidemic_model`: HSGP (basis × spectral density), AR(1)
(non-centred), SIR (coupled chain-binomial in fractions). Delays branch on
`delay_family`. `random = c("basis_coefs"/"ar_innov", ...)` marks the latent field for
the Laplace approximation.

---

## 5. How to extend

### Add a delay family
1. New S7 class + constructor in `03_delay_class.R` (give it a `num_id`).
2. CDF/pmf in `10_delay.R::.delay_distribution_functions` (add a branch).
3. Default prior(s) in `07_default_priors.R`.
4. Objective branch in `14_objective_joint.R` (and any reconstruction in `13`).
5. Sampling branch in `32_prior_only.R::.sample_prior_parlist` for prior_only.
6. Tests in `tests/testthat/`; mention it in `SKILL.md`.

### Add an epidemic process
1. S7 class + constructor in `04_epidemic_class.R` (new `num_id`, e.g. 4).
2. Reconstruction + objective branch in `13_epidemic.R` / `14_objective_joint.R`
   (and the `if (!epidemic_model %in% c(1,2,3)) abort` guard).
3. Defaults in `07`, prior_only in `32`, autoplot/diagnostic if needed.

### Add a prior distribution
1. `*_prior()` constructor + `num_id` in `00_prior_class.R`.
2. log-density branch in `06_prior_density.R::prior_lpdf`.
3. RNG branch in `08_sample.R` (prior_class method) and `.PRIOR_CODE_NAME`.

### Add an exported function / method
Write roxygen with `@export`; regenerate with `devtools::document()`. **Never edit
`NAMESPACE` or `man/*.Rd` by hand.**

---

## 6. Gotchas (read before debugging)

- **RTMB objects are not portable.** `fit$obj` (and anything holding it, e.g. a
  fitted `nowcast`) cannot be sent to a `future`/`parallel` worker and used there;
  the AD tape is an external pointer. Parallelise by **fitting inside the worker**
  and returning plain data frames (see `devel/paper_sim/simulation_study_analysis.R`).
- **Integer dates collapse the grid** (`max_time = 1`). Always use real `Date`s.
- **`sample()` is shadowed** by the package's S7 generic. Outside-package calls hit
  the `class_any` → `base::sample` fallback; if you add classes, remember a more
  specific method wins.
- **Hessian not positive-definite** warnings (CHOLMOD) come from the Laplace
  covariance when a fit is weakly identified; `fit()` has a retry ladder and
  `surprise()` falls back to the mode — usually benign but a signal the fit is poor.
- **`num_basis`** is auto (`~1.5√T`, capped); long daily series can ill-condition the
  HSGP — see the COVID notes in `MEMORY`/`devel`.
- **Two-stage** (`type="two_stage"`) fixes the delay (`is_constant=1`) from a recent
  window then imputes `K` times; `nowcast@fits` then has several fits, and
  `@fits[[1]]` is the first imputation (what `surprise()` uses).

---

## 7. Build / test / document workflow

```r
devtools::load_all(".")        # iterate
devtools::document()           # regenerate NAMESPACE + man/ after roxygen changes
devtools::test()               # run testthat (no Stan; tests run directly)
devtools::check(vignettes=FALSE)  # vignettes are slow; check them separately
```

- Tests live in `tests/testthat/`; helpers in `helper-*.R` (e.g. `.make_synth_tblnow`).
- Examples should run directly (no `\dontrun`). The old CLAUDE.md instruction to wrap
  examples in `instantiate::stan_cmdstan_exists()` is **stale** (no Stan now).
- Vignettes that fit models read **precomputed** data from `inst/extdata/*.rds`,
  regenerated by scripts in `devel/` (e.g. `precompute_prior_sensitivity.R`). Keep
  the vignette using only exported functions; let the `devel/` script use internals.

## 8. `devel/`

Throwaway exploration + the paper artifacts. Notable:
`devel/paper_sim/` (simulation study, parallel), `precompute_prior_sensitivity.R`
(priors vignette data). Many older `devel/*.R`/`*.md` are cmdstanr-era — stale.
