# Paper simulation study (new `diseasenowcasting` / RTMB package)

Uses only the exported package API (`tbl_now()`, `model()`, `nowcast()`,
`predict()`, `summary()`, `surprise()`).

## Run order (from the package root)

```sh
Rscript devel/paper_sim/simulation_study.R                       # 1. generate data
WORKERS=12 Rscript devel/paper_sim/simulation_study_analysis.R   # 2. fit + score + plots + ROC
```

A quick end-to-end check (a handful of days, all 18 models):

```sh
SMOKE=TRUE WORKERS=4 Rscript devel/paper_sim/simulation_study_analysis.R
```

The analysis is **parallelised over days with `future`** (one worker per day via
`foreach(...) %dofuture%`). Control the worker count with `WORKERS` (default =
cores − 1). Each worker fits every model for its day and computes that day's
surprise internally, because the RTMB fit object holds external pointers that
cannot be shipped between processes.

The full run fits **18 models** (3 epidemic × 2 likelihood × 3 delay) on each of
days 8–110 and, reusing each fit, scores the reporting-delay surprise of the
next day's arrivals — so every model gets a backlog-detection ROC.

## What the data generator does

`simulation_study.R` builds the latent incidence from a **deterministic SIR ODE
with a control-then-release transmission rate** (`deSolve`): `beta` is high,
suppressed by `CTRL_DROP` during `[CTRL_START, CTRL_END]` (an intervention), then
released — producing a **small first wave and a larger second wave** over ~110
days. Daily incident cases are then drawn with **negative-binomial** observation
noise (`size = PHI`), and each case gets a floored **Weibull** reporting delay.

This design makes the model comparison informative:

- **Two waves** cannot be reproduced by a constant-`beta` mechanistic SIR (one
  wave only), so the flexible **HSGP** fits the latent curve best and the AR(1)
  lags the turning points → **HSGP wins on WIS**.
- **NB noise on large counts** makes the overdispersion term `mu^2/PHI` dominate,
  so a **Poisson** likelihood is badly under-dispersed (its intervals miss the
  truth) → the **NB likelihood wins on WIS**.
- A **short** epidemic (~110 days) — no need for hundreds of days.

The paper's reporting-delay perturbation is applied **only to the upper bound of
the delay** (a transient *backlog*): on `N_PERTURB = 20` event-days the Weibull
**scale** is multiplied by `PERTURB_FACTOR = 3`, so those days' reports arrive
much later. Every case carries a `perturbed` flag — the ground truth for the
surprise ROC.

Key knobs (top of `simulation_study.R`): `N_POP`, `R0`, `GAMMA`, `CTRL_DROP`,
`CTRL_START`, `CTRL_END`, `PHI`, `DELAY_SHAPE/SCALE`, `N_PERTURB`,
`PERTURB_FACTOR`.

## What the analysis produces (`simulations/`)

| File | Content |
|------|---------|
| `plot_final_sims_complete_with_wis.pdf` | final (lag 0) nowcasts + WIS — the paper figure (18 panels) |
| `wis_pt.pdf` | mean WIS by lag (0–15) |
| `wis.csv` | WIS summary table, ranked (lag 0) |
| `wis_table.tex` | **publication WIS table** (LaTeX, all 18 models, averaged over lags 0–`TABLE_LAG`); auto-built caption |
| `wis_table_metrics.csv` | the numbers behind `wis_table.tex` |
| `roc_surprise.pdf` | **per-model** ROC for backlog detection (6 panels, coloured by delay) |
| `surprise_metrics_barplot.pdf` | **AUC / sensitivity / specificity bars per model** (`ggh4x`: columns grouped by likelihood, then process) |
| `surprise_metrics.csv` | AUC, sensitivity, specificity **for each of the 18 models** |
| `surprise_df.rds/.csv` | per-observation surprise scores (all models) |
| `day_surprise.rds` | per-model, per-event-day aggregated surprise |
| `all_steps.rds` | tidy nowcasts |

The LaTeX table (`wis_table.tex`) is averaged over the **nowcast horizon** (lags
0–`TABLE_LAG`, default 7) rather than lag 0, because that is where NB beats
Poisson for every epidemic process; at lag 0 alone a misspecified epidemic model
+ NB can over-widen at the censored edge. It needs the colour definitions
`delayA`/`delayB`/`delayC` and packages `multirow`, `booktabs`, `xcolor` in your
preamble. The barplot and table code live at the **end** of
`simulation_study_analysis.R`.

## Surprise / ROC design (now for every model)

In the day-*t* worker we fit each model as of *t* and then score the reports that
first become observable the **next** day (`report == t + 1`) under that fit —
which never saw them, so the test is honest. The score is the one-sided upper
tail `P(D ≥ d)` (small = a surprisingly long delay).

Detection is evaluated at the **event-day** level (the natural unit for a
backlog): a day's score is its most surprising arriving report, `min_t P(D ≥ d)`,
and the day is flagged when that minimum falls below `1 − level` (default
`level = 0.99`). Ground truth = whether the day was perturbed. The ROC sweeps the
threshold; AUC / sensitivity / specificity are reported per model.

All 18 models are scored with the exported `surprise(type = "delay")`, including
the Dirichlet (non-parametric) delay. (Earlier, `surprise()` errored for the
Dirichlet family — an undefined `rc`/`parlist` in `.surprise_internal` — and the
analysis carried a `delay_tail_fallback()` workaround; that bug is now fixed in
the package and the workaround has been removed.)

## Fit type (per delay family)

Fitting is **two-stage** for every model except the Dirichlet (non-parametric)
delay, which uses one-stage (its two-stage simplex imputation is unstable here).
Two-stage re-injects the reporting-delay uncertainty, giving wider, better-
calibrated intervals — the recommended HSGP/NB model reaches ~0.9 90%-interval
coverage while still beating AR(1) and SIR on WIS. See `fit_type_for()` in the
analysis script.
