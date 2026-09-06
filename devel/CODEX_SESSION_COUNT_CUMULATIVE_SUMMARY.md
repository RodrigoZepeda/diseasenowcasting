# Codex session summary: count-cumulative integration

Date: 2026-09-05

## Request and scope

This session continued from `devel/INTEGRATE_COUNT_CUMULATIVE.md` while treating
the existing dirty worktree as untrusted. The requested migration replaced the
production count-cumulative fixed-`p` interpretation with a finite-horizon
retraction-kernel model. The user also requested a third observation composite,
`hurdle_ztpoisson`, whose conditional nonzero magnitude is a zero-truncated
Poisson indexed by its own mean `(alpha + omega) / pi` and has no magnitude
dispersion parameter.

This session did **not** implement the paper's event -> report -> validation
procedure for linelist and count-incidence data. Those validation/retraction
changes were already present in the dirty repository and were preserved and
regression-tested, but they are not work claimed by this session.

## Statistical implementation

The production count-cumulative component now supports:

- cumulative-level Poisson and negative-binomial composite likelihoods;
- signed hurdle--zero-truncated-negative-binomial updates; and
- signed hurdle--zero-truncated-Poisson updates.

The implementation uses the primitive finite-horizon retraction kernel

```text
h_R(l) = retraction_mass * g_R(l)
S_R(a) = 1 - sum_{l <= a} h_R(l)
```

and the retained cumulative-reporting probability

```text
q_C(d) = sum_{r=0}^d g_D(r) S_R(d-r).
```

For signed updates it constructs

```text
alpha_t(d) = mu_t g_D(d)
omega_t(d) = mu_t sum_{r=0}^{d-1} g_D(r) h_R(d-r)
total = alpha + omega
pi = (1 - exp(-total)) plogis(eta).
```

Conditional on movement, the sign probability is `alpha / total` and the
magnitude's own mean is `total / pi`. Both truncated magnitude laws numerically
invert their parent mean on the RTMB tape. This preserves
`E[Delta] = alpha - omega`. `hurdle_ztpoisson` never creates or reads a
`magnitude_size` parameter.

The settlement target is configurable as `C_t(H)`, defaults to `H=26`, and is
carried through printing, prediction, update, backtest, prior-only operation and
serialization. Operational predictions anchor at the cumulative value observed
at the origin and simulate subsequent signed updates sequentially. Any negative
terminal total projected to zero is counted in the returned diagnostics.

## Public API and engine work

Added dedicated files:

- `R/04_count_cumulative_class.R`
- `R/17_count_cumulative_data.R`
- `R/28_count_cumulative_likelihood.R`

The dedicated component was routed through model construction, priors, data
preparation, objective construction, optimization, reconstruction, prediction,
update, backtest, diagnostics, parameter reporting, prior-only draws and
save/load. Count-cumulative data no longer use `confirm_p` or report a biological
truth probability. The old `is_confirmation=TRUE` preparation path and the old
fixed-`p` objective are both guarded by explicit errors, while the numerical
Skellam helpers remain available only as legacy/research helpers.

Cumulative-level fits integrate latent epidemic effects with RTMB Laplace;
hurdle fits use joint MAP. A bounded `nlminb` ladder is followed, when needed,
by a bounded L-BFGS-B polish accepted only when the objective is no worse within
numerical tolerance and the gradient improves. During production testing a
compressed-clock hurdle-ZTPoisson fit exposed a gradient of `0.1528`; allowing
the guarded polish for MAP fits reduced the repeated gradient to `0.005512`.

## As-of preparation and clocks

Count-cumulative preparation now filters both event and report dates at the
historical origin before deriving strata, clocks, temporal effects, covariates,
priors or initial values. It completes explicit zero cells only inside the
observable as-of triangle and maintains a separate observation mask, so future
cells are not scored as zero.

Tests and development helpers cover both:

- calendar time, which retains real missing weeks and fills eligible cells; and
- compressed publication time, which makes observed publication weeks
  consecutive across surveillance-season gaps.

Mutation tests add 100,000 to all post-origin terminal values and verify that
prepared inputs, priors, fitted parameters and seeded predictions at the origin
are unchanged. The empirical multiplier comparator also requires both age-`a`
and horizon-`H` values to have been observable by the same origin.

## Compatibility and repository corrections

Linelist and count-incidence validation behavior was kept separate from the new
count-cumulative component. Existing confirmation/retraction tests remained in
the full package suite. Additional workflow tests cover fit, prediction,
save/load, update, prior-only and backtest for hurdle-ZTPoisson.

Other corrections made while auditing the dirty repository:

- replaced calls to the renamed
  `tbl.now::censor_reporting_delays_above()` function;
- fixed temporal-effect conversion that previously coerced mixed factor/numeric
  data through a character matrix;
- made update initialization safely resize stratum vectors and matrices when a
  new stratum appears; and
- avoided attempting to remove temporal effects from objects that never had
  them.

## Documentation

The cumulative sections of the mathematical, validation and introductory
vignettes were replaced with the finite-horizon `h_R`/`S_R`/`q_C` formulation,
the three observation composites, own-mean truncated laws, anchored prediction
and the composite-likelihood uncertainty limitation. README, NEWS, roxygen and
generated help were updated consistently. Historical fitting origins,
retrospective scoring truth and target event weeks are distinguished explicitly.

## Tests and completed verification

- Focused count-cumulative tests: exit 0, no focused failures or warnings.
- Production AD smoke matrix: 27/27 finite objective/gradient evaluations for
  AR/HSGP/SIR x lognormal/gamma/generalized-gamma x
  cumulative/hurdle-ZTNB/hurdle-ZTPoisson.
- Texas production subset at `2025-05-31`, both clocks, all four variants:
  8/8 finite fits and predictions, 8/8 optimizer code zero, 8/8 leakage checks,
  all maximum gradients below 0.1; overall maximum `0.008858`.
- Full source build and `R CMD check --no-manual`: `Status: OK`; 1,237 test
  assertions passed, zero failed, six expected warnings and 55 intentional
  CRAN-environment skips. All vignettes rebuilt successfully.
- `git diff --check`: exit 0.

The six full-suite warnings were five backtests dropping a deliberately
too-recent evaluation date and one `tbl.now` warning about undeclared `race`
values being pooled. Restricted-network repository-index messages appeared
during dependency checking but did not produce an R CMD check WARNING or NOTE.

## Full production sweep

The following checkpointed command was launched at the end of this session:

```sh
Rscript devel/run_count_cumulative_integration_gate.R
```

Its default configuration is 53 FluSight locations including US, five origins,
both calendar and compressed clocks, `H=26`, 250 predictive draws, and all four
model variants: 530 jobs and 2,120 production fits. The runner records errors,
objective values, optimizer codes, maximum gradients, elapsed time, parameter
and prediction finiteness, `q_C` range, terminal retention, zero-projection
counts, leakage assertions and origin-safe empirical comparison scores. It
checkpoints after every job and exits nonzero if any hard gate fails.

The sweep completed all 530 jobs and 2,120 fits after the recovery. All fits,
predictions and leakage assertions were finite/passing. Every cumulative-level,
hurdle-ZTNB and compressed-clock hurdle-ZTPoisson fit had optimizer code zero
and maximum gradient below 0.1. Five calendar-clock hurdle-ZTPoisson fits, all
for US at the five historical origins, remained finite but returned optimizer
code one and maximum gradients from 0.552 to 9.676. The user chose to proceed
with these as warning results because hurdle-ZTNB was stable. The production
warning now displays the optimizer code/message and gradient and recommends
`hurdle_ztnb`.

### Prediction failure diagnosis and recovery

The first full-sweep process later stopped after 428/530 jobs. Its checkpoint
contained 36 prediction failures, all in cumulative Poisson/NB fits, with
`missing value where TRUE/FALSE needed`. Reproduction with the original Alabama
data and seeds localized the sequence:

1. an unconstrained Gaussian Laplace draw placed the conditional retraction
   delay almost entirely beyond `H=26`;
2. every natural-scale CDF value on ages 1:26 underflowed to exactly zero;
3. CDF differencing and normalization computed `0/0`, making the retraction PMF,
   `h_R`, most of `S_R`, `q_C`, and `omega` non-finite;
4. `rpois()` produced `NA`, after which `if (running_level < 0)` received `NA`.

The reconstruction now forms finite-horizon bin probabilities from log CDF or
log survival differences, selects the stable tail representation per numeric
bin, and normalizes on the log scale. A regression test uses the exact
tail-only parameter draw. All 36 recorded failures were then rerun with their
original data, fit seed, prediction seed and 250 draws: 36/36 passed and zero
failed. Results are in
`devel/count_cumulative_integration_results/retry_prediction_failures_after_log_pmf_fix.csv`.

The original checkpoint was preserved with suffix
`.before_log_pmf_retry`. The 30 affected jobs' partial diagnostics and scores
were removed recoverably, and the production runner resumed them in full before
the remaining jobs. A ten-minute heartbeat watcher reports exact new failures
or an unexpected stop. If the sweep completes cleanly it runs
`devel/render_count_cumulative_state_results.R`, a separately checkpointed pass
that uses `tbl.now::tidy()` to retain 90% prediction intervals, creates
state-faceted plots for every model/clock, and reports per-state mean WIS, mean
and median absolute error, median signed error, and 90% interval coverage.

## Independent review

`devel/CLAUDE_VERIFY_COUNT_CUMULATIVE_INTEGRATION.md` contains the detailed
equation-to-code-to-test map, dirty-diff caveats, old-path audit, exact test and
optimization evidence, empirical subset table, known limitations and the
required independent-review checklist.

## Epidemic-process comparison

The original full sweep and interval refits used `ar1_epidemic()`. A subsequent
calendar-only comparison held all other settings fixed and crossed
`hurdle_ztnb` and `hurdle_ztpoisson` with the package's default
`hsgp_epidemic()` and `sir_epidemic()` constructors. The new runner is
`devel/run_count_cumulative_epidemic_comparison.R`; it checkpoints every fit
and retains `tbl.now::tidy()` 90% intervals in the same pass. The merger and
plotter is `devel/render_count_cumulative_epidemic_comparison.R`.

Of 1,060 HSGP/SIR fits, 1,059 completed. Ohio SIR hurdle-ZTPoisson at origin
2025-04-26 failed every optimizer initialization with `NA/NaN function
evaluation` / `NA/NaN gradient evaluation`; its state score therefore has 20
targets rather than 25 and is visibly identified as incomplete. No target was
imputed. All other state/process/model score rows have 25 targets.

At the aggregate level, HSGP had the lowest mean WIS for both hurdle laws:
25.182 for ZTNB and 34.591 for ZTPoisson. It was the lowest-WIS epidemic process
in 42/53 state series for ZTNB and 36/53 for ZTPoisson. Full aggregate and
per-state results, the incomplete-fit record, and plot links are in
`devel/count_cumulative_integration_results/COUNT_CUMULATIVE_EPIDEMIC_COMPARISON.md`.
