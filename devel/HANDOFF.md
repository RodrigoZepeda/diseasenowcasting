# Handoff — `skellam` branch, diseasenowcasting 2.2.0

**Written 2026-09-02, updated end of the 2026-09-02 diagnostics session.** This is
the entry point. Read this first, then the documents it points at. Nothing on this
branch is committed.

There are **two independent threads** here. The first is finished; the second is
now diagnosed. Do not confuse them.

| thread | state |
|---|---|
| A. The `validation_process()` rename and the 2.2.0 release work | **done**, suite green |
| B. Why count-cumulative `p` goes to 0.1 on FluSight | **diagnosed** — finite-horizon confounding (Result C) is the primary cause, proven by parameter recovery; the residual is diffuse negative administrative batches. The successor to hard-fixing `p` is not a prior on `p`. See `spa_diagnostics/FINDINGS.md` §I (esp. I.7) and §B.3a below. |

**What THIS session did (2026-09-02 diagnostics):** ran the decisive
parameter-recovery experiment and its follow-ups. New scripts
`spa_diagnostics/18`–`22` and figure `recovery_figure.png`; new section
`FINDINGS.md` §I; new memory `p-identifiability-confounding`. No code/tests/docs in
`R/`, `man/`, `vignettes/` were touched this session — it was pure diagnosis. The
production behaviour (fixing `p`) is unchanged and is now better justified.

---

## 0. Rules that will save you a day

* **Never `library(diseasenowcasting)` in a `devel/` script.** The installed build
  is a *different, older package* (2.1.0, with the pre-fix Bessel density). Session
  1 measured a whole set of conclusions against it without noticing. Every script
  uses `pkgload::load_all(".")`.
* **Run the suite with `NOT_CRAN=true`.** The fit-heavy tests are `skip_on_cran()`
  and a clean run otherwise looks far greener than it is.

  ```bash
  NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_dir("tests/testthat")'
  ```
* **FluSight needs two data-preparation rules**, both properties of the file, not
  the model. Any analysis skipping them is measuring an artefact:

  ```r
  START <- as.Date("2023-09-23")            # the first as_of in the file
  raw <- flusight |>
    filter(location_name == st, target_end_date >= START, as_of >= START) |>
    filter(as_of <= max(target_end_date))   # truncate to the last event date
  ```

  Windowing is essential — without it 126 of 197 Texas event weeks first appear at
  a delay of more than a week, up to 145 weeks. The truncation costs 7 event weeks
  because `as_of` has an off-season publication gap, and it does **not** remove the
  trailing empty grid cell it was meant to — `now` follows the report axis and
  `align_weeks()` displaces the two axes by different amounts.
* **Anything that sums over a latent count needs a FEASIBILITY-driven window, not a
  moment-based one.** This bit three times in one session. See §5.

---

## 0b. tbl.now is moving fast — check its version FIRST

**tbl.now was upgraded from 0.28.0 to 0.29.0 mid-session, and the source tree at
`~/Documents/tbl.now` is already at 0.31.0.** That broke six tests and caused one
silent regression before it was noticed. `DESCRIPTION` now requires
`tbl.now (>= 0.29.0)`.

**Before anything else, check the installed version and read its NEWS.** A rename
in tbl.now shows up here as a swallowed error, not a failure.

### What 0.29.0 changed for us

| change | consequence |
|---|---|
| `is_censored` -> **`is_censored_report`** (breaking, #54) | `tbl_now(is_censored = )` now partially matches TWO formals and errors with "argument 8 matches multiple formal arguments" |
| `get_is_censored()` **removed** | `R/17_prepare_from_tblnow.R` called it inside `tryCatch(error = character(0))`, so report-side censoring was **silently disabled**. Fixed: it now calls `get_is_censored_report()` directly and fails loudly. |
| **`is_censored_validation` is new (#53)** | see below -- this reverses a design decision |
| `validation_levels` new (#54) | for data not recorded in English; unused here so far |
| **`change_now()` re-censors instead of erroring (#51)** | our issue, FIXED. Every test helper currently works around it by not pinning `now` on the object; that workaround can go. |
| **`covid_us` carries a validation process (#52)** | our issue, FIXED. The auto-detection finally has a real dataset to integration-test against -- gap 4 of `TEST_PLAN_validation.md`. |

### The design decision this reverses

The approved plan was: *"keep one `validation_censored=` argument, file a tbl.now
issue for a validation censoring attribute, and drop the argument once it lands."*

**It has landed.** `nowcast(validation_censored = )` should now read
`get_is_censored_validation()` off the `tbl_now` instead, at which point
auto-detection is complete and NO validation arguments survive on `nowcast()`.
That is a small, well-defined task and it is not done.

### Queued behind the source tree (0.30.0, 0.31.0)

* `censor_delays_above()` -> `censor_reporting_delays_above()`; the censoring
  family becomes six verbs on two axes. `tests/testthat/test-censoring-surprise-backtest.R`
  and `R/22_update.R`'s advice string will need the new name.
* **Time-grid coarsening (`#56`)** -- directly relevant to Thread B's interval work.
* The `*_confirmed()` getters are removed in favour of `get_*_validated_cases()`.

---

## THREAD A — the release work (done)

### A.1 What shipped

`validation_process()` replaces 14 exports with 5. The component describing what
happens to a report *after* it is filed is called a **validation** process
throughout, matching tbl.now 0.28.0.

| was | is |
|---|---|
| `confirmation_process()`, `resolution_process()` | `validation_process()` |
| `model(confirmation = )` | `model(validation = )` |
| `retract_delay = `, `resolution_delay = ` | `validation_delay = ` |
| 12 × `*_retraction()` / `*_confirmation()` / `*_resolution()` | 4 × `*_validation()` |
| `no_confirmation()` | `no_validation()` |
| `tidy()` | `parameters()` |

**The validation process is detected, not requested.** `nowcast()` no longer takes
`retraction_date` / `confirmation_date` / their censoring arguments. It reads the
process off the data: a `tbl_now` carrying `validation_date` / `validation_type`,
or count-cumulative data (whose down-revisions *are* retractions). Only
`validation_censored =` survives — because at the time of writing a `tbl_now` had
no attribute for it. **It does now (tbl.now 0.29.0); see §0b.** Dropping the
argument is an open task.

The mode (`confirmation_only` / `retraction_only` / `both`) is inferred from
`unique(validation_type)` over the **full** data, so it cannot flip between
backtest dates. `validation_process(mode = )` asserts instead.

Class slots `@retraction_date` / `@confirmation_date` / two censoring slots became
`@validation_mode` + `@validation_censored`.

### A.2 Bugs fixed

* **#128** — count-cumulative promotes the inert default to a validation process.
  `model()` went from "failed to converge for all init attempts" to
  `convergence = 0`.
* **#129** — both halves. `.sample_prior_parlist()` now samples
  `logit_confirm_p` / `retract_mu` / `log_retract_sd_exc`, and
  `.simulate_prior_draws()` aborts with the captured condition instead of
  returning a silent, correctly-shaped, entirely-`NA` result.
* **Negative predictive quantiles.** The still-standing erroneous mass was drawn
  as an *independent* Poisson and subtracted from the observed cumulative, so
  nothing bounded it (`q5 = -4` on FluSight). It is a *sub-population* of that
  cumulative — `E[C] = lambda G_D + retraction_mean` exactly — so it is now a
  binomial thinning of the observed rows, bounded by construction and free of the
  NB frailty, exactly as `.thin_standing_rows()` already was on the linelist side.
* **An unrecognised `validation_type`** (anything but `"confirmed"` / `"retracted"`
  on a dated row) used to fall through every `== "confirmed"` test and be silently
  counted as a RETRACTION in `both` mode. Now refused by name.

### A.3 Decisions taken (do not re-litigate)

* Hard rename, no deprecated aliases.
* Auto-detection only; `validation_censored =` the single surviving argument
  — *superseded by tbl.now 0.29.0, see §0b*.
* Version **2.2.0** — tbl.now's `.onLoad()` hands `tidy()` over at that version.
* **All-pending data**: `mode = "auto"` with nothing resolved fits the ordinary
  count model. An **asserted** mode does NOT error — it keeps the mode and lets `p`
  be carried by its prior. *"That's why this is a Bayesian method."* The same now
  applies to "nothing confirmed **yet**" in the as-of view, which used to abort.
* Vignette renamed to `Validation_processes.Rmd` and rewritten, not just renamed.

### A.4 State

**Suite: 1092 pass, 1 fail** (verified 2026-09-02, after the tbl.now 0.29.0
compatibility fix in §0b). The one failure is `test-temporal-and-output.R:91` —
`.temporal_effect_matrix()` returns all-zero day-of-week columns with an "NAs
introduced by coercion" warning. It predates all of this work, silently disables
day-of-week covariates for **all** daily data, and deserves its own PR.

Docs updated: `NEWS.md` 2.2.0, `SKILL.md` §2a, `vignette("Mathematics")` §9.4,
`introduction.Rmd`, `Validation_processes.Rmd`.

**`vignette("Mathematics")` §9.4 now says the OPPOSITE of what it used to.** It
previously claimed a *strong* data-informed prior identifies `p`. That was the
pre-fix rationalisation and it is false. If you are citing that section anywhere —
the paper included — cite the new text.

Filed on tbl.now, both marked AI-generated, and **both since fixed in 0.29.0** —
see §0b: [#51](https://github.com/RodrigoZepeda/tbl.now/issues/51) (`change_now()`
aborting instead of re-censoring) and
[#52](https://github.com/RodrigoZepeda/tbl.now/issues/52) (no shipped dataset
carrying validation columns).

---

## THREAD B — why count-cumulative `p` goes to 0.1 (open)

**Read `devel/spa_diagnostics/FINDINGS.md` in full before touching this.** Scripts
are `devel/spa_diagnostics/01_*.R` … `17_*.R`, run in order.

### B.1 The problem

On FluSight the likelihood puts `p` at **0.10**. The empirical down-revision rate
is **0.957** (Texas), and the mature-cohort asymptote is 0.952, so the
finite-follow-up bias is only ~0.005 — `p_empirical` is a good estimate.

At the optimum the model posits **406,405 expected additions and 357,051 expected
retractions** to explain a series whose total absolute movement is ~63,000, with
**99.4% of `g_C` on lag 1**. It predicts **295,186 down-revisions against 2,741
observed** — a factor of 108. The high-`p` solution predicts 2,975, within 8.5%,
with the delay profile right.

### B.2 What has been RULED OUT (with evidence)

| hypothesis | verdict |
|---|---|
| Saddlepoint approximation | **Ruled out.** Net effect on the comparison is −0.38 nats against a 14,275-nat gap (0.003%), *in the direction that disfavours* low `p`. Across the whole profile the exact and SPA curves differ by ≤0.41 nats out of 20,250. |
| Weak/flat identification | **No.** The profile is sharply peaked: `p = 0.15` is 44.6 nats down, the empirical value 14,600. The likelihood is confidently wrong, not uninformative. |
| Marginal-likelihood information loss | **Real but minor.** An exact adjacent-pair composite moves the optimum only 0.10 → 0.20. |
| Poisson under-dispersion | **Ruled out.** `nb_likelihood()` gives a virtually identical profile (`phi ≈ 1.7`, active, and it displaces the churn not at all). |
| Batch severity explaining state heterogeneity | **Ruled out.** The four states are indistinguishable on max/p99/mean revision size. |
| Fabricated zeros as *the cause* | **Ruled out.** Real cells alone still prefer `p = 0.10` by 12,412 nats. |

### B.3 What is ESTABLISHED

**The gap is 272 outlier cells, not a systematic signal.** Decomposing the
14,275 nats on Texas:

| cell type | n | gap | per cell |
|---|---:|---:|---:|
| `z = 0` | 1304 | +1,445 | +1.1 |
| `z > 0` | 231 | +7,164 | **+31.0** |
| `z < 0` | 41 | +5,665 | **+138.2** |

The top **ten** cells are 53% of the gap. Deleting 25 of 1576 cells moves the
optimum from 0.10 to 0.60.

**The publication cadence has holes, and that is a contributing artefact.**
FluSight publishes 63 snapshots for 105 event weeks, with gaps of 14, 18, 21, 42
and **203** days (the off-season). `prepare_data()` builds a DENSE
(event-time × delay) array and zero-fills it, so **688 of 1576 cells (43.7%)
correspond to snapshots that were never published**, all entering as observed
zeros. For an event week first published at delay *k*, the model is told "zero at
delays 0..k−1" and then "+<the whole count> at delay k".

**The intervals are GIVEN, not detected.** The observable delays for an event week
are exactly those carrying a snapshot; consecutive observed delays give
`(d_prev, d_next]`. This dissolves §10/14/15/16/20 of the batch-detection plan —
there is no interval to infer, no threshold to calibrate, no double counting.
Reading the two biggest outliers this way takes them from **97.9 sd and 95.5 sd**
to **3.2 sd and 2.8 sd**. They were never anomalies: they are first observations
after a skipped snapshot, misread as delay-1 increments.

**But a true interval refit does not solve it** (`17_true_interval_refit.R`,
nuisance parameters re-optimised under each objective, saturated `lambda_t`, point
arm reproduces `p = 0.10` as a control):

| `p` | point | interval | `g_C(1)` interval |
|---|---:|---:|---:|
| 0.10 | **0.00** | −668 | 1.000 |
| **0.40** | −648 | **0.00** | **0.854** |
| 0.60 | −1907 | −54 | 0.927 |
| 0.96 | −13220 | −4727 | 0.619 |

Optimum 0.10 → 0.40, broad 0.40–0.60 plateau, `g_C(1)` off the boundary, penalty at
the empirical value cut from 13,220 to 4,727. **Still far from 0.95.**

*(The `p = 0.40` fit has `g_C` median 3.2e11 — a degenerate optimiser corner. The
`p = 0.60` fit beside it is sane and 54 nats worse. The plateau is real; the
precise optimum inside it is not.)*

### B.3a RESOLVED 2026-09-02 — parameter recovery (`FINDINGS.md` §I)

The decisive experiment ran: simulate from the model's own generative process on
the real cadence and interval mechanism, refit `p`. Verdict:

* **The estimator is sound.** `p_true = 0.95 -> p_hat = 0.92-0.97` when retraction
  falls within the observation horizon. It never collapses to 0.1-0.4 on
  correctly-generated high-`p` data. Result A (intrinsic defect) is **rejected**.
* **A long retraction tail confounds `p`.** With `Gbar(horizon) = 0.80`,
  `p_true = 0.60 -> p_hat = 0.15` — the exact real-data pathology, from a dataset
  whose retained-fraction-at-horizon (0.92) *looks like* real Texas. Result C
  (finite-horizon confounding `p <-> Gbar(a)`) is **confirmed as the primary
  cause**.
* **On the real data** (script 19): pinning the retraction tail short lifts the
  optimum from `p ~ 0.50` to `~0.70`. So confounding is active and accounts for
  most of the gap; the residual `~0.70 -> 0.95` is the §B.4 negative batches.

This **demotes** the "batch revisions are THE cause" reading of §B.1/FINDINGS
Conclusion to a residual. The production fix (fixing `p`) is now justified by
non-identifiability, not just misspecification — rewrite §B.5's rationale.

**Follow-ups run this session (FINDINGS §I.7):**

* *Isolating the residual* (`21_delete_negbatch.R`): deleting only the 3 worst
  negatives (event times 15/16/17) with the tail pinned short lifts the optimum
  `p = 0.65 -> 0.75`. Real, but the negatives are **diffuse** (weeks 11-16, 75),
  not 3 deletable outliers, so this alone does not reach 0.9.
* *Prototyping the successor* (`22_prototype_successor.R`): a weakly-informative
  prior on `logit(p)` is a **weak lever** (only a near-delta ≈ the hard fix moves
  `p`; `s=0.05 -> 0.69`); bounding `Gbar(horizon)` alone is **insufficient**
  (`p=0.55`, because late within-horizon retraction also confounds). Identifying
  `p` needs the tail constrained **short in shape** — external retraction-*timing*,
  not a prior on `p`. So the hard fix is closer to necessary than hoped.

### B.4 The residual suspect (now a smaller, isolated term)

The thing the interval reading provably **cannot** touch: event weeks 15–17 have
every snapshot present, so their `−386 / −385 / −382` are byte-identical under both
observation models. Three consecutive fully-published snapshots each removing ~20%
of an event week's count. That is an administrative shock indexed by snapshot
`s = t + d`, not by event time `t`, and no amount of temporal coarsening
represents it.

**Next experiment, not yet run:** delete only those three weeks from the *interval*
fit and re-profile. That isolates them from the cadence effect, which the earlier
deletion test conflated.

### B.5 Current production behaviour, and its justification

`default_priors()` **fixes** `p` at the empirical down-revision rate for
count-cumulative, and keeps the weak Beta for linelist / count-incidence (where the
cure block identifies `p` directly).

**The justification in the code and the vignette is now out of date.** It is
written as an identification fix. Thread B shows it is really a *guard against a
misspecified revision model* — which also explains the undercoverage rather than
being surprised by it, since fixing `p` blocks the inflation and the inflation was
the model's only cover for the outliers. Rewriting that rationale is a live task.

Backtested over 6 as-of dates × 4 states, fixing `p` is **catastrophe-avoidance,
not uniform improvement**: WIS 14.1 vs 46.4 (TX) and 9.8 vs 31.5 (CA), but 9.2 vs
9.0 (NY) and 9.6 vs 9.2 (FL) — and it costs coverage in every state. Both arms are
badly under-dispersed (nominal 50% covering 0.17–0.50). **That undercoverage is a
separate problem and must not be assumed to follow from fixing `p`.**

---

## 5. The trap that bit three times

Every summation over a latent count in this model failed the same way: a window
sized by mean-and-variance when the mass sits 20–50 standard deviations out.

1. The exact Skellam reference truncated at `qpois(1 - eps, alpha + omega)` — the
   *marginal* total — when the conditional mode is at `n* = |z| + 2w*` with
   `w* = (-|z| + sqrt(z^2 + 4 alpha omega)) / 2`. On a real cell
   (`z = 2512, alpha = 1204, omega = 974`) that is 3317 against a cutoff of 2528.
   **The sum was cut off before reaching its own maximum** and understated `logP`
   by 182 nats. It looked exactly like a catastrophic SPA bug.
2. The pairwise likelihood forced `bin_type = 1` into every inner Skellam, when a
   structurally-zero rate must take the Poisson branch. `lam4`/`lam5` are
   proportional to `(1 - p)`, so this corrupted the high-`p` end specifically.
3. The pairwise window was sized from `lam1`'s spread rather than from feasibility.
   At `d = 0` both `lam4` and `lam5` are structurally zero, so `Delta_1 = -N1 + N3`
   and an observed `v = -103` requires `n >= 103` — the window stopped at 37 and
   returned `-Inf` for a cell whose true log-probability is about −195.

**None of these is caught by the usual checks.** The pmf still sums to 1, the
moments are still exact, and Monte Carlo has nothing to say at probabilities of
e^-1269. Only the far tail is wrong — and under the low-`p` solution that is
exactly where the likelihood lives.

Keep `z = 2512, alpha = 1204, omega = 974` as a permanent stress test.

---

## 6. Where to pick up

**If continuing Thread B** — the diagnosis is settled (finite-horizon confounding,
`FINDINGS.md` §I; the two obvious successors were tried and found wanting, §I.7).
The work left is *design*, not diagnosis, in order:

1. **The retraction-timing question is now the crux.** `p` is identified only if the
   `g_C` tail is known to be short *in shape*. So the real deliverable is: where does
   external retraction-timing information come from? Options — a fixed/informative
   `g_C` from FluSight revision studies or another surveillance stream; joint
   learning of `g_C` from mature cohorts (long follow-up) applied to recent cohorts;
   or a hierarchical `g_C` across states/seasons. Prototype one and check it
   identifies `p` on simulated data (extend script 18) before real data.
2. **Model the negative administrative batches separately.** They are diffuse
   down-revisions (weeks 11-16, 75, …) that are *not* individual false-report
   retractions and must not be pushed through `omega_t ∝ mu_t(1-p)`. A
   snapshot-indexed (`s = t + d`) shock component for the negative side only (v3
   response §22) is the natural form. This is the residual `~0.75 -> 0.95`.
3. **Rewrite the fixed-`p` rationale** in `vignette("Mathematics")` §9.4 and the
   paper around non-identifiability (FINDINGS §I.4), not just "misspecification
   guard". A prior on `p` is explicitly NOT the fix (§I.7) — do not propose it.
4. Interval undercoverage is a **separate** problem — full-generative parametric
   bootstrap, not assumed to follow.

**The scripts** (all `pkgload::load_all`, `NOT_CRAN=true`, in `spa_diagnostics/`):
`18_param_recovery.R` (Monte-Carlo recovery; env `PR_MODE`=`time1`/`full`/`none`,
`PR_R`=reps; the estimator + real Texas cadence live here and the others source it
with `PR_MODE=none`), `19_real_tail_constraint.R` (free vs pinned-short on real
data), `20_recovery_figure.R` (the figure), `21_delete_negbatch.R` (residual
isolation), `22_prototype_successor.R` (prior-on-`p` vs tail-bound). Results in
`param_recovery.rds`.

**Estimator notes for whoever extends script 18.** The per-event-time `lambda_t` is
profiled out by a *vectorised two-stage grid* (`.grid_M` coarse-locate then
fine-refine, `neg_loglik`), validated to reproduce the exact per-`t` `optimize()`
profile (peak `p=0.92` on the seeded reference dataset) to a few nats — ~10x faster
than 93 scalar `optimize()` calls. Infeasible `theta` (a structurally-zero rate
against a same-sign observed increment) is rejected as `1e12`; do **not** replace
that with a soft finite penalty (it biases Nelder-Mead — cost me several iterations).
Warm-start continuation across the `p` grid removes local-optimum jaggedness. The
default long-tail start is infeasible on the real negative batches — seed from a
short-tail `start` (see script 19).

**If continuing Thread A**, the open items are:

1. **Drop `validation_censored=` and read `get_is_censored_validation()`** instead
   (§0b) — the tbl.now attribute it was waiting for has landed.
2. Rewrite the fixed-`p` rationale around misspecification (§B.5).
3. Catch up with tbl.now 0.30.0/0.31.0 when they are installed (§0b).
4. Use `covid_us` for end-to-end validation tests now that it carries a validation
   process (§0b).
5. The day-of-week bug (§A.4), and `coef()` omitting `logit_confirm_p` /
   `retract_mu` (read them off `parList`).

**Committing.** Nothing is committed. The numerics/bug fixes stand on their own and
are individually verifiable; the rename is one breaking change. Splitting them is
still the right call.

---

## 7. The documents

| file | what it is |
|---|---|
| `devel/spa_diagnostics/FINDINGS.md` | Thread B in full, with every number and every failed hypothesis |
| `devel/P_IDENTIFIABILITY.md` | the `p` problem stated for a reader deciding what to do; §7 needs revising per §B.5 |
| `devel/TEST_PLAN_validation.md` | the validation-process test grid, 10 invariants, 7 known gaps |
| `devel/HANDOFF_validation.md` | the earlier handoff; §0.1–0.7 are current, §1 onward is session-1 state |
| `Next diagnostic steps for the FluSight...md` | the first external diagnostic plan (SPA / marginal / identification) |
| `Response to FINDINGS_v2_...md` | the second — profile, joint likelihood, section 18's seven steps |
| `Next steps after FINDINGS_v3_...md` | the third — the interval observation model and batch detection |
| `main_journal_revised.tex` | the driving specification; cite equations by name (`alphasimplified`, `omegadef`, `noconfirmcum`, `chilinelist`) |

---

## 8. A note on how this went

Four hypotheses of mine were refuted by measurement during Thread B: NB
overdispersion, batch severity explaining the state heterogeneity, fabricated zeros
as the cause, and the prediction that a true interval refit would move `p` *up*
relative to the staged evaluation (it moved down, 0.60 → 0.40). Each was tested
rather than argued, which is why they are recorded rather than believed.

Two claims in `FINDINGS.md` were walked back after being stated too strongly — the
covariance "fingerprint is absent" result (contaminated in both directions; see
§C.1 there) and "the joint likelihood WILL prefer low `p`" (an expectation, not a
result). Both corrections are in the document.

The staged-evaluation pattern is worth knowing: evaluating objective X along
parameters fitted under objective Y has understated the effect twice and overstated
it once. Do not trust a staged profile to settle anything; re-optimise.
