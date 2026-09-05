# Handoff: the validation process (`skellam` branch)

**Written 2026-09-01.** Everything below is uncommitted work on branch `skellam`.
The driving specification is `main_journal_revised.tex` in the repo root — cite it
by equation name (`alphasimplified`, `omegadef`, `noconfirmcum`, `chilinelist`),
not by number.

---

## 0. Session 2 (2026-09-01, later) -- read this before section 1

Section 1's table below is the state at the END of session 1. This section
supersedes it where they disagree, and corrects three things session 1 got wrong.

### 0.1 Corrections to this document

* **Section 6's verification command tested the wrong package.**
  `devel/validation_diagnostics.R` and `devel/benchmark_validation_flusight.R`
  both called `library(diseasenowcasting)`, which loads the **installed** build.
  That build was 2.1.0, still carrying `.log_bessel_i_series` -- the broken
  density session 1 replaced. Every number session 1 quoted from those scripts
  describes pre-fix code. Both scripts now call `pkgload::load_all(".")`.
  **Always load the working tree.**
* **Section 5.3 misattributed two of the three test failures.** Only
  `test-temporal-and-output.R:91` is the `.temporal_effect_matrix()` day-of-week
  bug. The two `test-censoring-surprise-backtest.R` errors were
  `censor_delays_above(quiet = TRUE)` -- an argument tbl.now renamed to
  `verbose`. Fixed; that file is green. The day-of-week bug is untouched and
  still deserves its own PR.
* **Section 5.1's premise was wrong, and its options would not have worked.**
  See 0.3.

### 0.2 The FluSight data must be windowed to the snapshot era

`tbl.now::flusight` has `target_end_date` from 2022-02-05 but `as_of` only from
**2023-09-23**. Every event week before the first snapshot is left-truncated on
the report axis: its count "arrives" at the delay separating it from the first
snapshot. On Texas, **126 of 197** event weeks have their first observation at
delay > 1 week, up to **145 weeks**. Nothing can fit that.

```r
START <- as.Date("2023-09-23")
raw <- flusight |> filter(location_name == st, target_end_date >= START, as_of >= START)
```

This is a property of the dataset, not of the model. Any benchmark that skips it
is measuring the truncation.

### 0.3 Section 5.1 is DECIDED: `p` is fixed by default for count-cumulative

Windowing fixes the *delay* but not `p`: the empirical centre is invariant to it
(Texas 0.9599 unwindowed vs 0.9579 windowed). Sweeping the prior on the WORKING
TREE, windowed:

| state | arm | p_hat | g_C median | delay_mu |
|---|---|---|---|---|
| Texas (emp 0.958) | conc 10 / 100 / 300 | 0.111 / 0.120 / 0.142 | 0.04 | 3.08 / 2.97 / 2.71 |
| Texas | **fixed** | -- | 0.98 | **-0.164** |
| California (emp 0.990) | conc 10 / 100 / 300 | 0.094 / 0.103 / 0.124 | 0.04 | 3.23 / 3.11 / 2.83 |
| California | **fixed** | -- | 1.55 | **-0.122** |

Thirty times more prior information buys 0.03 against a gap of 0.85. The reason
is a **degenerate churn optimum**: gross reports ~10x the truth arrive and ~90%
are retracted at the shortest lag `g_C` allows, which cancels out of the observed
increments (so it is free) and lets `g_D` stretch to a 15-25 week median. A prior
cannot remove a second optimum.

`default_priors()` now **fixes** `p` at the empirical down-revision rate for
count-cumulative, and keeps the weak Beta for linelist / count-incidence (where
the cure block identifies `p` directly). `p = beta_prior(...)` still frees it.
Rationale is in the code comment; do not shorten it.

### 0.4 What landed this session

| | |
|---|---|
| 5.2 negative quantiles | **fixed** -- `R/15_nowcast.R`. The still-standing erroneous mass was an INDEPENDENT Poisson subtracted from the observed cumulative, so nothing bounded it. `E[C] = lambda G_D + retraction_mean` exactly, so it is a sub-population: now a binomial thinning of the observed rows, `prob = retraction_mean/observed_mean`, carrying no frailty (same argument as `.thin_standing_rows()`). `min(q5) = 0` on FluSight, was -4. |
| #128 | **fixed** -- `R/20_nowcast_class.R`. Count-cumulative promotes the inert default to a `confirmation_process()`, as a retraction column already did. `model()` went from "failed to converge for all init attempts" to `convergence = 0`. |
| #129 | **fixed** -- `R/32_prior_only.R`. `.sample_prior_parlist()` now samples `logit_confirm_p` / `retract_mu` / `log_retract_sd_exc`; `.simulate_prior_draws()` keeps the first condition and aborts (or warns on a partial failure) instead of returning a silent all-`NA` result. 1000 draws, 0 NA. |
| 5.1 | **decided + implemented**, see 0.3 |
| regression net | started -- 4 tests in `test-prior-only.R` (including one asserting the parlist supplies every free name `.joint_reconstruct()` reads) and 3 in `test-confirmation.R` for the `p` default. |

Suite: **1088 pass, 1 fail** -- the pre-existing day-of-week failure only.

### 0.5 OPEN: a missing event-week is modelled as an observed zero

On windowed FluSight Texas the headline nowcast is **1**, for a week with no
data, instead of **232** for the real last week.

```
n distinct event dates : 112     max_time : 113
event-times with NO rows in the data : 113      <- the only one
fitted lambda, last 6  : 128.4 140.1 151.0 174.6 219.3   6.0
observed cumulative    : 115   125   135   159   217     0
G_D(0)                 : 0.555
```

`prepare_data()` zero-fills the full `max_time` grid, so an event week absent
from every snapshot reads as "0 cases observed at delay 0". With `G_D(0) = 0.555`
that is strong evidence, and `lambda` collapses to 6 from 219. `@target` points
at that cell.

**The user's ruling:** an absent cell is not a zero -- that is how `tbl_now` is
constructed -- and the package must call `tbl.now::complete_zeroes()` where it
requires zeros rather than fabricating them. tbl.now's own documentation agrees:
"a missing cell is ambiguous: it could be a genuine zero, or a delay so long the
report has not arrived yet ... Filling beyond that would invent observations from
the future."

Measured on windowed Texas, `complete_zeroes()` is **safe for cumulative data**:
negatives stay at exactly 69 and `sum_down` is unchanged, so it invents no
down-revisions. What it adds is **leading zeros** -- "the cumulative was 0 at
delays 0-3, then 50 at delay 4". Those are exactly what identifies `g_D`, and
without them the de-accumulation makes the first observation an increment at its
own delay, i.e. the model is told every case arrived at delay 4.

**RESOLVED -- the hypothesis was wrong.** Fitting with and without
`complete_zeroes()`, across `p` fixed / Beta(10) / Beta(300), on both states, gave
**bit-identical** results (p_hat 0.9897 / 0.0936 / 0.1229 either way). The engines
explain it:

```
rows in tbl_now       : 3131  vs  6441      complete_zeroes adds 3310 explicit zeros
engine m dims         : 3131  vs  6441      they do reach `m`
case_counts identical : TRUE
increment_array ident : TRUE                <- what the cumulative likelihood consumes
```

`.deaccumulate_to_increments()` already reconstructs the full delay path per event
week, so the leading zeros were never absent. `complete_zeroes()` changes `m` but
not `increment_array` / `case_counts`, and the cumulative path reads only those.
**On this path the implicit fill and tbl.now's sanctioned completion agree
exactly**, so there is no fabricated-zero bug for count-cumulative and 0.3 stands
on its own evidence.

`complete_zeroes()` also independently asserts the phantom cell's zero (it creates
the 2025-11-09 row at delay 0), because a report *could* have arrived by `now`. So
what remains is narrower than first written: `now` comes from the REPORT axis and
lands one week past the last event week, so the grid carries a week that -- for a
stream publishing only completed weeks -- cannot have been reported at any delay
yet. `d_star` says delay 0 is observable there; for FluSight it is not.

Still open, and worth a decision: whether `d_star` should account for an event
period that has not closed, or whether the grid should not extend past the last
event period the stream could have published. Do NOT "fix" it by suppressing
zeros; that was tested and is not the mechanism.

---

### 0.6 Session 2, part 2: the rename and the docs

Items 1, 2, 3, 5 and 6 of section 1 are **done**. `validation_process()` replaces
14 exports with 5; `nowcast()` detects the process instead of taking column
arguments (only `validation_censored =` survives, because a `tbl_now` has no
attribute for it); `tidy()` is `parameters()`; DESCRIPTION is 2.2.0. The vignette
is `Validation_processes.Rmd`, rewritten rather than renamed. NEWS, SKILL.md
section 2a, `vignette("Mathematics")` section 9.4 and `introduction.Rmd` are all
updated. `devel/TEST_PLAN_validation.md` and `devel/P_IDENTIFIABILITY.md` exist.

Two decisions taken during the work, both from the user:

* **All-pending data.** `mode = "auto"` with nothing resolved fits the ordinary
  count model. An **asserted** mode does NOT error -- it keeps the mode and lets
  `p` be carried by its prior. "That's why this is a Bayesian method." The same
  now applies to "nothing confirmed *yet*" in the as-of view, which used to abort.
* **An unrecognised `validation_type` is an error.** A dated row labelled
  anything but `"confirmed"` / `"retracted"` used to fall through every
  `== "confirmed"` test and be silently counted as a RETRACTION in `both` mode.
  Both the mode inference and the as-of path now refuse it by name.

`vignette("Mathematics")` section 9.4 previously claimed a **strong** data-informed
prior identified `p` and that a weak one let "the Skellam variance abuse the
retraction stream". That was the pre-fix rationalisation and is false; it now
states the ridge, the degenerate optimum, and the constraint. If you are citing
the maths, cite the new text.

### 0.7 OPEN: `d_star` on an event period that has not closed

Left deliberately, at the user's direction. On FluSight the final grid cell claims
delay 0 is observable for a week that has not ended, so it cannot have been
reported at any delay. `now` follows the REPORT axis and `align_weeks()` displaces
the two axes by different amounts (event Saturdays back 6 days, report Wednesdays
back 3), so a 4-day gap becomes a 7-day one -- exactly one extra event-time.

Things already ruled out, do not re-test them:

* it is not a fabricated zero -- `complete_zeroes()` asserts that same cell;
* no data-side truncation removes it (three variants measured, see
  `devel/backtest_flusight_p.R`);
* it does not affect the backtest conclusions, which score over six as-of dates.

The open question is whether `d_star` should reflect an unclosed event period, or
whether the grid should stop at the last period the stream could have published.

---

## 1. The original request, and where each part stands

| # | Asked for | Status |
|---|---|---|
| 1 | Use `confirmation_process` automatically when `tbl_now` carries `validation_date` / `validation_type` | **not started** |
| 2 | Implement the line-list cases (confirmation-only / retraction-only / both) per the tex, user-settable but inferred | **not started** (the maths is already right — see §4) |
| 3 | Default to adding a validation process when those columns exist | **not started** |
| 4 | Make count-cumulative match the tex; error on confirmations there | **numerics DONE**, plumbing not started |
| 5 | Remove `tidy()` so tbl.now's is used | **not started** |
| 6 | Resolve #128 and #129, design regression tests and a cross-session test plan | **not started** |

Only item 4's *numerics* landed. That was not the plan — it expanded because the
count-cumulative likelihood turned out to be wrong at realistic counts and unable to
fit at realistic retraction rates, which blocks testing anything else.

---

## 2. Decisions already made — do not re-litigate

These came from the user directly. A future session should treat them as settled.

* The component is **`validation_process()`**, a **hard rename** with no deprecated
  aliases (matching what tbl.now 0.28.0 did). `confirmation_process()`,
  `resolution_process()`, `model(confirmation = )` and the `*_confirmation()` /
  `*_retraction()` / `*_resolution()` lag constructors all **go away**.
  `model(validation = validation_process(...))`, `validation_delay = `.
* Mode is set on the component — `validation_process(mode = c("auto",
  "confirmation_only", "retraction_only", "both"))` — **not** on `nowcast()`.
* `auto` infers the mode from `unique(validation_type)` over the **full** data, not
  the as-of view, so the mode is a stable property of the data source and does not
  flip between backtest dates.
* A `validation_date` with an `NA` `validation_type` is an **error**. The row is
  resolved but its sign is unknown, so it cannot enter either lag law. tbl.now
  already warns at construction; this is the second and final ask.
* Count-cumulative **may** carry a validation process — the tex's `g^val_{D-}` is
  exactly that — but **confirmations there are an error**: eq. `noconfirmcum` sets
  `g^val_{D+} = 0` because a confirmation does not change a cumulative count, so the
  confirmation-delay parameters are unidentifiable. Do **not** warn that
  "event + report + validation is impossible for count-cumulative"; that is wrong.
* The parameter tidier is renamed **`parameters()`**, not `tidy_parameters()`.
* The cross-session test plan goes in `devel/TEST_PLAN_validation.md`.

---

## 3. What changed in the working tree this session

Three source files and one test file. Everything else modified in `git status`
predates this session.

| File | Change |
|---|---|
| `R/14_objective_joint.R` | `eta_stream` and `retraction_mean` now divide by `p` |
| `R/28_confirmation_likelihood.R` | Skellam log-pmf rewritten as a saddlepoint; `.log_bessel_i_series()` / `.bessel_series_terms()` deleted |
| `R/07_default_priors.R` | count-cumulative `p` prior: floor removed, concentration 300 → 10 |
| `tests/testthat/test-confirmation.R` | Skellam tests rewritten around the new design |

New files: `devel/benchmark_validation_flusight.R`, `devel/validation_diagnostics.R`,
this handoff.

### 3.1 The `omega` fix

`R/14_objective_joint.R` computed `omega_d = lambda_t (1 - p) (g_D * g_C)(d)`; eq.
`omegadef` gives `omega_d = mu_t (1 - p) (g_D * g_C)(d)` with `mu_t = lambda_t / p`.
A factor of `p` too small.

This is **not** a harmless reparametrisation. Matching both moments requires `p'`
solving `p'^2 - p' + (1 - p) = 0` together with `lambda' = lambda p'/p`, so the
reported epidemic mean — the nowcast target — came out biased low:

| true `p` | coded `p'` | `lambda'/lambda` |
|---|---|---|
| 0.98 | 0.9796 | −0.04% |
| 0.90 | 0.8873 | −1.41% |
| 0.80 | 0.7236 | −9.55% |
| 0.70 | no real root | unrepresentable |

`options(diseasenowcasting.legacy_retraction_rate = TRUE)` reproduces the old form
for benchmarking. **Remove that option once the benchmark has been run** — it is
scaffolding, not a feature.

### 3.2 The Skellam log-pmf

Now a closed-form **saddlepoint** (the saddle solves a quadratic in `e^s`) blended
with the exact ascending series for the small-`alpha+beta` corner. No Bessel call.

| | before | after |
|---|---|---|
| worst error, 80 cells | 5729 nats | **0.00044** |
| gradients finite | no | **yes** |
| pmf sums to 1 | no | **yes** |
| Poisson + NB converge at `p = 0.98` | no | **yes** |
| FluSight fits | no | **yes** (37 s Poisson, 924 s NB) |

**The transferable lesson, which cost three failed designs to learn:** a weight of
zero does **not** neutralise a branch. A blend's derivative carries
`d(weight) * value`, so an unused branch whose value is far from the truth injects a
spurious gradient. Guarding the Bessel call by feeding `1` into its `log()` left the
branch at +673 against a true log-pmf of −4.56, and the fit then converged to
`p = 0.68` on data generated at `p = 0.90`. Both surviving branches are bounded
relative to the truth: the saddlepoint is never more than a few nats out, and the
series is an all-positive sum (hence a lower bound) floored near the saddlepoint.

The full rationale, including the three abandoned designs and the measurements that
killed each, is in the roxygen on `.log_skellam_increment()`. Read it before
touching that function.

Tunables, all in `R/28_confirmation_likelihood.R`:
`.SKELLAM_SERIES_TERMS` (60), `.SKELLAM_SERIES_GAP` (12),
`.SKELLAM_SWITCH_SCALE` (0.25), `.SKELLAM_SERIES_FLOOR` (20).

### 3.3 The `p` prior

Was `p_hat <- max(0.9, min(1 - retracted/appeared, 0.995))` with concentration 300.
Now floor removed, concentration 10.

Justification: the "weak prior makes `p` collapse" rationale was an artefact of the
broken density. With it fixed, sweeping concentration 300 → 1 on simulated data moves
`p_hat` by <0.01 and the *weakest* prior is the *most* accurate. The centre cannot be
fixed — a cumulative stream shows "not retracted **yet**", so it overstates `p` by
+0.007 at `p = 0.99` rising to +0.10 at `p = 0.7` — which is why it must be weak.

**But see §5.1: this is contradicted on real data.** It is the least settled part of
this session's work.

---

## 4. What is already correct and needs no work

The **line-list** maths in `R/31_retraction_likelihood.R` already matches eq.
`chilinelist` exactly:

```
confirmed  -> p * g_D+(d_val)
retracted  -> (1 - p) * g_D-(d_val)
pending    -> p * Gbar_D+(a) + (1 - p) * Gbar_D-(a)
```

Modes 0/1/2 (retraction-only / confirmation-only / both) all exist and are tested.
**Item 2 is therefore plumbing, not modelling.** The recommended route for items
1–3 is to derive, from tbl.now's one-date-plus-type representation, the two vectors
the existing pipeline already accepts:

* `confirmation_date` := `validation_date` where `validation_type == "confirmed"`
* `retraction_date`   := `validation_date` where `validation_type == "retracted"`

then set `resolution_mode` from the inferred mode. That reuses the whole tested path
and keeps the diff small.

---

## 5. Open problems, in the order they should be tackled

### 5.1 FluSight fits, but lands somewhere absurd — **decide this first**

`p_hat = 0.073` with a lag median of 0.03 weeks: "93% of reports are retracted
instantly". The empirical centre is **0.96** (97,327 positive vs 3,905 negative
increments on Texas). The concentration-10 prior cannot hold it; concentration 300
would have.

This is the article's own warning made concrete — a cumulative stream needs "a model
constraint, informative prior information, or additional validation data". The
tension is real and both sides are right:

* the weak prior is correct when the model is **well specified** (the simulation is
  generated from it, and the weak prior wins there);
* the strong prior was silently compensating for **misspecification** on real
  snapshot data, where a revision is not really "a false case being retracted".

Options: revert to a strong concentration; pick an intermediate; keep it weak and
document that count-cumulative `p` is unidentified under misspecification; or warn
when the fitted `p` departs far from the empirical centre. **Not decided.** Do not
change it without asking.

### 5.2 Predictive quantiles go negative

FluSight gives `q5 = -4` and `q5 = -1.05` at the last two event times. A count
target must not do that. Separate bug, not yet investigated.

### 5.3 Pre-existing failures, unrelated to this work

`test-temporal-and-output.R` and `test-censoring-surprise-backtest.R` — 1 failure and
2 errors. `.temporal_effect_matrix()` returns all-zero day-of-week columns with an
`NAs introduced by coercion` warning at `storage.mode(X) <- "double"`. Present before
any change this session. Worth its own fix; it silently disables day-of-week
covariates for **all** daily data, not just this path.

### 5.4 Smaller things

* `coef()` omits `logit_confirm_p` / `retract_mu` although they are fitted. Read them
  off `fit@fits[[1]]$parList`. Worth fixing while doing `parameters()` (item 5).
* FluSight NB takes ~15 minutes for one fit. Usable but slow.
* `tbl.now::change_now()` **errors** on a validation-carrying `tbl_now` when the new
  `now` precedes the latest validation, instead of re-censoring. `backtest()` does
  not hit it (dcast3 masks as-of itself) but users will. **Issue to file on tbl.now.**
* **No shipped tbl.now dataset carries `validation_date` / `validation_type`**, so
  items 1–3 have nothing to test against end-to-end. `hai_bucaramanga` has three date
  columns (`specimen_date`, `received_date`, `report_date`) and its docs point at
  `add_validation_date()`, so it is the natural candidate. **Issue to file on
  tbl.now.**

---

## 6. Verifying the current state

```bash
Rscript devel/validation_diagnostics.R
```

Expected: every row of block A converges; `p_hat` within ~0.03 of truth and the lag
median near 3; block C worst error <1e-3 nats and total mass 1.000.

Full suite (the fit-heavy tests are `skip_on_cran()`, so the flag matters):

```bash
NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_dir("tests/testthat")'
```

Expected: 1065 pass, and only the three pre-existing failures from §5.3.

---

## 7. Suggested order for the next session

1. **Decide §5.1** (the `p` prior under misspecification). It gates whether
   count-cumulative is trustworthy on real data, and everything downstream inherits it.
2. **#128** — count-cumulative must promote the inert default to a validation
   process. `nowcast()` already does this for a retraction column; extend the same
   branch to `is_cumulative`.
3. **#129** — `.sample_prior_parlist()` never samples `logit_confirm_p`,
   `retract_mu`, `log_retract_sd_exc`, so every prior-only draw for cumulative data
   is `NA`; and `.simulate_prior_draws()` swallows the error in a `tryCatch` that
   discards it, returning a full-size all-`NA` result with no warning. Fix both:
   sample the parameters, and abort (surfacing the captured condition) when no draw
   succeeded.
4. **Regression net** — a prior-only smoke matrix over data_type × model components,
   plus a test asserting the parlist supplies every name `.joint_reconstruct()`
   reads, so a new component cannot reintroduce #129.
5. **Items 1–3** — `validation_process()` rename and `validation_date`
   auto-detection, via the two-vector route in §4.
6. **Item 5** — delete the `tidy` generic; rename the parameter table to
   `parameters()`. tbl.now's `.onLoad()` already has the handover coded and will take
   over automatically once dcast3's method disappears — see its comment naming
   "diseasenowcasting 2.2.0". Update the two vignettes, `SKILL.md`, `NEWS.md`,
   `helper-retraction.R`, `test-confirmation-mode.R`, and the `print()` message at
   `R/21_nowcast_methods.R:218`.
7. **Item 6** — `devel/TEST_PLAN_validation.md`, covering data_type × validation mode
   × model component × fit type, plus the two tbl.now issues from §5.4.

---

## 8. Notes for whoever picks this up

* Read the roxygen on `.log_skellam_increment()` before touching the density. Three
  designs failed there and the reasons are not guessable from the code.
* `coef()` is not the way to read validation parameters; use `parList`.
* The fit-heavy tests skip on CRAN — always run with `NOT_CRAN=true` or a clean run
  looks far greener than it is.
* Nothing here is committed. Consider committing the numerics fixes separately from
  the plumbing, since they stand on their own and are individually verifiable.
