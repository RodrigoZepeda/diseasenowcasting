# Cross-session test plan: the validation process

**Written 2026-09-01, `skellam` branch, diseasenowcasting 2.2.0.**

The validation process is the widest component in the package: it multiplies
across data type, mode, model component and fit type, and most of those cells are
reachable only through `nowcast()`. This is the map of what is covered, what is
not, and what a future change must not break.

Run everything with `NOT_CRAN=true` — the fit-heavy tests are `skip_on_cran()` and
a clean run looks far greener than it is:

```bash
NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_dir("tests/testthat")'
```

**Never `library(diseasenowcasting)` in a devel script.** The installed build is a
different, older package; session 1 measured a whole set of conclusions against it
without noticing. Every `devel/` script uses `pkgload::load_all(".")`.

---

## 1. The grid

### 1.1 data type × mode

| | `retraction_only` | `confirmation_only` | `both` | none |
|---|---|---|---|---|
| **linelist** | covered | covered | covered | covered |
| **count-incidence** | covered (bit-identical to linelist) | covered | gap | covered |
| **count-cumulative** | covered | **must error** (covered) | **must error** (covered) | covered |

The count-incidence claim that matters is *bit-identical engines and
log-likelihoods* against the linelist form, since every statistic is a weighted
tally. `test-confirmation-mode.R` asserts it for `confirmation_only`; the
`both` cell is a **gap**.

### 1.2 mode inference

| data | `mode = "auto"` | asserted mode |
|---|---|---|
| only `"retracted"` outcomes | `retraction_only` | matching: ok; other: error |
| only `"confirmed"` outcomes | `confirmation_only` | matching: ok; other: error |
| both outcomes | `both` | narrower: **warning**, not error |
| nothing resolved | falls back to `"none"` | **error** |
| date present, type `NA` | **error** | **error** |

The asymmetry is deliberate and is the thing most likely to be "fixed" by
mistake: an assertion the data cannot satisfy is an error, while an inference
with no evidence falls back to the ordinary count model. Asserting a *narrower*
mode than the data support throws information away but is legal, so it warns.

Inference reads the **full** data, not the as-of view. A test must exist that
walks two as-of dates and asserts the mode does not change; without it a future
refactor that reads the filtered frame will pass everything else.

### 1.3 model components × validation

`validation_process()` must compose with every delay family and every epidemic
process, and with both likelihoods. The cheap way to cover this is a **prior-only
smoke matrix** — no fitting, so the whole grid runs in seconds:

```
data_type   x  {linelist, count-incidence, count-cumulative}
epidemic    x  {hsgp, ar1, sir}
delay       x  {lognormal, gamma, generalized_gamma, dirichlet}
likelihood  x  {poisson, nb}
validation  x  {absent, present}
```

Assert per cell: no error, no `NA` in the draws, all draws finite and `>= 0`.
This is what would have caught #129 immediately, and it is currently only
partially built (`test-prior-only.R` covers the cumulative cells).

### 1.4 censoring

Four patterns, from the two flags:

| report side (`is_censored`) | validation side (`validation_censored`) | covered |
|---|---|---|
| exact | exact | yes |
| censored | exact | yes |
| exact | censored | yes |
| censored | censored | yes |

`validation_censored` is the **only** surviving validation argument, because a
`tbl_now` has no attribute for it. If tbl.now ever gains one, this argument
should be deprecated in favour of reading it, and these four cells are what
protects that change.

---

## 2. Invariants a change must not break

Each of these is a claim about the maths, not about the implementation. If one
fails, the model is wrong, not just the code.

1. **count-incidence == linelist**, bit-identical engines and log-likelihoods.
   Aggregation is a weighting, not an approximation.
2. **`rho(j)` moves the right way.** Rises with report age under retraction (an
   old standing report is probably genuine), falls under confirmation (an old
   unconfirmed report probably never will be), flat at `p` under `both` with a
   shared lag.
3. **Same-period rows.** Dropped under `retraction_only` (never visible in any
   data vintage), kept under `confirmation_only` (a test can come back the day it
   is ordered). This is the whole reason `lag_offset` exists.
4. **A confirmed case enters the nowcast with certainty; a retracted one with
   probability zero.** The predictive must never fall below the confirmed count.
5. **The settled count is never negative.** A count target cannot be. The
   cumulative path had this bug; the linelist path never did, because it thins
   observed rows rather than subtracting an independent draw.
6. **Nothing resolved reduces exactly.** An all-pending validation column must
   give a fit identical to the same data with no validation process at all —
   same `nll`, not merely a similar answer.
7. **`p = 1` with observed retractions is an error.** It asserts retractions are
   impossible while looking at some.
8. **Competing risks nests the shared-lag model.** Setting both lag laws equal
   must recover the shared-lag fit.
9. **The mode does not depend on the as-of date.**
10. **Every free parameter `.joint_reconstruct()` reads is supplied by
    `.sample_prior_parlist()`**, and every *fixed* one is absent. This is the
    structural guard against #129 returning; it is in `test-prior-only.R`.

---

## 3. Known gaps, in priority order

1. **`both` mode on count-incidence.** The only unrepresented data-type × mode
   cell. Aggregating a two-signed resolution is exactly where a weighting bug
   would hide.
2. **The prior-only smoke matrix of §1.3** is not built out beyond the cumulative
   cells.
3. **No as-of-stability test for mode inference** (§1.2).
4. **No shipped dataset carries `validation_date` / `validation_type`**, so every
   test above runs on synthetic data. Filed as
   [tbl.now#52](https://github.com/RodrigoZepeda/tbl.now/issues/52).
   `hai_bucaramanga` is the natural candidate — three date columns and a
   `final_condition` / `case_type` that may encode a real outcome.
5. **`change_now()` cannot walk backwards** past a validation
   ([tbl.now#51](https://github.com/RodrigoZepeda/tbl.now/issues/51)). Every test
   helper works around it by not pinning `now` on the object. When the issue is
   fixed, the workaround can go — and a test should assert `change_now()`
   re-censors rather than aborting.
6. **`coef()` omits `logit_confirm_p` / `retract_mu`** although they are fitted;
   they must be read off `parList`. Deliberately out of scope for 2.2.0.
7. **`.temporal_effect_matrix()` returns all-zero day-of-week columns** with an
   "NAs introduced by coercion" warning. Unrelated to validation, but it silently
   disables day-of-week covariates for *all* daily data, and it is the one
   remaining suite failure (`test-temporal-and-output.R:91`). Deserves its own PR.

---

## 4. Count-cumulative: what is being tested and what cannot be

The cumulative path is different in kind, and the plan should not pretend
otherwise.

* There are **no individual delay observations**. The appearance delay is
  informed only by the signed-increment likelihood, which is why `p` and `g_D`
  are not separately identified there — see `devel/P_IDENTIFIABILITY.md`.
* Consequently, **`p` is fixed by default** and the tests assert the *constraint*
  (`is_constant == 1L`), not a recovered value. A test that asserts `p_hat`
  approximates the truth on cumulative data is testing the plug-in estimator, not
  the model.
* The **zeros are load-bearing**. 83% of the cells entering the Skellam path on
  FluSight Texas are zero increments, and they are what pins `g_D`.
  `tbl.now::complete_zeroes()` is provably a no-op here — `increment_array` and
  `case_counts` come out identical — because `prepare_data()` builds the array
  zero-initialised and the likelihood walks a dense `0:horizon_t`. Do not
  "fix" this; it was measured.
* **Real-data behaviour is backtested, not spot-checked.**
  `devel/backtest_flusight_p.R` scores `p` fixed against `p` estimated over six
  as-of dates per state. A single as-of date gave a misleading answer once
  already.

FluSight itself needs two data-preparation rules, both properties of the file:

```r
START <- as.Date("2023-09-23")            # the first as_of
raw <- flusight |>
  filter(location_name == st, target_end_date >= START, as_of >= START) |>
  filter(as_of <= max(target_end_date))
```

Windowing is essential: 126 of 197 Texas event weeks otherwise first appear at a
delay of more than a week, up to 145 weeks. The truncation costs 7 event weeks
here because `as_of` has an off-season publication gap, and it does **not** remove
the trailing empty grid cell it was meant to — `now` follows the report axis and
`align_weeks()` displaces the two axes by different amounts. Both facts are
recorded in the script.
