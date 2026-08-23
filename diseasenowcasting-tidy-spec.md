# Brief: align `diseasenowcasting`'s `tidy()` with the cross-package nowcast contract

**Audience:** an LLM (or contributor) working in the `diseasenowcasting` repo.
**Goal:** make `tidy()` on a `diseasenowcasting` nowcast return the *same table*
that every other nowcasting engine returns, so downstream code (plotting,
scoring, comparison) does not special-case this package.

Everything below is verified against **diseasenowcasting as currently installed**
(`tidy` generic signature `function(x, conf.level = 0.95, ...)`), R 4.5.3.

---

## 1. Why this is needed

`tbl.now` normalises the output of seven nowcasting packages behind one
`tidy()`. Six of them work. `diseasenowcasting` does not, for two separate
reasons — and the first is the important one.

### Root cause A — the package defines its own `tidy` generic

```r
diseasenowcasting::tidy      # function (x, conf.level = 0.95, ...) UseMethod("tidy")
environment(diseasenowcasting::tidy)   # <namespace:diseasenowcasting>, NOT generics
```

The broom ecosystem's convention is to **re-export the generic from
`generics`**, so that every package's methods land on one shared generic.
Because this package declares a *new* generic instead, methods that other
packages register on `generics::tidy` are invisible to it. After
`library(diseasenowcasting)`, the bare name `tidy` resolves to this package's
generic, and any such method stops being found.

Observed consequence:

```r
library(diseasenowcasting)
tidy(predict(fit))
#> Error: No `tidy()` method for objects of class
#>   <diseasenowcasting::nowcast_prediction/S7_object>.
```

The method exists — it is just registered on the other generic.

### Root cause B — there is no method for the prediction object

`nowcast()` returns the model; the per-date estimates live in `predict(fit)`.
There is a method for the **fit** (`tidy.diseasenowcasting::nowcast`, returning
model parameters) but none for the **prediction**.

Combined, these make `tidy(fit)` silently return a *parameter* table where a
user comparing engines expects a *nowcast*: 45 rows of `term`/`std.error`
instead of 655 rows of per-date estimates. No error — just the wrong table.

---

## 2. What to change

### Change 1 (required): re-export the `generics` generic

Delete the locally-defined generic and re-export instead:

```r
#' @importFrom generics tidy
#' @export
generics::tidy
```

Register every method with `@exportS3Method generics::tidy`.

This alone fixes the masking for every downstream package, and is the single
highest-value change in this brief. Note it drops `conf.level` from the generic's
signature — `generics::tidy` is `function(x, ...)`. Keep `conf.level` as a
**method** argument (see §4); that is how `broom` handles it too.

### Change 2 (required): add a method for `nowcast_prediction`

Returning the contract in §3. Reference implementation in §4.

### Change 3 (recommended): rename the existing parameter table

Today `tidy()` on the fit returns model parameters:

```
dim: 45 x 6
cols: term, estimate, std.error, conf.low, conf.high, type
type values: covariate, epidemic_hsgp, epidemic_intercept, likelihood
```

**This is defensible broom grammar** — `tidy()` classically means "one row per
model term" — so renaming is a judgement call, not a bug fix. The argument for
renaming is that users comparing nowcasting engines call `tidy(fit)` and expect
a nowcast; every other engine gives them one.

**Proposed name: `tidy_parameters()`.** It says exactly what it returns and reads
naturally next to `tidy()`. Alternatives, in order of preference:

| name | note |
|---|---|
| `tidy_parameters()` | **recommended** — unambiguous, no clash |
| `model_parameters()` | matches the `parameters` package's verb; risks looking like an integration that does not exist |
| `tidy_terms()` | accurate but less obvious to a reader |

Do **not** call it `glance()`: that verb means a one-row model summary, and this
is one row per term.

If Change 3 is adopted, `tidy()` on the **fit** should return the nowcast, by
delegating: `tidy(predict(x), ...)`. Then `tidy()` means the same thing whichever
object a user happens to hold.

---

## 3. The contract `tidy()` must satisfy

One row per event date, per stratum. Column names, order and types are fixed —
downstream code binds these tables across engines, so a renamed or reordered
column breaks the comparison.

| column | type | meaning |
|---|---|---|
| `event_date` | `Date` | event/reference date, on the model's own grid — **do not re-grid** |
| `stratum` | `character` | stratum label; **`"all"`** when unstratified |
| `estimate` | `numeric` | point nowcast: the posterior **median** |
| `conf.low` | `numeric` | lower interval bound (broom naming) |
| `conf.high` | `numeric` | upper interval bound |
| `level` | `numeric` | the width the interval **actually** has, e.g. `0.95` |
| `engine` | `character` | `"diseasenowcasting"` |

Rules that matter:

* **Return a `tibble`**, sorted by `stratum` then `event_date`.
* **`level` is not decoration.** Engines differ (epinowcast defaults to a 90%
  band, others to 95%); without this column they get compared as if identical.
* **`estimate` is the median**, not the mean — the other engines report medians.
* **Never re-grid `event_date`.** Packages bin onto week starts of their own
  choosing; silently snapping hides a real difference between engines.
* **A `probs` argument** appends one column per requested quantile, named
  `q5`, `q50`, `q95` (`probs * 100`, so `0.025` → `q2.5`). This package keeps
  draws, so arbitrary quantiles are exact rather than approximations.

### Where the numbers come from

`predict(fit)` is an S7 object of class `"diseasenowcasting::nowcast_prediction"`
with these properties:

| property | shape (example) | use |
|---|---|---|
| `event_dates` | `Date`, length 655 | `event_date` |
| `draws` | matrix `2000 x 655` (draws × event times) | pooled estimates |
| `strata_draws` | array `2000 x 655 x 2`, or `NULL` | per-stratum estimates |
| `strata_levels` | `"Female", "Male"`; **`"all"`** when unstratified | `stratum` |

**The stratified case is the one that gets missed.** A stratified fit populates
`strata_draws`; reading only `draws` returns a pooled table with every row
labelled `"all"` even though the fit reports two strata. Branch on
`strata_draws` being non-`NULL`, not on `strata_levels`, because
`strata_levels` is `"all"` (length 1) rather than `NULL` in the unstratified
case.

---

## 4. Reference implementation

This is the logic `tbl.now` runs today, so a method matching it is known to
produce the right table on real fits.

```r
#' @param x A `nowcast_prediction` from `predict()`.
#' @param probs Optional probabilities in `[0, 1]`; adds one `q*` column each.
#' @param conf.level Interval width. Default `0.95`.
#' @exportS3Method generics::tidy
`tidy.diseasenowcasting::nowcast_prediction` <- function(x,
                                                         probs = NULL,
                                                         conf.level = 0.95,
                                                         ...) {
  event_dates   <- S7::prop(x, "event_dates")
  strata_draws  <- S7::prop(x, "strata_draws")
  strata_levels <- S7::prop(x, "strata_levels")

  tail_probs <- c((1 - conf.level) / 2, 1 - (1 - conf.level) / 2)

  summarise_draws <- function(draws, stratum) {
    out <- tibble::tibble(
      event_date = as.Date(event_dates),
      stratum    = as.character(stratum),
      estimate   = apply(draws, 2, stats::median),
      conf.low   = apply(draws, 2, stats::quantile, probs = tail_probs[1]),
      conf.high  = apply(draws, 2, stats::quantile, probs = tail_probs[2]),
      level      = conf.level,
      engine     = "diseasenowcasting"
    )
    if (!is.null(probs) && length(probs) > 0L) {
      stopifnot(is.numeric(probs), !anyNA(probs), all(probs >= 0 & probs <= 1))
      quantile_cols <- lapply(probs, function(p) {
        unname(apply(draws, 2, stats::quantile, probs = p))
      })
      names(quantile_cols) <- paste0(
        "q", format(probs * 100, trim = TRUE, scientific = FALSE,
                    drop0trailing = TRUE)
      )
      out <- dplyr::bind_cols(out, tibble::as_tibble(quantile_cols))
    }
    out
  }

  # A stratified fit carries `strata_draws`: draws x event times x stratum.
  if (!is.null(strata_draws) && length(strata_levels) > 0L) {
    per_stratum <- lapply(seq_along(strata_levels), function(k) {
      summarise_draws(strata_draws[, , k, drop = TRUE], strata_levels[k])
    })
    out <- dplyr::bind_rows(per_stratum)
  } else {
    out <- summarise_draws(S7::prop(x, "draws"), "all")
  }

  out[order(out$stratum, out$event_date), ]
}
```

### S3 registration gotcha

The class name contains `::`, which cannot appear in an S3 method **name**, so
`@exportS3Method` may not be able to express it depending on the roxygen
version. If it cannot, register by hand in `.onLoad()`:

```r
registerS3method(
  "tidy", "diseasenowcasting::nowcast_prediction",
  tidy_nowcast_prediction,           # the function, defined under a plain name
  envir = asNamespace("generics")
)
```

Also call `S7::methods_register()` in `.onLoad()`.

---

## 5. Backward compatibility

Change 3 alters what `tidy(fit)` returns, which is a breaking change. Suggested
handling:

* Add `tidy_parameters()` and make it the documented way to get the parameter
  table.
* Have `tidy()` on the fit return the nowcast, warning once per session that the
  return value changed and naming `tidy_parameters()`.
* Note it prominently in `NEWS.md`.

If a breaking change is unacceptable, adopt **Changes 1 and 2 only**. Those two
are non-breaking and already fix the cross-package problem: `tidy(fit)` keeps
returning parameters, `tidy(predict(fit))` returns the nowcast, and neither
masks anything.

---

## 6. Tests to add

1. `tidy(predict(fit))` returns exactly the columns in §3, in that order, with
   those types.
2. **Unstratified:** `stratum` is `"all"` throughout; row count equals
   `length(event_dates)`.
3. **Stratified:** one block per level of `strata_levels`; row count is
   `length(event_dates) * length(strata_levels)`; no row is labelled `"all"`.
   *(This is the case a naive implementation silently gets wrong.)*
4. `conf.low <= estimate <= conf.high` for every row.
5. `conf.level = 0.5` gives a strictly narrower interval than `0.95`.
6. `probs = c(0.05, 0.95)` adds `q5` and `q95`, with `q5 <= q95`.
7. `event_date` is unchanged from `S7::prop(pred, "event_dates")` — no re-gridding.
8. After `library(diseasenowcasting)`, a method registered on `generics::tidy`
   by another package is still found (guards Change 1).
