# tidy() -- the cross-package nowcast contract.
#
# One row per event date per stratum, with fixed column names/order/types, so
# downstream code can bind these tables across nowcasting engines.  The generic
# is re-exported from `generics` (not defined here) so methods other packages
# register on the shared generic stay visible after library(diseasenowcasting).

.contract_columns <- c("event_date", "stratum", "estimate",
                       "conf.low", "conf.high", "level", "engine")

# The "tidy() changed meaning" warning fires once per session; reset it so each
# test sees the same behaviour regardless of the order tests run in.  (testthat
# runs tests with the package namespace on the search path, so the internal
# state environment is visible here.)
.reset_tidy_warning <- function() {
  .tidy_warning_state$warned <- FALSE
}

.fit_unstratified <- function(Tn = 40L, seed = 200) {
  nowcast(.make_synth_tblnow(Tn = Tn, seed = seed),
          model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
          type = "one_stage", n_draws = 150, seed = 1)
}

.fit_stratified <- function(Tn = 45L, seed = 201) {
  nowcast(.make_strata_tblnow(Tn = Tn, seed = seed),
          model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
          type = "one_stage", n_draws = 150, seed = 1)
}

# ── 1. the column contract ──────────────────────────────────────────────────

test_that("tidy() returns exactly the contract columns, in order, with the right types", {
  pred <- predict(.fit_unstratified(), seed = 2)
  td   <- tidy(pred)

  expect_s3_class(td, "tbl_df")
  expect_identical(names(td), .contract_columns)
  expect_s3_class(td$event_date, "Date")
  expect_type(td$stratum,   "character")
  expect_type(td$estimate,  "double")
  expect_type(td$conf.low,  "double")
  expect_type(td$conf.high, "double")
  expect_type(td$level,     "double")
  expect_type(td$engine,    "character")
  expect_true(all(td$engine == "diseasenowcasting"))
  expect_true(all(td$level == 0.95))
})

# ── 2. unstratified ─────────────────────────────────────────────────────────

test_that("tidy() labels an unstratified fit 'all', one row per event date", {
  pred <- predict(.fit_unstratified(), seed = 2)
  td   <- tidy(pred)

  expect_true(all(td$stratum == "all"))
  expect_equal(nrow(td), length(S7::prop(pred, "event_dates")))
  expect_false(anyDuplicated(td$event_date) > 0L)
})

# ── 3. stratified: the case a naive implementation silently gets wrong ───────

test_that("tidy() reads per-stratum draws, one block per stratum", {
  pred <- predict(.fit_stratified(), seed = 2)
  lvls <- S7::prop(pred, "strata_levels")
  n_ev <- length(S7::prop(pred, "event_dates"))

  expect_false(is.null(S7::prop(pred, "strata_draws")))   # the fit is stratified
  expect_setequal(lvls, c("A", "B"))

  td <- tidy(pred)
  expect_setequal(unique(td$stratum), lvls)
  expect_equal(nrow(td), n_ev * length(lvls))
  expect_false(any(td$stratum == "all"))                  # never pooled
  # sorted by stratum then event_date, so each block is contiguous and ordered
  expect_identical(td$stratum, sort(rep(lvls, each = n_ev)))
  for (lvl in lvls)
    expect_false(is.unsorted(td$event_date[td$stratum == lvl]))

  # the per-stratum medians are genuinely the stratum's draws, not the pooled ones
  strata_draws <- S7::prop(pred, "strata_draws")
  for (k in seq_along(lvls)) {
    expected <- apply(strata_draws[, , k, drop = TRUE], 2L, stats::median)
    expect_equal(td$estimate[td$stratum == lvls[k]], unname(expected))
  }
})

test_that("tidy() on a stratified fit does not collapse to the pooled draws", {
  pred   <- predict(.fit_stratified(), seed = 2)
  pooled <- apply(S7::prop(pred, "draws"), 2L, stats::median)
  td     <- tidy(pred)
  # every stratum is a strict part of the total, so no block can equal the pool
  for (lvl in unique(td$stratum))
    expect_false(isTRUE(all.equal(td$estimate[td$stratum == lvl], unname(pooled))))
})

# ── 4. interval brackets the estimate ───────────────────────────────────────

test_that("conf.low <= estimate <= conf.high on every row", {
  for (pred in list(predict(.fit_unstratified(), seed = 2),
                    predict(.fit_stratified(),   seed = 2))) {
    td <- tidy(pred)
    expect_true(all(is.finite(td$estimate)))
    expect_true(all(td$conf.low  <= td$estimate))
    expect_true(all(td$estimate <= td$conf.high))
  }
})

# ── 5. conf.level actually changes the width ────────────────────────────────

test_that("a narrower conf.level gives a strictly narrower interval", {
  pred <- predict(.fit_unstratified(), seed = 2)
  wide   <- tidy(pred, conf.level = 0.95)
  narrow <- tidy(pred, conf.level = 0.50)

  narrow_width <- narrow$conf.high - narrow$conf.low
  wide_width   <- wide$conf.high   - wide$conf.low

  expect_true(all(narrow$level == 0.50))
  # No row widens.  Fully-observed early dates have a degenerate predictive, so
  # their width is 0 at either level -- compare the totals for strictness.
  expect_true(all(narrow_width <= wide_width))
  expect_lt(sum(narrow_width), sum(wide_width))
})

test_that("tidy() rejects an out-of-range conf.level", {
  pred <- predict(.fit_unstratified(), seed = 2)
  expect_error(tidy(pred, conf.level = 0),   "conf.level")
  expect_error(tidy(pred, conf.level = 1),   "conf.level")
  expect_error(tidy(pred, conf.level = c(0.5, 0.9)), "conf.level")
})

# ── 6. probs adds exact quantile columns ────────────────────────────────────

test_that("probs appends one q* column per probability", {
  pred <- predict(.fit_unstratified(), seed = 2)
  td   <- tidy(pred, probs = c(0.05, 0.95))

  expect_identical(names(td), c(.contract_columns, "q5", "q95"))
  expect_true(all(td$q5 <= td$q95))
  # exact, not approximated: they are the draws' own quantiles
  draws <- S7::prop(pred, "draws")
  expect_equal(td$q5,  unname(apply(draws, 2L, stats::quantile, probs = 0.05, names = FALSE)))
  expect_equal(td$q95, unname(apply(draws, 2L, stats::quantile, probs = 0.95, names = FALSE)))
  # q50 is the same summary as `estimate`
  expect_equal(tidy(pred, probs = 0.5)$q50, td$estimate)
})

test_that("probs names use probs * 100 and keep fractional widths", {
  pred <- predict(.fit_unstratified(), seed = 2)
  td <- tidy(pred, probs = c(0.025, 0.5, 0.975))
  expect_identical(names(td), c(.contract_columns, "q2.5", "q50", "q97.5"))
})

test_that("probs columns are added per stratum, not recycled", {
  pred <- predict(.fit_stratified(), seed = 2)
  td   <- tidy(pred, probs = 0.9)
  expect_identical(names(td), c(.contract_columns, "q90"))
  expect_equal(nrow(td), length(S7::prop(pred, "event_dates")) *
                         length(S7::prop(pred, "strata_levels")))
  expect_true(all(td$estimate <= td$q90))
})

test_that("tidy() rejects invalid probs", {
  pred <- predict(.fit_unstratified(), seed = 2)
  expect_error(tidy(pred, probs = c(0.5, 1.5)), "probs")
  expect_error(tidy(pred, probs = c(0.5, NA)),  "probs")
  expect_error(tidy(pred, probs = "0.5"),       "probs")
  # an empty probs is a no-op, not an error
  expect_identical(names(tidy(pred, probs = numeric(0))), .contract_columns)
})

# ── 7. no re-gridding ───────────────────────────────────────────────────────

test_that("event_date is the model's own grid, untouched", {
  pred <- predict(.fit_unstratified(), seed = 2)
  grid <- as.Date(S7::prop(pred, "event_dates"))
  expect_equal(tidy(pred)$event_date, grid)

  # stratified: each block repeats the same grid
  pred_s <- predict(.fit_stratified(), seed = 2)
  grid_s <- as.Date(S7::prop(pred_s, "event_dates"))
  td_s   <- tidy(pred_s)
  for (lvl in unique(td_s$stratum))
    expect_equal(td_s$event_date[td_s$stratum == lvl], grid_s)
})

# ── 8. the generic is `generics::tidy`, so nothing is masked ────────────────

test_that("tidy() is the generics generic, not a package-local one", {
  expect_identical(diseasenowcasting::tidy, generics::tidy)
  expect_identical(environmentName(environment(diseasenowcasting::tidy)), "generics")
  # ...and the methods are registered on it
  for (cls in c("diseasenowcasting::nowcast", "diseasenowcasting::nowcast_prediction"))
    expect_false(is.null(utils::getS3method("tidy", cls, optional = TRUE,
                                            envir = asNamespace("generics"))))
})

test_that("a method another package registers on generics::tidy is still found", {
  # Guards the masking bug: with a package-local `tidy` generic this dispatches
  # to our generic, which never sees the foreign method.
  registerS3method("tidy", "dn_foreign_test_class",
                   function(x, ...) "foreign method reached",
                   envir = asNamespace("generics"))
  on.exit(rm(list = "tidy.dn_foreign_test_class",
             envir = get(".__S3MethodsTable__.", envir = asNamespace("generics"))),
          add = TRUE)

  obj <- structure(list(), class = "dn_foreign_test_class")
  expect_identical(tidy(obj), "foreign method reached")
})

test_that("tidy() has no default method, so it does not change other packages", {
  expect_null(utils::getS3method("tidy", "default", optional = TRUE,
                                 envir = asNamespace("generics")))
  expect_error(tidy(1:10), "no applicable method")
})

# ── tidy() on the fit delegates to predict() ────────────────────────────────

test_that("tidy() on a fit returns the nowcast table and warns once per session", {
  nc <- .fit_unstratified()
  .reset_tidy_warning()

  expect_warning(td <- tidy(nc, seed = 2), "model_parameters")
  expect_identical(names(td), .contract_columns)
  expect_equal(nrow(td), nc@target)

  # warned once: the second call is silent
  expect_silent(tidy(nc, seed = 2))
})

test_that("tidy() on a fit is identical to tidy() on its prediction", {
  nc <- .fit_unstratified()
  .reset_tidy_warning()
  from_fit  <- suppressWarnings(tidy(nc, probs = c(0.1, 0.9), seed = 11))
  from_pred <- tidy(predict(nc, seed = 11), probs = c(0.1, 0.9))
  expect_equal(from_fit, from_pred)
})

test_that("tidy() on a fit passes n_draws and seed through to predict()", {
  nc <- .fit_unstratified()
  .reset_tidy_warning()
  suppressWarnings({
    small <- tidy(nc, n_draws = 40, seed = 3)
    again <- tidy(nc, n_draws = 40, seed = 3)
  })
  expect_equal(small, again)                     # seed makes it reproducible
  expect_equal(nrow(small), nc@target)
})

test_that("tidy() works on a stratified fit straight from nowcast()", {
  nc <- .fit_stratified()
  .reset_tidy_warning()
  td <- suppressWarnings(tidy(nc, seed = 2))
  expect_setequal(unique(td$stratum), c("A", "B"))
  expect_equal(nrow(td), nc@target * 2L)
})

test_that("tidy() works after a save/load round-trip", {
  nc <- .fit_unstratified()
  f  <- tempfile(fileext = ".rds")
  save_nowcast(nc, f)
  nc2 <- suppressMessages(load_nowcast(f))
  .reset_tidy_warning()
  td <- suppressWarnings(tidy(nc2, seed = 2))
  expect_identical(names(td), .contract_columns)
  expect_true(all(is.finite(td$estimate)))
})

# ── model_parameters() is the old tidy() table ──────────────────────────────

test_that("model_parameters() returns the per-term table tidy() used to return", {
  nc <- .fit_unstratified()
  mp <- model_parameters(nc)
  expect_s3_class(mp, "data.frame")
  expect_identical(names(mp), c("term", "estimate", "std.error",
                                "conf.low", "conf.high", "type"))
  expect_gt(nrow(mp), 0L)
  expect_false("event_date" %in% names(mp))
})
