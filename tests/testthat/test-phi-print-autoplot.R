# Tests for likelihood phi, delegated common plotting, canonical output over
# sparse recent grids, and the common result printer.

suppressMessages(library(tbl.now))

.daily_tn <- function(Tn = 30L, seed = 1, start = as.Date("2020-01-01"),
                      strata = FALSE) {
  set.seed(seed)
  rows <- list()
  grp_levels <- if (strata) c("A", "B") else NA
  for (g in grp_levels) {
    for (t in seq_len(Tn)) {
      n <- rpois(1, 6 + 4 * sin(2 * pi * t / 12))
      if (n > 0) for (i in seq_len(n)) {
        d <- rpois(1, 2)
        row <- data.frame(onset = start + (t - 1), reported = start + (t - 1) + d)
        if (strata) row$grp <- g
        rows[[length(rows) + 1]] <- row
      }
    }
  }
  df <- do.call(rbind, rows)
  if (strata) tbl_now(df, event_date = onset, report_date = reported, strata = grp,
                      data_type = "linelist", verbose = FALSE)
  else        tbl_now(df, event_date = onset, report_date = reported,
                      data_type = "linelist", verbose = FALSE)
}

# ── TASK 1: phi lives on the likelihood, not nowcast() ───────────────────────

test_that("nowcast() has no phi argument", {
  expect_false("phi" %in% names(formals(nowcast)))
})

test_that("nb_likelihood() default phi is lognormal_prior(log(20), 0.5)", {
  ph <- nb_likelihood()@phi
  expect_true(S7::S7_inherits(ph, diseasenowcasting:::prior_class))
  expect_equal(ph@name, "LogNormal")
  expect_equal(ph@stan_params[1:2], c(log(20), 0.5), tolerance = 1e-8)
})

test_that("phi set on the likelihood flows into the fitted priors", {
  tn  <- .daily_tn(seed = 2)
  mdl <- model(nb_likelihood(phi = lognormal_prior(log(5), 0.4)),
               hsgp_epidemic(), lognormal_delay())
  nc  <- nowcast(tn, mdl, type = "one_stage", n_draws = 80, seed = 1)
  # priors$phi_nb should reflect the likelihood's phi prior
  expect_equal(nc@priors$phi_nb$params[1:2], c(log(5), 0.4), tolerance = 1e-8)
  # and it is carried for update()
  expect_true(S7::S7_inherits(nc@phi, diseasenowcasting:::prior_class))
})

test_that("poisson_likelihood nowcast works (no phi)", {
  tn <- .daily_tn(seed = 3)
  nc <- nowcast(tn, model(poisson_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 80, seed = 1)
  expect_true(all(is.finite(summary(predict(nc, seed = 2))$median)))
})

# ── TASK 2: delegated common autoplot ────────────────────────────────────────

test_that("autoplot uses the tbl.now common result method", {
  tn <- .daily_tn(Tn = 40L, seed = 4)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 150, seed = 1)
  p  <- autoplot(nc)
  expect_s3_class(p, "ggplot")
  expect_true(tbl.now::is_tbl_nowcast(nc))
  expect_identical(nc@method, "diseasenowcasting")
})

# ── TASK 3: common output works when now >> last observed ────────────────────

test_that("common predictions span the grid when now is past the last observation", {
  tn <- .daily_tn(Tn = 15L, seed = 5)             # onsets up to 2020-01-15
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 120,
                now = as.Date("2020-01-25"), seed = 1)   # 10 days past last onset
  event_col <- nc@event_date
  expect_equal(length(unique(nc@predictions[[event_col]])), nc@target)
  expect_equal(max(nc@predictions[[event_col]]), as.Date("2020-01-25"))
  expect_true(all(is.finite(nc@predictions$.value)))
  expect_s3_class(autoplot(nc), "ggplot")
})

test_that("common quantile rows are complete over the fitted event grid", {
  tn <- .daily_tn(Tn = 40L, seed = 7)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 120, seed = 1)
  event_col <- nc@event_date
  levels <- sort(unique(nc@predictions$.quantile_level))
  counts <- table(nc@predictions[[event_col]])
  expect_equal(length(counts), nc@target)
  expect_true(all(counts == length(levels)))
})

test_that("stratified common output preserves strata and plots", {
  tn <- .daily_tn(Tn = 20L, seed = 6, strata = TRUE)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 120,
                now = as.Date("2020-01-28"), seed = 1)
  p  <- autoplot(nc)
  expect_s3_class(p, "ggplot")
  expect_identical(nc@strata, "grp")
  expect_setequal(unique(nc@predictions$grp), c("A", "B"))
})

# ── TASK 4: pretty cli printing ──────────────────────────────────────────────

test_that("print(model) runs for all component combinations", {
  expect_no_error(print(model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())))
  expect_no_error(print(model(poisson_likelihood(), ar1_epidemic(), gamma_delay())))
  expect_no_error(print(model(nb_likelihood(), sir_epidemic(), dirichlet_delay())))
  expect_no_error(print(model(nb_likelihood(), hsgp_epidemic(),
                              generalized_gamma_delay(),
                              strata_pooling = "hierarchical")))
})

test_that("print(nowcast) uses the common tbl_nowcast summary", {
  tn <- .daily_tn(seed = 7)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 120, seed = 1)
  out <- capture.output(print(nc))
  expect_true(any(grepl("tbl_nowcast", out, fixed = TRUE)))
  expect_true(any(grepl("diseasenowcasting", out, fixed = TRUE)))
  expect_true(any(grepl("quantile levels", out, fixed = TRUE)))
  # print returns the object invisibly
  expect_identical(suppressMessages(print(nc)), nc)
})

test_that("print(nowcast) works for a stratified two-stage Dirichlet fit", {
  tn <- .daily_tn(seed = 8, strata = TRUE)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), dirichlet_delay()),
                type = "two_stage", K = 3, n_draws = 120, seed = 1)
  expect_no_error(suppressMessages(print(nc)))
})
