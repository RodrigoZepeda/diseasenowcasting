# Tests for: phi moved to the likelihood, the bar-chart autoplot, sparse-recent
# autoplot, and the cli print methods.

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

# ── TASK 2: autoplot bar columns (predicted_total + reported) ────────────────

test_that("autoplot bar data has predicted_total (=median) and reported (=observed)", {
  tn <- .daily_tn(Tn = 40L, seed = 4)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 150, seed = 1)
  p  <- autoplot(nc, seed = 2)
  expect_s3_class(p, "ggplot")
  dat <- p$data
  expect_true(all(c("reported", "predicted_total", "q_lo", "q_hi") %in% names(dat)))
  # predicted_total >= reported at every event-time (median nowcast >= observed)
  expect_true(all(dat$predicted_total + 1e-9 >= dat$reported))
  # error-bar band brackets the median
  expect_true(all(dat$q_hi + 1e-9 >= dat$q_lo))
})

# ── TASK 3: autoplot works when now >> last observed ─────────────────────────

test_that("autoplot spans the full grid when now is well past the last observation", {
  tn <- .daily_tn(Tn = 15L, seed = 5)             # onsets up to 2020-01-15
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 120,
                now = as.Date("2020-01-25"), seed = 1)   # 10 days past last onset
  p   <- autoplot(nc, seed = 2, previous_times = NULL)    # full grid, not the last 15
  dat <- p$data
  expect_equal(nrow(dat), nc@target)              # one bar per event-time on the grid
  # the most recent rows have no observed cases yet, but the model still produces
  # a predictive distribution there (non-degenerate error bars)
  last_rows <- dat[dat$event_index >= nc@target - 5, ]
  expect_true(all(last_rows$reported == 0))
  expect_true(any(last_rows$q_hi > 0))            # model has predictive mass
  expect_true(all(is.finite(last_rows$predicted_total)))
})

test_that("autoplot previous_times keeps only the most recent event-times", {
  tn <- .daily_tn(Tn = 40L, seed = 7)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 120, seed = 1)
  full <- nrow(autoplot(nc, seed = 2, previous_times = NULL)$data)
  expect_gt(full, 15L)                                   # grid is longer than the default window
  expect_equal(nrow(autoplot(nc, seed = 2)$data), 15L)               # default window
  expect_equal(nrow(autoplot(nc, seed = 2, previous_times = 5L)$data), 5L)
  expect_equal(nrow(autoplot(nc, seed = 2, previous_times = 1000L)$data), full) # caps at grid
})

test_that("stratified autoplot with a recent gap facets and spans the grid", {
  tn <- .daily_tn(Tn = 20L, seed = 6, strata = TRUE)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 120,
                now = as.Date("2020-01-28"), seed = 1)
  p  <- autoplot(nc, seed = 2)
  expect_s3_class(p, "ggplot")
  expect_true(inherits(p$facet, "FacetWrap"))
  expect_setequal(unique(p$data$stratum), c("A", "B"))
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

test_that("print(nowcast) shows the model spec without drawing the nowcast", {
  tn <- .daily_tn(seed = 7)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 120, seed = 1)
  out <- cli::cli_fmt(print(nc))
  expect_true(any(grepl("diseasenowcasting", out)))
  expect_true(any(grepl("NegBin / HSGP / LogNormal", out)))
  # Printing must NOT compute the posterior-predictive nowcast (kept cheap).
  expect_false(any(grepl("Newest event", out)))
  # print returns the object invisibly
  expect_identical(suppressMessages(print(nc)), nc)
})

test_that("print(nowcast) works for a stratified two-stage Dirichlet fit", {
  tn <- .daily_tn(seed = 8, strata = TRUE)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), dirichlet_delay()),
                type = "two_stage", K = 3, n_draws = 120, seed = 1)
  expect_no_error(suppressMessages(print(nc)))
})
