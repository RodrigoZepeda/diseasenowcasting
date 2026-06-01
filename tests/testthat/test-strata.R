# Stratified nowcasting: per-stratum epidemic means with a shared delay / phi /
# kernel, the coupled-SIR force of infection, and the "missing" category.

# Two strata with distinct peaks; shared lognormal delay.
.make_strata_tblnow <- function(Tn = 70L, seed = 1, na_frac = 0) {
  set.seed(seed)
  ln <- lognormal_native(log(4), 3); start <- as.Date("2023-01-01"); rows <- list()
  gen <- function(grp, peak, amp) for (t in 1:Tn) {
    n <- rpois(1, amp * exp(-0.5 * ((t - peak) / 14)^2) + 3)
    if (n > 0) for (i in seq_len(n)) {
      d <- max(0L, round(rlnorm(1, ln$log_location, ln$log_scale)))
      rows[[length(rows) + 1]] <<- data.frame(onset = start + (t - 1),
                                              reported = start + (t - 1) + d, grp = grp)
    }
  }
  gen("A", 35, 35); gen("B", 45, 20)
  d <- do.call(rbind, rows); d <- d[d$reported <= start + Tn - 1, ]
  if (na_frac > 0) d$grp[sample(nrow(d), floor(na_frac * nrow(d)))] <- NA
  tbl.now::tbl_now(d, event_date = onset, report_date = reported, strata = grp,
                   data_type = "linelist", verbose = FALSE)
}

test_that("stratified one-stage nowcast converges and totals are finite", {
  tn  <- .make_strata_tblnow(seed = 1)
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  nc  <- nowcast(tn, mdl, type = "one_stage", n_draws = 200, seed = 1)
  expect_equal(nc@engine$num_strata, 2L)
  s <- summary(predict(nc, seed = 2))
  expect_equal(nrow(s), nc@target)
  expect_true(all(is.finite(s$median)))
})

test_that("AR1 and coupled-SIR converge stratified", {
  tn <- .make_strata_tblnow(seed = 2)
  for (epi in list(ar1_epidemic(), sir_epidemic())) {
    nc <- nowcast(tn, model(nb_likelihood(), epi, lognormal_delay()),
                  type = "one_stage", n_draws = 150, seed = 1)
    expect_equal(nc@engine$num_strata, 2L)
    expect_true(all(is.finite(summary(predict(nc, seed = 2))$median)))
  }
})

test_that("missing strata values form an explicit 'missing' category", {
  tn  <- .make_strata_tblnow(seed = 3, na_frac = 0.08)
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  prep <- prepare_from_tbl_now(tn, mdl)
  expect_true("missing" %in% prep$strata_levels)
  expect_equal(prep$data$num_strata, 3L)
  nc <- nowcast(tn, mdl, type = "one_stage", n_draws = 150, seed = 1)
  expect_equal(nc@engine$num_strata, 3L)
  expect_true(all(is.finite(summary(predict(nc, seed = 2))$median)))
})

test_that("unstratified path is unaffected (num_strata == 1)", {
  tn  <- .make_synth_tblnow(Tn = 70L, seed = 4)
  nc  <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                 type = "one_stage", n_draws = 150, seed = 1)
  expect_equal(nc@engine$num_strata, 1L)
  expect_true(all(is.finite(summary(predict(nc, seed = 2))$median)))
})
