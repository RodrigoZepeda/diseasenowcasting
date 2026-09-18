# A number in a parameter slot means "hold this here".
#
# It used to mean nothing at all for the epidemic and likelihood parameters:
# `build_joint_obj()` read the prior entry's `$dist` and never its
# `$is_constant`/`$fixed`, so a supplied number was replaced by a standard-normal
# prior and the parameter was estimated anyway.  These tests pin each slot and
# check the fit came back holding exactly the value asked for -- the only
# assertion that can tell "honoured" from "quietly ignored".

# Natural-scale readers for the constrained parameters, mirroring the maps in
# build_joint_obj().  Written out rather than imported so that a change to a
# constraint map has to be made here too, deliberately.
signed_unit <- function(x) -0.999 + 1.998 * stats::plogis(x)
bounded <- function(x, upper) upper * stats::plogis(x)

fit_pinned <- function(data, model_object) {
  suppressMessages(nowcast(data, model_object, type = "one_stage",
                           n_draws = 100, seed = 3, temporal_effects = "none"))
}

test_that("a fixed value is held, not silently estimated", {
  skip_on_cran()
  tn <- .make_synth_tblnow(Tn = 55L, seed = 2)
  sigma_max <- function(nc) nc@engine$ar_sigma_max

  cases <- list(
    list(model(nb_likelihood(phi = 5), ar1_epidemic(), lognormal_delay()),
         function(nc) exp(nc@fits[[1]]$parList$log_phi_nb), 5),
    list(model(nb_likelihood(mu = 3), ar1_epidemic(), lognormal_delay()),
         function(nc) nc@fits[[1]]$parList$mu_intercept, 3),
    list(model(nb_likelihood(), ar1_epidemic(phi = 0.9), lognormal_delay()),
         function(nc) signed_unit(nc@fits[[1]]$parList$ar_phi_unc), 0.9),
    list(model(nb_likelihood(), ar1_epidemic(sigma = 0.1), lognormal_delay()),
         function(nc) bounded(nc@fits[[1]]$parList$log_ar_sigma_unc, sigma_max(nc)), 0.1),
    list(model(nb_likelihood(), hsgp_epidemic(alpha = 1.5), lognormal_delay()),
         function(nc) exp(nc@fits[[1]]$parList$log_gp_alpha), 1.5),
    list(model(nb_likelihood(), hsgp_epidemic(ell = 0.7), lognormal_delay()),
         function(nc) exp(nc@fits[[1]]$parList$log_gp_ell), 0.7),
    list(model(nb_likelihood(), sir_epidemic(R0 = 2.5), lognormal_delay()),
         function(nc) exp(nc@fits[[1]]$parList$log_R0), 2.5),
    list(model(nb_likelihood(), sir_epidemic(gamma = 0.2), lognormal_delay()),
         function(nc) stats::plogis(nc@fits[[1]]$parList$u_gamma), 0.2),
    list(model(nb_likelihood(), sir_epidemic(N_eff = 0.3), lognormal_delay()),
         function(nc) stats::plogis(nc@fits[[1]]$parList$u_neff), 0.3)
  )
  for (case in cases) {
    nc <- fit_pinned(tn, case[[1]])
    expect_equal(as.numeric(case[[2]](nc)), case[[3]], tolerance = 1e-6,
                 label = .model_label(case[[1]]))
  }
})

test_that("the time-series trends hold a fixed value too", {
  skip_on_cran()
  tn <- .make_synth_tblnow(Tn = 55L, seed = 4)
  sigma_max <- function(nc) nc@engine$ar_sigma_max

  cases <- list(
    list(model(nb_likelihood(), arima_epidemic(sigma = 0.08), lognormal_delay()),
         function(nc) bounded(nc@fits[[1]]$parList$log_arima_sigma_unc, sigma_max(nc)), 0.08),
    list(model(nb_likelihood(), arima_epidemic(p = 2, q = 0, ar = c(0.4, -0.2)), lognormal_delay()),
         function(nc) signed_unit(nc@fits[[1]]$parList$arima_ar_pacf_unc[, 1]), c(0.4, -0.2)),
    list(model(nb_likelihood(), arima_epidemic(drift = 0.03), lognormal_delay()),
         function(nc) nc@fits[[1]]$parList$arima_drift, 0.03),
    list(model(nb_likelihood(), ets_epidemic(beta = 0.15), lognormal_delay()),
         function(nc) stats::plogis(nc@fits[[1]]$parList$ets_beta_unc), 0.15),
    list(model(nb_likelihood(), sts_epidemic(slope_phi = 0.95), lognormal_delay()),
         function(nc) signed_unit(nc@fits[[1]]$parList$sts_slope_phi_unc), 0.95),
    list(model(nb_likelihood(), theta_epidemic(drift = 0.02), lognormal_delay()),
         function(nc) nc@fits[[1]]$parList$ets_drift, 0.02),
    list(model(nb_likelihood(), random_walk_epidemic(sigma = 0.05), lognormal_delay()),
         function(nc) bounded(nc@fits[[1]]$parList$log_ets_sigma_unc, sigma_max(nc)), 0.05)
  )
  for (case in cases) {
    nc <- fit_pinned(tn, case[[1]])
    expect_equal(as.numeric(case[[2]](nc)), case[[3]], tolerance = 1e-6,
                 label = .model_label(case[[1]]))
  }
})

test_that("a pinned parameter survives predict(), strata and two_stage", {
  skip_on_cran()
  tn <- .make_strata_tblnow(Tn = 55L, seed = 3)

  # predict() rebuilds from a posterior DRAW, which has no entry for a pinned
  # parameter -- it is not in the Laplace precision.  This is the case that used
  # to fail with "non-numeric argument to mathematical function".
  nc <- fit_pinned(tn, model(nb_likelihood(phi = 5), ar1_epidemic(phi = 0.9), lognormal_delay()))
  estimates <- summary(predict(nc, n_draws = 200, seed = 1))
  expect_true(all(is.finite(estimates$median)))
  expect_true(all(estimates$q97.5 >= estimates$q2.5))

  # One value per stratum, in the order the strata are laid out.
  per_stratum <- fit_pinned(tn, model(nb_likelihood(), ar1_epidemic(phi = c(0.2, 0.8)),
                                      lognormal_delay()))
  expect_equal(signed_unit(per_stratum@fits[[1]]$parList$ar_phi_unc), c(0.2, 0.8),
               tolerance = 1e-6)

  # A single value is shared across strata.
  shared <- fit_pinned(tn, model(nb_likelihood(), ar1_epidemic(phi = 0.5), lognormal_delay()))
  expect_equal(signed_unit(shared@fits[[1]]$parList$ar_phi_unc), c(0.5, 0.5),
               tolerance = 1e-6)

  two_stage <- suppressMessages(nowcast(
    tn, model(nb_likelihood(), ar1_epidemic(sigma = 0.05), lognormal_delay()),
    type = "two_stage", K = 3, n_draws = 100, seed = 1, temporal_effects = "none"))
  expect_equal(two_stage@engine$ar_sigma_max *
                 stats::plogis(two_stage@fits[[1]]$parList$log_ar_sigma_unc),
               c(0.05, 0.05), tolerance = 1e-6)
})

test_that("a pinned parameter leaves the optimisation but stays readable", {
  skip_on_cran()
  tn <- .make_synth_tblnow(Tn = 50L, seed = 5)
  nc <- fit_pinned(tn, model(nb_likelihood(), hsgp_epidemic(alpha = 1.5), lognormal_delay()))

  # Not optimised...
  expect_false("log_gp_alpha" %in% names(nc@fits[[1]]$obj$par))
  # ...but still reported, because the value is the answer to "what was alpha?".
  expect_equal(exp(unname(coef(nc)["log_gp_alpha"])), 1.5, tolerance = 1e-6)
  # ...and absent from parameters(), which reports estimates with uncertainty and
  # a pinned parameter has none.
  expect_false("log_gp_alpha" %in% parameters(nc)$term)
})

test_that("an out-of-domain fixed value is refused where the user can see it", {
  # Statically-knowable domains fail at construction.
  expect_error(ar1_epidemic(phi = 1.5), "open interval \\(-1, 1\\)")
  expect_error(sir_epidemic(gamma = 2), "open interval \\(0, 1\\)")
  expect_error(sir_epidemic(N_eff = 1.2), "open interval \\(0, 1\\)")
  expect_error(nb_likelihood(phi = 0), "positive line")
  expect_error(arima_epidemic(ar = 1.2), "open interval \\(-1, 1\\)")
  expect_error(ets_epidemic(beta = 2), "open interval \\(0, 1\\)")
  expect_error(sts_epidemic(slope_phi = -3), "open interval \\(-1, 1\\)")

  # Valid values still construct.
  expect_no_error(ar1_epidemic(phi = 0.9))
  expect_no_error(nb_likelihood(phi = 5))
  expect_no_error(arima_epidemic(ar = 0.4))
})

test_that("a data-dependent domain error is not buried by the init ladder", {
  skip_on_cran()
  tn <- .make_synth_tblnow(Tn = 45L, seed = 6)
  # `sigma` is bounded by the engine's ar_sigma_max, which the constructor does
  # not know.  The check therefore happens at fit time, inside the loop that
  # retries six initialisations -- and no retry can rescue a value outside its
  # domain, so the message must survive rather than become "failed to converge".
  expect_error(
    suppressMessages(nowcast(tn, model(nb_likelihood(), ar1_epidemic(sigma = 3), lognormal_delay()),
                             type = "one_stage", n_draws = 20, seed = 1, temporal_effects = "none")),
    "outside the open interval")
  expect_error(
    suppressMessages(nowcast(tn, model(nb_likelihood(), ar1_epidemic(phi = c(0.1, 0.2, 0.3)),
                                       lognormal_delay()),
                             type = "one_stage", n_draws = 20, seed = 1, temporal_effects = "none")),
    "1 stratum")
})

test_that("fixing the intercept is refused under hierarchical pooling", {
  skip_on_cran()
  tn <- .make_strata_tblnow(Tn = 45L, seed = 7)
  # Pooling is a property of the model(), not a nowcast() argument.
  pooled <- model(nb_likelihood(mu = 3), ar1_epidemic(), lognormal_delay(),
                  strata_pooling = "hierarchical")
  expect_error(
    suppressMessages(nowcast(tn, pooled, type = "one_stage", n_draws = 20, seed = 1,
                             temporal_effects = "none")),
    "hierarchical pooling")
})

test_that("leaving everything free changes nothing", {
  skip_on_cran()
  # The fix threads `is_fixed` flags through the prior and Jacobian terms.  A
  # transcription slip there would move the mode of every ordinary fit, so pin
  # the free path against known values rather than trusting inspection.
  tn <- .make_synth_tblnow(Tn = 50L, seed = 8)
  processes <- list(hsgp_epidemic(), ar1_epidemic(), sir_epidemic(),
                    arima_epidemic(), sts_epidemic(), ets_epidemic())
  for (process in processes) {
    nc <- fit_pinned(tn, model(nb_likelihood(), process, lognormal_delay()))
    expect_true(is.finite(nc@fits[[1]]$nll), label = process@name)
    expect_true(all(is.finite(nc@fits[[1]]$lambda)), label = process@name)
  }
})
