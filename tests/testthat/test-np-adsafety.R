# AD-safety of the Dirichlet / non-parametric delay path.
# .nonparametric_delay_functions() does in-place out[mask] <- ... subassignment
# into advectors (the most AD-fragile construct here), so check the GRADIENT —
# not just the value — is finite at several parameter points.

test_that("Dirichlet joint objective has a finite value AND gradient", {
  m <- .make_synth(Tn = 60L, seed = 5)$m   # shared helper
  mdl <- model(nb_likelihood(), hsgp_epidemic(), dirichlet_delay())
  dat <- prepare_data(mdl, m, max_time = 60L, delay_only = FALSE)
  pr  <- default_priors(mdl, dat, phi = lognormal_prior(log(20), 0.5))
  built <- diseasenowcasting:::build_joint_obj(dat, pr, use_random = FALSE)
  obj <- built$obj

  expect_true(is.finite(obj$fn(obj$par)))
  expect_true(all(is.finite(obj$gr(obj$par))))           # gradient clean at init

  set.seed(99)
  for (i in 1:3) {                                        # and at perturbed points
    perturbed <- obj$par + rnorm(length(obj$par), 0, 0.5)
    expect_true(is.finite(obj$fn(perturbed)))
    expect_true(all(is.finite(obj$gr(perturbed))))
  }
})
