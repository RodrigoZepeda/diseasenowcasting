# Tests for likelihood, delay, and epidemic class constructors

# ── Likelihood ──────────────────────────────────────────────────────────────
test_that("nb_likelihood() creates a valid likelihood object", {
  lik <- nb_likelihood()
  expect_true(S7::S7_inherits(lik, diseasenowcasting:::likelihood_class))
  expect_equal(lik@num_id, 1L)       # NB = 1
  expect_equal(tolower(lik@name), "nb")
})

test_that("poisson_likelihood() creates a valid likelihood object", {
  lik <- poisson_likelihood()
  expect_true(S7::S7_inherits(lik, diseasenowcasting:::likelihood_class))
  expect_equal(lik@num_id, 0L)       # Poisson = 0
})

test_that("nb_likelihood accepts custom phi prior", {
  lik <- nb_likelihood(phi = lognormal_prior(log(20), 0.5))
  expect_true(S7::S7_inherits(lik@phi, diseasenowcasting:::prior_class))
  expect_equal(lik@phi@name, "LogNormal")
})

# ── Delay ────────────────────────────────────────────────────────────────────
test_that("all delay constructors return correct num_id", {
  expect_equal(lognormal_delay()@num_id,           1L)
  expect_equal(gamma_delay()@num_id,               2L)
  expect_equal(generalized_gamma_delay()@num_id,   3L)
  expect_equal(dirichlet_delay()@num_id,           4L)
})

test_that("delay constructors accept custom priors", {
  ld <- lognormal_delay(mu = normal_prior(log(5), 0.5), sigma = gamma_prior(2, 1))
  expect_true(S7::S7_inherits(ld@mu,    diseasenowcasting:::prior_class))
  expect_true(S7::S7_inherits(ld@sigma, diseasenowcasting:::prior_class))
})

test_that("dirichlet_delay stores bins parameter", {
  dd <- dirichlet_delay(bins = 10L)
  expect_equal(dd@bins, 10L)
})

test_that("generalized_gamma_delay has a Q slot", {
  gg <- generalized_gamma_delay()
  expect_true(!is.null(gg@Q))
})

# ── Epidemic ─────────────────────────────────────────────────────────────────
test_that("all epidemic constructors return correct num_id", {
  expect_equal(hsgp_epidemic()@num_id,  1L)
  expect_equal(ar1_epidemic()@num_id,   2L)
  expect_equal(sir_epidemic()@num_id,   3L)
})

test_that("hsgp_epidemic accepts num_basis", {
  ep <- hsgp_epidemic(num_basis = 20L)
  expect_equal(ep@num_basis, 20L)
})

test_that("sir_epidemic stores N_pop", {
  ep <- sir_epidemic(N_pop = 5e6)
  expect_equal(ep@N_pop, 5e6)
})

test_that("model() combines all three components", {
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  expect_true(S7::S7_inherits(mdl, diseasenowcasting:::model_class))
  expect_true(S7::S7_inherits(mdl@likelihood, diseasenowcasting:::likelihood_class))
  expect_true(S7::S7_inherits(mdl@epidemic,   diseasenowcasting:::epidemic_process_class))
  expect_true(S7::S7_inherits(mdl@delay,      diseasenowcasting:::delay_process_class))
})

test_that("model() print method runs without error", {
  mdl <- model(poisson_likelihood(), ar1_epidemic(), gamma_delay())
  expect_no_error(print(mdl))
})
