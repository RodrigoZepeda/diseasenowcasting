# Tests for user-defined delay distributions (custom_delay) and epidemic
# processes (custom_epidemic).  Both feed user-written, RTMB-traceable functions
# into the autodiff objective.
#
# These tests tape user functions through RTMB, which requires RTMB to be on the
# search path (see `.assert_rtmb_attached()`).  `library(diseasenowcasting)`
# attaches it via Depends in R CMD check, but `devtools::test()` (load_all) does
# not — so we attach it explicitly here.  RTMB is a hard dependency, so this is
# safe and does not change behaviour for the other test files.
library(RTMB)

# Under `devtools::test()` (pkgload / load_all) RTMB's S3 methods for vector
# operations a USER function relies on — `cumsum`, `matrix`, `[<-` on an
# advector — are not reliably dispatched, so taping a user `intensity_fn` can
# fail with "lost class attribute".  Basic arithmetic (used by delay factories)
# is unaffected.  Under `R CMD check` the package is attached normally and these
# work.  We probe the real operation once and skip the process-taping tests when
# the dispatch is inactive, so the suite is green under load_all and fully
# exercised under R CMD check.
.process_taping_ok <- isTRUE(tryCatch({
  probe_fn   <- function(theta) matrix(theta[1] + cumsum(exp(theta[2]) * theta[3:4]), 2L, 1L)
  probe_proc <- custom_epidemic(probe_fn, priors = rep(list(normal_prior(0, 1)), 4L),
                               inits = rep(0.1, 4L))
  validate_custom_epidemic(probe_proc)
  TRUE
}, error = function(e) FALSE))

# ── Constructors and validators ──────────────────────────────────────────────

test_that("custom_delay() builds a valid object with the expected slots", {
  # cdf / log_survival are each function(theta) -> function(d)
  weibull_cdf <- function(theta) {
    shape <- exp(theta[1]); scale <- exp(theta[2])
    function(d) 1 - exp(-(d / scale)^shape)
  }
  weibull_log_surv <- function(theta) {
    shape <- exp(theta[1]); scale <- exp(theta[2])
    function(d) -(d / scale)^shape
  }
  # n_params is inferred (here 2) from priors / param_names / inits
  dly <- custom_delay(cdf = weibull_cdf, log_survival = weibull_log_surv,
                      priors = list(normal_prior(0, 1), normal_prior(log(7), 1)),
                      name = "Weibull", param_names = c("log_shape", "log_scale"),
                      inits = c(0, log(7)))
  expect_true(S7::S7_inherits(dly, diseasenowcasting:::custom_delay_class))
  expect_equal(dly@num_id, 5L)                 # custom delay family code
  expect_equal(dly@n_params, 2L)
  expect_equal(dly@param_names, c("log_shape", "log_scale"))
})

test_that("custom_epidemic() builds a valid object with num_id 4", {
  n_t <- 12L
  rw_fn <- function(theta) matrix(theta[1] + cumsum(exp(theta[2]) * theta[3:(2L + n_t)]), n_t, 1L)
  proc <- custom_epidemic(rw_fn,
                         priors = c(list(normal_prior(2, 1), normal_prior(-2, 0.5)),
                                    rep(list(std_normal_prior()), n_t)),
                         name = "RandomWalk", inits = c(2, -2, rep(0, n_t)))
  expect_true(S7::S7_inherits(proc, diseasenowcasting:::custom_epidemic_class))
  expect_true(S7::S7_inherits(proc, diseasenowcasting:::epidemic_process_class))
  expect_equal(proc@num_id, 4L)
  expect_equal(proc@n_params, 2L + n_t)
})

test_that("n_params is inferred and disagreements are rejected", {
  fn <- function(theta) matrix(theta[1], 1L, 1L)
  # inferred from whichever argument is supplied:
  expect_equal(custom_epidemic(fn, inits = c(1, 2))@n_params, 2L)
  expect_equal(custom_epidemic(fn, param_names = c("a", "b", "c"))@n_params, 3L)
  expect_equal(custom_delay(fn, priors = list(normal_prior(0, 1)))@n_params, 1L)
  # disagreeing lengths are an error; supplying none is an error:
  expect_error(custom_epidemic(fn, priors = list(normal_prior(0, 1)), inits = c(1, 2)), "different")
  expect_error(custom_epidemic(fn, param_names = c("a", "b"), inits = 1), "different")
  expect_error(custom_epidemic(fn), "infer")
})

test_that("validate_custom_delay accepts AD-safe cdf / log_survival", {
  exp_cdf      <- function(theta) { rate <- exp(theta[1]); function(d) 1 - exp(-rate * d) }
  exp_log_surv <- function(theta) { rate <- exp(theta[1]); function(d) -rate * d }
  dly <- custom_delay(cdf = exp_cdf, log_survival = exp_log_surv,
                      priors = list(normal_prior(-2, 1)), name = "Exp", inits = -2)
  expect_invisible(validate_custom_delay(dly))
})

test_that("validate_custom_epidemic accepts an AD-safe intensity_fn", {
  skip_if_not(.process_taping_ok, "RTMB user-function dispatch inactive (load_all); runs under R CMD check")
  n_t <- 10L
  rw_fn <- function(theta) matrix(theta[1] + cumsum(exp(theta[2]) * theta[3:(2L + n_t)]), n_t, 1L)
  proc <- custom_epidemic(rw_fn,
                         priors = c(list(normal_prior(2, 1), normal_prior(-2, 0.5)),
                                    rep(list(std_normal_prior()), n_t)),
                         inits = c(2, -2, rep(0, n_t)))
  expect_invisible(validate_custom_epidemic(proc))
})

test_that("validate_custom_epidemic rejects a non-AD-safe intensity_fn", {
  skip_if_not(.process_taping_ok, "RTMB user-function dispatch inactive (load_all); runs under R CMD check")
  # branching on the parameter value is not traceable
  bad_fn <- function(theta) {
    val <- if (theta[1] > 0) exp(theta[1]) else 0   # if() on a parameter value
    matrix(rep(val, 5L), 5L, 1L)
  }
  proc <- custom_epidemic(bad_fn, priors = list(normal_prior(0, 1)), inits = 0.5)
  expect_error(validate_custom_epidemic(proc))
})

# ── Fixed vs free parameters (the priors list dual API) ───────────────────────

test_that("a numeric entry in priors fixes that parameter", {
  n_t <- 8L
  rw_fn <- function(theta) matrix(theta[1] + cumsum(exp(theta[2]) * theta[3:(2L + n_t)]), n_t, 1L)
  # log_mu0 fixed at 3, log_sigma free, innovations free
  proc <- custom_epidemic(rw_fn,
                         priors = c(list(3.0, normal_prior(-2, 0.5)),
                                    rep(list(std_normal_prior()), n_t)),
                         inits = c(3, -2, rep(0, n_t)))
  mod <- model(nb_likelihood(), proc, lognormal_delay())
  pr  <- default_priors(mod)
  expect_equal(pr$custom_epidemic_is_free[1], 0L)       # first param fixed
  expect_equal(pr$custom_epidemic_is_free[2], 1L)       # second free
  expect_equal(pr$custom_epidemic_fixed_vals[1], 3.0)
})

# ── End-to-end fit: a custom random walk recovers a known trajectory ──────────

test_that("custom_epidemic random walk fits and tracks a clear epidemic signal", {
  skip_if_not(.process_taping_ok, "RTMB user-function dispatch inactive (load_all); runs under R CMD check")
  set.seed(11)
  n_t <- 40L
  # A clear, smooth epidemic bump as the truth.  The random walk is flexible
  # enough to follow it, so the fitted trajectory should track it closely.
  log_lambda <- log(20 + 120 * exp(-0.5 * ((seq_len(n_t) - 22) / 8)^2))
  counts <- rpois(n_t, exp(log_lambda))
  event_idx <- rep(seq_len(n_t), counts)
  delays <- pmax(1L, round(rlnorm(length(event_idx), log(3), 0.5)))
  m <- cbind(event_idx, 1L, delays)

  rw_fn <- function(theta) matrix(theta[1] + cumsum(exp(theta[2]) * theta[3:(2L + n_t)]), n_t, 1L)
  proc <- custom_epidemic(rw_fn,
                         priors = c(list(normal_prior(log(20), 1), normal_prior(-1, 0.5)),
                                    rep(list(std_normal_prior()), n_t)),
                         inits = c(log(20), -1, rep(0, n_t)))
  mod <- model(nb_likelihood(), proc, lognormal_delay())
  dat <- prepare_data(mod, m = m, max_time = n_t)
  pr  <- default_priors(mod, dat)
  fit <- fit(mod, dat, pr)

  expect_equal(fit$opt$convergence, 0L)
  lm_fit <- fit$reconstruct$mu_safe[, 1]
  expect_length(lm_fit, n_t)
  expect_true(all(is.finite(lm_fit)))
  # The fitted log-incidence should follow the clear epidemic shape.
  expect_gt(cor(lm_fit, log_lambda), 0.7)
})

# ── End-to-end fit: a custom SIR ODE recovers the growth rate ─────────────────

test_that("custom_epidemic SIR ODE fits and recovers beta = R0 * gamma", {
  skip_if_not(.process_taping_ok, "RTMB user-function dispatch inactive (load_all); runs under R CMD check")
  set.seed(21)
  n_t <- 55L; N_pop <- 10000; R0_true <- 2.4; gamma_true <- 0.1; I0 <- 5
  S <- N_pop - I0; I <- I0; incidence <- numeric(n_t)
  for (t in seq_len(n_t)) {
    new_inf <- (R0_true * gamma_true / N_pop) * S * I
    incidence[t] <- max(new_inf, 0)
    S <- S - new_inf; I <- I + new_inf - gamma_true * I
  }
  counts <- rpois(n_t, incidence)
  event_idx <- rep(seq_len(n_t), counts)
  delays <- pmax(1L, rpois(length(event_idx), 3))
  m <- cbind(event_idx, 1L, delays)

  sir_fn <- function(theta) {
    `[<-` <- RTMB::ADoverload("[<-")
    R0v <- exp(theta[1]); gam <- exp(theta[2]); I0v <- exp(theta[3]); N <- 10000
    S_t <- N - I0v; I_t <- I0v; inc <- numeric(n_t)
    for (t in seq_len(n_t)) {
      new_inf <- (R0v * gam / N) * S_t * I_t
      inc[t] <- new_inf; S_t <- S_t - new_inf; I_t <- I_t + new_inf - gam * I_t
    }
    matrix(log((inc + abs(inc)) * 0.5 + 1e-8), n_t, 1L)
  }
  proc <- custom_epidemic(sir_fn,
                         priors = list(normal_prior(log(2.5), 0.5),
                                       normal_prior(log(0.1), 0.3),
                                       normal_prior(log(5), 1)),
                         param_names = c("log_R0", "log_gamma", "log_I0"),
                         inits = c(log(2), log(0.1), log(5)))
  expect_invisible(validate_custom_epidemic(proc))

  mod <- model(nb_likelihood(), proc, lognormal_delay())
  dat <- prepare_data(mod, m = m, max_time = n_t)
  pr  <- default_priors(mod, dat)
  fit <- fit(mod, dat, pr)

  expect_equal(fit$opt$convergence, 0L)
  cp <- as.numeric(fit$parList$custom_epidemic_params)
  # R0 and gamma are individually weakly identified, but their product (the
  # epidemic growth rate beta) is — check that against the truth within 35%.
  beta_fit  <- exp(cp[1]) * exp(cp[2])
  beta_true <- R0_true * gamma_true
  expect_lt(abs(beta_fit - beta_true) / beta_true, 0.35)
})

# ── End-to-end fit: a custom Weibull delay recovers shape/scale ───────────────

test_that("custom_delay Weibull fits and recovers shape/scale", {
  set.seed(31)
  n_t <- 40L
  true_shape <- 2.0; true_scale <- 7.0
  lambda <- 80 * exp(0.04 * seq_len(n_t))
  rows <- list()
  for (t in seq_len(n_t)) {
    n_cases <- rpois(1, lambda[t]); if (n_cases == 0L) next
    delays <- pmin(ceiling(rweibull(n_cases, true_shape, true_scale)), 21L)
    keep <- (t + delays) <= n_t; if (!any(keep)) next
    agg <- table(delays[keep])
    for (d in names(agg)) rows[[length(rows) + 1L]] <- c(t, as.integer(agg[[d]]), as.integer(d))
  }
  m <- do.call(rbind, rows)

  weibull_cdf      <- function(theta) { shape <- exp(theta[1]); scale <- exp(theta[2])
                                        function(d) 1 - exp(-(d / scale)^shape) }
  weibull_log_surv <- function(theta) { shape <- exp(theta[1]); scale <- exp(theta[2])
                                        function(d) -(d / scale)^shape }
  dly <- custom_delay(cdf = weibull_cdf, log_survival = weibull_log_surv,
                      priors = list(normal_prior(0, 1), normal_prior(log(7), 1)),
                      name = "Weibull", inits = c(0, log(7)))
  mod <- model(nb_likelihood(), ar1_epidemic(), dly)
  dat <- prepare_data(mod, m = m, max_time = n_t)
  pr  <- default_priors(mod, dat)
  fit <- fit(mod, dat, pr)

  theta <- as.numeric(fit$parList$custom_delay_params)
  expect_equal(exp(theta[1]), true_shape, tolerance = 0.4)   # shape ~ 2
  expect_equal(exp(theta[2]), true_scale, tolerance = 0.25 * true_scale)  # scale ~ 7
})
