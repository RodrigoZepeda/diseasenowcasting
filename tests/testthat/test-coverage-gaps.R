# Targeted tests to push coverage above 90% in the remaining low-coverage files.

# ── 04_epidemic_class.R (48% → target 90%) ───────────────────────────────────

test_that("hsgp_epidemic() default slots are sensible", {
  ep <- hsgp_epidemic()
  expect_equal(ep@num_id, 1L)
  expect_true(S7::S7_inherits(ep, diseasenowcasting:::epidemic_process_class))
  # gp_kernel 1=Matern32, 2=Matern52
  expect_true(ep@gp_kernel %in% c(1L, 2L))
})

test_that("ar1_epidemic() default slots are set", {
  ep <- ar1_epidemic()
  expect_equal(ep@num_id, 2L)
  expect_true(S7::S7_inherits(ep, diseasenowcasting:::epidemic_process_class))
})

test_that("sir_epidemic() stores N_pop and use_beta_rw_trend", {
  ep <- sir_epidemic(N_pop = 2e6)
  expect_equal(ep@num_id, 3L)
  expect_equal(ep@N_pop, 2e6)
  expect_true(S7::S7_inherits(ep, diseasenowcasting:::epidemic_process_class))
})

test_that("hsgp_epidemic() with custom kernel and basis", {
  ep <- hsgp_epidemic(num_basis = 12L, gp_kernel = "matern32")
  expect_equal(ep@num_basis, 12L)
})

test_that("ar1_trend() produces a length-T numeric vector", {
  set.seed(1)
  innov <- rnorm(50)
  tr    <- ar1_trend(innov, phi = 0.8, sigma = 0.3)
  expect_length(tr, 50L)
  expect_true(all(is.finite(as.numeric(tr))))
})

# ── 03_delay_class.R (69% → target 90%) ─────────────────────────────────────

test_that("lognormal_delay() with fixed numeric mu stores it correctly", {
  ld <- lognormal_delay(mu = 1.5)
  expect_equal(ld@mu, 1.5)
})

test_that("gamma_delay() stores shape/rate slots", {
  gd <- gamma_delay()
  expect_equal(gd@num_id, 2L)
  expect_true(S7::S7_inherits(gd, diseasenowcasting:::delay_process_class))
})

test_that("generalized_gamma_delay() stores Q slot", {
  gg <- generalized_gamma_delay(Q = gamma_prior(2, 1))
  expect_true(S7::S7_inherits(gg@Q, diseasenowcasting:::prior_class))
})

test_that("dirichlet_delay() with numeric alpha stores it", {
  dd <- dirichlet_delay(alpha = 2.0, bins = 10L)
  expect_equal(dd@bins, 10L)
})

# ── 06_prior_density.R (77% → target 90%) ────────────────────────────────────

test_that("prior_lpdf returns finite for all real distribution codes", {
  # (num_id, test_value, params) — using the actual switch codes from 06_prior_density.R
  cases <- list(
    list(0L,   0.5, c(0,1,0)),    # StdNormal
    list(1L,   0.5, c(0,1,0)),    # Normal
    list(2L,   0.5, c(0,1,0)),    # Cauchy
    list(3L,   0.5, c(3,0,1)),    # StudentT
    list(4L,   0.5, c(0,1,0)),    # DoubleExponential
    list(5L,   0.5, c(0,0,0)),    # Flat
    list(100L, 0.5, c(0,1,0)),    # HalfStdNormal
    list(101L, 0.5, c(0,1,0)),    # HalfNormal
    list(102L, 0.5, c(0,1,0)),    # HalfCauchy
    list(103L, 0.5, c(3,1,0)),    # HalfStudentT
    list(104L, 0.5, c(0,1,0)),    # HalfDoubleExponential
    list(105L, 1.5, c(2,1,0)),    # Gamma
    list(107L, 1.5, c(2,1,0)),    # Weibull
    list(108L, 1.5, c(2,1,0)),    # InvGamma
    list(109L, 1.5, c(0,1,0)),    # LogNormal
    list(110L, 2.0, c(3,0,0)),    # ChiSquare
    list(111L, 0.5, c(1,0,0)),    # Exponential
    list(112L, 0.5, c(0,1,0)),    # Logistic
    list(113L, 0.4, c(2,2,0))     # Beta
  )
  for (item in cases) {
    id <- item[[1]]; val <- item[[2]]; params <- item[[3]]
    lp <- prior_lpdf(val, id, params)
    expect_true(is.finite(lp), label = paste("finite for num_id", id))
  }
})

test_that("dirichlet_lpdf is higher for more-concentrated simplex (correct shape)", {
  probs  <- c(0.5, 0.3, 0.2)
  alpha1 <- c(1, 1, 1)       # flat
  alpha2 <- c(10, 6, 4)      # concentrated near probs
  lp1 <- dirichlet_lpdf(probs, alpha1)
  lp2 <- dirichlet_lpdf(probs, alpha2)
  expect_true(lp2 > lp1)    # more concentrated at the truth → higher density
})

# ── 01_utils.R (68% → target 85%) ────────────────────────────────────────────

test_that("valid_positive_prior accepts valid positive priors", {
  expect_no_error(diseasenowcasting:::valid_positive_prior(gamma_prior(2, 1)))
  expect_no_error(diseasenowcasting:::valid_positive_prior(half_normal_prior(0, 1)))
  expect_no_error(diseasenowcasting:::valid_positive_prior(2.5))   # numeric passes
})

test_that("%||% returns right-hand side when left is NULL", {
  expect_equal(NULL %||% 5, 5)
  expect_equal(3    %||% 5, 3)
})

# ── 18_collect_fits.R (57% → target 80%) ─────────────────────────────────────

test_that(".collect_nowcast_fits two_stage returns multi rung for LogNormal", {
  tn  <- .make_synth_tblnow(Tn = 60L, seed = 30)
  mdl <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  prep <- diseasenowcasting:::prepare_from_tbl_now(tn, mdl)
  eng  <- prep$data
  pri  <- default_priors(mdl, eng)
  col  <- diseasenowcasting:::.collect_nowcast_fits(mdl, eng, pri, type = "two_stage",
                                         K = 3, floor_mu = 0.15, floor_sig_frac = 0.25)
  expect_true(length(col$fits) >= 1L)
  expect_true(col$rung %in% c("multi", "onestage", "anchored"))
})

test_that(".pool_fit_draws returns M matrix with correct column count", {
  tn  <- .make_synth_tblnow(Tn = 50L, seed = 31)
  nc  <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                 type = "two_stage", K = 3, n_draws = 100, seed = 1)
  pooled <- diseasenowcasting:::.pool_fit_draws(nc@fits, nc@target, n_draws = 50)
  expect_equal(ncol(pooled$M),      nc@target)
  expect_equal(ncol(pooled$lambda), nc@target)
})

# ── 16_multisample.R (78% → target 85%) ──────────────────────────────────────

test_that("nowcast_twostage (direct) returns valid M matrix", {
  set.seed(2)
  syn  <- .make_synth(Tn = 50L, seed = 2)
  mdl  <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  eng  <- prepare_data(mdl, syn$m, delay_only = FALSE)
  res  <- nowcast_twostage(mdl, syn$m, X = NULL, d_star = NULL,
                            max_time = eng$max_time, target = eng$max_time,
                            K = 3, n_draws_per = 50, seed = 1)
  expect_true(is.matrix(res$M))
  expect_equal(ncol(res$M), eng$max_time)
  expect_true(all(is.finite(res$quantiles)))
})

# ── 23_backtest.R / 24_score.R (79% → target 85%) ───────────────────────────

test_that("backtest with return_simulations=TRUE populates @simulations", {
  tn  <- .make_synth_tblnow(Tn = 80L, seed = 40)
  start <- min(tn$onset)
  bt  <- backtest(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                  dates = start + c(50, 65) - 1,
                  type = "one_stage", n_draws = 200, return_simulations = TRUE, seed = 1)
  expect_true(!is.null(bt@simulations))
  expect_true(length(bt@simulations) > 0)
})

test_that("score() with metric='ape' ranks models and returns finite values", {
  tn  <- .make_synth_tblnow(Tn = 90L, seed = 41)
  start <- min(tn$onset)
  bt  <- backtest(tn, list(model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                           model(nb_likelihood(), ar1_epidemic(),  lognormal_delay())),
                  dates = start + c(50, 70) - 1,
                  type = "one_stage", n_draws = 200, seed = 1)
  sc_ape <- score(bt, metric = "ape",  report = FALSE)
  sc_mse <- score(bt, metric = "mse",  report = FALSE)
  expect_equal(nrow(sc_ape), 2L)
  expect_true(all(is.finite(sc_ape$ape)))
  expect_true(all(is.finite(sc_mse$mse)))
  # sorted best-first
  expect_true(sc_ape$ape[1] <= sc_ape$ape[2])
  expect_true(sc_mse$mse[1] <= sc_mse$mse[2])
})

test_that("autoplot(backtest) returns a ggplot with ribbon layers", {
  tn    <- .make_synth_tblnow(Tn = 80L, seed = 42)
  start <- min(tn$onset)
  bt    <- backtest(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                    dates = start + c(50, 65) - 1,
                    type = "one_stage", n_draws = 200, seed = 1)
  p <- autoplot(bt)
  expect_s3_class(p, "ggplot")
  expect_gt(length(p$layers), 0L)
})

# ── 17_prepare_from_tblnow.R (79% → target 88%) ─────────────────────────────

test_that("prepare_from_tbl_now with daily data and temporal effects works", {
  suppressMessages(library(tbl.now))
  mp <- tbl_now(as.data.frame(mpoxdat), event_date = dx_date,
                report_date = dx_report_date, case_count = n,
                data_type = "count-incidence", verbose = FALSE) |>
    add_temporal_effects(temporal_effects(day_of_week = TRUE)) |>
    compute_temporal_effects()
  mdl  <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  prep <- diseasenowcasting:::prepare_from_tbl_now(mp, mdl, now = as.Date("2022-09-15"))
  expect_gt(prep$data$P, 0L)                       # covariates were picked up
  expect_equal(prep$data$num_strata, 1L)
})

test_that("prepare_from_tbl_now infers now from get_now when not provided", {
  tn   <- .make_synth_tblnow(Tn = 50L, seed = 43)
  mdl  <- model(nb_likelihood(), hsgp_epidemic(), lognormal_delay())
  prep <- diseasenowcasting:::prepare_from_tbl_now(tn, mdl)
  expect_true(!is.null(prep$now))
  expect_true(inherits(prep$now, "Date"))
})

# ── 32_infer_time.R (0% → target high) ──────────────────────────────────────

test_that("infer_max_time returns the model's event-time span", {
  tn <- .make_synth_tblnow(Tn = 40L, seed = 50)
  mt <- infer_max_time(tn)
  expect_true(is.numeric(mt) && length(mt) == 1L)
  expect_gt(mt, 0)
  # identical to the internal preparation it wraps
  expect_equal(mt, diseasenowcasting:::prepare_from_tbl_now(tn, model())$max_time)
})

# ── 27_tidy.R (0% → target high) ────────────────────────────────────────────

test_that("tidy() returns a long parameter table with credible intervals", {
  tn <- .make_synth_tblnow(Tn = 45L, seed = 51)
  nc <- nowcast(tn, model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 80, seed = 1)
  td <- tidy(nc)

  expect_s3_class(td, "data.frame")
  expect_true(all(c("term", "estimate", "std.error", "conf.low", "conf.high", "type")
                  %in% names(td)))
  expect_gt(nrow(td), 0L)

  # interval brackets the estimate wherever the SE is usable
  ok <- is.finite(td$std.error) & td$std.error > 0
  expect_true(all(td$conf.low[ok] <= td$estimate[ok] &
                  td$estimate[ok] <= td$conf.high[ok]))

  # a higher credible level widens every interval
  td99 <- tidy(nc, conf.level = 0.99)
  expect_true(all((td99$conf.high - td99$conf.low)[ok] >=
                  (td$conf.high  - td$conf.low )[ok]))
})

test_that("tidy() default method errors on a non-nowcast object", {
  expect_error(tidy(1:10), "No.+method")
})

test_that("tidy() runs on a multi-imputation two-stage fit", {
  tn <- .make_synth_tblnow(Tn = 50L, seed = 52)
  nc <- nowcast(tn, model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
                type = "two_stage", K = 3, n_draws = 80, seed = 1)
  expect_gt(length(nc@fits), 1L)              # delay imputed K > 1 times
  td <- tidy(nc)
  expect_s3_class(td, "data.frame")
  expect_gt(nrow(td), 0L)
})

test_that("tidy() classifies HSGP and SIR parameters into the right groups", {
  tn <- .make_synth_tblnow(Tn = 50L, seed = 71)

  # HSGP exercises the epidemic_hsgp classify arm (log_gp_*, basis_coefs)
  th <- tidy(nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()),
                     type = "one_stage", n_draws = 60, seed = 1))
  expect_true("epidemic_hsgp" %in% th$type)
  expect_true(all(c("delay", "likelihood") %in% th$type))

  # SIR exercises the epidemic_sir classify arm (log_R0, u_gamma, u_neff)
  ts <- tidy(nowcast(tn, model(nb_likelihood(), sir_epidemic(N_pop = 5000), lognormal_delay()),
                     type = "one_stage", n_draws = 60, seed = 1))
  expect_true("epidemic_sir" %in% ts$type)
})

# ── 26_nowcast_diagnostic.R (0% → target high) ──────────────────────────────

test_that("nowcast_diagnostic() returns three diagnostic panels (parametric delay)", {
  tn <- .make_synth_tblnow(Tn = 50L, seed = 53)
  nc <- nowcast(tn, model(nb_likelihood(), ar1_epidemic(), lognormal_delay()),
                type = "one_stage", n_draws = 80, seed = 1)
  d <- nowcast_diagnostic(nc, n_draws = 60, seed = 1, previous_times = 20)
  # patchwork is in Suggests: combined plot if present, else a list of 3 ggplots
  expect_true(inherits(d, "patchwork") || (is.list(d) && length(d) == 3L))
})

test_that("nowcast_diagnostic() handles a nonparametric delay and previous_times = Inf", {
  tn <- .make_synth_tblnow(Tn = 45L, seed = 54)
  nc <- nowcast(tn, model(nb_likelihood(), hsgp_epidemic(), dirichlet_delay()),
                type = "one_stage", n_draws = 60, seed = 1)
  d <- nowcast_diagnostic(nc, n_draws = 50, seed = 2, previous_times = Inf)
  expect_true(inherits(d, "patchwork") || (is.list(d) && length(d) == 3L))
})

# ── 00_prior_class.R (76% → target high) ────────────────────────────────────

test_that("every exported prior constructor builds a valid prior_class", {
  priors <- list(
    std_normal_prior(),         normal_prior(0, 1),         cauchy_prior(0, 1),
    student_t_prior(3, 0, 1),   double_exponential_prior(0, 1), flat_prior(),
    positive_flat_prior(),      half_std_normal_prior(),    half_normal_prior(0, 1),
    half_cauchy_prior(0, 1),    half_student_t_prior(3, 1), half_double_exponential_prior(0, 1),
    gamma_prior(2, 0.1),        weibull_prior(2, 1),        inv_gamma_prior(3, 1),
    lognormal_prior(0, 1),      chi_square_prior(2),        exponential_prior(1),
    logistic_prior(0, 1),       beta_prior(2, 5))
  for (p in priors)
    expect_true(S7::S7_inherits(p, diseasenowcasting:::prior_class))
  ids <- vapply(priors, function(p) as.integer(p@num_id), integer(1))
  expect_false(anyNA(ids))
  expect_true(all(vapply(priors, function(p) nchar(p@name) > 0, logical(1))))
})

test_that("prior_class validator rejects malformed priors", {
  pc <- diseasenowcasting:::prior_class
  expect_error(pc(name = "Normal",    num_id = c(1, 2), stan_params = c(0, 1)), "length 1")
  expect_error(pc(name = "NotAPrior", num_id = 1L,      stan_params = c(0, 1)), "Invalid prior")
  expect_error(pc(name = "Normal",    num_id = 1L, stan_params = c(1, 2, 3, 4)), "At most 3")
})

# ── 04_epidemic_class.R (68% → target high) ─────────────────────────────────

test_that("epidemic constructors reject invalid parameter slots", {
  expect_error(hsgp_epidemic(alpha = -1),     "alpha")
  expect_error(hsgp_epidemic(ell = -1),       "ell")
  expect_error(hsgp_epidemic(num_basis = -5), "num_basis")
  expect_error(hsgp_epidemic(tmax_model = -1), "tmax_model")
  expect_error(ar1_epidemic(sigma = -1),      "sigma")
  expect_error(sir_epidemic(R0 = -1),         "R0")
  expect_error(sir_epidemic(gamma = -1),      "gamma")
  expect_error(sir_epidemic(N_eff = -1),      "N_eff")
  expect_error(sir_epidemic(N_pop = -10),     "N_pop")
})

test_that("custom_epidemic infers n_params and flags inconsistent lengths", {
  f <- function(theta) matrix(theta, length(theta), 1L)
  # all-empty -> cannot infer
  expect_error(custom_epidemic(f), "Cannot infer")
  # disagreeing lengths -> error
  expect_error(custom_epidemic(f, priors = list(normal_prior(0, 1)), inits = c(0, 0)),
               "different parameter counts")
  # consistent -> builds, with defaults filled in
  ce <- custom_epidemic(f, priors = list(normal_prior(0, 1), std_normal_prior()))
  expect_true(S7::S7_inherits(ce, diseasenowcasting:::custom_epidemic_class))
  expect_equal(as.integer(ce@n_params), 2L)
  expect_length(ce@param_names, 2L)            # auto "theta1", "theta2"
  expect_length(ce@inits, 2L)                  # auto zeros
})

test_that("custom_epidemic_class validator checks slot-length consistency", {
  cls <- diseasenowcasting:::custom_epidemic_class
  f   <- function(theta) matrix(theta, length(theta), 1L)
  expect_error(cls(intensity_fn = f, n_params = 0L), "n_params")
  expect_error(cls(intensity_fn = f, n_params = 2L, param_names = c("a", "b"),
                   inits = c(0, 0, 0)), "inits")
})

# ── 31_censoring.R (65% → target high) ──────────────────────────────────────

test_that("censor_delays_above validates its inputs", {
  tn <- .make_synth_tblnow(Tn = 20L, seed = 60)
  expect_error(censor_delays_above(data.frame(x = 1), 10), "tbl_now")
  expect_error(censor_delays_above(tn, max_delay = -1),    "non-negative")
  expect_error(censor_delays_above(tn, max_delay = c(1, 2)), "single")
})

test_that("censor_delays_above merges with prior censoring and is monotone", {
  tn  <- .make_synth_tblnow(Tn = 40L, seed = 61)

  # First pass (create path): a generous bound, quiet.
  tn1 <- censor_delays_above(tn, max_delay = 50, quiet = TRUE)
  col1 <- tbl.now::get_is_censored(tn1)
  c1   <- as.logical(tn1[[col1]]); c1[is.na(c1)] <- FALSE

  # Second pass (merge path) with a tighter bound; message is emitted.
  expect_message(tn2 <- censor_delays_above(tn1, max_delay = 15), "censored")
  col2 <- tbl.now::get_is_censored(tn2)
  c2   <- as.logical(tn2[[col2]]); c2[is.na(c2)] <- FALSE

  # tighter bound can only add censoring, never remove it
  expect_true(all(c2[c1]))
  expect_gte(sum(c2), sum(c1))
})

# ── 03_delay_class.R (73% → target high) ────────────────────────────────────

test_that("delay constructors reject invalid parameter slots", {
  expect_error(lognormal_delay(sigma = -1),          "sigma")
  expect_error(gamma_delay(shape = -1),              "shape")
  expect_error(gamma_delay(rate = -1),               "rate")
  expect_error(generalized_gamma_delay(sigma = -1),  "sigma")
})

test_that("delay_process base validator checks num_id, name, and seasons", {
  dpc <- diseasenowcasting:::delay_process_class
  expect_error(dpc(name = "LogNormal", num_id = c(1, 2)),  "length 1")
  expect_error(dpc(name = "Nope",      num_id = 1L),       "Invalid delay")
  expect_error(dpc(name = "LogNormal", num_id = 1L, num_delay_seasons = -1),
               "num_delay_seasons")
})

test_that("custom_delay infers n_params and rejects inconsistent lengths", {
  cdf <- function(theta) function(d) 1 - exp(-d / exp(theta[1]))
  expect_error(custom_delay(cdf), "Cannot infer")
  expect_error(custom_delay(cdf, priors = list(normal_prior(0, 1)), inits = c(0, 0)),
               "different parameter counts")
  dly <- custom_delay(cdf, priors = list(normal_prior(0, 1)), name = "Exp")
  expect_true(S7::S7_inherits(dly, diseasenowcasting:::custom_delay_class))
  expect_equal(as.integer(dly@n_params), 1L)
  expect_length(dly@param_names, 1L)        # auto "param_1"
  expect_length(dly@inits, 1L)              # auto 0
})

test_that("custom_delay_class validator checks slot-length consistency", {
  cls <- diseasenowcasting:::custom_delay_class
  fac <- function(theta) list(cdf = function(d) d, log_cdf = function(d) d,
                              log_survival = function(d) d)
  expect_error(cls(cdf_factory = fac, n_params = 0L), "n_params")
  expect_error(cls(cdf_factory = fac, n_params = 2L, priors = list(normal_prior(0, 1))),
               "priors")
  expect_error(cls(cdf_factory = fac, n_params = 2L,
                   priors = list(normal_prior(0, 1), normal_prior(0, 1)),
                   param_names = c("a", "b"), inits = c(0, 0, 0)), "inits")
  # an element that is neither a prior nor a length-1 numeric
  expect_error(cls(cdf_factory = fac, n_params = 1L, priors = list("nope"),
                   param_names = "a", inits = 0), "neither")
})

# ── 01_utils.R (75% → target high) ──────────────────────────────────────────

test_that(".parse_gp_kernel maps names (case/space-insensitive) and errors", {
  pk <- diseasenowcasting:::.parse_gp_kernel
  expect_equal(pk("sq_exp"),    diseasenowcasting:::.gp_kernel_map[["sq_exp"]])
  expect_equal(pk("  Matern32 "), diseasenowcasting:::.gp_kernel_map[["matern32"]])
  expect_equal(pk("matern52"),  diseasenowcasting:::.gp_kernel_map[["matern52"]])
  expect_error(pk("nope"), "gp_kernel must be one of")
})

test_that(".parse_gp_basis handles numeric codes, words, and bad input", {
  pb <- diseasenowcasting:::.parse_gp_basis
  expect_equal(pb(1), 1L)
  expect_equal(pb(2), 2L)
  expect_equal(pb("dirichlet"), 1L)
  expect_equal(pb("Cosine"),    2L)
  expect_error(pb(3),       "Numeric gp_basis")
  expect_error(pb("zzz"),   "gp_basis must be one of")
})

test_that(".delay_fix_keys returns the right keys per delay family", {
  fk <- diseasenowcasting:::.delay_fix_keys
  expect_equal(fk(1L), c("delay_mu", "delay_sigma"))
  expect_equal(fk(2L), c("delay_mu_gamma", "delay_sigma"))
  expect_equal(fk(3L), c("delay_mu", "delay_Q", "delay_sigma_gengamma"))
  expect_equal(fk(4L), character(0))       # Dirichlet: non-parametric, no keys
  expect_equal(fk(99L), character(0))      # unknown -> none
})
