# Standalone RTMB count-cumulative likelihood prototypes.
# Nothing in this file is wired into the package API.

ar1_path <- function(innovation, phi, sigma) {
  "[<-" <- RTMB::ADoverload("[<-")
  out <- RTMB::advector(numeric(length(innovation)))
  out[1L] <- innovation[1L] * sigma / sqrt(1 - phi^2)
  if (length(innovation) > 1L) {
    for (i in 2:length(innovation))
      out[i] <- phi * out[i - 1L] + sigma * innovation[i]
  }
  out
}

delay_pmf <- function(family, log_mean, log_sd, raw_q, max_delay) {
  sd <- 0.01 + exp(log_sd)
  if (family == "generalized_gamma") {
    shape_q <- 0.05 + 2.95 * plogis(raw_q)
    f <- diseasenowcasting:::.delay_distribution_functions(
      3L, log_mean, shape_q, sd
    )
  } else {
    family_id <- if (family == "lognormal") 1L else 2L
    f <- diseasenowcasting:::.delay_distribution_functions(
      family_id, log_mean, sd
    )
  }
  cdf <- f$cdf(seq_len(max_delay + 1L))
  c(cdf[1L], cdf[-1L] - cdf[-length(cdf)])
}

convolve_pmf <- function(left, right) {
  "[<-" <- RTMB::ADoverload("[<-")
  out <- rep(left[1L] * 0, length(left) + length(right) - 1L)
  for (i in seq_along(left)) {
    for (j in seq_along(right))
      out[i + j - 1L] <- out[i + j - 1L] + left[i] * right[j]
  }
  out
}

nb_log_pmf <- function(k, size, mean) {
  lgamma(k + size) - lgamma(size) - lgamma(k + 1) +
    size * (log(size) - log(size + mean)) +
    k * (log(mean) - log(size + mean))
}

hsgp_setup <- function(n_time, num_basis = min(20L, ceiling(1.5 * sqrt(n_time)))) {
  scaled <- diseasenowcasting:::hsgp_time_scaled(n_time, n_time)
  left <- 1 + 0.62
  right <- 1 + 0.62
  list(
    basis = diseasenowcasting:::hsgp_basis(scaled, left, right, num_basis, 1L),
    frequency = seq_len(num_basis) * pi / (left + right),
    num_basis = num_basis
  )
}

make_prototype_data <- function(panel, epidemic = "ar", N_pop = 3e7,
                                num_basis = NULL) {
  epidemic <- match.arg(epidemic, c("ar", "hsgp", "sir"))
  cells <- panel$cells
  event_levels <- panel$event_levels
  event_slot <- match(cells$event_index, event_levels)
  n_time <- length(event_levels)
  max_delay <- max(cells$age)
  hs <- if (epidemic == "hsgp")
    hsgp_setup(n_time, num_basis %||% min(20L, ceiling(1.5 * sqrt(n_time))))
  else list(basis = matrix(0, n_time, 0L), frequency = numeric(), num_basis = 0L)

  list(
    increment = as.numeric(cells$increment),
    age = as.integer(cells$age),
    previous_moved = as.numeric(cells$previous_moved),
    event_slot = as.integer(event_slot),
    n_time = n_time,
    max_delay = as.integer(max_delay),
    p_fixed = panel$p_empirical,
    epidemic = epidemic,
    N_pop = N_pop,
    hsgp_basis = hs$basis,
    hsgp_frequency = hs$frequency,
    num_basis = hs$num_basis,
    event_levels = event_levels
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

initial_parameters <- function(data, observation = "zinb") {
  observation <- match.arg(observation, c("skellam", "zinb"))
  first <- data$age == 0L
  event_first <- tapply(data$increment[first], data$event_slot[first], sum)
  initial_log_mean <- log(max(stats::median(event_first[event_first > 0]), 1))
  parameters <- list(
    intercept = initial_log_mean,
    appearance_log_mean = log(0.8),
    appearance_log_sd = log(0.8),
    appearance_raw_q = -2,
    retraction_log_mean = log(1.5),
    retraction_log_sd = log(0.8),
    retraction_raw_q = -2
  )
  if (data$epidemic == "ar") {
    parameters$ar_phi_raw <- stats::qlogis((0.8 + 0.999) / 1.998)
    parameters$ar_sigma_raw <- stats::qlogis(0.15)
    parameters$ar_innovation <- rep(0, data$n_time)
  } else if (data$epidemic == "hsgp") {
    parameters$log_gp_alpha <- log(0.5)
    parameters$log_gp_ell <- log(0.8)
    parameters$basis_coef <- rep(0, data$num_basis)
  } else {
    parameters$log_R0 <- log(1.5)
    parameters$recovery_raw <- stats::qlogis(0.2)
    parameters$susceptible_raw <- stats::qlogis(0.2)
    parameters$ar_phi_raw <- 0
    parameters$ar_sigma_raw <- stats::qlogis(0.08)
    parameters$ar_innovation <- rep(0, data$n_time)
  }
  if (observation == "zinb") {
    parameters$zi_intercept <- stats::qlogis(0.6)
    parameters$zi_age <- 0.5
    parameters$zi_previous <- -0.5
    parameters$log_nb_size <- log(0.3)
  }
  parameters
}

build_rtmb_prototype <- function(data,
                                 observation = c("skellam", "zinb"),
                                 appearance_delay = c("lognormal", "gamma",
                                                      "generalized_gamma"),
                                 retraction_delay = appearance_delay) {
  observation <- match.arg(observation)
  appearance_delay <- match.arg(appearance_delay)
  retraction_delay <- match.arg(retraction_delay,
                                c("lognormal", "gamma", "generalized_gamma"))
  parameters <- initial_parameters(data, observation)

  objective <- function(par) {
    RTMB::getAll(par, data)
    "[<-" <- RTMB::ADoverload("[<-")

    g_d <- delay_pmf(appearance_delay, appearance_log_mean,
                     appearance_log_sd, appearance_raw_q, max_delay)
    # Retractions have support 1, 2, ... .
    g_c_body <- delay_pmf(retraction_delay, retraction_log_mean,
                          retraction_log_sd, retraction_raw_q, max_delay)
    g_c <- c(g_c_body[1L] * 0, g_c_body)
    g_w <- convolve_pmf(g_d, g_c)

    if (epidemic == "ar") {
      phi <- -0.999 + 1.998 * plogis(ar_phi_raw)
      sigma <- plogis(ar_sigma_raw)
      log_mu <- intercept + ar1_path(ar_innovation, phi, sigma)
      log_prior_epi <- dnorm(intercept, 4, 3, log = TRUE) +
        dnorm(phi, 0.8, 0.35, log = TRUE) +
        dnorm(sigma, 0, 0.35, log = TRUE) +
        sum(dnorm(ar_innovation, 0, 1, log = TRUE))
    } else if (epidemic == "hsgp") {
      gp_alpha <- exp(log_gp_alpha)
      gp_ell <- exp(log_gp_ell)
      weights <- diseasenowcasting:::hsgp_spectral_weights(
        hsgp_frequency, gp_alpha, gp_ell, 2L
      )
      log_mu <- intercept + as.vector(hsgp_basis %*% (basis_coef * weights))
      log_prior_epi <- dnorm(intercept, 4, 3, log = TRUE) +
        dnorm(gp_alpha, 0, 1, log = TRUE) +
        dgamma(gp_ell, shape = 3, rate = 1, log = TRUE) +
        sum(dnorm(basis_coef, 0, 1, log = TRUE)) + log_gp_alpha + log_gp_ell
    } else {
      R0 <- exp(log_R0)
      recovery <- plogis(recovery_raw)
      susceptible_fraction <- plogis(susceptible_raw)
      effective_population <- susceptible_fraction * N_pop
      phi <- -0.999 + 1.998 * plogis(ar_phi_raw)
      sigma <- plogis(ar_sigma_raw)
      beta_trend <- ar1_path(ar_innovation, phi, sigma)
      susceptible <- 1 - exp(intercept) / effective_population
      infected <- exp(intercept) / effective_population
      incidence <- RTMB::advector(numeric(n_time))
      for (i in seq_len(n_time)) {
        beta <- R0 * recovery * exp(beta_trend[i])
        new_infected <- susceptible * (1 - exp(-beta * infected))
        incidence[i] <- new_infected * effective_population
        susceptible <- susceptible * exp(-beta * infected)
        infected <- new_infected + (1 - recovery) * infected
      }
      log_mu <- log((incidence + abs(incidence)) / 2 + 1e-8)
      log_prior_epi <- dnorm(intercept, 4, 3, log = TRUE) +
        dlnorm(R0, log(2), 0.5, log = TRUE) +
        dbeta(recovery, 2, 8, log = TRUE) +
        dbeta(susceptible_fraction, 2, 5, log = TRUE) +
        dnorm(phi, 0.8, 0.35, log = TRUE) +
        dnorm(sigma, 0, 0.35, log = TRUE) +
        sum(dnorm(ar_innovation, 0, 1, log = TRUE))
    }
    # Same smooth upper cap used by the package objective.
    upper <- 16
    log_mu <- upper - log1p(exp(upper - log_mu))
    mu <- exp(log_mu)

    log_likelihood <- 0
    for (i in seq_along(increment)) {
      d <- age[i] + 1L
      alpha <- mu[event_slot[i]] * g_d[d] + 1e-10
      omega <- mu[event_slot[i]] * (1 - p_fixed) * g_w[d] + 1e-10
      z <- increment[i]
      if (observation == "skellam") {
        bin_type <- if (age[i] == 0L) 0L else 1L
        log_likelihood <- log_likelihood +
          diseasenowcasting:::.log_skellam_increment(z, alpha, omega, bin_type)
      } else if (age[i] == 0L) {
        log_likelihood <- log_likelihood + dpois(z, alpha, log = TRUE)
      } else {
        total <- alpha + omega
        prob_up <- alpha / total
        logit_structural_zero <- zi_intercept + zi_age * log1p(age[i]) +
          zi_previous * previous_moved[i]
        structural_zero <- plogis(logit_structural_zero)
        # Ordinary NB mean. Because the active NB includes zero,
        # E|Delta|=(1-zeta)*nb_mean=alpha+omega exactly.
        nb_mean <- total / (1 - structural_zero + 1e-8)
        nb_size <- exp(log_nb_size)
        if (z == 0) {
          lp_structural <- log(structural_zero)
          lp_nb_zero <- log1p(-structural_zero) +
            nb_log_pmf(0, nb_size, nb_mean)
          log_likelihood <- log_likelihood +
            diseasenowcasting:::.logspace_add(lp_structural, lp_nb_zero)
        } else if (z > 0) {
          log_likelihood <- log_likelihood +
            log1p(-structural_zero) + log(prob_up) +
            nb_log_pmf(z, nb_size, nb_mean)
        } else {
          log_likelihood <- log_likelihood +
            log1p(-structural_zero) + log1p(-prob_up) +
            nb_log_pmf(-z, nb_size, nb_mean)
        }
      }
    }

    log_prior_delay <-
      dnorm(appearance_log_mean, log(1), 1, log = TRUE) +
      dnorm(appearance_log_sd, log(0.8), 0.8, log = TRUE) +
      dnorm(retraction_log_mean, log(1.5), 0.8, log = TRUE) +
      dnorm(retraction_log_sd, log(0.8), 0.8, log = TRUE)
    if (appearance_delay == "generalized_gamma")
      log_prior_delay <- log_prior_delay + dnorm(appearance_raw_q, -2, 1, log = TRUE)
    if (retraction_delay == "generalized_gamma")
      log_prior_delay <- log_prior_delay + dnorm(retraction_raw_q, -2, 1, log = TRUE)
    log_prior_obs <- if (observation == "zinb")
      dnorm(zi_intercept, 0, 2, log = TRUE) +
      dnorm(zi_age, 0, 1, log = TRUE) +
      dnorm(zi_previous, 0, 1, log = TRUE) +
      dnorm(log_nb_size, log(1), 1.5, log = TRUE)
    else 0

    -(log_likelihood + log_prior_epi + log_prior_delay + log_prior_obs)
  }

  obj <- RTMB::MakeADFun(objective, parameters, silent = TRUE)
  list(obj = obj, data = data, observation = observation,
       appearance_delay = appearance_delay,
       retraction_delay = retraction_delay,
       parameters = parameters)
}

fit_rtmb_prototype <- function(built, control = list(iter.max = 1200,
                                                     eval.max = 3000)) {
  opt <- nlminb(built$obj$par, built$obj$fn, built$obj$gr, control = control)
  gradient <- max(abs(built$obj$gr(opt$par)))
  if (!is.finite(gradient) || gradient > 0.1) {
    polished <- optim(opt$par, built$obj$fn, built$obj$gr, method = "BFGS",
                     control = list(maxit = 1000, reltol = 1e-10))
    if (is.finite(polished$value) && polished$value <= opt$objective) {
      opt$par <- polished$par
      opt$objective <- polished$value
      opt$convergence <- polished$convergence
      opt$message <- paste("BFGS polish:", polished$message %||% "")
    }
  }
  built$fit <- opt
  built$par_list <- built$obj$env$parList(opt$par)
  built$nll <- opt$objective
  built
}

reconstruct_prototype <- function(fit) {
  p <- fit$par_list
  d <- fit$data
  g_d <- delay_pmf(fit$appearance_delay, p$appearance_log_mean,
                   p$appearance_log_sd, p$appearance_raw_q, d$max_delay)
  body <- delay_pmf(fit$retraction_delay, p$retraction_log_mean,
                    p$retraction_log_sd, p$retraction_raw_q, d$max_delay)
  g_c <- c(0, body)
  g_w <- numeric(length(g_d) + length(g_c) - 1L)
  for (i in seq_along(g_d)) for (j in seq_along(g_c))
    g_w[i + j - 1L] <- g_w[i + j - 1L] + g_d[i] * g_c[j]
  if (d$epidemic == "ar") {
    phi <- -0.999 + 1.998 * plogis(p$ar_phi_raw)
    sigma <- plogis(p$ar_sigma_raw)
    trend <- numeric(d$n_time)
    trend[1] <- p$ar_innovation[1] * sigma / sqrt(1 - phi^2)
    if (d$n_time > 1L) for (i in 2:d$n_time)
      trend[i] <- phi * trend[i - 1L] + sigma * p$ar_innovation[i]
    log_mu <- p$intercept + trend
  } else if (d$epidemic == "hsgp") {
    weights <- diseasenowcasting:::hsgp_spectral_weights(
      d$hsgp_frequency, exp(p$log_gp_alpha), exp(p$log_gp_ell), 2L
    )
    log_mu <- p$intercept + as.vector(d$hsgp_basis %*% (p$basis_coef * weights))
  } else {
    R0 <- exp(p$log_R0); recovery <- plogis(p$recovery_raw)
    eff <- plogis(p$susceptible_raw) * d$N_pop
    phi <- -0.999 + 1.998 * plogis(p$ar_phi_raw)
    sigma <- plogis(p$ar_sigma_raw)
    trend <- numeric(d$n_time)
    trend[1] <- p$ar_innovation[1] * sigma / sqrt(1 - phi^2)
    if (d$n_time > 1L) for (i in 2:d$n_time)
      trend[i] <- phi * trend[i - 1L] + sigma * p$ar_innovation[i]
    susceptible <- 1 - exp(p$intercept) / eff
    infected <- exp(p$intercept) / eff
    mu <- numeric(d$n_time)
    for (i in seq_len(d$n_time)) {
      beta <- R0 * recovery * exp(trend[i])
      new <- susceptible * (1 - exp(-beta * infected))
      mu[i] <- new * eff
      susceptible <- susceptible * exp(-beta * infected)
      infected <- new + (1 - recovery) * infected
    }
    log_mu <- log(pmax(mu, 1e-8))
  }
  mu <- exp(16 - log1p(exp(16 - log_mu)))
  list(mu = mu, g_d = as.numeric(g_d), g_c = as.numeric(g_c),
       g_w = g_w, p = d$p_fixed, par = p)
}

simulate_update <- function(fit, event_slot, age, previous_moved = 0,
                            n = 1000L) {
  r <- reconstruct_prototype(fit)
  alpha <- max(r$mu[event_slot] * r$g_d[age + 1L], 1e-10)
  omega <- max(r$mu[event_slot] * (1 - r$p) * r$g_w[age + 1L], 1e-10)
  if (fit$observation == "skellam")
    return(stats::rpois(n, alpha) - stats::rpois(n, omega))

  lp <- r$par$zi_intercept + r$par$zi_age * log1p(age) +
    r$par$zi_previous * previous_moved
  zeta <- plogis(lp)
  active <- stats::runif(n) >= zeta
  out <- integer(n)
  if (any(active)) {
    total <- alpha + omega
    magnitude <- stats::rnbinom(sum(active), size = exp(r$par$log_nb_size),
                                mu = total / (1 - zeta + 1e-8))
    sign <- ifelse(stats::runif(sum(active)) < alpha / total, 1L, -1L)
    out[active] <- sign * magnitude
  }
  out
}
