# RTMB prototypes for the two count-cumulative formulations in
# main_identifiability_update.tex.  Nothing here is part of the package API.

`%||%` <- function(x, y) if (is.null(x)) y else x

proto_ar1_path <- function(innovation, phi, sigma) {
  "[<-" <- RTMB::ADoverload("[<-")
  out <- RTMB::advector(numeric(length(innovation)))
  out[1L] <- innovation[1L] * sigma / sqrt(1 - phi^2)
  if (length(innovation) > 1L) {
    for (i in 2:length(innovation))
      out[i] <- phi * out[i - 1L] + sigma * innovation[i]
  }
  out
}

proto_delay_pmf <- function(family, log_mean, log_sd, raw_q, max_delay) {
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
  pmf <- c(cdf[1L], cdf[-1L] - cdf[-length(cdf)])
  pmf / sum(pmf)
}

proto_hsgp_setup <- function(n_time,
                             num_basis = min(20L, ceiling(1.5 * sqrt(n_time)))) {
  scaled <- diseasenowcasting:::hsgp_time_scaled(n_time, n_time)
  boundary <- 1 + 0.62
  list(
    basis = diseasenowcasting:::hsgp_basis(
      scaled, boundary, boundary, num_basis, 1L
    ),
    frequency = seq_len(num_basis) * pi / (2 * boundary),
    num_basis = num_basis
  )
}

proto_nb_logpmf <- function(k, size, mean) {
  lgamma(k + size) - lgamma(size) - lgamma(k + 1) +
    size * (log(size) - log(size + mean)) +
    k * (log(mean) - log(size + mean))
}

# Psi_size^{-1}(nu), evaluated with a fixed number of damped Newton iterations
# so the inverse remains on the RTMB tape.  The returned value is the PARENT NB
# mean; nu is the mean after zero truncation.
ztnb_parent_mean <- function(nu, size, iterations = 30L) {
  log_m <- log(nu)
  for (iteration in seq_len(iterations)) {
    m <- exp(log_m)
    p0 <- exp(size * (log(size) - log(size + m)))
    nonzero <- 1 - p0 + 1e-12
    psi <- m / nonzero
    q_prime <- size * p0 / (size + m)
    psi_prime <- (nonzero - m * q_prime) / (nonzero^2)
    step <- (psi - nu) / (psi_prime * m + 1e-12)
    # Smooth damping prevents a single poor iteration from leaving m <= 0.
    log_m <- log_m - step / sqrt(1 + step^2)
  }
  exp(log_m)
}

make_identifiability_data <- function(panel, epidemic = c("ar", "hsgp", "sir"),
                                      N_pop = 3e7, num_basis = NULL) {
  epidemic <- match.arg(epidemic)
  cells <- panel$cells
  event_levels <- panel$event_levels
  event_slot <- match(cells$event_num, event_levels)
  n_time <- length(event_levels)
  hs <- if (epidemic == "hsgp") {
    proto_hsgp_setup(n_time, num_basis %||%
                       min(20L, ceiling(1.5 * sqrt(n_time))))
  } else {
    list(basis = matrix(0, n_time, 0L), frequency = numeric(), num_basis = 0L)
  }

  list(
    cumulative = as.numeric(cells$cumulative),
    increment = as.numeric(cells$increment),
    age = as.integer(cells$delay),
    previous_nonzero = as.numeric(cells$previous_nonzero),
    event_slot = as.integer(event_slot),
    n_time = n_time,
    H = as.integer(panel$settlement_horizon),
    epidemic = epidemic,
    N_pop = N_pop,
    hsgp_basis = hs$basis,
    hsgp_frequency = hs$frequency,
    num_basis = hs$num_basis,
    event_levels = event_levels
  )
}

initial_identifiability_parameters <- function(data, model) {
  first <- data$age == 0L
  first_by_event <- tapply(data$cumulative[first], data$event_slot[first], sum)
  positive <- first_by_event[first_by_event > 0]
  initial_level <- if (length(positive)) stats::median(positive) else 10
  par <- list(
    intercept = log(max(initial_level, 1)),
    report_log_mean = log(0.8),
    report_log_sd = log(0.8),
    report_raw_q = -2,
    retract_log_mean = log(2),
    retract_log_sd = log(1),
    retract_raw_q = -2,
    # h_R(l) = retract_mass * g_R(l).  Only h_R and its survival are used by
    # either likelihood; this compact factorization supplies the requested
    # familiar delay families and a finite-horizon tail restriction.
    retract_mass_raw = stats::qlogis(0.05)
  )

  if (data$epidemic == "ar") {
    par$ar_phi_raw <- stats::qlogis((0.8 + 0.999) / 1.998)
    par$ar_sigma_raw <- stats::qlogis(0.15)
    par$ar_innovation <- rep(0, data$n_time)
  } else if (data$epidemic == "hsgp") {
    par$log_gp_alpha <- log(0.5)
    par$log_gp_ell <- log(0.8)
    par$basis_coef <- rep(0, data$num_basis)
  } else {
    par$log_R0 <- log(1.5)
    par$recovery_raw <- stats::qlogis(0.2)
    par$susceptible_raw <- stats::qlogis(0.2)
    par$ar_phi_raw <- 0
    par$ar_sigma_raw <- stats::qlogis(0.08)
    par$ar_innovation <- rep(0, data$n_time)
  }

  if (model == "cumulative_nb") par$log_count_size <- log(10)
  if (model == "hurdle_ztnb") {
    par$movement_intercept <- -1
    par$movement_age <- -0.5
    par$movement_previous <- 0.5
    par$log_magnitude_size <- log(1)
  }
  par
}

build_identifiability_model <- function(
    data,
    model = c("cumulative_poisson", "cumulative_nb", "hurdle_ztnb"),
    report_delay = c("lognormal", "gamma", "generalized_gamma"),
    retraction_delay = report_delay,
    use_random = FALSE) {
  model <- match.arg(model)
  report_delay <- match.arg(report_delay)
  retraction_delay <- match.arg(
    retraction_delay, c("lognormal", "gamma", "generalized_gamma")
  )
  parameters <- initial_identifiability_parameters(data, model)

  objective <- function(par) {
    RTMB::getAll(par, data)
    "[<-" <- RTMB::ADoverload("[<-")

    g_D <- proto_delay_pmf(report_delay, report_log_mean, report_log_sd,
                           report_raw_q, H)
    g_R <- proto_delay_pmf(retraction_delay, retract_log_mean,
                           retract_log_sd, retract_raw_q, H - 1L)
    retract_mass <- plogis(retract_mass_raw)
    h_R <- retract_mass * g_R

    # S_R[a+1] = P(R > a), a=0,...,H.
    S_R <- RTMB::advector(numeric(H + 1L))
    S_R[1L] <- 1
    if (H >= 1L) {
      running <- h_R[1L] * 0
      for (a in seq_len(H)) {
        running <- running + h_R[a]
        S_R[a + 1L] <- 1 - running
      }
    }

    if (epidemic == "ar") {
      phi <- -0.999 + 1.998 * plogis(ar_phi_raw)
      sigma <- plogis(ar_sigma_raw)
      log_mu <- intercept + proto_ar1_path(ar_innovation, phi, sigma)
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
      beta_trend <- proto_ar1_path(ar_innovation, phi, sigma)
      susceptible <- 1 - exp(intercept) / effective_population
      infected <- exp(intercept) / effective_population
      incidence <- RTMB::advector(numeric(n_time))
      for (tt in seq_len(n_time)) {
        beta <- R0 * recovery * exp(beta_trend[tt])
        new_infected <- susceptible * (1 - exp(-beta * infected))
        incidence[tt] <- new_infected * effective_population
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

    log_mu <- 16 - log1p(exp(16 - log_mu))
    mu <- exp(log_mu)

    # Delay-specific flow and cumulative-retention probabilities.
    alpha_unit <- g_D
    omega_unit <- RTMB::advector(numeric(H + 1L))
    q_C <- RTMB::advector(numeric(H + 1L))
    for (d in 0:H) {
      omega_d <- g_D[1L] * 0
      q_d <- g_D[1L] * 0
      for (r in 0:d) {
        q_d <- q_d + g_D[r + 1L] * S_R[d - r + 1L]
        if (r < d)
          omega_d <- omega_d + g_D[r + 1L] * h_R[d - r]
      }
      omega_unit[d + 1L] <- omega_d
      q_C[d + 1L] <- q_d
    }

    log_likelihood <- 0
    for (i in seq_along(age)) {
      dd <- age[i] + 1L
      mu_i <- mu[event_slot[i]]
      if (model == "cumulative_poisson") {
        log_likelihood <- log_likelihood +
          dpois(cumulative[i], mu_i * q_C[dd] + 1e-10, log = TRUE)
      } else if (model == "cumulative_nb") {
        count_size <- exp(log_count_size)
        log_likelihood <- log_likelihood + proto_nb_logpmf(
          cumulative[i], count_size, mu_i * q_C[dd] + 1e-10
        )
      } else {
        alpha <- mu_i * alpha_unit[dd] + 1e-12
        omega <- mu_i * omega_unit[dd] + 1e-12
        total <- alpha + omega
        direction_up <- alpha / total
        eta <- movement_intercept + movement_age * log1p(age[i]) +
          movement_previous * previous_nonzero[i]
        # 1-exp(-total) is <= min(1,total), so admissibility is automatic.
        nonnull <- (1 - exp(-total)) * plogis(eta)
        z <- increment[i]
        if (z == 0) {
          log_likelihood <- log_likelihood + log1p(-nonnull)
        } else {
          own_mean <- total / nonnull
          magnitude_size <- exp(log_magnitude_size)
          parent_mean <- ztnb_parent_mean(own_mean, magnitude_size)
          p0 <- exp(magnitude_size *
                      (log(magnitude_size) -
                         log(magnitude_size + parent_mean)))
          direction_lp <- if (z > 0) log(direction_up) else log1p(-direction_up)
          log_likelihood <- log_likelihood + log(nonnull) + direction_lp +
            proto_nb_logpmf(abs(z), magnitude_size, parent_mean) -
            log1p(-p0)
        }
      }
    }

    log_prior_delay <-
      dnorm(report_log_mean, log(1), 1, log = TRUE) +
      dnorm(report_log_sd, log(0.8), 0.8, log = TRUE) +
      dnorm(retract_log_mean, log(2), 1, log = TRUE) +
      dnorm(retract_log_sd, log(1), 0.8, log = TRUE) +
      dbeta(retract_mass, 1.5, 20, log = TRUE) +
      log(retract_mass) + log1p(-retract_mass)
    if (report_delay == "generalized_gamma")
      log_prior_delay <- log_prior_delay + dnorm(report_raw_q, -2, 1, log = TRUE)
    if (retraction_delay == "generalized_gamma")
      log_prior_delay <- log_prior_delay + dnorm(retract_raw_q, -2, 1, log = TRUE)

    log_prior_obs <- 0
    if (model == "cumulative_nb")
      log_prior_obs <- dnorm(log_count_size, log(10), 1.5, log = TRUE)
    if (model == "hurdle_ztnb") {
      log_prior_obs <- dnorm(movement_intercept, -1, 2, log = TRUE) +
        dnorm(movement_age, 0, 1, log = TRUE) +
        dnorm(movement_previous, 0, 1, log = TRUE) +
        dnorm(log_magnitude_size, 0, 1.5, log = TRUE)
    }

    RTMB::REPORT(mu)
    RTMB::REPORT(g_D)
    RTMB::REPORT(h_R)
    RTMB::REPORT(S_R)
    RTMB::REPORT(q_C)
    RTMB::REPORT(alpha_unit)
    RTMB::REPORT(omega_unit)
    -(log_likelihood + log_prior_epi + log_prior_delay + log_prior_obs)
  }

  random <- switch(
    data$epidemic,
    ar = "ar_innovation",
    hsgp = "basis_coef",
    sir = "ar_innovation"
  )
  random_arg <- if (isTRUE(use_random)) random else NULL
  obj <- RTMB::MakeADFun(objective, parameters, random = random_arg,
                         silent = TRUE)
  list(obj = obj, data = data, model = model, report_delay = report_delay,
       retraction_delay = retraction_delay, parameters = parameters,
       use_random = isTRUE(use_random), random = random)
}

prototype_parameter_bounds <- function(par, H) {
  nm <- names(par)
  lower <- rep(-Inf, length(par))
  upper <- rep(Inf, length(par))
  set_bounds <- function(pattern, lo, hi) {
    index <- grepl(pattern, nm)
    lower[index] <<- lo
    upper[index] <<- hi
  }
  set_bounds("^intercept$", -5, 16)
  set_bounds("_(log_mean|log_sd)$", -6, log(max(H, 2)) + 2)
  set_bounds("raw_q$", -10, 10)
  set_bounds("retract_mass_raw$", -12, 12)
  set_bounds("ar_phi_raw$|ar_sigma_raw$", -10, 10)
  set_bounds("^log_gp_alpha$|^log_gp_ell$", -8, 8)
  set_bounds("^log_R0$", -6, 6)
  set_bounds("recovery_raw$|susceptible_raw$", -10, 10)
  set_bounds("log_count_size$|log_magnitude_size$", -8, 12)
  set_bounds("movement_", -12, 12)
  list(lower = lower, upper = upper)
}

fit_identifiability_model <- function(
    built, control = list(iter.max = 1000, eval.max = 2500),
    polish_maxit = 1000L) {
  bounds <- prototype_parameter_bounds(built$obj$par, built$data$H)
  opt <- nlminb(
    built$obj$par, built$obj$fn, built$obj$gr,
    lower = bounds$lower, upper = bounds$upper, control = control
  )
  gradient <- max(abs(built$obj$gr(opt$par)))
  if (!is.finite(gradient) || gradient > 0.1) {
    polished <- optim(opt$par, built$obj$fn, built$obj$gr, method = "BFGS",
                      control = list(maxit = polish_maxit, reltol = 1e-10))
    if (is.finite(polished$value) && polished$value <= opt$objective) {
      opt$par <- polished$par
      opt$objective <- polished$value
      opt$convergence <- polished$convergence
      opt$message <- paste("BFGS polish:", polished$message %||% "")
      gradient <- max(abs(built$obj$gr(opt$par)))
    }
  }
  # The Laplace objective can be very flat in most directions with one
  # moderately sloped hyperparameter. BFGS sometimes declares convergence from
  # relative objective change alone in that geometry. A final limited-memory
  # pass uses a projected-gradient stopping rule and resolved the remaining
  # all-location cumulative warning without changing the fitted q_C materially.
  if (isTRUE(built$use_random) &&
      (!is.finite(gradient) || gradient > 0.05)) {
    lbfgs <- optim(
      opt$par, built$obj$fn, built$obj$gr, method = "L-BFGS-B",
      lower = bounds$lower, upper = bounds$upper,
      control = list(maxit = max(2000L, polish_maxit), factr = 1e4,
                     pgtol = 1e-8)
    )
    lbfgs_gradient <- max(abs(built$obj$gr(lbfgs$par)))
    objective_tolerance <- 1e-8 * (1 + abs(opt$objective))
    if (is.finite(lbfgs$value) && is.finite(lbfgs_gradient) &&
        lbfgs$value <= opt$objective + objective_tolerance &&
        lbfgs_gradient < gradient) {
      opt$par <- lbfgs$par
      opt$objective <- lbfgs$value
      opt$convergence <- lbfgs$convergence
      opt$message <- paste("L-BFGS-B polish:", lbfgs$message %||% "")
      gradient <- lbfgs_gradient
    }
  }
  built$fit <- opt
  # With Laplace random effects, parList() without an argument reconstructs the
  # complete parameter list at the final inner mode. Passing opt$par would only
  # supply the outer fixed block.
  built$obj$fn(opt$par)
  built$par_list <- if (isTRUE(built$use_random))
    built$obj$env$parList() else built$obj$env$parList(opt$par)
  built$components <- identifiability_components(built)
  built$max_gradient <- gradient
  built
}

identifiability_components <- function(fit) {
  p <- fit$par_list %||% fit$obj$env$parList(fit$obj$par)
  d <- fit$data
  H <- d$H
  g_D <- proto_delay_pmf(fit$report_delay, p$report_log_mean,
                         p$report_log_sd, p$report_raw_q, H)
  g_R <- proto_delay_pmf(fit$retraction_delay, p$retract_log_mean,
                         p$retract_log_sd, p$retract_raw_q, H - 1L)
  h_R <- plogis(p$retract_mass_raw) * g_R
  S_R <- c(1, 1 - cumsum(h_R))

  if (d$epidemic == "ar") {
    phi <- -0.999 + 1.998 * plogis(p$ar_phi_raw)
    sigma <- plogis(p$ar_sigma_raw)
    trend <- numeric(d$n_time)
    trend[1L] <- p$ar_innovation[1L] * sigma / sqrt(1 - phi^2)
    if (d$n_time > 1L) for (i in 2:d$n_time)
      trend[i] <- phi * trend[i - 1L] + sigma * p$ar_innovation[i]
    log_mu <- p$intercept + trend
  } else if (d$epidemic == "hsgp") {
    weights <- diseasenowcasting:::hsgp_spectral_weights(
      d$hsgp_frequency, exp(p$log_gp_alpha), exp(p$log_gp_ell), 2L
    )
    log_mu <- p$intercept +
      as.vector(d$hsgp_basis %*% (p$basis_coef * weights))
  } else {
    R0 <- exp(p$log_R0)
    recovery <- plogis(p$recovery_raw)
    effective_population <- plogis(p$susceptible_raw) * d$N_pop
    phi <- -0.999 + 1.998 * plogis(p$ar_phi_raw)
    sigma <- plogis(p$ar_sigma_raw)
    trend <- numeric(d$n_time)
    trend[1L] <- p$ar_innovation[1L] * sigma / sqrt(1 - phi^2)
    if (d$n_time > 1L) for (i in 2:d$n_time)
      trend[i] <- phi * trend[i - 1L] + sigma * p$ar_innovation[i]
    susceptible <- 1 - exp(p$intercept) / effective_population
    infected <- exp(p$intercept) / effective_population
    mu_path <- numeric(d$n_time)
    for (i in seq_len(d$n_time)) {
      beta <- R0 * recovery * exp(trend[i])
      new_infected <- susceptible * (1 - exp(-beta * infected))
      mu_path[i] <- new_infected * effective_population
      susceptible <- susceptible * exp(-beta * infected)
      infected <- new_infected + (1 - recovery) * infected
    }
    log_mu <- log(pmax(mu_path, 1e-8))
  }
  mu <- exp(16 - log1p(exp(16 - log_mu)))

  omega_unit <- q_C <- numeric(H + 1L)
  for (delay in 0:H) {
    q_C[delay + 1L] <- sum(
      g_D[seq_len(delay + 1L)] * rev(S_R[seq_len(delay + 1L)])
    )
    if (delay > 0L) {
      omega_unit[delay + 1L] <- sum(
        g_D[seq_len(delay)] * rev(h_R[seq_len(delay)])
      )
    }
  }
  list(mu = mu, g_D = g_D, h_R = h_R, S_R = S_R, q_C = q_C,
       alpha_unit = g_D, omega_unit = omega_unit,
       terminal_retention = q_C[H + 1L])
}

ztnb_parent_mean_numeric <- function(nu, size) {
  if (length(nu) > 1L) {
    return(vapply(nu, ztnb_parent_mean_numeric, numeric(1), size = size))
  }
  if (nu <= 1 + 1e-10) return(1e-10)
  psi <- function(m) {
    log_p0 <- size * (log(size) - log(size + m))
    m / (-expm1(log_p0))
  }
  stats::uniroot(function(m) psi(m) - nu,
                 lower = 1e-12, upper = max(nu * 2, 2),
                 extendInt = "upX", tol = 1e-11)$root
}

simulate_ztnb <- function(n, own_mean, size) {
  own_mean <- rep_len(own_mean, n)
  parent_mean <- ztnb_parent_mean_numeric(own_mean, size)
  out <- integer(n)
  remaining <- seq_len(n)
  while (length(remaining)) {
    proposal <- stats::rnbinom(length(remaining), size = size,
                               mu = parent_mean[remaining])
    accepted <- proposal > 0
    out[remaining[accepted]] <- proposal[accepted]
    remaining <- remaining[!accepted]
  }
  out
}

simulate_terminal_count <- function(fit, event_num, current, age,
                                    n = 1000L,
                                    reconstruction = c("anchored", "direct"),
                                    previous_nonzero = 0) {
  reconstruction <- match.arg(reconstruction)
  slot <- match(event_num, fit$data$event_levels)
  if (is.na(slot)) return(NULL)
  H <- fit$data$H
  age <- min(as.integer(age), H)
  comp <- fit$components
  mu <- as.numeric(comp$mu[slot])

  if (reconstruction == "direct") {
    terminal_mean <- mu * as.numeric(comp$q_C[H + 1L])
    if (fit$model == "cumulative_nb") {
      return(stats::rnbinom(n, size = exp(fit$par_list$log_count_size),
                            mu = terminal_mean))
    }
    return(stats::rpois(n, terminal_mean))
  }

  draws <- rep(as.numeric(current), n)
  previous_moved <- rep(as.numeric(previous_nonzero), n)
  if (age >= H) return(pmax(draws, 0))
  for (dd in seq.int(age + 1L, H)) {
    alpha <- mu * as.numeric(comp$alpha_unit[dd + 1L])
    omega <- mu * as.numeric(comp$omega_unit[dd + 1L])
    if (fit$model == "hurdle_ztnb") {
      total <- alpha + omega + 2e-12
      eta <- fit$par_list$movement_intercept +
        fit$par_list$movement_age * log1p(dd) +
        fit$par_list$movement_previous * previous_moved
      nonnull <- (1 - exp(-total)) * plogis(eta)
      moved <- stats::runif(n) < nonnull
      update <- integer(n)
      if (any(moved)) {
        own_mean <- total / nonnull
        magnitude <- simulate_ztnb(sum(moved), own_mean[moved],
                                   exp(fit$par_list$log_magnitude_size))
        up <- stats::runif(sum(moved)) < alpha / total
        update[moved] <- ifelse(up, magnitude, -magnitude)
      }
      previous_moved <- as.numeric(update != 0)
    } else {
      update <- stats::rpois(n, alpha) - stats::rpois(n, omega)
    }
    draws <- draws + update
  }
  pmax(draws, 0)
}
