# =============================================================================
# Prior-only simulation  (nowcast(..., prior_only = TRUE))
# =============================================================================
# prior_only draws epidemic-process parameters from their PRIORS (ignoring the
# likelihood) and reconstructs the latent incidence, so users can see "what does
# this prior yield" -- e.g. how moving the SIR R0 prior or the AR(1) phi prior
# reshapes the epidemic.  The result is a normal nowcast_class, so predict() /
# autoplot() / median() / quantile() all work.
# =============================================================================

# Prior distribution code (num_id) -> name, for sampling from default_priors() entries.
.PRIOR_CODE_NAME <- c(
  "0" = "StdNormal", "1" = "Normal", "2" = "Cauchy", "3" = "StudentT",
  "4" = "DoubleExponential", "5" = "Flat",
  "100" = "HalfStdNormal", "101" = "HalfNormal", "102" = "HalfCauchy",
  "103" = "HalfStudentT", "104" = "HalfDoubleExponential",
  "105" = "Gamma", "107" = "Weibull", "108" = "InvGamma", "109" = "LogNormal",
  "110" = "ChiSquare", "111" = "Exponential", "112" = "Logistic", "113" = "Beta")

#' Sample from a default_priors() list entry `{dist, params, is_constant, fixed}`.
#' @keywords internal
#' @noRd
.sample_prior_entry <- function(entry, n = 1L) {
  if (is.null(entry)) return(rep(0, n))
  if (isTRUE(entry$is_constant == 1L)) return(rep(as.numeric(entry$fixed), n))
  nm <- .PRIOR_CODE_NAME[[as.character(entry$dist)]]
  if (is.null(nm)) return(rep(0, n))
  pc <- prior_class(name = nm, num_id = as.integer(entry$dist),
                    stan_params = as.numeric(entry$params))
  as.numeric(sample(pc, n))
}

#' Draw one unconstrained parameter list from the model priors.
#'
#' Inverts the transforms used in `.joint_reconstruct()` so that the
#' *constrained* parameters follow their priors (e.g. `R0 = exp(log_R0)` ->
#' `log_R0 = log(sample(R0_prior))`; `ar_phi = -0.999 + 1.998*plogis(unc)` ->
#' invert the logistic).  Bounded parameters are clamped to their support.
#' @keywords internal
#' @noRd
.sample_prior_parlist <- function(engine, priors, num_basis, n_strata) {
  # Draw a single value from one prior entry, and clamp a value to a range.
  draw_one <- function(prior_entry) .sample_prior_entry(prior_entry, 1L)
  clamp    <- function(value, lo, hi) min(max(value, lo), hi)

  n_time         <- engine$max_time
  epidemic_model <- engine$epidemic_model
  delay_family   <- engine$delay_family
  ar_sigma_max   <- engine$ar_sigma_max %||% 1
  parlist        <- list()

  # -- delay ----------------------------------------------------------------
  if (delay_family == 4L) {                          # Dirichlet (non-parametric)
    # Draw a probability simplex from the Dirichlet(alpha) prior via the
    # standard gamma trick, then store it as the unconstrained logits the model
    # uses (each bin relative to the final, reference bin).
    n_bins <- as.integer(engine$np_model_length)
    alpha  <- pmax(as.numeric(priors$delay_probs$params), 1e-3)
    gamma_draws <- stats::rgamma(length(alpha), shape = alpha)
    simplex     <- gamma_draws / sum(gamma_draws)
    parlist$delay_logits <- log(pmax(simplex[seq_len(n_bins)], 1e-8)) -
                            log(pmax(simplex[n_bins + 1L], 1e-8))
  } else {
    if (!isTRUE(priors$delay_mu$is_constant == 1L))
      parlist$delay_mu <- draw_one(priors$delay_mu)
    if (!isTRUE(priors$delay_sigma$is_constant == 1L))
      parlist$log_delay_sigma_excess <- log(max(draw_one(priors$delay_sigma) - 0.01, 1e-6))
    if (delay_family == 3L && !isTRUE(priors$delay_Q$is_constant == 1L)) {  # Generalised Gamma
      # Q lives in (0.05, 3) on the natural scale; store it on the logit scale.
      q_natural <- clamp(draw_one(priors$delay_Q), 0.051, 2.99)
      parlist$delay_Q <- stats::qlogis((q_natural - 0.05) / 2.95)
    }
  }

  # -- likelihood overdispersion --------------------------------------------
  if (engine$is_negative_binomial == 1L)
    parlist$log_phi_nb <- log(max(draw_one(priors$phi_nb), 1e-3))

  # -- epidemic process -----------------------------------------------------
  # Each parameter is drawn from its prior then mapped to the UNCONSTRAINED scale
  # the objective expects (log for positives, logit for bounded quantities).
  if (epidemic_model == 3L) {                        # coupled SIR
    parlist$log_R0  <- log(max(draw_one(priors$R0), 1e-3))
    parlist$u_gamma <- stats::qlogis(clamp(draw_one(priors$gamma_sir), 1e-4, 1 - 1e-4))
    parlist$u_neff  <- stats::qlogis(clamp(draw_one(priors$N_eff),     1e-4, 1 - 1e-4))
    parlist$ar_phi_unc       <- stats::qlogis((clamp(draw_one(priors$ar_phi), -0.998, 0.998) + 0.999) / 1.998)
    parlist$log_ar_sigma_unc <- stats::qlogis(clamp(draw_one(priors$ar_sigma), 1e-4, ar_sigma_max - 1e-4) / ar_sigma_max)
    parlist$ar_innov         <- matrix(stats::rnorm(n_time * n_strata), n_time, n_strata)
  } else {
    parlist$mu_intercept <- vapply(seq_len(n_strata), function(stratum) draw_one(priors$mu_intercept), numeric(1))
    if (engine$P > 0)
      parlist$gamma <- matrix(.sample_prior_entry(priors$gamma_cov, engine$P * n_strata),
                              engine$P, n_strata)
    if (epidemic_model == 1L) {                      # HSGP
      parlist$log_gp_alpha <- log(max(draw_one(priors$gp_alpha), 1e-3))
      parlist$log_gp_ell   <- log(max(draw_one(priors$gp_ell),   1e-3))
      parlist$basis_coefs  <- matrix(stats::rnorm(num_basis * n_strata), num_basis, n_strata)
    } else {                                         # AR(1)
      parlist$ar_phi_unc       <- stats::qlogis((clamp(draw_one(priors$ar_phi), -0.998, 0.998) + 0.999) / 1.998)
      parlist$log_ar_sigma_unc <- stats::qlogis(clamp(draw_one(priors$ar_sigma), 1e-4, ar_sigma_max - 1e-4) / ar_sigma_max)
      parlist$ar_innov         <- matrix(stats::rnorm(n_time * n_strata), n_time, n_strata)
    }
  }
  parlist
}

#' Generate prior-predictive nowcast draws (the prior_only engine).
#' @keywords internal
#' @noRd
.simulate_prior_draws <- function(engine, priors, n_draws, Bmat, freq, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n_time   <- engine$max_time
  n_strata <- as.integer(engine$num_strata %||% 1L)
  num_basis <- ncol(Bmat)
  is_nb    <- engine$is_negative_binomial == 1L

  # SIR needs a non-zero seed of infectious cases at t = 1, otherwise the
  # epidemic can never start; force at least one case in the first event-time.
  if (engine$epidemic_model == 3L) {
    case_counts <- if (is.matrix(engine$case_counts)) engine$case_counts
                   else matrix(engine$case_counts, n_time, n_strata)
    if (sum(case_counts[1, ]) <= 0) case_counts[1, ] <- pmax(case_counts[1, ], 1)
    engine$case_counts <- case_counts
  }

  # Total (summed over strata) and per-stratum draws of the latent incidence
  # (lambda) and the observed-count posterior predictive.
  nowcast_draws <- lambda_draws <- matrix(NA_real_, n_draws, n_time)
  nowcast_strata <- lambda_strata <- array(NA_real_, c(n_draws, n_time, n_strata))
  for (draw_index in seq_len(n_draws)) {
    parlist       <- .sample_prior_parlist(engine, priors, num_basis, n_strata)
    reconstructed <- tryCatch(.joint_reconstruct(engine, priors, parlist, Bmat, freq),
                              error = function(e) NULL)
    if (is.null(reconstructed)) next
    lambda_matrix <- matrix(reconstructed$lambda, n_time, n_strata)
    phi_nb        <- if (is_nb) reconstructed$phi_nb else NA_real_
    predicted     <- matrix(.epidemic_rng(is_nb, as.numeric(lambda_matrix) + 1e-8, phi_nb),
                            n_time, n_strata)
    lambda_strata[draw_index, , ]  <- lambda_matrix
    nowcast_strata[draw_index, , ] <- predicted
    lambda_draws[draw_index, ]  <- rowSums(lambda_matrix)
    nowcast_draws[draw_index, ] <- rowSums(predicted)
  }
  list(M = nowcast_draws, lambda_draws = lambda_draws, M_strata = nowcast_strata,
       lambda_strata = lambda_strata, n_strata = n_strata)
}

#' Return precomputed prior-only draws in the `.nowcast_draws()` shape.
#' @keywords internal
#' @noRd
.prior_only_draws <- function(fit, target = NULL, n_draws = NULL,
                              probs = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)) {
  s <- fit$prior_sims
  n_time <- ncol(s$M); target <- target %||% n_time
  avail  <- nrow(s$M)
  idx <- if (is.null(n_draws) || n_draws == avail) seq_len(avail)
         else sample.int(avail, n_draws, replace = n_draws > avail)
  M  <- s$M[idx, , drop = FALSE]; lam <- s$lambda_draws[idx, , drop = FALSE]
  Ms <- s$M_strata[idx, , , drop = FALSE]; ls <- s$lambda_strata[idx, , , drop = FALSE]
  td <- M[, target]
  list(M = M, lambda_draws = lam, M_strata = Ms, lambda_strata = ls, n_strata = s$n_strata,
       nowcast = summarise_nowcast_matrix(M), draws = td,
       quantiles = stats::quantile(td, probs = probs, na.rm = TRUE), target = target)
}

#' Build the HSGP basis/frequencies for an engine (mirrors build_joint_obj()).
#' @keywords internal
#' @noRd
.hsgp_basis_for_engine <- function(engine) {
  if (engine$epidemic_model != 1L)
    return(list(Bmat = matrix(0.0, engine$max_time, 0L), freq = numeric(0)))
  ts <- hsgp_time_scaled(engine$max_time, engine$tmax_model)
  list(
    Bmat = hsgp_basis(ts, engine$gp_L_left, engine$gp_L_right, engine$num_basis, engine$gp_basis),
    freq = seq_len(engine$num_basis) * pi / (engine$gp_L_left + engine$gp_L_right))
}

