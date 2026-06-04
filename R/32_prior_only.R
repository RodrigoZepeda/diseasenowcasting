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
  s1   <- function(e) .sample_prior_entry(e, 1L)
  clamp <- function(x, lo, hi) min(max(x, lo), hi)
  n_time <- engine$max_time
  em     <- engine$epidemic_model
  fam    <- engine$delay_family
  asmax  <- engine$ar_sigma_max %||% 1
  pl <- list()

  # -- delay ----------------------------------------------------------------
  if (fam == 4L) {                                   # Dirichlet (non-parametric)
    nb    <- as.integer(engine$np_model_length)
    alpha <- pmax(as.numeric(priors$delay_probs$params), 1e-3)
    g  <- stats::rgamma(length(alpha), shape = alpha); sx <- g / sum(g)
    pl$delay_logits <- log(pmax(sx[seq_len(nb)], 1e-8)) - log(pmax(sx[nb + 1L], 1e-8))
  } else {
    if (!isTRUE(priors$delay_mu$is_constant == 1L))
      pl$delay_mu <- s1(priors$delay_mu)
    if (!isTRUE(priors$delay_sigma$is_constant == 1L))
      pl$log_delay_sigma_excess <- log(max(s1(priors$delay_sigma) - 0.01, 1e-6))
    if (fam == 3L && !isTRUE(priors$delay_Q$is_constant == 1L)) {  # Generalised Gamma
      sq <- clamp(s1(priors$delay_Q), 0.051, 2.99)
      pl$delay_Q <- stats::qlogis((sq - 0.05) / 2.95)
    }
  }

  # -- likelihood overdispersion --------------------------------------------
  if (engine$is_negative_binomial == 1L)
    pl$log_phi_nb <- log(max(s1(priors$phi_nb), 1e-3))

  # -- epidemic process -----------------------------------------------------
  if (em == 3L) {                                    # coupled SIR
    pl$log_R0  <- log(max(s1(priors$R0), 1e-3))
    pl$u_gamma <- stats::qlogis(clamp(s1(priors$gamma_sir), 1e-4, 1 - 1e-4))
    pl$u_neff  <- stats::qlogis(clamp(s1(priors$N_eff),     1e-4, 1 - 1e-4))
    pl$ar_phi_unc       <- stats::qlogis((clamp(s1(priors$ar_phi), -0.998, 0.998) + 0.999) / 1.998)
    pl$log_ar_sigma_unc <- stats::qlogis(clamp(s1(priors$ar_sigma), 1e-4, asmax - 1e-4) / asmax)
    pl$ar_innov         <- matrix(stats::rnorm(n_time * n_strata), n_time, n_strata)
  } else {
    pl$mu_intercept <- vapply(seq_len(n_strata), function(s) s1(priors$mu_intercept), numeric(1))
    if (engine$P > 0)
      pl$gamma <- matrix(.sample_prior_entry(priors$gamma_cov, engine$P * n_strata),
                         engine$P, n_strata)
    if (em == 1L) {                                  # HSGP
      pl$log_gp_alpha <- log(max(s1(priors$gp_alpha), 1e-3))
      pl$log_gp_ell   <- log(max(s1(priors$gp_ell),   1e-3))
      pl$basis_coefs  <- matrix(stats::rnorm(num_basis * n_strata), num_basis, n_strata)
    } else {                                         # AR(1)
      pl$ar_phi_unc       <- stats::qlogis((clamp(s1(priors$ar_phi), -0.998, 0.998) + 0.999) / 1.998)
      pl$log_ar_sigma_unc <- stats::qlogis(clamp(s1(priors$ar_sigma), 1e-4, asmax - 1e-4) / asmax)
      pl$ar_innov         <- matrix(stats::rnorm(n_time * n_strata), n_time, n_strata)
    }
  }
  pl
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

  # SIR needs a non-zero seed of infectious cases at t = 1.
  if (engine$epidemic_model == 3L) {
    cc <- if (is.matrix(engine$case_counts)) engine$case_counts else matrix(engine$case_counts, n_time, n_strata)
    if (sum(cc[1, ]) <= 0) cc[1, ] <- pmax(cc[1, ], 1)
    engine$case_counts <- cc
  }

  M <- lambda <- matrix(NA_real_, n_draws, n_time)
  M_strata <- lambda_strata <- array(NA_real_, c(n_draws, n_time, n_strata))
  for (d in seq_len(n_draws)) {
    pl <- .sample_prior_parlist(engine, priors, num_basis, n_strata)
    rc <- tryCatch(.joint_reconstruct(engine, priors, pl, Bmat, freq), error = function(e) NULL)
    if (is.null(rc)) next
    lam    <- matrix(rc$lambda, n_time, n_strata)
    phi_nb <- if (is_nb) rc$phi_nb else NA_real_
    pred   <- matrix(.epidemic_rng(is_nb, as.numeric(lam) + 1e-8, phi_nb), n_time, n_strata)
    lambda_strata[d, , ] <- lam;  M_strata[d, , ] <- pred
    lambda[d, ] <- rowSums(lam);  M[d, ] <- rowSums(pred)
  }
  list(M = M, lambda_draws = lambda, M_strata = M_strata,
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

