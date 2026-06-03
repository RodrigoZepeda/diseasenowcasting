# =============================================================================
# Prior-only simulation  (nowcast(..., prior_only = TRUE)) + ess() diagnostic
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

# ── ess(): importance-sampling effective sample size of the Laplace fit ───────

# Latent-field parameter names (high-dimensional; profiled out for the ESS).
.LATENT_PAR_NAMES <- c("basis_coefs", "ar_innov", "delta_intercept")

#' Effective sample size of the Laplace approximation (hyperparameters)
#'
#' `diseasenowcasting` fits with a Laplace approximation: it draws *independent*
#' samples from a Gaussian \eqn{N(\hat\theta, H^{-1})} centred at the posterior
#' mode, so there is no MCMC autocorrelation.  The relevant diagnostic is instead
#' how well that Gaussian matches the true posterior, which `ess()` measures with
#' the **importance-sampling effective sample size**
#' \deqn{\mathrm{ESS} = \left(\sum_i w_i\right)^2 / \sum_i w_i^2, \qquad
#'        w_i \propto p(\theta_i \mid \text{data}) / q(\theta_i).}
#'
#' The reweighting is done over the **hyperparameters only** (the delay,
#' overdispersion, intercept and epidemic-kernel parameters), profiling the
#' high-dimensional latent field (the GP / AR coefficients) at its conditional
#' Laplace mode.  This is essential: a plain importance-sampling ESS over the
#' full parameter vector degenerates with dimension and is uninformative.  A high
#' ESS (close to `n_draws`) means the Gaussian approximation of the
#' hyperparameter posterior is reliable; a low ESS means it is skewed /
#' heavy-tailed / weakly identified -- common early in an epidemic when the data
#' barely move the priors.
#'
#' @param object A `nowcast_class` (fitted, not `prior_only`).
#' @param n_draws Number of Laplace draws to reweight (default 1000).
#' @param threshold Warn when `ESS / n_draws` falls below this (default 0.10).
#' @param warn If `TRUE` (default), emit a warning when the ESS is low.
#' @param seed Optional RNG seed.
#' @returns A single numeric: the importance-sampling ESS over the
#'   hyperparameters.  The ratio `ESS / n_draws` is attached as attribute
#'   `"ratio"`, and the number of hyperparameters as `"n_hyper"`.
#' @examples
#' if (requireNamespace("tbl.now", quietly = TRUE)) {
#'   # nc <- nowcast(my_tbl_now)
#'   # ess(nc)
#' }
#' @export
ess <- function(object, n_draws = 1000L, threshold = 0.10, warn = TRUE, seed = NULL) {
  if (!S7::S7_inherits(object, nowcast_class))
    cli::cli_abort("`object` must be a {.cls nowcast_class} (from {.fn nowcast}).")
  fit <- object@fits[[1]]
  if (isTRUE(fit$prior_only))
    cli::cli_abort("ESS is undefined for a {.code prior_only} nowcast (there is no likelihood).")
  if (is.null(fit$obj))
    cli::cli_abort("This fit carries no objective; ESS cannot be computed.")
  if (!is.null(seed)) set.seed(seed)

  obj  <- fit$obj
  mode <- as.numeric(obj$env$last.par.best)
  pnm  <- names(obj$env$last.par.best)
  H    <- tryCatch(methods::as(obj$he(obj$env$last.par.best), "sparseMatrix"),
                   error = function(e) NULL)
  if (is.null(H)) cli::cli_abort("Could not form the Laplace precision (Hessian).")

  # Partition parameters into hyperparameters (h) and latent field (z).
  is_latent <- pnm %in% .LATENT_PAR_NAMES
  h_idx <- which(!is_latent); z_idx <- which(is_latent)
  if (length(h_idx) == 0L) cli::cli_abort("No hyperparameters found to assess.")

  mode_h <- mode[h_idx]
  ess_val <- tryCatch({
    if (length(z_idx) == 0L) {
      # No latent field: marginal precision is just H.
      S <- as.matrix(H[h_idx, h_idx, drop = FALSE])
      cond_z <- function(h) numeric(0)
      Hzh <- NULL
    } else {
      Hhh <- H[h_idx, h_idx, drop = FALSE]
      Hzz <- H[z_idx, z_idx, drop = FALSE]
      Hzh <- H[z_idx, h_idx, drop = FALSE]
      Wzh <- Matrix::solve(Hzz, Hzh)                      # H_zz^{-1} H_zh  [n_z x n_h]
      S   <- as.matrix(Hhh - Matrix::crossprod(Hzh, Wzh)) # Schur complement (marginal precision of h)
      mode_z <- mode[z_idx]
      cond_z <- function(h) as.numeric(mode_z - Wzh %*% (h - mode_h))
    }
    S <- (S + t(S)) / 2
    L <- chol(S + diag(1e-8 * mean(abs(diag(S))) + 1e-12, nrow(S)))  # S = L'L

    # Reweight draws of the hyperparameters (latent profiled at its conditional mode).
    lw <- numeric(n_draws)
    for (i in seq_len(n_draws)) {
      h <- as.numeric(mode_h + backsolve(L, stats::rnorm(length(mode_h))))
      x <- mode
      x[h_idx] <- h
      if (length(z_idx) > 0L) x[z_idx] <- cond_z(h)
      dh <- h - mode_h
      quad <- as.numeric(crossprod(dh, S %*% dh))        # (h-mode_h)' S (h-mode_h)
      lw[i] <- -as.numeric(obj$fn(x)) + 0.5 * quad       # log target - log proposal (const cancels)
    }
    lw <- lw[is.finite(lw)]
    if (length(lw) < 2L) NA_real_ else {
      lw <- lw - max(lw); w <- exp(lw); sum(w)^2 / sum(w^2)
    }
  }, error = function(e) NA_real_)

  ratio <- ess_val / n_draws
  if (isTRUE(warn) && is.finite(ratio) && ratio < threshold) {
    cli::cli_warn(c(
      "!" = "Low effective sample size: ESS = {round(ess_val, 1)} of {n_draws} draws ({round(100 * ratio)}%).",
      "i" = "The Gaussian (Laplace) approximation of the hyperparameters may be unreliable -- the posterior is likely skewed or weakly identified (common with little data). Interpret the intervals with caution."))
  }
  structure(ess_val, ratio = ratio, n_hyper = length(h_idx))
}
