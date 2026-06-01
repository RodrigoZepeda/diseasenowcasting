# =============================================================================
# Posterior-predictive nowcast (Laplace sampling)
# =============================================================================
# Mirrors generated_quantities.stan:
#   lambda[t]        = exp(log_mean_capped[t])
#   lambda_future[t] = lambda[t] * (1 - Gstar[t]) + TOL
#   nowcast[t]       = case_counts[t] + epidemic_rng(lambda_future[t], nb_size)
# where the count draw is NB-2 (size = 1/phi_nb) or Poisson, with overflow
# guards.  Posterior parameter draws come from the Laplace approximation:
# sample the parameter vector from N(mode, precision^{-1}) -- joint Hessian for
# the joint-mode fit (cmdstanr $laplace() regime) or the sdreport joint
# precision for the marginal (random=) fit.
# =============================================================================

#' Split a named flat parameter vector into a parList-style named list
#' (order within each repeated name is preserved). Avoids TMB's parList(x)
#' recycling warning when fed an explicit (sampled) vector.
#' @keywords internal
#' @noRd
.split_named_vector <- function(named_vector) {
  parameter_names <- names(named_vector)
  split(unname(named_vector), factor(parameter_names, levels = unique(parameter_names)))
}

#' Sample from N(mean_vector, precision^{-1}) given a sparse precision matrix
#' (the canonical TMB recipe via a supernodal Cholesky).
#'
#' The joint-mode Hessian can be indefinite (not positive-definite) at a saddle
#' mode -- notably SIR on long series, where the beta random-walk variance
#' collapses.  When the Cholesky fails we add an increasing ridge to the
#' diagonal until the matrix is PD (a standard Laplace-sampling safeguard;
#' harmless when the precision is already PD).
#' @keywords internal
#' @noRd
.sample_mvnorm_precision <- function(mean_vector, precision_matrix, n_samples) {
  standard_normal <- matrix(rnorm(length(mean_vector) * n_samples), ncol = n_samples)
  cholesky <- tryCatch(Matrix::Cholesky(precision_matrix, super = TRUE), error = function(e) NULL)
  if (is.null(cholesky)) {
    diagonal_scale <- mean(abs(Matrix::diag(precision_matrix))) + 1e-8
    for (ridge_exponent in -6:0) {
      ridge <- diagonal_scale * 10^ridge_exponent
      ridged_precision <- precision_matrix +
        Matrix::Diagonal(nrow(precision_matrix), x = ridge)
      cholesky <- tryCatch(Matrix::Cholesky(ridged_precision, super = TRUE), error = function(e) NULL)
      if (!is.null(cholesky)) break
    }
    if (is.null(cholesky))
      cli::cli_abort("Posterior precision is not positive-definite even after ridging.")
  }
  standard_normal <- Matrix::solve(cholesky, standard_normal, system = "Lt")
  standard_normal <- Matrix::solve(cholesky, standard_normal, system = "Pt")
  mean_vector + as.matrix(standard_normal)
}

#' Overflow-safe posterior-predictive count draw (port of `epidemic_rng`)
#' @keywords internal
#' @noRd
.epidemic_rng <- function(is_negbin, rate, phi_nb) {
  max_rate <- 1e8
  rate <- ifelse(!is.finite(rate) | rate < 0, 0, pmin(rate, max_rate))
  n_cells <- length(rate)
  if (!is_negbin) {
    draws <- numeric(n_cells)
    positive <- rate > 0
    draws[positive] <- rpois(sum(positive), rate[positive])
    return(draws)
  }
  nb_size <- 1 / phi_nb               # NB-2 size; eff_phi_nb = 1/phi_nb
  if (!is.finite(nb_size) || nb_size <= 0) nb_size <- 1e-4
  draws <- numeric(n_cells)
  is_large_mean <- rate >= 1e5        # Normal approx for very large means (matches Stan guard)
  if (any(!is_large_mean)) draws[!is_large_mean] <- rnbinom(sum(!is_large_mean), size = nb_size, mu = rate[!is_large_mean])
  if (any(is_large_mean)) {
    nb_variance <- rate[is_large_mean] + rate[is_large_mean]^2 / nb_size
    draws[is_large_mean] <- pmax(0, round(rnorm(sum(is_large_mean), rate[is_large_mean], sqrt(nb_variance))))
  }
  draws
}

#' Quantile-table summary of a pooled nowcast draws matrix
#'
#' Matches diseasenowcast2's `.summarise_nowcast_matrix()` exactly: one row per
#' event (`.event_num` 0-indexed, ascending) with columns
#' `mean, median, sd, mad, q2.5, q5, q10, q25, q50, q75, q90, q95, q97.5`.
#'
#' @param draws_matrix Pooled draws matrix `[n_draws x max_time]`.
#' @returns A data.frame in the scoring-pipeline format.
#' @export
summarise_nowcast_matrix <- function(draws_matrix) {
  quantile_probs  <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
  quantile_names  <- c("q2.5", "q5", "q10", "q25", "q50", "q75", "q90", "q95", "q97.5")
  per_event <- lapply(seq_len(ncol(draws_matrix)), function(event_index) {
    event_draws <- draws_matrix[, event_index]
    event_draws <- event_draws[is.finite(event_draws)]
    if (!length(event_draws)) return(c(rep(NA_real_, 4), rep(NA_real_, length(quantile_probs))))
    c(mean(event_draws), stats::median(event_draws), stats::sd(event_draws), stats::mad(event_draws),
      stats::quantile(event_draws, quantile_probs, names = FALSE))
  })
  summary_table <- as.data.frame(do.call(rbind, per_event))
  names(summary_table) <- c("mean", "median", "sd", "mad", quantile_names)
  summary_table$.event_num <- seq_len(ncol(draws_matrix)) - 1L
  summary_table
}

#' Posterior-predictive nowcast draws (full curve) from a joint RTMB fit
#'
#' Samples the parameter vector from the Laplace approximation and, per draw,
#' reconstructs the latent incidence `lambda[t]` AND draws the complete
#' posterior-predictive count `nowcast[t]` at every event time.  Returns both a
#' `[n_draws x max_time]` predictive-nowcast matrix (the Stan
#' generated-quantities `nowcast[t]` analogue) and the latent-incidence matrix.
#'
#' @param fit A joint fit from [fit()] (non-`delay_only`).
#' @param n_draws Number of posterior parameter draws.
#' @param target Optional event index for the convenience `quantiles`/`median`
#'   summary (default newest).
#' @param probs Quantile probabilities for the convenience summary.
#' @param seed Optional RNG seed.
#' @returns list(`M` = predictive draws matrix, `lambda_draws` = latent
#'   incidence matrix, `nowcast` = [summarise_nowcast_matrix()] table,
#'   `draws`/`quantiles`/`median`/`observed` at `target`).
#' @keywords internal
#' @noRd
.nowcast_draws <- function(fit, target = NULL, n_draws = 1000L,
                           probs = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975),
                           seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  data <- fit$data; priors <- fit$priors
  n_time <- data$max_time
  target <- target %||% n_time
  obj <- fit$obj
  is_negbin <- data$is_negative_binomial == 1L

  # Laplace posterior precision at the mode.
  #  - joint-mode fit (no random=, the fast default): precision = Hessian of the
  #    joint nll = obj$he(mode).  This is exactly cmdstanr $laplace().
  #  - marginal fit (random=): precision = sdreport joint precision over (fixed, random).
  if (isFALSE(fit$use_random %||% FALSE)) {
    mode_vector <- obj$env$last.par.best
    precision_matrix <- methods::as(obj$he(mode_vector), "sparseMatrix")
  } else {
    sd_report <- RTMB::sdreport(obj, getJointPrecision = TRUE)
    precision_matrix <- sd_report$jointPrecision
    mode_vector <- obj$env$last.par.best
    if (is.null(precision_matrix)) {
      precision_matrix <- methods::as(solve(sd_report$cov.fixed), "sparseMatrix")
      mode_vector <- sd_report$par.fixed
    }
  }
  parameter_names <- names(mode_vector)
  parameter_draws <- .sample_mvnorm_precision(as.numeric(mode_vector), precision_matrix, n_draws)

  n_strata <- as.integer(data$num_strata %||% 1L)
  case_counts_mat <- if (is.matrix(data$case_counts)) data$case_counts else matrix(data$case_counts, n_time, n_strata)
  observed_total  <- rowSums(case_counts_mat)                          # total observed per event-time

  # Total (summed over strata) drives the existing summary/score path; per-stratum
  # arrays are kept for stratified inspection.  At n_strata == 1 the total equals
  # the single column, so unstratified output is byte-for-byte the old behaviour.
  nowcast_draws <- matrix(NA_real_, n_draws, n_time)                   # predictive counts, total
  lambda_draws  <- matrix(NA_real_, n_draws, n_time)                   # latent incidence, total
  nowcast_strata <- array(NA_real_, c(n_draws, n_time, n_strata))
  lambda_strata  <- array(NA_real_, c(n_draws, n_time, n_strata))
  for (draw_index in seq_len(n_draws)) {
    parlist <- .split_named_vector(setNames(parameter_draws[, draw_index], parameter_names))
    reconstructed <- .joint_reconstruct(data, priors, parlist, fit$Bmat, fit$freq)
    lambda_mat <- matrix(reconstructed$lambda, n_time, n_strata)
    gstar_mat  <- matrix(reconstructed$Gstar,  n_time, n_strata)
    lambda_future <- as.numeric(lambda_mat * (1 - gstar_mat)) + 1e-8   # flattened [T*S]
    phi_nb <- if (is_negbin) reconstructed$phi_nb else NA_real_
    pred_cells <- matrix(.epidemic_rng(is_negbin, lambda_future, phi_nb), n_time, n_strata) + case_counts_mat
    nowcast_strata[draw_index, , ] <- pred_cells
    lambda_strata[draw_index, , ]  <- lambda_mat
    nowcast_draws[draw_index, ] <- rowSums(pred_cells)
    lambda_draws[draw_index, ]  <- rowSums(lambda_mat)
  }

  target_draws <- nowcast_draws[, target]
  target_quantiles <- quantile(target_draws, probs = probs, na.rm = TRUE)
  list(M = nowcast_draws, lambda_draws = lambda_draws,
       M_strata = nowcast_strata, lambda_strata = lambda_strata, n_strata = n_strata,
       nowcast = summarise_nowcast_matrix(nowcast_draws),
       draws = target_draws, quantiles = target_quantiles,
       median = unname(target_quantiles[which.min(abs(probs - 0.5))]),
       target = target, observed = observed_total[target])
}
