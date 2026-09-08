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

#' Posterior-predictive draw of the counts STILL TO COME for each origin
#'
#' Under the negative binomial each origin carries a gamma frailty
#' Lambda_t ~ Gamma(r, r) scales its whole reporting cloud. Drawing the
#' future count from that PRIOR frailty gives the right *marginal* spread but the
#' wrong *conditional* one: the k_t reports already in hand for an origin pin its
#' frailty down.  Conjugacy gives the posterior directly,
#'
#' Lambda_t conditional on k_t is Gamma with shape r + k_t and rate
#' r + expected_observed_count,
#'
#' so the future count is negative binomial with size r + k_t and mean
#' future_mean * (r + k_t) / (r + observed_mean). The difference is large: with
#' r = 6.7, lambda = 200 and 60% of reports in, the prior draw has SD ~32.6
#' against a true ~11.6, while this form gives ~11.6.  Ignoring it made every NB
#' interval roughly twice too wide at short horizons.
#'
#' At k_t = 0 and observed_mean = 0 this collapses to the prior draw, which is
#' correct: an origin with nothing observed yet says nothing about its frailty.
#'
#' **Off by default.**  The update is right *given the model is right*, and on data
#' simulated from the model it fixes the calibration outright (50% intervals move
#' from 0.77 coverage to 0.51).  On real data the epidemic mean is always somewhat
#' misspecified, and the prior draw's extra width was quietly absorbing that:
#' switching the update on made mean WIS worse on every disease in
#' `devel/benchmark_retraction.R` (COVID 11.1 -> 160.4) at essentially unchanged
#' coverage, because sharper intervals expose a biased centre.  So this is opt-in
#' via `options(diseasenowcasting.conditional_frailty = TRUE)` until the underlying
#' misfit is dealt with, and the default reproduces the published benchmarks.
#'
#' @param is_negbin Whether the likelihood is negative binomial.
#' @param future_mean Expected count still to arrive, per cell.
#' @param observed_mean Expected count already observed, per cell.
#' @param observed_count Count actually observed, per cell.
#' @param phi_nb NB overdispersion (`1 / r`).
#' @returns A numeric vector of draws, one per cell.
#' @keywords internal
#' @noRd
.future_count_draw <- function(is_negbin, future_mean, observed_mean, observed_count, phi_nb) {
  if (!is_negbin) return(.epidemic_rng(FALSE, future_mean, phi_nb))
  if (!isTRUE(getOption("diseasenowcasting.conditional_frailty", FALSE)))
    return(.epidemic_rng(TRUE, future_mean, phi_nb))
  frailty_size <- 1 / phi_nb
  if (!is.finite(frailty_size) || frailty_size <= 0) frailty_size <- 1e-4
  posterior_shape <- frailty_size + pmax(as.numeric(observed_count), 0)
  posterior_rate  <- frailty_size + pmax(as.numeric(observed_mean), 0)

  # The posterior frailty MEAN, (r + k) / (r + m), is the factor by which this
  # origin ran hot or cold relative to the fitted mean.  It is meaningful only in
  # so far as the fit is roughly right: a ratio of 3000 does not mean the origin is
  # 3000x hotter, it means `lambda_t G*_t` and `k_t` disagree wildly -- which does
  # happen in the two-stage path, where the delay is held at an imputed value and
  # the epidemic mean absorbs the mismatch.  Multiplying an already-large future
  # mean by an unbounded ratio turned a handful of such origins into 1e8-case
  # predictions and destroyed the benchmark.  Bounding it keeps the calibration
  # gain where the model fits (ratios there sit within a few tens of percent of 1)
  # and degrades back to the marginal draw where it does not.
  frailty_ratio <- posterior_shape / posterior_rate
  frailty_ratio <- pmin(pmax(frailty_ratio, 0.2), 5)
  conditional_mean <- as.numeric(future_mean) * frailty_ratio
  # `.epidemic_rng()` takes a SCALAR size, but the posterior size varies by cell, so
  # the draw is done here.  Guards mirror it: clamp the rate, and use the normal
  # approximation for very large means.
  conditional_mean <- ifelse(!is.finite(conditional_mean) | conditional_mean < 0, 0,
                             pmin(conditional_mean, 1e8))
  draws <- numeric(length(conditional_mean))
  is_large <- conditional_mean >= 1e5
  ordinary <- !is_large & conditional_mean > 0
  if (any(ordinary))
    draws[ordinary] <- stats::rnbinom(sum(ordinary), size = posterior_shape[ordinary],
                                      mu = conditional_mean[ordinary])
  if (any(is_large)) {
    variance <- conditional_mean[is_large] +
      conditional_mean[is_large]^2 / posterior_shape[is_large]
    draws[is_large] <- pmax(0, round(stats::rnorm(sum(is_large),
                                                  conditional_mean[is_large], sqrt(variance))))
  }
  draws
}

#' Draw the genuine share of the standing rows (linelist retraction predictive)
#'
#' The standing tables built by `.linelist_retraction_stats()` hold one row per
#' non-empty cell, with `cell` the column-major index into the `[n_time x n_strata]`
#' matrix.  Each row draws `Binomial(count, rho)` -- from the age lookup for exactly
#' observed reports, from the per-pattern vector for censored ones -- and the
#' successes are summed back into that matrix.
#'
#' @param standing_rows Exact standing counts, columns `cell` / `stratum` / `age` /
#'   `count` (may be `NULL`).
#' @param rho `[age + 1, stratum]` genuine-probability lookup.
#' @param standing_censored_rows Censored standing counts (may be `NULL`).
#' @param rho_censored Per-pattern genuine probability, aligned to
#'   `standing_censored_rows` (may be `NULL`).
#' @param n_time,n_strata Grid dimensions.
#' @returns A `[n_time x n_strata]` matrix of retained standing cases.
#' @keywords internal
#' @noRd
.thin_standing_rows <- function(standing_rows, rho,
                                standing_censored_rows = NULL, rho_censored = NULL,
                                n_time, n_strata) {
  retained <- matrix(0.0, n_time, n_strata)
  rho <- matrix(rho, ncol = n_strata)

  accumulate <- function(table_rows, probabilities) {
    if (is.null(table_rows) || nrow(table_rows) == 0L) return(invisible(NULL))
    genuine <- stats::rbinom(nrow(table_rows), size = as.integer(table_rows[, "count"]),
                             prob = pmin(pmax(probabilities, 0), 1))
    by_cell <- rowsum(genuine, as.integer(table_rows[, "cell"]), reorder = FALSE)
    cells   <- as.integer(rownames(by_cell))
    retained[cells] <<- retained[cells] + as.numeric(by_cell)
  }

  if (!is.null(standing_rows) && nrow(standing_rows) > 0L) {
    ages   <- as.integer(standing_rows[, "age"])
    strata <- as.integer(standing_rows[, "stratum"])
    accumulate(standing_rows, rho[cbind(ages + 1L, strata)])
  }
  if (!is.null(standing_censored_rows) && nrow(standing_censored_rows) > 0L &&
      !is.null(rho_censored)) {
    accumulate(standing_censored_rows, rho_censored)
  }
  retained
}

#' Draw from zero-truncated count laws indexed by their own mean
#' @keywords internal
#' @noRd
.draw_ztpoisson_own_mean <- function(own_mean) {
  parent_mean <- .ztpoisson_parent_mean(own_mean)
  if (!is.finite(parent_mean) || parent_mean < 1e-8) return(1)
  probability_zero <- exp(-parent_mean)
  max(1, stats::qpois(
    probability_zero + (1 - probability_zero) * stats::runif(1L),
    parent_mean
  ))
}

#' @keywords internal
#' @noRd
.draw_ztnb_own_mean <- function(own_mean, size) {
  parent_mean <- .ztnb_parent_mean(own_mean, size)
  if (!is.finite(parent_mean) || parent_mean < 1e-8) return(1)
  probability_zero <- (size / (size + parent_mean))^size
  max(1, stats::qnbinom(
    probability_zero + (1 - probability_zero) * stats::runif(1L),
    size = size, mu = parent_mean
  ))
}

#' Anchored finite-horizon draw for one count-cumulative parameter draw
#'
#' Every event/stratum starts from its latest observed `C_t(d*)`.  Hurdle models
#' then simulate the specified signed update marginals sequentially, carrying
#' the previous-movement state.  The cumulative-level model uses independent
#' Poisson addition/withdrawal updates as an explicitly labelled anchored
#' approximation; their means telescope to `mu * (q_C(H) - q_C(d*))`.
#'
#' @keywords internal
#' @noRd
.draw_count_cumulative_terminal <- function(data, reconstructed) {
  n_time <- as.integer(data$max_time)
  n_strata <- as.integer(data$num_strata %||% 1L)
  cc <- reconstructed$count_cumulative
  H <- cc$settlement_horizon
  terminal <- matrix(0.0, n_time, n_strata)
  projection_count <- 0L

  for (s in seq_len(n_strata)) {
    for (t in seq_len(n_time)) {
      observed_delays <- which(data$observation_mask[t, , s])
      if (!length(observed_delays)) next
      horizon <- max(observed_delays) - 1L
      anchor <- data$cumulative_level_array[t, horizon + 1L, s]
      running_level <- anchor
      previous_nonzero <-
        data$signed_update_array[t, horizon + 1L, s] != 0

      if (horizon < H) {
        for (delay in seq.int(horizon + 1L, H)) {
          index <- delay + 1L
          alpha <- reconstructed$lambda[t, s] * cc$alpha_unit[index]
          omega <- reconstructed$lambda[t, s] * cc$omega_unit[index]
          update <- 0
          if (cc$observation == 1L) {
            # Anchored update approximation for the level composite.  This is
            # not an exact conditional draw from the dependent level process.
            update <- stats::rpois(1L, max(alpha, 0)) -
              stats::rpois(1L, max(omega, 0))
          } else {
            total <- alpha + omega
            eta <- cc$movement[["intercept"]] +
              cc$movement[["age"]] * log1p(delay) +
              cc$movement[["previous"]] * previous_nonzero
            movement_probability <-
              .count_cumulative_movement_probability(total, eta)
            moved <- stats::rbinom(1L, 1L,
                                    min(max(movement_probability, 0), 1)) == 1L
            if (moved) {
              direction <- if (stats::runif(1L) < alpha / total) 1 else -1
              own_mean <- total / movement_probability
              magnitude <- if (cc$observation == 2L) {
                .draw_ztnb_own_mean(own_mean, cc$magnitude_size)
              } else {
                .draw_ztpoisson_own_mean(own_mean)
              }
              update <- direction * magnitude
            }
          }
          running_level <- running_level + update
          previous_nonzero <- update != 0
        }
      }
      if (running_level < 0) {
        projection_count <- projection_count + 1L
        running_level <- 0
      }
      terminal[t, s] <- running_level
    }
  }

  list(
    terminal = terminal,
    projection_count = projection_count,
    reconstruction = if (cc$observation == 1L)
      "anchored independent signed-Poisson update approximation" else
      "anchored sequential hurdle updates"
  )
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
                           seed = sample.int(.Machine$integer.max, 1)) {
  if (!is.null(seed)) set.seed(seed)
  # Prior-only nowcasts carry precomputed prior-predictive draws.
  if (isTRUE(fit$prior_only)) return(.prior_only_draws(fit, target, n_draws, probs))
  data <- fit$data; priors <- fit$priors
  n_time <- data$max_time
  target <- target %||% n_time
  obj <- fit$obj
  is_count_cumulative <- isTRUE(data$is_count_cumulative == 1L)
  is_negbin <- data$is_negative_binomial == 1L &&
    (!is_count_cumulative ||
       identical(as.integer(data$count_cumulative_observation), 1L))

  # Laplace posterior precision at the mode.
  #  - joint-mode fit (no random=, the fast default): precision = Hessian of the
  #    joint nll = obj$he(mode).  This is exactly cmdstanr $laplace().
  #  - marginal fit (random=): precision = sdreport joint precision over (fixed, random).
  if (is.null(obj)) {
    # A saved/loaded fit: the live RTMB tape is gone, but save_nowcast() stored
    # the Laplace mode and precision -- sample from those directly (any n_draws).
    mode_vector      <- fit$mode
    precision_matrix <- fit$precision
    if (is.null(mode_vector) || is.null(precision_matrix))
      cli::cli_abort(c("This fit has no live RTMB objective and no stored Laplace mode/precision.",
                       "i" = "Re-load with {.code load_nowcast(file, rebuild = TRUE)}, or re-fit."))
  } else if (isFALSE(fit$use_random %||% FALSE)) {
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
  is_retraction   <- isTRUE(data$is_linelist_retraction == 1L)
  # Under retractions `case_counts` is every row ever seen (what the count block
  # models); the number of cases currently ON THE BOOKS -- and so the base the
  # predictive completes, and the "observed" series to report -- is the standing
  # count, which excludes rows already retracted.
  # Under confirmation the cases already on the books for the target are the
  # CONFIRMED ones; under retraction they are the ones still standing.
  resolution_mode <- if (is_retraction) as.integer(data$resolution_mode %||% 0L) else 0L
  # `resolved_counts` holds only the POSITIVE resolutions, so it is the confirmed
  # total in modes 1 and 2 and an all-zero matrix in retraction-only mode.
  observed_mat <- if (is_count_cumulative) {
    anchored <- matrix(0.0, n_time, n_strata)
    for (s in seq_len(n_strata)) for (t in seq_len(n_time)) {
      observed_delays <- which(data$observation_mask[t, , s])
      if (length(observed_delays))
        anchored[t, s] <- data$cumulative_level_array[
          t, max(observed_delays), s
        ]
    }
    anchored
  } else if (!is_retraction) case_counts_mat
    else if (resolution_mode >= 1L) data$resolved_counts
    else data$standing_counts
  observed_total  <- rowSums(observed_mat)                             # total observed per event-time

  # Total (summed over strata) drives the existing summary/score path; per-stratum
  # arrays are kept for stratified inspection.  At n_strata == 1 the total equals
  # the single column, so unstratified output is byte-for-byte the old behaviour.
  nowcast_draws <- matrix(NA_real_, n_draws, n_time)                   # predictive counts, total
  lambda_draws  <- matrix(NA_real_, n_draws, n_time)                   # latent incidence, total
  nowcast_strata <- array(NA_real_, c(n_draws, n_time, n_strata))
  lambda_strata  <- array(NA_real_, c(n_draws, n_time, n_strata))
  is_confirmation <- isTRUE(data$is_confirmation == 1L)
  projection_count <- 0L
  cumulative_reconstruction <- NULL
  for (draw_index in seq_len(n_draws)) {
    parlist <- .split_named_vector(setNames(parameter_draws[, draw_index], parameter_names))
    reconstructed <- .joint_reconstruct(data, priors, parlist, fit$Bmat, fit$freq)
    lambda_mat <- matrix(reconstructed$lambda, n_time, n_strata)
    phi_nb <- if (is_negbin) reconstructed$phi_nb else NA_real_
    if (is_count_cumulative) {
      cumulative_draw <- .draw_count_cumulative_terminal(data, reconstructed)
      pred_cells <- cumulative_draw$terminal
      projection_count <- projection_count + cumulative_draw$projection_count
      cumulative_reconstruction <- cumulative_draw$reconstruction
    } else if (is_confirmation) {
      # Final settled count = observed cumulative + future genuine additions -
      # still-standing erroneous mass.  Under NB the additions and retractions of
      # one origin share a SINGLE gamma frailty Lambda ~ Gamma(r, r) (they are
      # thinnings of the same overdispersed report cloud), so draw one Lambda per
      # (time, stratum) and make both terms Poisson(mean * Lambda) -- drawing them
      # as two independent NBs would double-count and inflate the frailty noise.
      confirmation_means <- reconstructed$confirmation
      # Same conjugate update as `.future_count_draw()`, but drawn explicitly here
      # because the additions and the still-standing retractions of one origin must
      # share ONE frailty.  The observed cumulative C_t(d*) is Poisson in that same
      # frailty, so Lambda | C ~ Gamma(r + C, r + E C).
      shared_frailty <- if (is_negbin) {
        frailty_size    <- 1 / phi_nb
        observed_mean   <- pmax(as.numeric(confirmation_means$observed_mean), 0)
        posterior_shape <- frailty_size + pmax(as.numeric(case_counts_mat), 0)
        posterior_rate  <- frailty_size + observed_mean
        matrix(stats::rgamma(n_time * n_strata, shape = posterior_shape,
                             rate = posterior_rate), n_time, n_strata)
      } else {
        matrix(1, n_time, n_strata)
      }
      future_additions <- matrix(
        stats::rpois(
          n_time * n_strata,
          pmax(as.numeric(confirmation_means$addition_mean), 0) * as.numeric(shared_frailty)
        ),
        n_time, n_strata
      )
      # The erroneous mass still on the books is a SUB-POPULATION of the observed
      # cumulative, not a stream of its own.  The decomposition is exact:
      # E[C_t(d*)] = mu_t [G_D - (1 - p) G_W] splits into genuine-on-books
      # (lambda_t G_D) plus still-standing erroneous (`retraction_mean`), so the
      # erroneous share of what has already been counted is
      # `retraction_mean / observed_mean` and lies in [0, 1] by construction.
      #
      # Drawing that mass as an INDEPENDENT Poisson let it exceed the cumulative it
      # is then subtracted from, and the settled count came out negative at the
      # newest event-times -- where the cumulative is still near zero but the
      # retraction intensity is not.  A count target must not go negative.
      # Thinning the observed rows binomially is bounded by construction, has the
      # same mean when the cumulative sits at its expectation, and -- exactly as
      # `.thin_standing_rows()` argues in the linelist branch -- carries no
      # frailty, because it thins a count that has already been observed.
      erroneous_share <- pmin(pmax(
        as.numeric(confirmation_means$retraction_mean) /
          pmax(as.numeric(confirmation_means$observed_mean), 1e-8), 0), 1)
      standing_retractions <- matrix(
        stats::rbinom(n_time * n_strata,
                      size = pmax(floor(as.numeric(case_counts_mat)), 0),
                      prob = erroneous_share),
        n_time, n_strata
      )
      pred_cells <- case_counts_mat + future_additions - standing_retractions
    } else {
      gstar_mat  <- matrix(reconstructed$Gstar,  n_time, n_strata)
      lambda_future <- as.numeric(lambda_mat * (1 - gstar_mat)) + 1e-8   # flattened [T*S]
      # Condition the frailty on what this origin has already shown (see
      # `.future_count_draw()`).  Under a resolution model the rows observed are
      # the GROSS reports, so the conditioning mean is mu_t G* = (lambda_t / p) G*.
      gross_mat <- if (is_retraction)
        lambda_mat / max(reconstructed$retraction$p, 1e-8) else lambda_mat
      future_cases <- matrix(
        .future_count_draw(is_negbin, lambda_future,
                           as.numeric(gross_mat * gstar_mat),
                           as.numeric(case_counts_mat), phi_nb),
        n_time, n_strata)
      pred_cells <- if (is_retraction) {
        # Settled genuine count = (standing rows that turn out to be genuine) +
        # (genuine cases not yet reported).  Each standing row of report age j is
        # kept independently with probability rho(j) -- a binomial thinning that is
        # free of the NB frailty, so the future term alone carries overdispersion.
        # Settled target = (already-resolved rows that count) + (unresolved rows
        # thinned by rho) + (cases not yet reported that will count).  Under
        # confirmation the first term is the confirmed cases, which are certain;
        # under retraction it is zero, since a retracted case is gone.
        reconstructed$retraction$resolved_weight * data$resolved_counts +
        .thin_standing_rows(data$standing_rows, reconstructed$retraction$rho,
                            data$standing_censored_rows, reconstructed$retraction$rho_censored,
                            n_time, n_strata) + future_cases
      } else {
        future_cases + case_counts_mat
      }
    }
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
       target = target, observed = observed_total[target],
       estimand = if (is_count_cumulative)
         sprintf("C_t(%d): finite-horizon settled retention",
                 as.integer(data$settlement_horizon)) else NULL,
       cumulative_reconstruction = cumulative_reconstruction,
       negative_projection_count = projection_count)
}
