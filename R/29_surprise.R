# =============================================================================
# surprise() -- anomaly detection via posterior predictive surprise scores
# =============================================================================
# Two types of surprise, both based on how likely the new data is under the
# fitted posterior:
#
# TYPE 1 -- new case count at event time t+1 (or any event time):
#   Given a fitted nowcast at time T, how surprising is count `n_new` at
#   event time t  Uses the posterior-predictive distribution of N_t.
#
# TYPE 2 -- new late delay (delay d_new >> max observed delay d*):
#   Given a fitted delay distribution G_D, how surprising is a report with
#   delay d_new > d*_current  Uses 1 - G_D(d_new | theta).
#
# Metrics (Zepeda-Tello & Shaman, 2025):
#   lpd                : log E_theta[p(obs | theta)]  (log predictive density)
#   ppp_right          : P(replicated >= observed | posterior)
#   ppp_left           : P(replicated <= observed | posterior)
#   relative_surprise  : rho = exp(lpd - lpd_mode), in (0, 1]; 1 = not surprising
# =============================================================================

#' Compute surprise scores for new observations
#'
#' @description
#' Given a fitted nowcast and new data that arrived *after* the fit, compute
#' how surprising the new data is under the fitted posterior.
#'
#' Two types of surprise are supported (select via the `type` argument):
#' - `"count"`: How surprising is a new case count at a given event time
#' - `"delay"`: How surprising is a report with a very long delay
#' - `"both"`: Compute both (requires appropriate `new_data` format).
#'
#' @param object A `nowcast_class` object (or `fit()` result).
#' @param new_data For `type = "count"`: a data.frame with columns
#'   `event_index` (0-indexed) and `count` (observed new total at that time).
#'   For `type = "delay"`: a data.frame with columns `delay` (numeric delay
#'   values) and optionally `weight` (counts with that delay, default 1).
#'   For `type = "both"`: supply both sets of columns.
#' @param type Which surprise type(s) to compute. One of `"count"`,
#'   `"delay"`, `"both"` (default `"both"`).
#' @param n_draws Number of posterior draws (default 500).
#' @param seed Optional RNG seed.
#' @returns A list with:
#'   - `$count_surprise` (data.frame): one row per event time with
#'     `event_index`, `observed`, `posterior_mean`, `lpd`, `ppp_right`,
#'     `ppp_left`, `relative_surprise`.
#'   - `$delay_surprise` (data.frame): one row per delay value with
#'     `delay`, `tail_prob` (= 1 - G_D(delay)), `lpd`, `relative_surprise`.
#' @param level Credible level used to flag surprises (default `0.99`).  A count
#'   is flagged when it falls in the outer `(1 - level)/2` tail of the posterior
#'   predictive (too high or too low); a delay is flagged when `P(D >= d)` or
#'   `P(D <= d)` is below `1 - level` (surprisingly long or short).
#' @export
surprise <- function(object, new_data, type = c("both", "count", "delay"),
                     level = 0.99, n_draws = 500L, seed = sample.int(.Machine$integer.max, 1)) {
  UseMethod("surprise")
}

#' @method surprise default
#' @export
surprise.default <- function(object, ...) {
  cli::cli_abort("No `surprise()` method for objects of class {.cls {class(object)}}.")
}

#' @noRd
S7::method(surprise, nowcast_class) <- function(object, new_data,
                                                  type = c("both","count","delay"),
                                                  level = 0.99, n_draws = 500L, seed = sample.int(.Machine$integer.max, 1)) {
  type <- match.arg(type)
  if (!is.null(seed)) set.seed(seed)
  fit    <- object@fits[[1]]
  .surprise_internal(fit, new_data, type, level, n_draws, seed)
}

#' Surprise score on a raw fit() result
#' @method surprise list
#' @param object A list returned by [fit()].
#' @param new_data See [surprise()].
#' @param type See [surprise()].
#' @param level See [surprise()].
#' @param n_draws See [surprise()].
#' @param seed See [surprise()].
#' @export
surprise.list <- function(object, new_data, type = c("both","count","delay"),
                          level = 0.99, n_draws = 500L, seed = sample.int(.Machine$integer.max, 1)) {
  if (!is.null(object$obj)) {
    type <- match.arg(type)
    if (!is.null(seed)) set.seed(seed)
    .surprise_internal(object, new_data, type, level, n_draws, seed)
  } else {
    cli::cli_abort("Unrecognized list -- expected a fit() result.")
  }
}

# -- Internal engine -----------------------------------------------------------

#' Reconstruct the fitted reporting-delay distribution for one parameter draw
#'
#' Builds the delay CDF/PMF function list (`$cdf`, ...) for a single posterior
#' draw, branching on the delay family.  Centralising this removes what used to
#' be three near-identical copies inside `.surprise_internal()` -- one of which
#' (the modal-PMF copy) referenced an undefined `rc`/`parlist` on the Dirichlet
#' branch and so errored for non-parametric delays.
#'
#' @param parlist A named parameter list for one draw (from `.split_named_vector`).
#' @param data,priors,fit The prepared engine, prior bundle, and fit object.
#' @param family Integer delay-family code (1 LogNormal, 2 Gamma, 3 GenGamma,
#'   4 Dirichlet, 5 Custom).
#' @returns A delay-distribution function list, or throws (callers wrap in tryCatch).
#' @keywords internal
#' @noRd
.delay_fns_for_parlist <- function(parlist, data, priors, fit, family) {
  if (family == 4L) {
    # Dirichlet (non-parametric): the delay pmf is a simplex, either fixed or
    # rebuilt from this draw's logits (last bin is the reference).
    n_bins  <- as.integer(data$np_model_length)
    simplex <- if (isTRUE(priors$delay_probs$is_constant == 1L)) {
      priors$delay_probs$fixed
    } else {
      exp_logits <- exp(parlist$delay_logits)
      c(exp_logits, 1) / (sum(exp_logits) + 1)
    }
    .nonparametric_delay_functions(simplex, n_bins)
  } else if (family == 5L) {
    theta <- as.numeric(parlist$custom_delay_params)
    priors$cdf_factory(theta)
  } else {
    reconstructed <- .joint_reconstruct(data, priors, parlist, fit$Bmat, fit$freq)
    if (family == 3L) {  # Generalised-Gamma needs the transformed shape Q
      shape_q <- .gengamma_shape_transform(parlist$delay_Q %||% -2)$shape_Q
      .delay_distribution_functions(3L, reconstructed$delay_mu, shape_q, reconstructed$delay_sigma)
    } else {
      .delay_distribution_functions(family, reconstructed$delay_mu, reconstructed$delay_sigma)
    }
  }
}

.surprise_internal <- function(fit, new_data, type, level = 0.99, n_draws, seed) {
  count_tail <- (1 - level) / 2     # two-sided tail for counts
  delay_tail <- (1 - level)         # one-sided tail for delays
  data   <- fit$data
  priors <- fit$priors
  family <- data$delay_family
  n_time <- data$max_time
  is_nb  <- data$is_negative_binomial == 1L

  # -- Posterior parameter draws from the Laplace approximation --------------
  obj       <- fit$obj
  mode_vec  <- obj$env$last.par.best
  precision <- tryCatch(methods::as(obj$he(mode_vec), "sparseMatrix"), error = function(e) NULL)
  par_draws <- if (!is.null(precision))
    tryCatch(.sample_mvnorm_precision(as.numeric(mode_vec), precision, n_draws),
             error = function(e) matrix(as.numeric(mode_vec), ncol = 1L))
  else matrix(as.numeric(mode_vec), ncol = 1L)
  par_names   <- names(mode_vec)
  n_par_draws <- ncol(par_draws)

  # Split the i-th column of par_draws into a named parameter list.
  parlist_for_draw <- function(draw_index) {
    .split_named_vector(setNames(par_draws[, draw_index], par_names))
  }
  # Discretised pmf P(D = delay) = G(delay + 0.5) - G(delay - 0.5).
  delay_pmf <- function(delay_fns, delay) {
    pmax(0, as.numeric(delay_fns$cdf(delay + 0.5)) -
            as.numeric(delay_fns$cdf(pmax(0, delay - 0.5))))
  }

  count_result <- delay_result <- NULL

  # -- TYPE 1: count surprise ------------------------------------------------
  if (type %in% c("count", "both")) {
    if (!all(c("event_index", "count") %in% names(new_data)))
      cli::cli_abort("For type=\"count\", `new_data` must have columns `event_index` and `count`.")

    rows <- lapply(seq_len(nrow(new_data)), function(row_index) {
      event_index_1based <- as.integer(new_data$event_index[row_index]) + 1L
      observed_count     <- as.integer(new_data$count[row_index])

      # Cases already reported at this event-time (the floor of the prediction).
      observed_so_far <- if (event_index_1based <= n_time && is.matrix(data$case_counts))
        sum(data$case_counts[event_index_1based, ]) else 0L

      # Posterior-predictive total N_t = observed-so-far + a draw of the still-
      # unreported cases, for each posterior parameter draw.
      predicted_totals <- vapply(seq_len(n_par_draws), function(draw_index) {
        reconstructed <- tryCatch(
          .joint_reconstruct(data, priors, parlist_for_draw(draw_index), fit$Bmat, fit$freq),
          error = function(e) NULL)
        if (is.null(reconstructed)) return(NA_real_)
        lambda <- if (event_index_1based <= n_time)
          rowSums(matrix(reconstructed$lambda, n_time, data$num_strata))[event_index_1based] else NA_real_
        Gstar  <- if (event_index_1based <= n_time)
          rowSums(matrix(reconstructed$Gstar,  n_time, data$num_strata))[event_index_1based] else 0
        lambda_unreported <- max(0, lambda * (1 - Gstar))
        unreported <- .epidemic_rng(is_nb, lambda_unreported + 1e-8, reconstructed$phi_nb)
        observed_so_far + unreported
      }, numeric(1))

      predicted_totals <- predicted_totals[is.finite(predicted_totals)]
      if (length(predicted_totals) == 0)
        return(data.frame(event_index = new_data$event_index[row_index], observed = observed_count,
          posterior_mean = NA, lpd = NA, ppp_right = NA, ppp_left = NA, relative_surprise = NA))

      # LPD = log of the (smoothed) predictive mass at the observed count; the
      # relative surprise normalises it by the mass at the predictive mode.
      pmf_at_observed <- mean(predicted_totals == observed_count) + 1e-12
      pmf_at_mode     <- max(tabulate(pmax(1L, predicted_totals + 1L))) / length(predicted_totals) + 1e-12
      lpd      <- log(pmf_at_observed)
      relative <- exp(lpd - log(pmf_at_mode))

      data.frame(
        event_index       = new_data$event_index[row_index],
        observed          = observed_count,
        posterior_mean    = round(mean(predicted_totals), 1),
        posterior_median  = round(stats::median(predicted_totals), 1),
        lpd               = round(lpd, 4),
        ppp_right         = round(mean(predicted_totals >= observed_count), 4),
        ppp_left          = round(mean(predicted_totals <= observed_count), 4),
        relative_surprise = round(pmax(0, pmin(1, relative)), 4)
      )
    })
    count_result <- do.call(rbind, rows)
    # Two-sided flag at the chosen level: observed count in the outer tail.
    too_high <- !is.na(count_result$ppp_right) & count_result$ppp_right < count_tail
    too_low  <- !is.na(count_result$ppp_left)  & count_result$ppp_left  < count_tail
    count_result$direction     <- ifelse(too_high, "high", ifelse(too_low, "low", ""))
    count_result$is_surprising <- too_high | too_low
  }

  # -- TYPE 2: delay surprise ------------------------------------------------
  if (type %in% c("delay", "both")) {
    if (!("delay" %in% names(new_data)))
      cli::cli_abort("For type=\"delay\", `new_data` must have a `delay` column.")
    new_delays    <- as.numeric(new_data$delay)
    delay_weights <- if ("weight" %in% names(new_data)) as.numeric(new_data$weight)
                     else rep(1, length(new_delays))

    # Grid over which each draw's PEAK (modal) pmf is found -- the normaliser for
    # the relative-surprise score.  Covers the realistic delay range and any
    # queried delay.
    max_grid_delay <- suppressWarnings(max(c(50, new_delays), na.rm = TRUE))
    mode_grid      <- 0:ceiling(max_grid_delay)

    rows <- lapply(seq_along(new_delays), function(row_index) {
      new_delay <- new_delays[row_index]

      # Reconstruct the delay distribution ONCE per draw, then derive all three
      # quantities we need from it (tail prob, pmf at the delay, peak pmf).
      per_draw <- lapply(seq_len(n_par_draws), function(draw_index) {
        delay_fns <- tryCatch(
          .delay_fns_for_parlist(parlist_for_draw(draw_index), data, priors, fit, family),
          error = function(e) NULL)
        if (is.null(delay_fns)) return(NULL)
        list(tail_prob = as.numeric(1 - delay_fns$cdf(new_delay)),   # P(D >= new_delay)
             pmf_at    = delay_pmf(delay_fns, new_delay),
             peak_pmf  = max(delay_pmf(delay_fns, mode_grid)))
      })
      per_draw <- Filter(Negate(is.null), per_draw)
      if (length(per_draw) == 0)
        return(data.frame(delay = new_delay, weight = delay_weights[row_index],
                          mean_tail_prob = NA, lpd = NA, relative_surprise = NA))

      mean_tail        <- mean(vapply(per_draw, `[[`, numeric(1), "tail_prob"))
      pmf_at_new_delay <- mean(vapply(per_draw, `[[`, numeric(1), "pmf_at")) + 1e-12
      peak_pmf         <- max(vapply(per_draw, `[[`, numeric(1), "peak_pmf")) + 1e-12

      mean_cdf  <- 1 - mean_tail                 # P(D <= new_delay)
      too_long  <- mean_tail < delay_tail        # surprisingly late report
      too_short <- mean_cdf  < delay_tail        # surprisingly early report
      data.frame(
        delay              = new_delay,
        weight             = delay_weights[row_index],
        mean_tail_prob     = round(mean_tail, 6),   # P(D >= d)
        cdf_prob           = round(mean_cdf, 6),    # P(D <= d)
        lpd                = round(log(pmf_at_new_delay), 4),
        relative_surprise  = round(pmax(0, pmin(1, exp(log(pmf_at_new_delay) - log(peak_pmf)))), 4),
        direction          = ifelse(too_long, "long", ifelse(too_short, "short", "")),
        is_surprising      = too_long | too_short
      )
    })
    delay_result <- do.call(rbind, rows)
  }

  structure(list(count_surprise = count_result, delay_surprise = delay_result,
                 type = type, level = level, n_draws = n_draws),
            class = "diseasenowcasting_surprise")
}

#' @export
print.diseasenowcasting_surprise <- function(x, ...) {
  cli::cli_h2("diseasenowcasting surprise scores (type: {x$type}, level: {x$level %||% 0.99})")
  if (!is.null(x$count_surprise)) {
    cli::cli_h3("Count surprise")
    print(x$count_surprise, row.names = FALSE)
    n_surp <- sum(x$count_surprise$is_surprising, na.rm = TRUE)
    if (n_surp > 0) cli::cli_alert_warning("{n_surp} event-time{?s} flagged as surprising (count too high/low).")
    else cli::cli_alert_success("No surprisingly unusual counts detected.")
  }
  if (!is.null(x$delay_surprise)) {
    cli::cli_h3("Delay surprise")
    print(x$delay_surprise, row.names = FALSE)
    n_surp <- sum(x$delay_surprise$is_surprising, na.rm = TRUE)
    if (n_surp > 0) cli::cli_alert_warning("{n_surp} delay{?s} flagged as surprising (too long/short).")
    else cli::cli_alert_success("No surprisingly long/short delays detected.")
  }
  invisible(x)
}
