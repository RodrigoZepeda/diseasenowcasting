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
.surprise_internal <- function(fit, new_data, type, level = 0.99, n_draws, seed) {
  count_tail <- (1 - level) / 2     # two-sided tail for counts
  delay_tail <- (1 - level)         # one-sided tail for delays
  data   <- fit$data
  priors <- fit$priors
  family <- data$delay_family
  n_time <- data$max_time
  is_nb  <- data$is_negative_binomial == 1L

  # Sample posterior
  obj      <- fit$obj
  mode_vec <- obj$env$last.par.best
  prec_mat <- tryCatch(methods::as(obj$he(mode_vec), "sparseMatrix"), error = function(e) NULL)
  par_draws <- if (!is.null(prec_mat))
    tryCatch(.sample_mvnorm_precision(as.numeric(mode_vec), prec_mat, n_draws),
             error = function(e) matrix(as.numeric(mode_vec), ncol = 1L))
  else matrix(as.numeric(mode_vec), ncol = 1L)
  par_names <- names(mode_vec)

  count_result <- delay_result <- NULL

  # -- TYPE 1: count surprise ------------------------------------------------
  if (type %in% c("count", "both")) {
    if (!all(c("event_index","count") %in% names(new_data)))
      cli::cli_abort("For type=\"count\", `new_data` must have columns `event_index` and `count`.")

    rows <- lapply(seq_len(nrow(new_data)), function(r) {
      t_idx    <- as.integer(new_data$event_index[r]) + 1L   # 1-indexed
      n_obs    <- as.integer(new_data$count[r])

      # Posterior predictive of total N_t (observed so far + future unreported)
      obs_now  <- if (t_idx <= n_time && is.matrix(data$case_counts))
        sum(data$case_counts[t_idx, ]) else 0L

      preds <- vapply(seq_len(ncol(par_draws)), function(i) {
        pl  <- .split_named_vector(setNames(par_draws[, i], par_names))
        rc  <- tryCatch(.joint_reconstruct(data, priors, pl, fit$Bmat, fit$freq), error = function(e) NULL)
        if (is.null(rc)) return(NA_real_)
        lambda  <- if (t_idx <= n_time) rowSums(matrix(rc$lambda, n_time, data$num_strata))[t_idx]
                   else NA_real_
        Gstar   <- if (t_idx <= n_time) rowSums(matrix(rc$Gstar,  n_time, data$num_strata))[t_idx]
                   else 0
        lambda_future <- max(0, lambda * (1 - Gstar))
        phi_nb <- rc$phi_nb
        unrep  <- .epidemic_rng(is_nb, lambda_future + 1e-8, phi_nb)
        obs_now + unrep
      }, numeric(1))

      preds <- preds[is.finite(preds)]
      if (length(preds) == 0) return(data.frame(event_index=new_data$event_index[r], observed=n_obs,
        posterior_mean=NA, lpd=NA, ppp_right=NA, ppp_left=NA, relative_surprise=NA))

      # LPD via log of the fraction of draws matching the observation
      # Smooth approximation: kernel density on integer support
      pmf_obs  <- mean(preds == n_obs) + 1e-12   # empirical PMF at n_obs (smoothed)
      pmf_mode <- max(tabulate(pmax(1L, preds + 1L))) / length(preds) + 1e-12
      lpd      <- log(pmf_obs)
      rho      <- exp(lpd - log(pmf_mode))

      data.frame(
        event_index      = new_data$event_index[r],
        observed         = n_obs,
        posterior_mean   = round(mean(preds), 1),
        posterior_median = round(stats::median(preds), 1),
        lpd              = round(lpd, 4),
        ppp_right        = round(mean(preds >= n_obs), 4),
        ppp_left         = round(mean(preds <= n_obs), 4),
        relative_surprise = round(pmax(0, pmin(1, rho)), 4)
      )
    })
    count_result <- do.call(rbind, rows)
    # Two-sided flag at the chosen level: observed count in the outer tail.
    too_high <- !is.na(count_result$ppp_right) & count_result$ppp_right < count_tail
    too_low  <- !is.na(count_result$ppp_left)  & count_result$ppp_left  < count_tail
    count_result$direction    <- ifelse(too_high, "high", ifelse(too_low, "low", ""))
    count_result$is_surprising <- too_high | too_low
  }

  # -- TYPE 2: delay surprise ------------------------------------------------
  if (type %in% c("delay", "both")) {
    if (!("delay" %in% names(new_data)))
      cli::cli_abort("For type=\"delay\", `new_data` must have a `delay` column.")
    new_delays  <- as.numeric(new_data$delay)
    delay_wts   <- if ("weight" %in% names(new_data)) as.numeric(new_data$weight) else rep(1, length(new_delays))

    rows <- lapply(seq_along(new_delays), function(r) {
      d_new <- new_delays[r]
      tail_probs <- vapply(seq_len(ncol(par_draws)), function(i) {
        pl  <- .split_named_vector(setNames(par_draws[, i], par_names))
        fns <- tryCatch({
          if (family == 4L) {
            n_b <- as.integer(data$np_model_length)
            sp  <- if (isTRUE(priors$delay_probs$is_constant == 1L)) priors$delay_probs$fixed
                   else { el <- exp(pl$delay_logits); c(el, 1) / (sum(el) + 1) }
            .nonparametric_delay_functions(sp, n_b)
          } else {
            rc <- .joint_reconstruct(data, priors, pl, fit$Bmat, fit$freq)
            if (family == 3L)
              .delay_distribution_functions(3L, rc$delay_mu, .gengamma_shape_transform(pl$delay_Q %||% -2)$shape_Q, rc$delay_sigma)
            else .delay_distribution_functions(family, rc$delay_mu, rc$delay_sigma)
          }
        }, error = function(e) NULL)
        if (is.null(fns)) return(NA_real_)
        as.numeric(1 - fns$cdf(d_new))   # P(D >= d_new) = tail probability
      }, numeric(1))

      tail_probs <- tail_probs[is.finite(tail_probs)]
      if (length(tail_probs) == 0) return(data.frame(delay=d_new, weight=delay_wts[r],
        mean_tail_prob=NA, lpd=NA, relative_surprise=NA))

      mean_tail <- mean(tail_probs)
      # PMF at d_new using mean delay PDF (finite difference)
      pmf_approx <- mean(vapply(seq_len(ncol(par_draws)), function(i) {
        pl  <- .split_named_vector(setNames(par_draws[, i], par_names))
        fns <- tryCatch(
          if (family == 4L) {
            n_b <- as.integer(data$np_model_length)
            sp  <- if (isTRUE(priors$delay_probs$is_constant == 1L)) priors$delay_probs$fixed
                   else { el <- exp(pl$delay_logits); c(el, 1) / (sum(el) + 1) }
            .nonparametric_delay_functions(sp, n_b)
          } else {
            rc <- .joint_reconstruct(data, priors, pl, fit$Bmat, fit$freq)
            if (family == 3L) .delay_distribution_functions(3L, rc$delay_mu, .gengamma_shape_transform(pl$delay_Q %||% -2)$shape_Q, rc$delay_sigma)
            else .delay_distribution_functions(family, rc$delay_mu, rc$delay_sigma)
          }, error = function(e) NULL)
        if (is.null(fns)) return(NA_real_)
        pmax(0, as.numeric(fns$cdf(d_new + 0.5)) - as.numeric(fns$cdf(pmax(0, d_new - 0.5))))
      }, numeric(1)), na.rm = TRUE) + 1e-12

      mode_pmf <- max(vapply(seq_len(ncol(par_draws)), function(i) {
        pl  <- .split_named_vector(setNames(par_draws[, i], par_names))
        fns <- tryCatch(
          if (family == 4L) {
            n_b <- as.integer(data$np_model_length)
            sp  <- if (isTRUE(priors$delay_probs$is_constant == 1L)) priors$delay_probs$fixed
                   else { el <- exp(pl$delay_logits); c(el, 1) / (sum(el) + 1) }
            .nonparametric_delay_functions(sp, n_b)
          } else {
            rc <- .joint_reconstruct(data, priors, pl, fit$Bmat, fit$freq)
            if (family == 3L) .delay_distribution_functions(3L, rc$delay_mu, .gengamma_shape_transform(pl$delay_Q %||% -2)$shape_Q, rc$delay_sigma)
            else .delay_distribution_functions(family, rc$delay_mu, rc$delay_sigma)
          }, error = function(e) NULL)
        if (is.null(fns)) return(NA_real_)
        mode_d <- exp(rc$log_loc %||% as.numeric(parlist$delay_mu %||% 1)) - 1
        pmax(0, as.numeric(fns$cdf(mode_d + 0.5)) - as.numeric(fns$cdf(pmax(0, mode_d - 0.5))))
      }, numeric(1)), na.rm = TRUE) + 1e-12

      mean_cdf  <- 1 - mean_tail                # P(D <= d_new) = G_D(d_new)
      too_long  <- mean_tail < delay_tail        # surprisingly late report
      too_short <- mean_cdf  < delay_tail        # surprisingly early report
      data.frame(
        delay              = d_new,
        weight             = delay_wts[r],
        mean_tail_prob     = round(mean_tail, 6),   # P(D >= d)
        cdf_prob           = round(mean_cdf, 6),    # P(D <= d)
        lpd                = round(log(pmf_approx), 4),
        relative_surprise  = round(pmax(0, pmin(1, exp(log(pmf_approx) - log(mode_pmf)))), 4),
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
