# =============================================================================
# ppc() -- posterior predictive check
# =============================================================================
# Two-panel check that is honest under censoring:
# (1) DELAY PPC: observed delay histogram vs posterior-predictive delay draws
#     from G_D(theta^(i)).  Always fully observed -- no censoring issue.
# (2) FULL-REPORT PPC: for event times where d* >= max observed delay (all
#     cases have been reported), compare the observed final count against
#     posterior-predictive N_t draws.
# =============================================================================

#' Posterior predictive check for a fitted nowcast
#'
#' @param object A `nowcast_class` object.
#' @param n_draws Number of posterior draws for the check (default 500).
#' @param seed Optional RNG seed.
#' @param min_full_report_delay Events are considered "fully reported" when
#'   their maximum observable delay `d*` is at least this many units (default
#'   the 90th percentile of observed delays).
#' @param ... Unused.
#' @returns A named list with slots `$plot` (a ggplot / patchwork), `$delay_ppc`
#'   (data frame), and `$count_ppc` (data frame).
#' @export
ppc <- function(object, n_draws = 500L, seed = sample.int(.Machine$integer.max, 1), min_full_report_delay = NULL, ...) {
  UseMethod("ppc")
}

#' @method ppc default
#' @export
ppc.default <- function(object, ...) {
  cli::cli_abort("No `ppc()` method for objects of class {.cls {class(object)}}.")
}

#' @noRd
S7::method(ppc, nowcast_class) <- function(object, n_draws = 500L, seed = sample.int(.Machine$integer.max, 1),
                                            min_full_report_delay = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)
  fit    <- object@fits[[1]]
  data   <- fit$data
  priors <- fit$priors
  family <- data$delay_family
  n_time <- data$max_time
  is_nb  <- data$is_negative_binomial == 1L

  # -- Sample posterior parameters --------------------------------------------
  # Draw from the Laplace posterior N(mode, Hessian^{-1}); fall back to the MAP
  # (a single "draw") if the precision matrix cannot be sampled.
  obj            <- fit$obj
  posterior_mode <- obj$env$last.par.best
  precision      <- methods::as(obj$he(posterior_mode), "sparseMatrix")
  par_draws <- tryCatch(
    .sample_mvnorm_precision(as.numeric(posterior_mode), precision, n_draws),
    error = function(e) NULL)
  if (is.null(par_draws)) {
    cli::cli_warn("Could not sample from posterior precision -- using MAP for PPC.")
    par_draws <- matrix(as.numeric(posterior_mode), ncol = 1L)
  }
  par_names <- names(posterior_mode)

  # -- Panel 1: Delay PPC ----------------------------------------------------
  # Observed delays and their case weights (column 3 = delay, column 2 = count).
  observed_delays <- as.integer(data$m[, 3])
  observed_weights <- as.numeric(data$m[, 2])
  # Plot delays up to a high quantile of the observed (cap at 60) so the long
  # right tail does not dominate the axis.
  max_delay_to_plot <- min(
    as.integer(quantile(rep(observed_delays, times = pmax(1L, round(observed_weights))),
                        0.995, na.rm = TRUE)) + 2L,
    60L)
  delay_grid <- seq(0, max_delay_to_plot, by = 0.5)

  # Posterior-predictive delay PMF for each parameter draw: reconstruct the delay
  # distribution for that draw, then difference its CDF over the delay grid.
  delay_ppc_draws <- lapply(seq_len(ncol(par_draws)), function(draw_index) {
    parlist_draw <- .split_named_vector(setNames(par_draws[, draw_index], par_names))
    delay_fns <- tryCatch({
      if (family == 4L) {
        # Dirichlet (non-parametric): the simplex is either fixed or rebuilt from
        # this draw's logits.
        n_bins <- as.integer(data$np_model_length)
        simplex <- if (isTRUE(priors$delay_probs$is_constant == 1L)) {
          priors$delay_probs$fixed
        } else {
          exp_logits <- exp(parlist_draw$delay_logits)
          c(exp_logits, 1) / (sum(exp_logits) + 1)
        }
        .nonparametric_delay_functions(simplex, n_bins)
      } else {
        reconstructed <- .joint_reconstruct(data, priors, parlist_draw, fit$Bmat, fit$freq)
        if (family == 3L)
          .delay_distribution_functions(3L, reconstructed$delay_mu,
            .gengamma_shape_transform(parlist_draw$delay_Q %||% -2)$shape_Q,
            reconstructed$delay_sigma)
        else
          .delay_distribution_functions(family, reconstructed$delay_mu, reconstructed$delay_sigma)
      }
    }, error = function(e) NULL)
    if (is.null(delay_fns)) return(NULL)
    pmf <- as.numeric(delay_fns$cdf(delay_grid + 0.5)) -
           as.numeric(delay_fns$cdf(pmax(0, delay_grid - 0.5)))
    data.frame(delay = delay_grid, pmf = pmf, draw = draw_index)
  })
  delay_ppc_df <- do.call(rbind, Filter(Negate(is.null), delay_ppc_draws))

  # Empirical (observed) delay PMF = weighted relative frequency of each delay.
  empirical_pmf <- data.frame(delay = observed_delays, weight = observed_weights) |>
    (\(d) { total <- sum(d$weight); aggregate(weight / total ~ delay, data = d, FUN = sum) })()
  names(empirical_pmf) <- c("delay", "pmf")
  empirical_pmf <- empirical_pmf[empirical_pmf$delay <= max_delay_to_plot, ]

  # 5/50/95% posterior-predictive band of the delay PMF at each delay.
  delay_band <- tapply(delay_ppc_df$pmf, delay_ppc_df$delay, function(pmf_at_delay)
    c(lo = quantile(pmf_at_delay, 0.05), hi = quantile(pmf_at_delay, 0.95),
      med = median(pmf_at_delay)))
  delay_summary <- data.frame(
    delay = as.numeric(names(delay_band)),
    lo    = vapply(delay_band, `[`, numeric(1), "lo"),
    hi    = vapply(delay_band, `[`, numeric(1), "hi"),
    med   = vapply(delay_band, `[`, numeric(1), "med")
  )

  delay_plot <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = delay_summary,
                         ggplot2::aes(x = delay, ymin = lo, ymax = hi), fill = "#fdae61", alpha = 0.4) +
    ggplot2::geom_line(data = delay_summary, ggplot2::aes(x = delay, y = med), colour = "#d73027", linewidth = 1) +
    ggplot2::geom_col(data = empirical_pmf, ggplot2::aes(x = delay, y = pmf), fill = "grey40", alpha = 0.5, width = 0.4) +
    ggplot2::labs(x = "Delay", y = "Probability", title = "Delay PPC (bars = observed; line+ribbon = posterior predictive)") +
    ggplot2::theme_minimal(base_size = 11)

  # -- Panel 2: Count PPC (fully-reported events) ----------------------------
  # An event-time is "fully reported" once its maximum observable delay d* is at
  # least `max_obs_delay` (default the 90th percentile of observed delays), i.e.
  # essentially all of its cases have had time to arrive.
  max_observable_delay_per_event <- as.numeric(data$d_star[, 1])
  max_obs_delay <- if (is.null(min_full_report_delay))
    as.integer(quantile(rep(observed_delays, pmax(1, round(observed_weights))), 0.90, na.rm = TRUE))
  else as.integer(min_full_report_delay)
  fully_reported_events <- which(max_observable_delay_per_event >= max_obs_delay)

  count_ppc_df <- NULL
  count_plot   <- ggplot2::ggplot() + ggplot2::labs(title = "No fully-reported events to check") +
                  ggplot2::theme_minimal(base_size = 11)

  if (length(fully_reported_events) > 0L) {
    observed_total <- rowSums(if (is.matrix(data$case_counts)) data$case_counts
                               else matrix(data$case_counts, n_time, 1))
    # Posterior-predictive total count N_t for each fully-reported event = the
    # cases already observed plus a draw of the (small) still-unreported remainder.
    count_ppc_rows <- lapply(seq_len(ncol(par_draws)), function(draw_index) {
      parlist_draw  <- .split_named_vector(setNames(par_draws[, draw_index], par_names))
      reconstructed <- tryCatch(.joint_reconstruct(data, priors, parlist_draw, fit$Bmat, fit$freq),
                                error = function(e) NULL)
      if (is.null(reconstructed)) return(NULL)
      lambda <- rowSums(matrix(reconstructed$lambda, n_time, data$num_strata))[fully_reported_events]
      Gstar  <- rowSums(matrix(reconstructed$Gstar,  n_time, data$num_strata))[fully_reported_events]
      predicted_unreported <- .epidemic_rng(is_nb, lambda * (1 - Gstar) + 1e-8, reconstructed$phi_nb)
      predicted_total <- observed_total[fully_reported_events] + predicted_unreported
      data.frame(t_idx = fully_reported_events - 1L, pred = predicted_total, draw = draw_index)
    })
    count_ppc_df <- do.call(rbind, Filter(Negate(is.null), count_ppc_rows))
    observed_fully_reported <- data.frame(t_idx = fully_reported_events - 1L,
                                          observed = observed_total[fully_reported_events])

    # 5/50/95% band of the predicted total at each fully-reported event-time.
    count_band <- tapply(count_ppc_df$pred, count_ppc_df$t_idx, function(pred_at_event)
      c(lo = quantile(pred_at_event, 0.05), hi = quantile(pred_at_event, 0.95),
        med = median(pred_at_event)))
    count_summary <- data.frame(
      t_idx = as.integer(names(count_band)),
      lo    = vapply(count_band, `[`, numeric(1), "lo"),
      hi    = vapply(count_band, `[`, numeric(1), "hi"),
      med   = vapply(count_band, `[`, numeric(1), "med")
    )
    count_summary <- merge(count_summary, observed_fully_reported, by = "t_idx")

    count_plot <- ggplot2::ggplot(count_summary, ggplot2::aes(x = t_idx)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), fill = "#4575b4", alpha = 0.3) +
      ggplot2::geom_line(ggplot2::aes(y = med), colour = "#4575b4", linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(y = observed), colour = "black", size = 1.5) +
      ggplot2::labs(x = "Event index", y = "Total cases",
                    title = paste0("Count PPC -- fully-reported events (d* >= ", max_obs_delay, "; black = observed)")) +
      ggplot2::theme_minimal(base_size = 11)
  }

  plots <- list(delay = delay_plot, count = count_plot)
  combined <- if (requireNamespace("patchwork", quietly = TRUE))
    patchwork::wrap_plots(plots, ncol = 1L)
  else plots

  invisible(list(plot = combined, delay_ppc = delay_ppc_df, count_ppc = count_ppc_df,
                 max_obs_delay = max_obs_delay, n_full_events = length(fully_reported_events)))
}
