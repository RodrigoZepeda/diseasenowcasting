# =============================================================================
# nowcast_diagnostic() -- three-panel internal diagnostic plot
# =============================================================================
# Not autoplot() -- the main autoplot(nowcast_class) is the bar-chart style in
# 25_autoplot.R.  This function gives a deeper look: delay histogram + fitted
# density, smoothed lambda, and the predictive nowcast as a line+ribbon.
# Call it explicitly: nowcast_diagnostic(nc, seed = 1)
# =============================================================================

#' Three-panel diagnostic plot for a fitted nowcast
#'
#' Produces three stacked panels: (1) reporting-delay histogram with the
#' fitted density, (2) smoothed latent incidence lambda with 50/90% CI, and (3)
#' posterior-predictive nowcast with observed counts.  Requires the
#' `patchwork` package for a combined output.
#'
#' @param object A `nowcast_class` object.
#' @param n_draws Number of posterior draws (default: min(`object@n_draws`, 500)).
#' @param seed Optional RNG seed.
#' @returns A `patchwork` combined plot (or a named list of three ggplots).
#' @export
nowcast_diagnostic <- function(object, n_draws = NULL, seed = sample.int(.Machine$integer.max, 1)) {
  if (!is.null(seed)) set.seed(seed)
  n_draws <- n_draws %||% min(object@n_draws, 500L)

  fit     <- object@fits[[1]]
  data    <- fit$data
  priors  <- fit$priors
  family  <- data$delay_family
  n_time  <- data$max_time
  observed_total <- rowSums(if (is.matrix(data$case_counts)) data$case_counts
                             else matrix(data$case_counts, n_time, 1))

  # Map each event-index to a calendar event-date (same grid predict() uses), and
  # read the time unit (e.g. "days"/"weeks") from the tbl_now for the axis labels.
  event_dates <- tryCatch({
    min_ev <- object@engine$min_event
    eu     <- object@engine$event_unit
    if (!is.null(min_ev))
      seq(as.Date(min_ev), by = as.character(eu), length.out = n_time) else NULL
  }, error = function(e) NULL)
  use_dates  <- !is.null(event_dates) && length(event_dates) == n_time
  time_unit  <- tryCatch(tbl.now::get_event_units(object@data), error = function(e) NULL) %||%
    "days/weeks"
  event_axis <- if (use_dates) "Event date" else "Event index"

  # -- PANEL 1: Delay distribution ------------------------------------------
  # Observed delays (column 3) and their case weights (column 2), trimmed to a
  # high quantile so the long tail does not stretch the axis (cap at 60).
  observed_delays  <- data$m[, 3]
  observed_weights <- data$m[, 2]
  max_delay_plot <- min(quantile(rep(observed_delays, times = pmax(1, round(observed_weights))),
                                 0.995, na.rm = TRUE) + 2, 60)
  hist_df <- data.frame(delay = observed_delays, weight = observed_weights) |>
    (\(d) d[d$delay <= max_delay_plot, ])()

  reconstructed     <- fit$reconstruct
  is_nonparametric  <- family == 4L
  # Fitted delay DENSITY (discretised pmf): the probability mass in a unit-width
  # window centred at each delay, directly comparable to the proportion histogram.
  delay_density_df <- tryCatch({
    delay_grid <- seq(0, max_delay_plot, by = 0.2)
    delay_fns <- if (is_nonparametric) {
      n_bins  <- as.integer(data$np_model_length)
      simplex <- if (isTRUE(priors$delay_probs$is_constant == 1L)) {
        priors$delay_probs$fixed
      } else {
        exp_logits <- exp(fit$parList$delay_logits)
        c(exp_logits, 1) / (sum(exp_logits) + 1)
      }
      .nonparametric_delay_functions(simplex, n_bins)
    } else if (family == 5L) {
      priors$cdf_factory(as.numeric(fit$parList$custom_delay_params))
    } else if (family == 3L) {
      .delay_distribution_functions(3L, reconstructed$delay_mu,
        .gengamma_shape_transform(fit$parList$delay_Q %||% -2)$shape_Q, reconstructed$delay_sigma)
    } else {
      .delay_distribution_functions(family, reconstructed$delay_mu, reconstructed$delay_sigma)
    }
    pmf <- as.numeric(delay_fns$cdf(delay_grid + 0.5)) -
           as.numeric(delay_fns$cdf(pmax(0, delay_grid - 0.5)))
    data.frame(delay = delay_grid, density = pmax(0, pmf))
  }, error = function(e) NULL)

  pal <- dn_palette()

  p1 <- ggplot2::ggplot() +
    ggplot2::geom_bar(data = hist_df,
                      ggplot2::aes(x = delay, weight = weight / sum(weight)),
                      fill = pal["predicted"], colour = "white", alpha = 0.8, width = 0.8) +
    ggplot2::labs(x = paste0("Reporting delay (", time_unit, ")"), y = "Density",
                  title = "Reporting-delay distribution")
  if (!is.null(delay_density_df))
    p1 <- p1 + ggplot2::geom_line(data = delay_density_df,
                                   ggplot2::aes(x = delay, y = density),
                                   colour = pal["accent"], linewidth = 1.2)
  p1 <- p1 + theme_diseasenowcasting()

  # -- PANEL 2: Smoothed epidemic (lambda) -----------------------------------
  lambda_draws <- .nowcast_lambda_draws(object, n_draws = n_draws, seed = seed)
  lambda_sum <- data.frame(
    t      = seq_len(n_time) - 1L,
    median = apply(lambda_draws, 2, stats::median, na.rm = TRUE),
    q5     = apply(lambda_draws, 2, stats::quantile, probs = 0.05,  na.rm = TRUE),
    q25    = apply(lambda_draws, 2, stats::quantile, probs = 0.25,  na.rm = TRUE),
    q75    = apply(lambda_draws, 2, stats::quantile, probs = 0.75,  na.rm = TRUE),
    q95    = apply(lambda_draws, 2, stats::quantile, probs = 0.95,  na.rm = TRUE)
  )
  lambda_sum$x <- if (use_dates) event_dates else lambda_sum$t
  p2 <- ggplot2::ggplot(lambda_sum, ggplot2::aes(x = x)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q5,  ymax = q95),
                         fill = pal["reported"], alpha = 0.15) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q25, ymax = q75),
                         fill = pal["reported"], alpha = 0.30) +
    ggplot2::geom_line(ggplot2::aes(y = median),
                       colour = pal["reported"], linewidth = 1) +
    ggplot2::labs(x = event_axis, y = expression(lambda[t]),
                  title = "Smoothed epidemic process") +
    theme_diseasenowcasting()

  # -- PANEL 3: Posterior-predictive nowcast ---------------------------------
  draw_result <- .nowcast_draws(fit, target = n_time, n_draws = n_draws, seed = seed)
  nc_sum <- draw_result$nowcast
  obs_df <- data.frame(t = seq_len(n_time) - 1L, observed = observed_total)
  if (use_dates) {
    nc_sum$x <- event_dates[nc_sum$.event_num + 1L]
    obs_df$x <- event_dates[obs_df$t + 1L]
  } else {
    nc_sum$x <- nc_sum$.event_num
    obs_df$x <- obs_df$t
  }

  p3 <- ggplot2::ggplot(nc_sum, ggplot2::aes(x = x)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q2.5, ymax = q97.5),
                         fill = pal["reported"], alpha = 0.15) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q25,  ymax = q75),
                         fill = pal["reported"], alpha = 0.30) +
    ggplot2::geom_line(ggplot2::aes(y = median),
                       colour = pal["reported"], linewidth = 1) +
    ggplot2::geom_point(data = obs_df, ggplot2::aes(x = x, y = observed),
                        colour = pal["dark"], size = 1.2) +
    ggplot2::labs(x = event_axis, y = "Cases",
                  title = "Nowcast (black = observed)") +
    theme_diseasenowcasting()

  plots <- list(delay = p1, lambda = p2, nowcast = p3)
  if (requireNamespace("patchwork", quietly = TRUE)) {
    patchwork::wrap_plots(plots, ncol = 1L)
  } else {
    cli::cli_inform(c("i" = "Install {.pkg patchwork} for a combined plot.",
                      "i" = "Returning a named list of three ggplot objects instead."))
    plots
  }
}
