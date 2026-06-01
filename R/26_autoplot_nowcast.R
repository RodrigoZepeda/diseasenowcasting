# =============================================================================
# nowcast_diagnostic() -- three-panel internal diagnostic plot
# =============================================================================
# Not autoplot() -- the main autoplot(nowcast_class) is the bar-chart style in
# 25_autoplot.R.  This function gives a deeper look: delay histogram + fitted
# CDF, smoothed lambda, and the predictive nowcast as a line+ribbon.
# Call it explicitly: nowcast_diagnostic(nc, seed = 1)
# =============================================================================

#' Three-panel diagnostic plot for a fitted nowcast
#'
#' Produces three stacked panels: (1) reporting-delay histogram with the
#' fitted CDF, (2) smoothed latent incidence lambda with 50/90% CI, and (3)
#' posterior-predictive nowcast with observed counts.  Requires the
#' `patchwork` package for a combined output.
#'
#' @param object A `nowcast_class` object.
#' @param n_draws Number of posterior draws (default: min(`object@n_draws`, 500)).
#' @param seed Optional RNG seed.
#' @returns A `patchwork` combined plot (or a named list of three ggplots).
#' @export
nowcast_diagnostic <- function(object, n_draws = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n_draws <- n_draws %||% min(object@n_draws, 500L)

  fit     <- object@fits[[1]]
  data    <- fit$data
  priors  <- fit$priors
  family  <- data$delay_family
  n_time  <- data$max_time
  observed_total <- rowSums(if (is.matrix(data$case_counts)) data$case_counts
                             else matrix(data$case_counts, n_time, 1))

  # -- PANEL 1: Delay distribution ------------------------------------------
  delay_vals <- data$m[, 3]
  delay_wts  <- data$m[, 2]
  max_delay_plot <- min(quantile(rep(delay_vals, times = pmax(1, round(delay_wts))),
                                 0.995, na.rm = TRUE) + 2, 60)
  hist_df <- data.frame(delay = delay_vals, weight = delay_wts) |>
    (\(d) d[d$delay <= max_delay_plot, ])()

  rc   <- fit$reconstruct
  is_np <- family == 4L
  delay_cdf_df <- tryCatch({
    d_seq <- seq(0, max_delay_plot, by = 0.2)
    map_cdf <- if (is_np) {
      n_bins <- as.integer(data$np_model_length)
      pl <- fit$parList
      simplex <- if (isTRUE(priors$delay_probs$is_constant == 1L)) priors$delay_probs$fixed
                 else { el <- exp(pl$delay_logits); c(el, 1) / (sum(el) + 1) }
      fns <- .nonparametric_delay_functions(simplex, n_bins); fns$cdf(d_seq)
    } else {
      fns <- if (family == 3L)
        .delay_distribution_functions(3L, rc$delay_mu,
          .gengamma_shape_transform(fit$parList$delay_Q %||% -2)$shape_Q, rc$delay_sigma)
        else .delay_distribution_functions(family, rc$delay_mu, rc$delay_sigma)
      fns$cdf(d_seq)
    }
    data.frame(delay = d_seq, cdf = as.numeric(map_cdf))
  }, error = function(e) NULL)

  pal <- dn_palette()

  p1 <- ggplot2::ggplot() +
    ggplot2::geom_bar(data = hist_df,
                      ggplot2::aes(x = delay, weight = weight / sum(weight)),
                      fill = pal["predicted"], colour = "white", alpha = 0.8, width = 0.8) +
    ggplot2::labs(x = "Reporting delay (days/weeks)", y = "Proportion / CDF",
                  title = "Reporting-delay distribution")
  if (!is.null(delay_cdf_df))
    p1 <- p1 + ggplot2::geom_line(data = delay_cdf_df,
                                   ggplot2::aes(x = delay, y = cdf),
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
  p2 <- ggplot2::ggplot(lambda_sum, ggplot2::aes(x = t)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q5,  ymax = q95),
                         fill = pal["reported"], alpha = 0.15) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q25, ymax = q75),
                         fill = pal["reported"], alpha = 0.30) +
    ggplot2::geom_line(ggplot2::aes(y = median),
                       colour = pal["reported"], linewidth = 1) +
    ggplot2::labs(x = "Event index", y = expression(lambda[t]),
                  title = "Smoothed latent incidence (lambda)") +
    theme_diseasenowcasting()

  # -- PANEL 3: Posterior-predictive nowcast ---------------------------------
  draw_result <- .nowcast_draws(fit, target = n_time, n_draws = n_draws, seed = seed)
  nc_sum <- draw_result$nowcast
  obs_df <- data.frame(t = seq_len(n_time) - 1L, observed = observed_total)

  p3 <- ggplot2::ggplot(nc_sum, ggplot2::aes(x = .event_num)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q2.5, ymax = q97.5),
                         fill = pal["reported"], alpha = 0.15) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q25,  ymax = q75),
                         fill = pal["reported"], alpha = 0.30) +
    ggplot2::geom_line(ggplot2::aes(y = median),
                       colour = pal["reported"], linewidth = 1) +
    ggplot2::geom_point(data = obs_df, ggplot2::aes(x = t, y = observed),
                        colour = pal["dark"], size = 1.2) +
    ggplot2::labs(x = "Event index", y = "Cases",
                  title = "Posterior-predictive nowcast (black = observed)") +
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
