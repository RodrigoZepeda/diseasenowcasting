# =============================================================================
# Colour palette + autoplot() methods
# =============================================================================
# Colour palette derived from the diseasenowcasting hex-sticker logo:
#   Primary green  #5F7E62  (reported bars, main epidemic line)
#   Dark charcoal  #262626  (navbar, background accents)
#   Burnt orange   #DA6529  (accent, SIR, surprise)
#   Heading green  #334335  (axis titles, headings)
#   Light sage     #A8BFA9  (predicted / not-yet-reported bars)
#   Mid sage       #7A9E7E  (AR1 model)
#   Pale orange    #E8956A  (lighter accent)
#   Slate grey     #607060  (secondary model lines)
#
# Extended palette for multi-model plots (backtest comparisons):
#   These extend the green-charcoal-orange family naturally.
# =============================================================================

# ── Exported colour palette ───────────────────────────────────────────────────

#' diseasenowcasting colour palette
#'
#' Returns hex colour codes matching the package's visual identity (derived
#' from the hex-sticker logo: dark charcoal background, sage green, burnt
#' orange accent).
#'
#' @param n Number of colours to return (1-8). If `NULL`, returns all 8.
#' @returns A named character vector of hex colour codes.
#' @export
dn_palette <- function(n = NULL) {
  pal <- c(
    reported       = "#5F7E62",   # dark sage green  -- reported bars / main model
    predicted      = "#A8BFA9",   # light sage        -- predicted/unreported bars
    accent         = "#DA6529",   # burnt orange      -- SIR, surprise, accent
    dark           = "#262626",   # charcoal          -- backgrounds, text
    heading        = "#334335",   # deep green        -- headings, axes
    mid            = "#7A9E7E",   # mid sage          -- AR1 / second model
    pale_accent    = "#E8956A",   # pale orange       -- third model / highlight
    slate          = "#607060"    # slate grey-green  -- fourth model
  )
  if (is.null(n)) return(pal)
  if (n > length(pal)) {
    extra <- grDevices::colorRampPalette(c("#5F7E62", "#DA6529", "#262626"))(n - length(pal))
    return(c(pal, stats::setNames(extra, paste0("extra", seq_along(extra)))))
  }
  pal[seq_len(n)]
}

# ── Theme ─────────────────────────────────────────────────────────────────────

#' ggplot2 theme matching the diseasenowcasting visual identity
#' @param base_size Base font size.
#' @export
theme_diseasenowcasting <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      axis.text.x          = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position      = "top",
      legend.justification = "right",
      legend.direction     = "horizontal",
      axis.title           = ggplot2::element_text(colour = "#334335"),
      strip.text           = ggplot2::element_text(colour = "#334335", face = "bold"),
      panel.grid.minor     = ggplot2::element_blank()
    )
}

# ── autoplot re-export ────────────────────────────────────────────────────────

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

# ── autoplot(nowcast_prediction_class) ───────────────────────────────────────
# Bar-chart style matching the original diseasenowcasting plot():
#   Dark-green bars  = already-reported counts (observed so far)
#   Light-sage bars  = predicted-but-not-yet-reported (median nowcast - observed)
#   Error bars       = 5%-95% posterior interval of the TOTAL nowcast
# x-axis: dates if `event_dates` supplied, else 0-indexed integers.
# Strata: if the underlying fit had >1 stratum, facet_wrap by stratum name.

#' Plot a posterior-predictive nowcast (bar-chart style)
#'
#' Replicates the original `diseasenowcasting` plot style: dark-green bars for
#' already-reported cases, light sage bars for model-predicted-but-not-yet-
#' reported cases, and error bars for the 5%-95% credible interval.
#'
#' @param object A `nowcast_prediction_class` from [predict()].
#' @param event_dates Optional `Date` vector of length `max_time` mapping event
#'   index to calendar date.  If `NULL`, integers (0, 1, ...) are used.
#' @param strata_names Optional character vector of stratum labels (length =
#'   number of strata).  Only used when the fit was stratified.
#' @param strata_draws Optional `[n_draws x max_time x n_strata]` array from
#'   `.nowcast_draws()$M_strata`.  When supplied the per-stratum breakdown is
#'   shown.  If `NULL`, the total (summed over strata) is plotted.
#' @param observed_strata Optional `[max_time x n_strata]` matrix of observed
#'   counts per stratum.
#' @param quantiles Length-2 numeric vector for the error-bar quantiles.
#'   Default `c(0.05, 0.95)`.
#' @param color Primary colour for the bars (default `"#5F7E62"`).
#' @param date_breaks Passed to `scale_x_date(date_breaks = ...)`.  E.g.
#'   `"1 month"`.  Only used when `event_dates` is supplied.
#' @param title Optional plot title.
#' @param ... Unused.
#' @returns A `ggplot` object.
#' @noRd
S7::method(autoplot, nowcast_prediction_class) <- function(
    object, event_dates = NULL, strata_names = NULL,
    strata_draws = NULL, observed_strata = NULL,
    quantiles = c(0.05, 0.95), color = "#5F7E62",
    date_breaks = NULL, title = NULL, ...) {

  draws   <- object@draws                    # [n_draws x max_time] total
  n_time  <- ncol(draws)
  n_draws <- nrow(draws)
  obs_total <- object@observed               # scalar (target event only)

  quantile_label <- paste0(
    round(quantiles[1] * 100), "% - ", round(quantiles[2] * 100), "% interval"
  )

  # ── Build per-stratum data if available, else use totals ───────────────────
  if (!is.null(strata_draws) && !is.null(observed_strata)) {
    n_strata <- dim(strata_draws)[3]
    if (is.null(strata_names)) strata_names <- paste0("Stratum ", seq_len(n_strata))
    plot_list <- lapply(seq_len(n_strata), function(s) {
      obs_s  <- as.numeric(observed_strata[, s])
      draw_s <- strata_draws[, , s, drop = TRUE]   # [n_draws x max_time]
      .make_nowcast_bar_df(draw_s, obs_s, quantiles, strata_names[s])
    })
    plot_df <- do.call(rbind, plot_list)
    has_strata <- n_strata > 1L
  } else {
    # Reconstruct approximate observed from object@observed (only target event)
    # Fall back to showing just the total nowcast without stratum split
    obs_vec <- rep(0, n_time)
    obs_vec[object@target] <- obs_total
    plot_df    <- .make_nowcast_bar_df(draws, obs_vec, quantiles, "Total")
    has_strata <- FALSE
  }

  # ── x-axis: dates or integer index ─────────────────────────────────────────
  if (!is.null(event_dates) && length(event_dates) >= n_time) {
    date_vec <- as.Date(event_dates[seq_len(n_time)])
    plot_df$x_val <- rep(date_vec, length.out = nrow(plot_df))
    use_dates <- TRUE
  } else {
    plot_df$x_val <- plot_df$event_index
    use_dates <- FALSE
  }

  # ── Build plot ─────────────────────────────────────────────────────────────
  col_predicted <- "#A8BFA9"   # light sage for predicted-unreported

  p <- ggplot2::ggplot(plot_df) +
    ggplot2::geom_bar(
      ggplot2::aes(x = .data$x_val, y = .data$reported,
                   fill = "Reported"),
      stat = "identity", colour = NA, alpha = 0.8
    ) +
    ggplot2::geom_bar(
      ggplot2::aes(x = .data$x_val, y = .data$predicted_extra,
                   fill = "Predicted, not yet reported"),
      stat = "identity", colour = NA, alpha = 0.45
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(x = .data$x_val, ymin = .data$q_lo, ymax = .data$q_hi),
      width = 0, colour = "gray40"
    ) +
    ggplot2::scale_fill_manual(
      name   = NULL,
      values = c("Reported" = color,
                 "Predicted, not yet reported" = color),
      labels = c("Reported",
                 paste0("Predicted, not yet reported\nError bars: ", quantile_label))
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        override.aes = list(alpha = c(0.8, 0.45), colour = NA)
      )
    ) +
    ggplot2::labs(y = "Cases", title = title) +
    theme_diseasenowcasting()

  # ── x-axis scale ───────────────────────────────────────────────────────────
  if (use_dates) {
    if (!is.null(date_breaks)) {
      p <- p + ggplot2::scale_x_date(date_labels = "%Y-%b-%d",
                                     minor_breaks = NULL,
                                     date_breaks = date_breaks)
    } else {
      p <- p + ggplot2::scale_x_date(date_labels = "%Y-%b-%d",
                                     minor_breaks = NULL)
    }
    p <- p + ggplot2::labs(x = "Event date")
  } else {
    p <- p + ggplot2::labs(x = "Event index")
  }

  # ── y-axis duplicate ───────────────────────────────────────────────────────
  if (requireNamespace("scales", quietly = TRUE)) {
    p <- p + ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 1),
      sec.axis = ggplot2::dup_axis(name = "Cases")
    )
  }

  # ── Facets for strata ──────────────────────────────────────────────────────
  if (has_strata) {
    p <- p + ggplot2::facet_wrap(~ stratum, scales = "free_y")
  } else {
    p <- p + ggplot2::theme(strip.text = ggplot2::element_blank())
  }

  p
}

#' Build bar-chart data frame for one stratum
#' @keywords internal
#' @noRd
.make_nowcast_bar_df <- function(draws_mat, obs_vec, quantiles, stratum_name) {
  n_time <- ncol(draws_mat)
  total_q_lo  <- apply(draws_mat, 2, stats::quantile, probs = quantiles[1], na.rm = TRUE)
  total_q_hi  <- apply(draws_mat, 2, stats::quantile, probs = quantiles[2], na.rm = TRUE)
  total_median <- apply(draws_mat, 2, stats::median, na.rm = TRUE)
  data.frame(
    event_index    = seq_len(n_time) - 1L,
    stratum        = stratum_name,
    reported       = pmax(0, obs_vec),
    predicted_extra = pmax(0, total_median - obs_vec),
    q_lo           = pmax(0, total_q_lo),
    q_hi           = pmax(0, total_q_hi),
    stringsAsFactors = FALSE
  )
}

# ── autoplot(nowcast_class) ───────────────────────────────────────────────────
# The full three-panel diagnostic from 26_autoplot_nowcast.R is still available.
# This convenience wrapper on the nowcast_class itself calls predict() internally
# and passes event dates + strata info from the engine.

#' Diagnostic autoplot for a fitted nowcast (bar-chart style)
#'
#' Convenience wrapper: calls `predict()` internally and passes the event
#' dates and strata from the underlying data so the resulting plot uses the
#' correct calendar dates and, for stratified fits, shows one panel per
#' stratum.
#'
#' @param object A [nowcast_class] object.
#' @param n_draws Number of posterior draws.
#' @param quantiles Length-2 quantile vector for error bars (default `c(0.05, 0.95)`).
#' @param color Bar fill colour (default `"#5F7E62"`).
#' @param date_breaks Passed to `scale_x_date()`, e.g. `"1 month"`.
#' @param title Optional title string.
#' @param seed RNG seed.
#' @param ... Unused.
#' @returns A `ggplot` object.
#' @noRd
S7::method(autoplot, nowcast_class) <- function(object, n_draws = NULL,
                                                 quantiles = c(0.05, 0.95),
                                                 color = "#5F7E62",
                                                 date_breaks = NULL,
                                                 title = NULL,
                                                 seed = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)
  n_draws <- n_draws %||% min(object@n_draws, 500L)
  fit     <- object@fits[[1]]
  data    <- fit$data
  n_time  <- data$max_time
  n_strata <- as.integer(data$num_strata %||% 1L)

  # ── Event dates from the tbl_now if available ─────────────────────────────
  event_dates <- tryCatch({
    min_event  <- object@engine$min_event %||% NULL
    event_unit <- object@engine$event_unit %||% "day"
    if (!is.null(min_event)) {
      seq(as.Date(min_event), by = event_unit, length.out = n_time)
    } else NULL
  }, error = function(e) NULL)

  # Fall back: try to get dates from the tbl_now data stored in nowcast
  if (is.null(event_dates)) {
    event_dates <- tryCatch({
      tn  <- object@data
      ev  <- tbl.now::get_event_date(tn)
      eu  <- tbl.now::get_event_units(tn)
      mn  <- min(tn[[ev]], na.rm = TRUE)
      seq(as.Date(mn), by = as.character(eu), length.out = n_time)
    }, error = function(e) NULL)
  }

  # ── Per-stratum draws and observed counts ─────────────────────────────────
  draws_out <- .nowcast_draws(fit, target = n_time, n_draws = n_draws, seed = seed)
  cc_mat    <- if (is.matrix(data$case_counts)) data$case_counts
               else matrix(data$case_counts, n_time, n_strata)

  # Strata names
  strata_names <- tryCatch({
    object@engine$strata_levels %||% paste0("Stratum ", seq_len(n_strata))
  }, error = function(e) paste0("Stratum ", seq_len(n_strata)))

  # Build a prediction_class for the convenience dispatch
  observed_total <- rowSums(cc_mat)
  obs_total_target <- observed_total[n_time]
  pred_obj <- nowcast_prediction_class(
    draws       = draws_out$M,
    target      = n_time,
    observed    = obs_total_target,
    event_index = seq_len(n_time) - 1L
  )

  autoplot(pred_obj,
           event_dates    = event_dates,
           strata_names   = strata_names,
           strata_draws   = if (n_strata > 1L) draws_out$M_strata else NULL,
           observed_strata = if (n_strata > 1L) cc_mat else NULL,
           quantiles   = quantiles,
           color       = color,
           date_breaks = date_breaks,
           title       = title)
}

# ── autoplot(backtest_class) ─────────────────────────────────────────────────
#' Plot a backtest: nowcast vs eventual truth across as-of dates, by model.
#'
#' @param object A `backtest_class`.
#' @param color_palette Named or unnamed character vector of hex colors.  If
#'   `NULL`, `dn_palette()` is used.
#' @param ... Unused.
#' @returns A `ggplot` object.
#' @noRd
S7::method(autoplot, backtest_class) <- function(object,
                                                  color_palette = NULL, ...) {
  results <- object@results
  if (is.null(results) || nrow(results) == 0)
    cli::cli_abort("Backtest has no results to plot.")
  results <- results[is.finite(results$final), , drop = FALSE]

  # d*=0 row (newest event) per (model, date_run)
  newest <- do.call(rbind, by(results, list(results$model, results$date_run), function(block) {
    block[which.max(block$.event_num), , drop = FALSE]
  }))
  newest$date_run <- as.Date(newest$date_run)

  models <- unique(newest$model)
  n_mod  <- length(models)
  if (is.null(color_palette)) {
    pal <- unname(dn_palette(n_mod))
  } else {
    pal <- rep_len(color_palette, n_mod)
  }
  color_map <- stats::setNames(pal, models)

  ggplot2::ggplot(newest, ggplot2::aes(x = .data$date_run)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$q2.5, ymax = .data$q97.5,
                                      fill = .data$model), alpha = 0.15) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$q25,  ymax = .data$q75,
                                      fill = .data$model), alpha = 0.30) +
    ggplot2::geom_line(ggplot2::aes(y = .data$q50, colour = .data$model),
                       linewidth = 0.9) +
    ggplot2::geom_point(ggplot2::aes(y = .data$final), colour = "#262626",
                        size = 1.5, shape = 16) +
    ggplot2::scale_colour_manual(values = color_map) +
    ggplot2::scale_fill_manual(values = color_map) +
    ggplot2::labs(x = "as-of date",
                  y = "Nowcast (d* = 0)",
                  colour = "Model", fill = "Model",
                  title = "Backtest: nowcast vs eventual truth (black points)") +
    theme_diseasenowcasting()
}
