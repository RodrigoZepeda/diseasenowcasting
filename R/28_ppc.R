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
  obj       <- fit$obj
  mode_vec  <- obj$env$last.par.best
  prec_mat  <- methods::as(obj$he(mode_vec), "sparseMatrix")
  par_draws <- tryCatch({
    .sample_mvnorm_precision(as.numeric(mode_vec), prec_mat, n_draws)
  }, error = function(e) NULL)
  if (is.null(par_draws)) {
    cli::cli_warn("Could not sample from posterior precision -- using MAP for PPC.")
    par_draws <- matrix(as.numeric(mode_vec), ncol = 1L)
  }
  par_names <- names(mode_vec)

  # -- Panel 1: Delay PPC ----------------------------------------------------
  obs_delays <- as.integer(data$m[, 3])
  obs_wts    <- as.numeric(data$m[, 2])
  max_d_plot <- min(as.integer(quantile(rep(obs_delays, times = pmax(1L, round(obs_wts))),
                                         0.995, na.rm = TRUE)) + 2L, 60L)
  d_seq      <- seq(0, max_d_plot, by = 0.5)

  # Posterior-predictive delay PMF for each draw
  delay_ppc_draws <- lapply(seq_len(ncol(par_draws)), function(i) {
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
    if (is.null(fns)) return(NULL)
    cdf_vals <- as.numeric(fns$cdf(d_seq + 0.5)) - as.numeric(fns$cdf(pmax(0, d_seq - 0.5)))
    data.frame(delay = d_seq, pmf = cdf_vals, draw = i)
  })
  delay_ppc_df <- do.call(rbind, Filter(Negate(is.null), delay_ppc_draws))
  # Empirical PMF
  emp_df <- data.frame(delay = obs_delays, weight = obs_wts) |>
    (\(d) { tot <- sum(d$weight); aggregate(weight/tot ~ delay, data = d, FUN = sum) })()
  names(emp_df) <- c("delay", "pmf")
  emp_df <- emp_df[emp_df$delay <= max_d_plot, ]

  delay_sum_raw <- tapply(delay_ppc_df$pmf, delay_ppc_df$delay, function(v)
    c(lo = quantile(v, 0.05), hi = quantile(v, 0.95), med = median(v)))
  delay_sum <- data.frame(
    delay = as.numeric(names(delay_sum_raw)),
    lo    = vapply(delay_sum_raw, `[`, numeric(1), "lo"),
    hi    = vapply(delay_sum_raw, `[`, numeric(1), "hi"),
    med   = vapply(delay_sum_raw, `[`, numeric(1), "med")
  )

  p_delay <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = delay_sum,
                         ggplot2::aes(x = delay, ymin = lo, ymax = hi), fill = "#fdae61", alpha = 0.4) +
    ggplot2::geom_line(data = delay_sum, ggplot2::aes(x = delay, y = med), colour = "#d73027", linewidth = 1) +
    ggplot2::geom_col(data = emp_df, ggplot2::aes(x = delay, y = pmf), fill = "grey40", alpha = 0.5, width = 0.4) +
    ggplot2::labs(x = "Delay", y = "Probability", title = "Delay PPC (bars = observed; line+ribbon = posterior predictive)") +
    ggplot2::theme_minimal(base_size = 11)

  # -- Panel 2: Count PPC (fully-reported events) ----------------------------
  d_star_vec  <- as.numeric(data$d_star[, 1])    # max observable delay per event-time
  max_obs_delay <- if (is.null(min_full_report_delay))
    as.integer(quantile(rep(obs_delays, pmax(1, round(obs_wts))), 0.90, na.rm = TRUE))
  else as.integer(min_full_report_delay)
  full_idx    <- which(d_star_vec >= max_obs_delay)   # event-times considered fully reported

  count_ppc_df <- NULL
  p_count      <- ggplot2::ggplot() + ggplot2::labs(title = "No fully-reported events to check") +
                  ggplot2::theme_minimal(base_size = 11)

  if (length(full_idx) > 0L) {
    observed_total <- rowSums(if (is.matrix(data$case_counts)) data$case_counts
                               else matrix(data$case_counts, n_time, 1))
    # Posterior predictive N_t for fully-reported events
    ppc_rows <- lapply(seq_len(ncol(par_draws)), function(i) {
      pl  <- .split_named_vector(setNames(par_draws[, i], par_names))
      rc  <- tryCatch(.joint_reconstruct(data, priors, pl, fit$Bmat, fit$freq), error = function(e) NULL)
      if (is.null(rc)) return(NULL)
      lambda  <- rowSums(matrix(rc$lambda, n_time, data$num_strata))[full_idx]
      Gstar   <- rowSums(matrix(rc$Gstar,  n_time, data$num_strata))[full_idx]
      phi_nb  <- rc$phi_nb
      pred_unreported <- .epidemic_rng(is_nb, lambda * (1 - Gstar) + 1e-8, phi_nb)
      pred_total <- observed_total[full_idx] + pred_unreported
      data.frame(t_idx = full_idx - 1L, pred = pred_total, draw = i)
    })
    count_ppc_df <- do.call(rbind, Filter(Negate(is.null), ppc_rows))
    obs_full  <- data.frame(t_idx = full_idx - 1L, observed = observed_total[full_idx])

    cnt_sum_raw <- tapply(count_ppc_df$pred, count_ppc_df$t_idx, function(v)
      c(lo = quantile(v, 0.05), hi = quantile(v, 0.95), med = median(v)))
    cnt_sum_df <- data.frame(
      t_idx = as.integer(names(cnt_sum_raw)),
      lo    = vapply(cnt_sum_raw, `[`, numeric(1), "lo"),
      hi    = vapply(cnt_sum_raw, `[`, numeric(1), "hi"),
      med   = vapply(cnt_sum_raw, `[`, numeric(1), "med")
    )
    cnt_sum_df <- merge(cnt_sum_df, obs_full, by = "t_idx")

    p_count <- ggplot2::ggplot(cnt_sum_df, ggplot2::aes(x = t_idx)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), fill = "#4575b4", alpha = 0.3) +
      ggplot2::geom_line(ggplot2::aes(y = med), colour = "#4575b4", linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(y = observed), colour = "black", size = 1.5) +
      ggplot2::labs(x = "Event index", y = "Total cases",
                    title = paste0("Count PPC -- fully-reported events (d* >= ", max_obs_delay, "; black = observed)")) +
      ggplot2::theme_minimal(base_size = 11)
  }

  plots <- list(delay = p_delay, count = p_count)
  combined <- if (requireNamespace("patchwork", quietly = TRUE))
    patchwork::wrap_plots(plots, ncol = 1L)
  else plots

  invisible(list(plot = combined, delay_ppc = delay_ppc_df, count_ppc = count_ppc_df,
                 max_obs_delay = max_obs_delay, n_full_events = length(full_idx)))
}
