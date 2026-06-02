# =============================================================================
# Methods for the nowcast S7 object
# =============================================================================
# coef()                  -> fitted parameter point estimates
# predict()               -> posterior-PREDICTIVE nowcast (draws, lazy)
# summary(predict(.))     -> mean/median/sd/quantiles of the predictions
# mean()/median()/quantile() -> posterior summaries of the LATENT incidence (lambda)
# summary(nowcast)        -> fit overview (params + latent-incidence summary)
# =============================================================================

#' @keywords internal
#' @noRd
.nowcast_lambda_draws <- function(object, n_draws = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n_draws <- n_draws %||% object@n_draws
  .pool_fit_draws(object@fits, object@target, n_draws = n_draws)$lambda
}

#' Parameter point estimates of a fitted nowcast
#'
#' Returns the fitted delay parameters, the NB overdispersion (if any), and the
#' epidemic-process hyperparameters from a representative fit.  For two-stage
#' fits the delay is summarised across the imputations (its posterior mean).
#' @param object A `nowcast_class` object.
#' @param ... Unused.
#' @returns A named numeric vector of parameter estimates.
#' @noRd
S7::method(coef, nowcast_class) <- function(object, ...) {
  representative <- object@fits[[1]]
  delay_mu    <- mean(vapply(object@fits, function(f) f$delay_mu %||% NA_real_, numeric(1)))
  delay_sigma <- mean(vapply(object@fits, function(f) f$delay_sigma %||% NA_real_, numeric(1)))
  out <- c(delay_mu = delay_mu, delay_sigma = delay_sigma)
  if (!is.na(representative$phi_nb %||% NA_real_)) out["phi_nb"] <- representative$phi_nb
  pl <- representative$parList
  for (nm in intersect(c("mu_intercept", "log_gp_alpha", "log_gp_ell", "ar_phi_unc",
                         "log_ar_sigma_unc", "log_R0", "u_gamma", "u_neff"), names(pl)))
    out[nm] <- pl[[nm]][1]
  out
}

#' Posterior-predictive nowcast from a fitted nowcast (lazy)
#'
#' Samples the Laplace posterior and draws the complete posterior-predictive
#' count at every event time (pooled over the two-stage imputations).
#' @param object A `nowcast_class` object.
#' @param n_draws Posterior draws (per fit); defaults to the object's `n_draws`.
#' @param summary If TRUE, return the [summarise_nowcast_matrix()] table directly
#'   instead of a `nowcast_prediction_class` object.  For a stratified fit the
#'   summary gains a `stratum` column (one block per stratum, plus a `"Total"`).
#' @param seed Optional RNG seed.
#' @param ... Unused.
#' @returns A `nowcast_prediction_class` object (or a summary data.frame).
#' @noRd
S7::method(predict, nowcast_class) <- function(object, n_draws = NULL,
                                                      summary = FALSE, seed = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)
  n_draws <- n_draws %||% object@n_draws
  per_fit <- max(1L, ceiling(n_draws / length(object@fits)))
  pooled  <- .pool_fit_draws(object@fits, object@target, n_draws = per_fit)
  draws_matrix <- pooled$M

  # Event dates + strata labels (when available)
  n_time      <- ncol(draws_matrix)
  strata_lvls <- object@engine$strata_levels %||% NULL
  event_dates <- tryCatch({
    min_ev <- object@engine$min_event; eu <- object@engine$event_unit
    if (!is.null(min_ev)) seq(as.Date(min_ev), by = as.character(eu), length.out = n_time) else NULL
  }, error = function(e) NULL)

  cc <- object@engine$case_counts
  cc_mat <- if (is.matrix(cc)) cc else matrix(cc, n_time, 1L)
  pred <- nowcast_prediction_class(
    draws        = draws_matrix, target = object@target,
    observed     = rowSums(cc_mat)[object@target],
    event_index  = seq_len(n_time) - 1L,
    strata_draws = pooled$M_strata,
    strata_levels = strata_lvls,
    event_dates  = event_dates,
    observed_series = rowSums(cc_mat),
    observed_strata = if (ncol(cc_mat) > 1L) cc_mat else NULL
  )
  if (summary) return(summary(pred))
  pred
}

#' Summary of a nowcast prediction: mean/median/sd/quantiles per event
#'
#' For a stratified prediction the result has a `stratum` column with one block
#' of rows per stratum followed by a `"Total"` block (summed over strata).
#' @param object A `nowcast_prediction_class`.
#' @param ... Unused.
#' @returns A data.frame (one row per event time, or per event-time x stratum).
#' @noRd
S7::method(summary, nowcast_prediction_class) <- function(object, ...) {
  total_tab <- summarise_nowcast_matrix(object@draws)
  if (is.null(object@strata_draws)) {
    if (!is.null(object@event_dates)) total_tab$event_date <- object@event_dates
    return(total_tab)
  }
  # Per-stratum blocks + total
  n_strata <- dim(object@strata_draws)[3]
  lvls <- object@strata_levels %||% paste0("Stratum ", seq_len(n_strata))
  per_stratum <- lapply(seq_len(n_strata), function(s) {
    tab <- summarise_nowcast_matrix(object@strata_draws[, , s, drop = TRUE])
    tab$stratum <- lvls[s]; tab
  })
  total_tab$stratum <- "Total"
  out <- do.call(rbind, c(per_stratum, list(total_tab)))
  if (!is.null(object@event_dates))
    out$event_date <- rep(object@event_dates, length.out = nrow(out))
  out
}

#' @noRd
S7::method(print, nowcast_prediction_class) <- function(x, ...) {
  cli::cli_text("<nowcast_prediction> {nrow(x@draws)} draws x {ncol(x@draws)} event-times")
  newest <- x@draws[, x@target]
  cli::cli_text("newest event (target {x@target}): observed {x@observed}, ",
                "median {round(stats::median(newest))} ",
                "[{round(stats::quantile(newest, 0.025))}, {round(stats::quantile(newest, 0.975))}]")
  invisible(x)
}

# -- Latent-incidence summaries: mean / median / sd (via summary) / quantile ---

#' Posterior mean of the latent incidence (per event time)
#' @param x A `nowcast_class` object.
#' @param ... Passed on (e.g. `n_draws`, `seed`).
#' @returns Numeric vector (length = number of event times).
#' @noRd
S7::method(mean, nowcast_class) <- function(x, ...) colMeans(.nowcast_lambda_draws(x, ...), na.rm = TRUE)

#' Posterior median of the latent incidence (per event time)
#' @param x A `nowcast_class` object.
#' @param ... Passed on.
#' @returns Numeric vector (length = number of event times).
#' @noRd
S7::method(median, nowcast_class) <- function(x, ...) {
  apply(.nowcast_lambda_draws(x, ...), 2, stats::median, na.rm = TRUE)
}

#' Posterior quantiles of the latent incidence
#' @param x A `nowcast_class` object.
#' @param probs Quantile probabilities.
#' @param ... Passed on.
#' @returns A matrix `[event-time x probs]`.
#' @noRd
S7::method(quantile, nowcast_class) <- function(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), ...) {
  lambda <- .nowcast_lambda_draws(x, ...)
  out <- t(apply(lambda, 2, stats::quantile, probs = probs, na.rm = TRUE))
  rownames(out) <- NULL
  out
}

#' Summary of a fitted nowcast (parameters + latent-incidence summary)
#' @param object A `nowcast_class`.
#' @param ... Passed on (e.g. `n_draws`).
#' @returns Invisibly, a list with `coef`, `latent` (mean/median/sd per time).
#' @noRd
S7::method(summary, nowcast_class) <- function(object, ...) {
  lambda <- .nowcast_lambda_draws(object, ...)
  latent <- data.frame(
    event_index = seq_len(ncol(lambda)) - 1L,
    mean   = colMeans(lambda, na.rm = TRUE),
    median = apply(lambda, 2, stats::median, na.rm = TRUE),
    sd     = apply(lambda, 2, stats::sd, na.rm = TRUE),
    q2.5   = apply(lambda, 2, stats::quantile, probs = 0.025, na.rm = TRUE),
    q97.5  = apply(lambda, 2, stats::quantile, probs = 0.975, na.rm = TRUE)
  )
  parameters <- stats::coef(object)
  cli::cli_h2("nowcast ({object@type}, rung = {object@rung}, {length(object@fits)} fit(s))")
  cli::cli_text("as of {format(object@now)}; {object@target} event-times")
  cli::cli_text("delay: mu = {round(parameters['delay_mu'], 3)}, sigma = {round(parameters['delay_sigma'], 3)}")
  invisible(list(coef = parameters, latent = latent, type = object@type, rung = object@rung))
}

#' Pretty cli print of a fitted nowcast
#' @noRd
S7::method(print, nowcast_class) <- function(x, ...) {
  n_strata <- as.integer(x@engine$num_strata %||% 1L)
  strata_txt <- if (n_strata > 1L) paste0(", ", n_strata, " strata") else ""

  cli::cli_rule(left = cli::col_green(cli::style_bold("diseasenowcasting")),
                right = "as of {format(x@now)}")
  cli::cli_text("{.strong Model}: ", .model_oneline(x@model))
  cli::cli_text(cli::col_grey(
    "{x@type} ({x@target} event-time{?s}{strata_txt}; {length(x@fits)} fit{?s}, rung '{x@rung}')"))

  # Newest-event nowcast headline (d* = 0) -- quick, small-n draw.
  headline <- tryCatch({
    pr <- predict(x, n_draws = 200L)
    tgt <- x@target
    draws_tgt <- pr@draws[, tgt]
    med <- stats::median(draws_tgt, na.rm = TRUE)
    lo  <- stats::quantile(draws_tgt, 0.05, na.rm = TRUE, names = FALSE)
    hi  <- stats::quantile(draws_tgt, 0.95, na.rm = TRUE, names = FALSE)
    obs <- pr@observed
    list(obs = obs, med = med, lo = lo, hi = hi)
  }, error = function(e) NULL)

  if (!is.null(headline)) {
    med_str <- cli::col_green(cli::style_bold(round(headline$med)))
    cli::cli_text(
      "{cli::symbol$bullet} {.strong Newest event}: observed {.val {round(headline$obs)}} ",
      "{cli::symbol$arrow_right} nowcast median {med_str} ",
      "[{round(headline$lo)}, {round(headline$hi)}] (90% CI)")
  }

  cli::cli_text(cli::col_grey(
    "Use {.fn predict} / {.fn autoplot} for the nowcast, {.fn coef} / {.fn summary} for estimates."))
  cli::cli_text(cli::col_grey(
    "Call {.code print(nc@model)} for the full model spec (including priors)."))
  invisible(x)
}
