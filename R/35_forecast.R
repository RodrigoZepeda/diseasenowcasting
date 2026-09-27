# =============================================================================
# Forecasting: carry a fitted nowcast past `now`
# =============================================================================
# A nowcast already contains everything a forecast needs.  The likelihood of an
# event time with nothing reported is identically one, so the posterior of the
# latent process at `now + k` is the process's own transition from the
# posterior at `now`: the innovations after `now` keep their N(0, 1) prior and
# are independent of everything the data informed.  A forecast is therefore
# the nowcast's own Laplace draws, with each recursion run `h` more steps
# (`.reconstruct_log_mean(horizon = h)`) and the same observation model applied
# to the new cells.  No refit, and the nowcast and forecast are one joint draw.
# =============================================================================

#' Forecast cases past the nowcast date
#'
#' Extends a fitted nowcast `h` event times past `now`. The forecast reuses the
#' fit's posterior draws: every latent epidemic recursion (AR(1), ARIMA,
#' exponential smoothing, structural, random-walk, SIR) is run `h` more steps
#' with fresh innovations, and the same observation model that produced the
#' nowcast turns the extended epidemic into predictive counts. Nothing is
#' refitted, and because the nowcast and the forecast come from the same draws
#' they form one joint predictive distribution.
#'
#' @details
#' After `now` nothing has been reported yet, so those event times add nothing
#' to the likelihood. The posterior of the epidemic at `now + k` is then exactly
#' the process's own `k`-step transition from its posterior at `now`, which is
#' what the forecast draws. The forecast count is the complete (eventually
#' reported) count at the new event time, drawn from the fitted negative
#' binomial or Poisson likelihood; for count-cumulative data it is the settled
#' `C_t(H)` target, drawn by running the fitted signed updates from zero.
#'
#' Epidemic processes differ in how far they can reach:
#'
#' * The recursive processes ([ar1_epidemic()], [arima_epidemic()],
#'   [ets_epidemic()], [sts_epidemic()], [random_walk_epidemic()],
#'   [naive_epidemic()], [theta_epidemic()], [sir_epidemic()]) forecast any
#'   horizon.
#' * [hsgp_epidemic()] evaluates its basis past `now` only up to the edge of its
#'   domain, and reverts towards the intercept as it approaches it; a longer
#'   horizon is an error.
#' * A [custom_epidemic()] returns a matrix of fixed length and cannot be
#'   extended.
#'
#' Temporal effects (seasonality, day of week, ...) are recomputed from the
#' calendar for the new dates. Other event covariates must be supplied through
#' `new_data`.
#'
#' @section Revision categories:
#' With a report-level [revision_process()] every report is eventually genuine
#' with probability `p`. A forecast draws the gross number of reports and splits
#' it, so `category` selects which part of the eventual (settled) status is
#' returned:
#'
#' | `category` | `retraction_only` | `confirmation_only` | `both` |
#' |---|---|---|---|
#' | `"overall"` | every report | every report | every report |
#' | `"confirmed"` | -- | confirmed eventually | resolves positive |
#' | `"retracted"` | retracted eventually | -- | resolves negative |
#' | `"pending"` | never retracted | never confirmed | -- |
#'
#' The default, `NULL`, is the fit's own estimand (bold in [nowcast()]'s
#' printout): `"pending"` for retraction-only data, `"confirmed"` otherwise, and
#' `"overall"` without a revision process. A category a mode cannot observe is
#' an error. With `include_nowcast = TRUE` the nowcast part of the result reports
#' the same category, so a single series runs from the past into the future.
#'
#' @param object A result of [nowcast()] or [auto_nowcast()].
#' @param h Number of event times past `now` to forecast (a positive integer).
#' @param category Which cases to forecast when the data carry revisions: one
#'   of `"overall"`, `"confirmed"`, `"retracted"`, `"pending"`, or `NULL` (the
#'   default) for the fit's own estimand. See *Revision categories*.
#' @param new_data A data frame of event-covariate values for the forecast
#'   dates: the `tbl_now`'s event-date column plus every event covariate, one row
#'   per new event time. Needed only when the model uses event covariates other
#'   than temporal effects.
#' @param include_nowcast If `TRUE` (the default), the result covers the nowcast
#'   event times as well as the forecast; if `FALSE`, only the `h` new ones.
#' @param n_draws Number of predictive draws. Defaults to the fit's `n_draws`.
#' @param quantile_levels Probabilities at which to summarise the draws.
#' @param seed Optional RNG seed.
#' @param ... Unused.
#' @returns A diseasenowcasting [tbl.now::tbl_nowcast] whose predictions and
#'   draws carry an extra `.horizon` column: the number of event times past
#'   `now` (`0` at `now`, negative for the nowcast, `1..h` for the forecast).
#'   `@metadata$diseasenowcasting$forecast` records the horizon, category and
#'   forecast dates. `autoplot()`, `tbl.now::tidy()` and scoring work as for a
#'   nowcast.
#'
#' @seealso [nowcast()], and `vignette("Forecasting")`.
#' @examplesIf interactive()
#' library(tbl.now)
#' data(denguedat)
#' dengue <- tbl_now(denguedat, event_date = onset_week,
#'                   report_date = report_week, data_type = "linelist",
#'                   now = as.Date("1990-10-01"), verbose = FALSE)
#' fit <- nowcast(dengue, model(nb_likelihood(), ar1_epidemic(),
#'                              lognormal_delay()),
#'                temporal_effects = "none", seed = 1)
#' fc <- forecast(fit, h = 2)
#' autoplot(fc)
#' @importFrom generics forecast
#' @rawNamespace S3method(forecast, "diseasenowcasting::diseasenowcasting_nowcast", forecast.diseasenowcasting_nowcast)
#' @export
forecast.diseasenowcasting_nowcast <- function(
    object, h = 1L, category = NULL, new_data = NULL, include_nowcast = TRUE,
    n_draws = NULL, quantile_levels = tbl.now::nowcast_quantile_levels(),
    seed = sample.int(.Machine$integer.max, 1), ...) {
  native <- .unwrap_nowcast(object)
  if (!is.null(seed)) set.seed(seed)
  spec <- .forecast_spec(native, h = h, category = category, new_data = new_data)
  n_draws <- n_draws %||% native@n_draws
  per_fit <- max(1L, ceiling(n_draws / length(native@fits)))
  pooled <- .pool_fit_draws(native@fits, native@target, n_draws = per_fit,
                            forecast = spec)
  prediction <- .forecast_prediction(native, pooled, spec,
                                     include_nowcast = isTRUE(include_nowcast))
  result <- .result_from_prediction(native, prediction, quantile_levels)
  horizon_of <- stats::setNames(
    prediction@event_index - (native@engine$max_time - 1L),
    as.character(prediction@event_dates)
  )
  event_col <- result@event_date
  result@predictions$.horizon <-
    unname(horizon_of[as.character(result@predictions[[event_col]])])
  result@draws$.horizon <- unname(horizon_of[as.character(result@draws[[event_col]])])
  metadata <- result@metadata
  metadata$diseasenowcasting$forecast <- list(
    horizon = spec$horizon, category = spec$category,
    estimand = spec$description,
    forecast_dates = utils::tail(prediction@event_dates, spec$horizon)
  )
  result@metadata <- metadata
  result
}

#' Validate the request and resolve everything a forecast draw needs
#' @keywords internal
#' @noRd
.forecast_spec <- function(native, h, category = NULL, new_data = NULL) {
  if (!is.numeric(h) || length(h) != 1L || !is.finite(h) || h < 1 ||
      h != round(h))
    cli::cli_abort("{.arg h} must be a single positive whole number, not {.val {h}}.")
  if (identical(native@type, "prior_only"))
    cli::cli_abort(c(
      "A prior-only nowcast cannot be forecast.",
      "i" = "Fit the model to data first with {.code nowcast(..., prior_only = FALSE)}."
    ), class = "diseasenowcasting_forecast_unsupported")
  engine <- native@engine
  if (as.integer(engine$epidemic_model) == 4L)
    cli::cli_abort(c(
      "A {.fn custom_epidemic} cannot be forecast.",
      "i" = "Its {.arg intensity_fn} returns a matrix of fixed length, so there is no recursion to continue past {.arg now}."
    ), class = "diseasenowcasting_forecast_unsupported")
  if (isTRUE(engine$is_count_cumulative == 1L) &&
      (as.integer(engine$P_delay %||% 0L) > 0L ||
       as.integer(engine$P_revision %||% 0L) > 0L))
    cli::cli_abort(c(
      "Count-cumulative forecasts do not yet support reporting or revision regressions.",
      "i" = "Their kernels are built per published cohort, and a future cohort has no covariate values."
    ), class = "diseasenowcasting_forecast_unsupported")
  h <- as.integer(h)
  resolved <- .forecast_category(native, category)
  if (as.integer(engine$epidemic_model) == 1L)
    .hsgp_extended_basis(engine, as.integer(engine$max_time) + h)
  c(list(horizon = h,
         X_future = .forecast_design(native, h, new_data)),
    resolved)
}

#' The cases a forecast counts, given the fit's revision process
#'
#' Returns the public `category`, its `part` of the eventual report split
#' (`"target"` = the fit's own estimand, `"complement"` = the remainder of the
#' reports, `"overall"` = all of them) and a sentence describing it.
#' @keywords internal
#' @noRd
.forecast_category <- function(native, category = NULL) {
  engine <- native@engine
  allowed <- c("overall", "confirmed", "retracted", "pending")
  if (!is.null(category)) {
    if (!is.character(category) || length(category) != 1L ||
        !category %in% allowed)
      cli::cli_abort("{.arg category} must be {.code NULL} or one of {.val {allowed}}.")
  }
  if (!isTRUE(engine$is_linelist_retraction == 1L)) {
    is_cumulative <- isTRUE(engine$is_count_cumulative == 1L)
    description <- if (is_cumulative)
      sprintf("C_t(%d): finite-horizon settled retention",
              as.integer(engine$settlement_horizon))
    else "all reported cases"
    if (!is.null(category) && !identical(category, "overall"))
      cli::cli_abort(c(
        "Category {.val {category}} needs a report-level revision process.",
        "i" = "This fit has none, so every forecast counts {.val overall} cases ({description})."
      ), class = "diseasenowcasting_forecast_category")
    return(list(category = "overall", part = "target", description = description))
  }
  mode <- as.integer(engine$resolution_mode %||% 0L)
  mode_name <- c("retraction_only", "confirmation_only", "both")[mode + 1L]
  # For each mode: the category that IS the fit's estimand, the one that is its
  # complement among the reports, and the one the mode never observes.
  roles <- switch(mode_name,
    retraction_only   = c(target = "pending",   complement = "retracted", absent = "confirmed"),
    confirmation_only = c(target = "confirmed", complement = "pending",   absent = "retracted"),
    both              = c(target = "confirmed", complement = "retracted", absent = "pending")
  )
  category <- category %||% roles[["target"]]
  if (identical(category, roles[["absent"]])) {
    reason <- switch(mode_name,
      retraction_only = "retraction-only data never record a confirmation",
      confirmation_only = "confirmation-only data never record a retraction",
      both = "every report eventually resolves when both outcomes are recorded")
    cli::cli_abort(c(
      "Category {.val {category}} is not defined for a {.val {mode_name}} revision process: {reason}.",
      "i" = "Use one of {.val {c('overall', roles[['target']], roles[['complement']])}}."
    ), class = "diseasenowcasting_forecast_category")
  }
  description <- switch(category,
    overall   = "all reports, whatever their eventual status",
    confirmed = "reports that are eventually confirmed",
    retracted = "reports that are eventually retracted",
    pending   = if (identical(mode_name, "retraction_only"))
      "reports that are never retracted" else "reports that are never confirmed")
  part <- if (identical(category, "overall")) "overall"
    else if (identical(category, roles[["target"]])) "target" else "complement"
  list(category = category, part = part, description = description)
}

#' Event-covariate rows for the forecast dates
#'
#' Rebuilds the engine's covariate matrix on the grid extended by `h` exactly as
#' `prepare_from_tbl_now()` built it, then checks the in-sample rows reproduce
#' the fitted `X` before returning the new ones.
#' @keywords internal
#' @noRd
.forecast_design <- function(native, h, new_data = NULL) {
  engine <- native@engine
  n_covariates <- as.integer(engine$P %||% 0L)
  if (n_covariates == 0L) return(NULL)
  data <- native@data
  n_time <- as.integer(engine$max_time)
  n_out <- n_time + h
  min_event <- engine$min_event
  event_unit <- engine$event_unit
  event_col <- tbl.now::get_event_date(data)
  effect_cols <- tbl.now::get_temporal_effect_cols(data)
  temporal <- .temporal_effect_matrices(data, min_event, event_unit, n_out,
                                        effect_cols)$event
  covariate_cols <- .covariate_roles(data)$event
  covariate_X <- NULL
  if (length(covariate_cols)) {
    future_dates <- .grid_event_dates(min_event, event_unit, n_out)[n_time + seq_len(h)]
    if (is.null(new_data))
      cli::cli_abort(c(
        "The model uses event covariates, which need values at the forecast dates.",
        "i" = "Supply {.arg new_data} with {.field {event_col}} and {.field {covariate_cols}} for {.val {as.character(future_dates)}}."
      ), class = "diseasenowcasting_forecast_covariates")
    new_data <- as.data.frame(new_data)
    missing_cols <- setdiff(c(event_col, covariate_cols), names(new_data))
    if (length(missing_cols))
      cli::cli_abort("{.arg new_data} is missing the column{?s} {.field {missing_cols}}.",
                     class = "diseasenowcasting_forecast_covariates")
    as_grid <- function(x) if (inherits(future_dates, "Date")) as.Date(x) else as.numeric(x)
    supplied <- as_grid(new_data[[event_col]])
    uncovered <- future_dates[!future_dates %in% supplied]
    if (length(uncovered))
      cli::cli_abort(c(
        "{.arg new_data} has no covariate values for {.val {as.character(uncovered)}}.",
        "i" = "Every forecast date needs a row."
      ), class = "diseasenowcasting_forecast_covariates")
    observed <- as.data.frame(data)[c(event_col, covariate_cols)]
    new_rows <- new_data[supplied %in% future_dates, c(event_col, covariate_cols), drop = FALSE]
    new_rows[[event_col]] <- as_grid(new_rows[[event_col]])
    combined <- dplyr::bind_rows(observed, new_rows)
    covariate_X <- .covariate_matrix(
      combined, event_col, min_event,
      function(to) .unit_steps(min_event, to, event_unit), n_out,
      covariate_cols = covariate_cols
    )
  }
  X <- cbind(temporal %||% matrix(0.0, n_out, 0L),
             covariate_X %||% matrix(0.0, n_out, 0L))
  in_sample <- X[seq_len(n_time), , drop = FALSE]
  if (ncol(X) != n_covariates ||
      !isTRUE(all.equal(unname(in_sample), unname(engine$X), tolerance = 1e-8)))
    cli::cli_abort(c(
      "Could not rebuild the fitted covariate matrix on the extended event grid.",
      "i" = "A categorical covariate in {.arg new_data} may carry a level the fit never saw."
    ), class = "diseasenowcasting_forecast_covariates")
  X[n_time + seq_len(h), , drop = FALSE]
}

#' Forecast (and category-specific nowcast) cells for one parameter draw
#'
#' @returns list(`future` `[h x S]` predictive counts of the requested
#'   category, `future_lambda` `[h x S]` their expected value, and `past`
#'   `[n_time x S]` the nowcast of that category when it is not the fit's own
#'   estimand).
#' @keywords internal
#' @noRd
.forecast_draw_cells <- function(data, priors, parlist, fit, reconstructed,
                                 forecast, phi_nb, is_negbin, pred_cells,
                                 observed_all, future_genuine = NULL,
                                 future_genuine_mean = NULL) {
  n_time <- as.integer(data$max_time)
  n_strata <- as.integer(data$num_strata %||% 1L)
  h <- forecast$horizon
  log_mean <- .reconstruct_log_mean(
    data, .fill_fixed_parameters(parlist, data, priors), fit$Bmat, fit$freq,
    horizon = h, X_future = forecast$X_future, priors = priors
  )
  upper <- data$mu_log_upper_bound
  future_log_mean <- log_mean[n_time + seq_len(h), , drop = FALSE]
  lambda_future <- exp(upper - log1p(exp(upper - future_log_mean)))

  if (isTRUE(data$is_count_cumulative == 1L)) {
    cc <- reconstructed$count_cumulative
    return(list(
      future = .draw_count_cumulative_future(lambda_future, cc),
      future_lambda = lambda_future * cc$terminal_retention,
      past = NULL
    ))
  }
  genuine <- matrix(.epidemic_rng(is_negbin, as.numeric(lambda_future), phi_nb),
                    h, n_strata)
  if (identical(forecast$part, "target"))
    return(list(future = genuine, future_lambda = lambda_future, past = NULL))

  # Reports that never count, per genuine one: (1 - p) / p, stratum by stratum.
  p <- reconstructed$retraction$p_by_stratum
  odds <- matrix((1 - p) / pmax(p, 1e-8), nrow = 1L)[rep(1L, h), , drop = FALSE]
  future_other <- .draw_complement_reports(genuine, lambda_future,
                                           lambda_future * odds, phi_nb, is_negbin)
  past_odds <- odds[rep(1L, n_time), , drop = FALSE]
  past_other <- .draw_complement_reports(future_genuine, future_genuine_mean,
                                         future_genuine_mean * past_odds,
                                         phi_nb, is_negbin)
  # Everything ever reported: the rows already in (retracted ones included) plus
  # every report still to come, genuine or not.
  past_overall <- observed_all + future_genuine + past_other
  if (identical(forecast$part, "overall"))
    return(list(future = genuine + future_other,
                future_lambda = lambda_future * (1 + odds),
                past = past_overall))
  list(future = future_other, future_lambda = lambda_future * odds,
       past = pmax(past_overall - pred_cells, 0))
}

#' Draw the non-genuine reports that accompany a draw of genuine ones
#'
#' Under the negative binomial the genuine and non-genuine reports of one
#' origin are two thinnings of one report cloud with a single gamma frailty
#' `Lambda ~ Gamma(r, r)`. Given `g` genuine reports with mean `m`, the frailty
#' is `Gamma(r + g, r + m)`, and the other reports are Poisson with mean
#' `complement_mean * Lambda`. Drawing them this way keeps an already drawn
#' genuine count untouched while making the pair jointly correct: their sum is
#' `NB(m / p, r)` and the genuine share is `Binomial(sum, p)`.
#' @keywords internal
#' @noRd
.draw_complement_reports <- function(genuine, genuine_mean, complement_mean,
                                     phi_nb, is_negbin) {
  complement_mean <- pmax(as.numeric(complement_mean), 0)
  complement_mean[!is.finite(complement_mean)] <- 0
  frailty <- if (is_negbin) {
    size <- 1 / phi_nb
    if (!is.finite(size) || size <= 0) size <- 1e-4
    stats::rgamma(length(complement_mean),
                  shape = size + pmax(as.numeric(genuine), 0),
                  rate = size + pmax(as.numeric(genuine_mean), 0))
  } else 1
  matrix(stats::rpois(length(complement_mean), pmin(complement_mean * frailty, 1e8)),
         nrow(as.matrix(genuine)), ncol(as.matrix(genuine)))
}

#' Settled count-cumulative level of cohorts with nothing published yet
#'
#' A future cohort starts at level zero with no previous movement and runs the
#' fitted signed updates through every report age `0..H`, the same law the
#' nowcast uses from each published anchor onwards.
#' @param lambda_future `[h x S]` latent incidence of the new cohorts.
#' @param cc The reconstructed count-cumulative block.
#' @keywords internal
#' @noRd
.draw_count_cumulative_future <- function(lambda_future, cc) {
  settlement <- as.integer(cc$settlement_horizon)
  terminal <- matrix(0.0, nrow(lambda_future), ncol(lambda_future))
  for (s in seq_len(ncol(lambda_future))) for (t in seq_len(nrow(lambda_future))) {
    running_level <- 0
    previous_nonzero <- FALSE
    for (delay in 0:settlement) {
      update <- .draw_count_cumulative_update(
        lambda_future[t, s] * cc$alpha_unit[delay + 1L],
        lambda_future[t, s] * cc$omega_unit[delay + 1L],
        delay, previous_nonzero, cc
      )
      running_level <- running_level + update
      previous_nonzero <- update != 0
    }
    terminal[t, s] <- max(running_level, 0)
  }
  terminal
}

#' Package pooled forecast draws as a prediction on the extended event axis
#' @keywords internal
#' @noRd
.forecast_prediction <- function(native, pooled, spec, include_nowcast = TRUE) {
  engine <- native@engine
  n_time <- as.integer(engine$max_time)
  h <- spec$horizon
  future <- pooled$forecast$strata
  strata_draws <- if (include_nowcast) {
    past <- pooled$forecast$nowcast_strata
    combined <- array(NA_real_, c(dim(past)[1L], n_time + h, dim(past)[3L]))
    combined[, seq_len(n_time), ] <- past
    combined[, n_time + seq_len(h), ] <- future
    combined
  } else future
  columns <- if (include_nowcast) seq_len(n_time + h) else n_time + seq_len(h)
  event_dates <- .grid_event_dates(engine$min_event, engine$event_unit, n_time + h)
  total <- apply(strata_draws, c(1L, 2L), sum)
  if (!is.matrix(total)) total <- matrix(total, nrow = dim(strata_draws)[1L])
  n_strata <- dim(strata_draws)[3L]
  nowcast_prediction_class(
    draws = total,
    target = n_time,
    observed = NA_real_,
    event_index = columns - 1L,
    strata_draws = if (n_strata > 1L) strata_draws else NULL,
    strata_levels = engine$strata_levels %||% NULL,
    event_dates = event_dates[columns],
    observed_series = NULL,
    observed_strata = NULL,
    estimand = spec$description,
    negative_projection_count = pooled$negative_projection_count,
    laplace_sampling = pooled$laplace_sampling %||% list()
  )
}
