# The public result is a tbl_nowcast subclass. The native fitted object remains
# intact in `@fit`, so modelling operations can unwrap it without installing
# methods on tbl.now's shared result class.

diseasenowcasting_result_class <- S7::new_class(
  "diseasenowcasting_nowcast",
  parent = tbl.now::tbl_nowcast,
  properties = list(
    model = model_class,
    type = S7::class_character,
    fits = S7::class_list,
    rung = S7::class_character,
    target = S7::class_numeric,
    engine = S7::class_list,
    priors = S7::class_list,
    phi = S7::class_any,
    n_draws = S7::class_numeric,
    fit_diagnostics = S7::new_property(S7::class_list, default = list()),
    revision_mode = S7::class_character,
    comparison = S7::new_property(S7::class_any, default = NULL)
  )
)

#' Wrap a native fit in the common result grammar
#'
#' @param fit A native `nowcast_class` object.
#' @param n_draws Number of predictive draws to retain.
#' @param seed Prediction seed.
#' @param quantile_levels Probabilities at which to summarise predictive draws.
#'
#' @return A diseasenowcasting `tbl_nowcast` subclass.
#'
#' @keywords internal
#' @noRd
.as_diseasenowcasting_result <- function(
    fit,
    n_draws = NULL,
    seed = NULL,
    quantile_levels = tbl.now::nowcast_quantile_levels()) {
  if (S7::S7_inherits(fit, diseasenowcasting_result_class)) {
    return(fit)
  }
  if (!S7::S7_inherits(fit, nowcast_class)) {
    cli::cli_abort("Internal error: expected a native {.cls nowcast} fit.")
  }

  quantile_levels <- sort(unique(as.numeric(quantile_levels)))
  if (length(quantile_levels) == 0L || anyNA(quantile_levels) ||
      any(quantile_levels <= 0 | quantile_levels >= 1)) {
    cli::cli_abort(
      "{.arg quantile_levels} must be probabilities strictly between 0 and 1."
    )
  }

  prediction <- stats::predict(
    fit,
    n_draws = n_draws %||% fit@n_draws,
    seed = seed
  )
  fit_diagnostics <- fit@fit_diagnostics
  fit_diagnostics$laplace_sampling <- prediction@laplace_sampling
  fit@fit_diagnostics <- fit_diagnostics
  .warn_laplace_sampling(fit_diagnostics)
  formatted <- .format_diseasenowcasting_prediction(prediction, fit)
  predictions <- .diseasenowcasting_draw_quantiles(
    formatted$draws,
    c(formatted$event_date, formatted$strata),
    quantile_levels
  )
  metadata <- .diseasenowcasting_prediction_metadata(prediction)
  metadata$fit_diagnostics <- fit_diagnostics
  metadata <- metadata[!vapply(metadata, is.null, logical(1))]

  diseasenowcasting_result_class(
    predictions = predictions,
    draws = formatted$draws,
    method = "diseasenowcasting",
    fit = fit,
    now = fit@now,
    event_date = formatted$event_date,
    strata = formatted$strata,
    data = fit@data,
    call = NULL,
    metadata = list(diseasenowcasting = metadata),
    model = fit@model,
    type = fit@type,
    fits = fit@fits,
    rung = fit@rung,
    target = fit@target,
    engine = fit@engine,
    priors = fit@priors,
    phi = fit@phi,
    n_draws = fit@n_draws,
    fit_diagnostics = fit_diagnostics,
    revision_mode = fit@revision_mode,
    comparison = fit@comparison
  )
}

#' Format a native prediction for the common result grammar
#'
#' @param prediction A `nowcast_prediction_class` object.
#' @param fit The native fitted object that produced it.
#'
#' @return A list containing tidy draws and axis metadata.
#'
#' @keywords internal
#' @noRd
.format_diseasenowcasting_prediction <- function(prediction, fit) {
  data <- fit@data
  event_col <- tbl.now::get_event_date(data)
  event_dates <- .diseasenowcasting_event_axis(prediction, fit)
  strata_cols <- tbl.now::get_strata(data) %||% character(0)
  strata_levels <- prediction@strata_levels
  strata_draws <- prediction@strata_draws

  stratified <- length(strata_cols) > 0L &&
    !is.null(strata_draws) &&
    length(dim(strata_draws)) == 3L &&
    length(strata_levels) == dim(strata_draws)[3L] &&
    !identical(strata_levels, "all")
  strata_values <- if (stratified) {
    .split_diseasenowcasting_strata(strata_levels, strata_cols, data = data)
  } else {
    NULL
  }

  if (stratified && !is.null(strata_values)) {
    n_draws <- dim(strata_draws)[1L]
    n_times <- dim(strata_draws)[2L]
    draws <- dplyr::tibble(
      .event_date = rep(
        rep(event_dates, each = n_draws),
        times = length(strata_levels)
      ),
      .draw = rep(
        seq_len(n_draws),
        times = n_times * length(strata_levels)
      ),
      .value = as.vector(strata_draws)
    )
    names(draws)[1L] <- event_col
    for (column in strata_cols) {
      draws[[column]] <- rep(
        strata_values[[column]],
        each = n_draws * n_times
      )
    }
    return(list(
      draws = draws,
      event_date = event_col,
      strata = strata_cols
    ))
  }

  if (length(strata_cols) > 0L) {
    cli::cli_warn(c(
      "The fitted model did not retain unambiguous draws for the declared \
       strata {.val {strata_cols}}; pooling over them.",
      "i" = "Avoid {.val |} inside stratum values until structured stratum \
             keys are available."
    ))
  }
  draws_matrix <- prediction@draws
  draws <- dplyr::tibble(
    .event_date = rep(event_dates, each = nrow(draws_matrix)),
    .draw = rep(seq_len(nrow(draws_matrix)), times = ncol(draws_matrix)),
    .value = as.vector(draws_matrix)
  )
  names(draws)[1L] <- event_col
  list(draws = draws, event_date = event_col, strata = character(0))
}

#' @keywords internal
#' @noRd
.diseasenowcasting_event_axis <- function(prediction, fit) {
  if (!is.null(prediction@event_dates)) {
    return(prediction@event_dates)
  }
  event_index <- prediction@event_index
  event_col <- tbl.now::get_event_date(fit@data)
  first_event <- min(fit@data[[event_col]], na.rm = TRUE)
  units <- tbl.now::get_event_units(fit@data)
  if (identical(units, "numeric") || !inherits(first_event, "Date")) {
    out <- first_event + as.integer(event_index)
    if (is.integer(first_event)) out <- as.integer(out)
    return(out)
  }
  seq(first_event, by = as.character(units), length.out = max(event_index) + 1L)[
    event_index + 1L
  ]
}

#' @keywords internal
#' @noRd
.split_diseasenowcasting_strata <- function(labels, strata_cols, data = NULL) {
  # Prefer the authoritative combinations in the source tbl_now. This makes a
  # literal "|" inside a value unambiguous: it is matched as data, not parsed as
  # syntax. Refuse only genuinely ambiguous encodings (two distinct source rows
  # that collapse to the same historical label).
  if (!is.null(data) && all(strata_cols %in% names(data))) {
    candidates <- unique(as.data.frame(data)[strata_cols])
    encoded <- do.call(paste, c(lapply(candidates, function(values) {
      values <- as.character(values)
      values[is.na(values) | values == ""] <- "missing"
      values
    }), sep = "|"))
    if (!anyDuplicated(encoded)) {
      matched <- match(as.character(labels), encoded)
      if (!anyNA(matched)) {
        return(dplyr::as_tibble(candidates[matched, , drop = FALSE]))
      }
    }
  }

  parts <- strsplit(as.character(labels), "|", fixed = TRUE)
  if (any(lengths(parts) != length(strata_cols))) {
    return(NULL)
  }
  values <- lapply(seq_along(strata_cols), function(index) {
    vapply(parts, function(part) part[[index]], character(1))
  })
  dplyr::as_tibble(stats::setNames(values, strata_cols))
}

#' @keywords internal
#' @noRd
.diseasenowcasting_draw_quantiles <- function(draws, keys, quantile_levels) {
  draws |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::group_modify(function(data, key) {
      dplyr::tibble(
        .quantile_level = quantile_levels,
        .value = unname(stats::quantile(
          data$.value,
          probs = quantile_levels,
          na.rm = TRUE
        ))
      )
    }) |>
    dplyr::ungroup()
}

#' @keywords internal
#' @noRd
.diseasenowcasting_prediction_metadata <- function(prediction) {
  list(
    estimand = prediction@estimand,
    cumulative_reconstruction = prediction@cumulative_reconstruction,
    negative_projection_count = prediction@negative_projection_count
  )
}

#' Recover the native fit from a public result
#'
#' @param x A public result or native fit.
#' @param arg Argument name used in errors.
#'
#' @return A native `nowcast_class` object.
#'
#' @keywords internal
#' @noRd
.unwrap_nowcast <- function(x, arg = "object") {
  if (S7::S7_inherits(x, diseasenowcasting_result_class)) {
    x <- x@fit
  }
  if (!S7::S7_inherits(x, nowcast_class)) {
    cli::cli_abort(
      "{.arg {arg}} must be a result from {.fn nowcast} or {.fn auto_nowcast}."
    )
  }
  x
}
