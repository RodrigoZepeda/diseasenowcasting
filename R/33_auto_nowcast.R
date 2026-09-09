# =============================================================================
# auto_nowcast() -- pick the best model by backtesting, then fit it
# =============================================================================
# Builds a candidate grid of (epidemic process x delay family) models sized to
# the amount of data, backtests them over several historical dates, scores them
# through scoringutils, selects the winner, and refits it on the full data.
# =============================================================================

#' Automatically select and fit the best nowcasting model
#'
#' Takes a `tbl_now` and **chooses a model for you**: it builds a grid of
#' candidate models (epidemic process x reporting-delay family) sized to how much
#' data you have, [backtest()]s them over several historical dates, converts the
#' canonical backtest to a scoringutils forecast, keeps the best one, and refits
#' it on the full data. The returned object
#' is an ordinary [nowcast()] result (so `autoplot()`, `predict()`, etc. work),
#' with the ranked scoreboard attached in its `comparison` slot.
#'
#' @details
#' **Candidate epidemic processes are chosen by series length** (`max_time`, the
#' number of event-times): a process becomes a candidate as soon as the series is
#' long enough to support it (SIR needs the least data, the HSGP the most) and is
#' never dropped for being *too* long, so the comparison always spans every
#' process the data can support.  With the default thresholds:
#' \itemize{
#'   \item `max_time < min_ar`             -> compares `{SIR}`;
#'   \item `min_ar <= max_time < min_hsgp` -> compares `{SIR, AR(1)}`;
#'   \item `max_time >= min_hsgp`          -> compares `{SIR, AR(1), HSGP}`.
#' }
#' Any process you pass explicitly via `sir` / `ar` / `hsgp` is *always* included
#' (regardless of length), which is how you make a prior compete: e.g. pass
#' `sir = sir_epidemic(R0 = lognormal_prior(log(3), 0.2))` and the SIR candidate
#' will use that R0 prior throughout the comparison.
#'
#' **Robustness.** A candidate that fails to converge on a backtest date simply
#' drops out of the comparison there (it never aborts the search), and candidates
#' are scored on the common set of dates where they all produced a forecast so a
#' model cannot "win" on a lucky subset.  The winner is then refit on the full
#' data; if that refit fails, `auto_nowcast()` falls through to the next-best
#' candidate (and so on), so it converges whenever any candidate would.
#'
#' **Candidate delays** default to LogNormal, Generalized-Gamma and Dirichlet;
#' override with `delays`.
#'
#' **Speed.** The grid is backtested with a fast configuration
#' (`n_draws_select` posterior draws over `n_dates` dates spread across the
#' history); only the winning model is refit with the full `n_draws`.  Backtesting
#' is the expensive step -- set a `future::plan()` (e.g.
#' `future::plan(multisession)`) for parallel speed-up.
#'
#' @param data A `tbl_now` object (`tbl.now::tbl_now()`).
#' @param metric A single score column produced by [scoringutils::score()] to
#'   minimise. Default `"wis"`.
#' @param relative_score Logical. When `TRUE` (the default), select on the
#'   corresponding relative skill from [scoringutils::add_relative_skill()]
#'   rather than on the raw mean score. This makes comparisons fair when models
#'   are not all available for exactly the same targets.
#' @param tie_break How to break effectively equal selection scores.
#'   `"epidemic_priority"` (default) prefers HSGP, then AR(1), then SIR, then a
#'   custom epidemic process. `"fastest"` prefers the candidate with the lowest
#'   median elapsed time per successful retrospective fit. The unused rule is
#'   applied second, followed by candidate-grid order for determinism.
#' @param type Stage strategy used for *both* the backtest and the final fit:
#'   `"auto"` (default), `"two_stage"`, or `"one_stage"` (see [nowcast()]).
#' @param sir,ar,hsgp Optional epidemic-process components (e.g.
#'   `sir_epidemic(R0 = ...)`) carrying your priors.  If supplied, that process is
#'   forced into the candidate grid; otherwise the plain constructor is used when
#'   the series length calls for it.
#' @param delays A list of delay components to compare.  Default:
#'   `list(lognormal_delay(), generalized_gamma_delay(), dirichlet_delay())`.
#' @param likelihood Either a single likelihood used for every candidate
#'   (default `nb_likelihood()`), or a **list** of likelihoods to compare too,
#'   e.g. `list(nb_likelihood(), poisson_likelihood())`.
#' @param models Optional [model()] object or list of them (e.g. carrying a
#'   [custom_delay()] / [custom_epidemic()]) appended to the candidate grid so
#'   they compete in the same backtest.
#' @param n_dates Number of historical dates to backtest over (default 6).
#' @param n_draws_select Posterior draws during the selection backtest (default
#'   500 -- kept small for speed).
#' @param n_draws Posterior draws for the final fit of the winning model
#'   (default 2000).
#' @param K Delay imputations for the **final** two-stage fit of the winning
#'   model (default 25).
#' @param K_select Delay imputations during the **selection** backtest (default
#'   10 -- kept small for speed, like `n_draws_select`).  The selection backtest
#'   fits the whole grid over many dates, so its cost scales with `K_select`;
#'   ranking the candidates is robust to a coarser imputation than the final fit.
#'   Lower it (e.g. `5`) for a long series where selection dominates the runtime.
#' @param min_ar,min_hsgp Series-length thresholds (in event-times) at which
#'   AR(1) and HSGP become candidates (defaults 15 and 30).
#' @param now As-of date for the final fit (default: the `tbl_now`'s `now`).
#' @param seed RNG seed.
#' @param verbose Print progress and the chosen model (default `TRUE`).
#' @param ... Passed through to [backtest()] and [nowcast()] (e.g.
#'   `temporal_effects`).
#'
#' @returns A diseasenowcasting subclass of [tbl.now::tbl_nowcast] (as from
#'   [nowcast()]) for the selected model, with the model-selection scoreboard
#'   retained on the diseasenowcasting subclass and its native fit:
#'   `list(scores, chosen, metric, relative_score, tie_break, timings, max_time)`.
#'
#' @seealso [diseasenowcasting_workflows], [nowcast()], [backtest()], [scoringutils::score()],
#'   [scoringutils::add_relative_skill()]
#'
#' @examples
#' \donttest{
#' library(tbl.now)
#' data(denguedat)
#' # A short window keeps this example quick (auto_nowcast fits a whole grid):
#' dn <- subset(denguedat,
#'              onset_week >= as.Date("1990-06-01") & onset_week <= as.Date("1990-12-01"))
#' tn <- tbl_now(dn, event_date = onset_week, report_date = report_week,
#'               data_type = "linelist", verbose = FALSE)
#' # Backtesting the grid is the expensive step -- uncomment to run candidates
#' # in parallel (then restore sequential afterwards):
#' # future::plan(future::multisession, workers = 4)
#' # Compare a couple of delays; make the SIR candidate use a custom R0 prior:
#' nc <- auto_nowcast(tn,
#'                    sir    = sir_epidemic(R0 = lognormal_prior(log(2), 0.3)),
#'                    delays = list(lognormal_delay(), dirichlet_delay()),
#'                    n_dates = 2, n_draws_select = 150, n_draws = 300,
#'                    temporal_effects = "none")
#' # future::plan(future::sequential)
#' best_model_name(nc)    # the winning model's label
#' comparison_scores(nc)  # the ranked scoreboard
#' best_score(nc)         # just the winner's row
#' selection_metric(nc)   # which metric chose it
#' winner <- best_model(nc)  # the model() object, to reuse elsewhere
#' }
#' @export
auto_nowcast <- function(data,
                         metric = "wis", relative_score = TRUE,
                         tie_break = c("epidemic_priority", "fastest"),
                         type   = c("auto", "two_stage", "one_stage"),
                         sir = NULL, ar = NULL, hsgp = NULL,
                         delays = NULL, likelihood = nb_likelihood(),
                         models = NULL,
                         n_dates = 6L, n_draws_select = 500L,
                         n_draws = 2000L, K = 25L, K_select = 10L,
                         min_ar = 15L, min_hsgp = 30L,
                         now = NULL, seed = sample.int(.Machine$integer.max, 1),
                         verbose = TRUE, ...) {
  if (!is.character(metric) || length(metric) != 1L || is.na(metric) ||
      !nzchar(metric)) {
    cli::cli_abort("{.arg metric} must be one non-empty score-column name.")
  }
  if (!is.logical(relative_score) || length(relative_score) != 1L ||
      is.na(relative_score)) {
    cli::cli_abort("{.arg relative_score} must be `TRUE` or `FALSE`.")
  }
  tie_break <- match.arg(tie_break)
  type   <- match.arg(type)
  auto_started <- proc.time()[["elapsed"]]
  if (!is.null(seed)) set.seed(seed)

  # -- 1. candidate epidemic processes, sized to the series length -------------
  # Lower-bound gating: a process becomes a candidate as soon as the series is
  # long enough to support it (SIR needs the least data, HSGP the most) and is
  # never dropped for being *too* long.  This way the comparison always spans
  # every process the data can support, so auto_nowcast can pick the genuine
  # best and -- crucially -- can always fall back to a process (e.g. HSGP) that a
  # plain nowcast() of the same length would have fit.
  max_time <- prepare_from_tbl_now(data, diseasenowcasting::model(), now = now)$max_time
  epis <- list()
  epis[["SIR"]] <- sir %||% sir_epidemic()                 # least data-hungry: always in
  if (max_time >= min_ar)   epis[["AR1"]]  <- ar   %||% ar1_epidemic()
  if (max_time >= min_hsgp) epis[["HSGP"]] <- hsgp %||% hsgp_epidemic()
  # A process supplied explicitly is forced in even on a short series (so its
  # priors compete regardless of length).
  if (!is.null(ar)   && is.null(epis[["AR1"]]))  epis[["AR1"]]  <- ar
  if (!is.null(hsgp) && is.null(epis[["HSGP"]])) epis[["HSGP"]] <- hsgp

  # -- 2. candidate delays and likelihoods -------------------------------------
  delay_candidates <- delays %||% list(lognormal_delay(),
                                       generalized_gamma_delay(),
                                       dirichlet_delay())
  # `likelihood` may be a single likelihood (e.g. nb_likelihood()) or a list to
  # compare (e.g. list(nb_likelihood(), poisson_likelihood())).
  likelihoods <- if (S7::S7_inherits(likelihood, likelihood_class)) list(likelihood) else likelihood

  # -- 3. candidate grid -------------------------------------------------------
  grid <- list()
  for (lik in likelihoods) for (e in epis) for (d in delay_candidates)
    grid[[length(grid) + 1L]] <- model(likelihood = lik, epidemic = e, delay = d)
  # Append any user-supplied custom model() objects (e.g. with a custom_delay()
  # or custom_epidemic()) so they compete in the same backtest.
  if (!is.null(models)) {
    if (S7::S7_inherits(models, model_class)) models <- list(models)
    grid <- c(grid, models)
  }
  grid_labels <- vapply(grid, .model_label, character(1))
  keep        <- !duplicated(grid_labels)          # drop label collisions
  grid <- grid[keep]; grid_labels <- grid_labels[keep]

  if (verbose)
    cli::cli_inform(c("i" = paste0(
      "auto_nowcast: comparing {length(grid)} candidate model{?s} ",
      "({length(likelihoods)} likelihood{?s} x {length(epis)} epidemic process{?es} x ",
      "{length(delay_candidates)} delay{?s}",
      if (!is.null(models)) " + custom models" else "",
      ") over {n_dates} backtest date{?s}; max_time = {max_time}.")))

  # -- 4. backtest the grid and score through scoringutils ---------------------
  # The canonical backtest already knows its predictions, truth, origins and
  # method labels. Its scoringutils coercion is therefore the authoritative
  # route for both ordinary and relative scores.
  bt <- tryCatch(
    backtest(data, models = grid, type = type, n_dates = n_dates,
             n_draws = n_draws_select, K = K_select, seed = seed,
             verbose = verbose, ...),
    error = function(e) NULL)

  scores <- NULL
  if (!is.null(bt)) {
    forecast <- scoringutils::as_forecast_quantile(bt)
    scored <- scoringutils::score(forecast)
    available_metrics <- scoringutils::get_metrics(scored)
    if (!metric %in% available_metrics) {
      cli::cli_abort(c(
        "The requested {.arg metric} {.val {metric}} was not produced by scoringutils.",
        "i" = "Available score columns: {.val {available_metrics}}."
      ))
    }

    selection_column <- metric
    if (isTRUE(relative_score)) {
      selection_column <- paste0(metric, "_relative_skill")
      if (length(unique(scored$model)) > 1L) {
        scored <- scoringutils::add_relative_skill(
          scored, compare = "model", metric = metric, test_type = NULL
        )
      } else {
        scored[[selection_column]] <- 1
      }
    }

    score_frame <- as.data.frame(scored)
    metric_columns <- intersect(
      c(scoringutils::get_metrics(scored), selection_column),
      colnames(score_frame)
    )
    scores <- score_frame |>
      dplyr::group_by(.data$model) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(metric_columns),
          ~ mean(.x, na.rm = TRUE)
        ),
        .groups = "drop"
      )

    fit_times <- bt$timings |>
      dplyr::filter(.data$success) |>
      dplyr::group_by(.data$.method) |>
      dplyr::summarise(
        median_fit_seconds = stats::median(.data$elapsed_seconds),
        total_fit_seconds = sum(.data$elapsed_seconds),
        successful_fits = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::rename(model = ".method")

    candidate_info <- dplyr::tibble(
      model = grid_labels,
      epidemic_priority = vapply(grid, .auto_epidemic_priority, integer(1L)),
      grid_order = seq_along(grid)
    )
    scores <- scores |>
      dplyr::left_join(fit_times, by = "model") |>
      dplyr::left_join(candidate_info, by = "model") |>
      dplyr::mutate(selection_score = .data[[selection_column]])
  }

  # -- 5. rank, resolving effectively equal scores deterministically -----------
  if (is.null(scores) || nrow(scores) == 0) {
    ranked_labels <- character(0)
  } else {
    scores <- .rank_auto_scores(scores, tie_break = tie_break)
    ranked_labels <- scores$model
  }

  # -- 6. refit the best-ranked candidate that converges on the full data ------
  # Try candidates best-first and fall through on ANY fit failure, so
  # auto_nowcast converges whenever any candidate does (its grid contains the
  # plain models, so it is at least as robust as fitting them individually).
  # Candidates the scoreboard could not rank (e.g. scoring was inconclusive) are
  # appended in grid order as a last resort.
  ranked_idx <- match(ranked_labels, grid_labels)
  ranked_idx <- c(ranked_idx, setdiff(seq_along(grid), ranked_idx))
  ranked_idx <- ranked_idx[!is.na(ranked_idx)]

  nc <- NULL
  chosen_idx <- NA_integer_
  refit_timings <- list()
  for (i in ranked_idx) {
    refit_error <- NA_character_
    refit_started <- proc.time()[["elapsed"]]
    nc <- tryCatch(
      nowcast(data, model = grid[[i]], type = type, n_draws = n_draws,
              K = K, now = now, seed = seed, ...),
      error = function(e) {
        refit_error <<- conditionMessage(e)
        NULL
      })
    refit_timings[[length(refit_timings) + 1L]] <- dplyr::tibble(
      model = grid_labels[[i]],
      elapsed_seconds = unname(proc.time()[["elapsed"]] - refit_started),
      success = !is.null(nc),
      error = refit_error
    )
    if (!is.null(nc)) { chosen_idx <- i; break }
  }
  if (is.null(nc))
    cli::cli_abort(c(
      "auto_nowcast: every candidate model failed to fit on the full data.",
      "i" = "Try a different {.arg type}, longer/cleaner data, or a smaller grid."))

  winner_label <- grid_labels[chosen_idx]
  top_label    <- if (length(ranked_labels)) ranked_labels[[1L]] else winner_label
  refit_timings <- dplyr::bind_rows(refit_timings)
  total_elapsed <- unname(proc.time()[["elapsed"]] - auto_started)
  if (verbose) {
    if (!identical(winner_label, top_label))
      cli::cli_warn(c("!" = paste0(
        "auto_nowcast: best-scoring model {.val {top_label}} failed to refit on ",
        "the full data; using next-best {.val {winner_label}}.")))
    score_name <- if (isTRUE(relative_score)) paste("relative", metric) else metric
    cli::cli_inform(c(
      "v" = paste0(
        "auto_nowcast: selected {.strong {winner_label}} (best {score_name}; ",
        "ties by {tie_break}) in {round(total_elapsed, 2)} seconds."
      )
    ))
  }

  quantile_levels <- sort(unique(nc@predictions$.quantile_level))
  native_nc <- .unwrap_nowcast(nc)
  native_nc@comparison <- list(
    scores = scores,
    chosen = winner_label,
    metric = metric,
    relative_score = relative_score,
    tie_break = tie_break,
    timings = list(
      backtest = if (is.null(bt)) NULL else bt$timings,
      refit = refit_timings,
      total_seconds = total_elapsed
    ),
    max_time = max_time
  )
  .as_diseasenowcasting_result(
    native_nc,
    n_draws = native_nc@n_draws,
    quantile_levels = quantile_levels
  )
}

#' Epidemic-process preference used only for tied selection scores
#' @keywords internal
#' @noRd
.auto_epidemic_priority <- function(candidate) {
  epidemic <- candidate@epidemic
  if (S7::S7_inherits(epidemic, hsgp_epidemic_class)) return(1L)
  if (S7::S7_inherits(epidemic, ar1_epidemic_class)) return(2L)
  if (S7::S7_inherits(epidemic, sir_epidemic_class)) return(3L)
  4L
}

#' Rank auto-nowcast candidates, using policy only inside score ties
#' @keywords internal
#' @noRd
.rank_auto_scores <- function(scores, tie_break, tolerance = 1e-8) {
  scores <- scores[order(scores$selection_score, na.last = TRUE), , drop = FALSE]
  if (nrow(scores) < 2L) return(scores)

  tie_group <- integer(nrow(scores))
  group <- 1L
  anchor <- scores$selection_score[[1L]]
  tie_group[[1L]] <- group
  for (i in 2:nrow(scores)) {
    current <- scores$selection_score[[i]]
    same <- is.finite(anchor) && is.finite(current) &&
      abs(current - anchor) <= tolerance * max(1, abs(anchor), abs(current))
    if (!same) {
      group <- group + 1L
      anchor <- current
    }
    tie_group[[i]] <- group
  }
  scores$.tie_group <- tie_group

  order_one_group <- function(part) {
    if (identical(tie_break, "fastest")) {
      part[order(
        part$median_fit_seconds,
        part$epidemic_priority,
        part$grid_order,
        na.last = TRUE
      ), , drop = FALSE]
    } else {
      part[order(
        part$epidemic_priority,
        part$median_fit_seconds,
        part$grid_order,
        na.last = TRUE
      ), , drop = FALSE]
    }
  }

  pieces <- split(scores, scores$.tie_group)
  scores <- dplyr::bind_rows(lapply(pieces, order_one_group))
  scores$.tie_group <- NULL
  rownames(scores) <- NULL
  scores
}

# -----------------------------------------------------------------------------
# Accessors for an auto_nowcast() result
# -----------------------------------------------------------------------------

# Pull the `comparison` slot, erroring clearly if `nc` is a plain nowcast().
.auto_comparison <- function(nc) {
  nc <- .unwrap_nowcast(nc, "nc")
  cmp <- nc@comparison
  if (is.null(cmp))
    cli::cli_abort(c(
      "This nowcast carries no model-selection comparison.",
      "i" = "These accessors only work on the result of {.fn auto_nowcast}."))
  cmp
}

#' Name of the model chosen by `auto_nowcast()`
#'
#' @param nc A `nowcast_class` returned by [auto_nowcast()].
#' @returns The winning model's label, a string of the form
#'   `"epidemic/likelihood/delay"` (e.g. `"HSGP/nb/Dirichlet"`).
#' @seealso [auto_nowcast()], [best_model()], [comparison_scores()]
#' @export
best_model_name <- function(nc) {
  .auto_comparison(nc)$chosen
}

#' The winning [model()] object from a nowcast
#'
#' Returns the fitted nowcast's [model()] specification.  For an [auto_nowcast()]
#' result this is the **selected** model, so you can reuse it elsewhere, e.g.
#' `nowcast(other_data, model = best_model(nc))` or pass it to [backtest()].
#'
#' @param nc A `nowcast_class` (from [nowcast()] or [auto_nowcast()]).
#' @returns A `model_class` object.
#' @seealso [auto_nowcast()], [best_model_name()]
#' @export
best_model <- function(nc) {
  nc <- .unwrap_nowcast(nc, "nc")
  nc@model
}

#' The model-selection scoreboard from `auto_nowcast()`
#'
#' The ranked table of candidate models that [auto_nowcast()] backtested, one row
#' per model, best-first by the selected raw or relative scoringutils metric.
#' In addition to scoringutils metrics, it includes `selection_score`, median
#' and total retrospective fit seconds, successful-fit count, epidemic priority,
#' and original grid order.
#'
#' @param nc A `nowcast_class` returned by [auto_nowcast()].
#' @returns A `data.frame`, one row per candidate model.
#' @seealso [auto_nowcast()], [best_score()], [selection_timings()],
#'   [scoringutils::score()]
#' @export
comparison_scores <- function(nc) {
  .auto_comparison(nc)$scores
}

#' The metric `auto_nowcast()` used to pick the winner
#'
#' @param nc A `nowcast_class` returned by [auto_nowcast()].
#' @returns The scoringutils score-column name supplied to [auto_nowcast()].
#'   Consult the result's
#'   `relative_score` comparison field to determine whether its relative skill
#'   was used.
#' @seealso [auto_nowcast()], [comparison_scores()]
#' @export
selection_metric <- function(nc) {
  .auto_comparison(nc)$metric
}

#' The scoreboard row for the model `auto_nowcast()` chose
#'
#' The single [comparison_scores()] row belonging to the winning model,
#' including its scoringutils metrics, selection score, and retrospective fit
#' timing, rather than the whole table.
#'
#' @param nc A `nowcast_class` returned by [auto_nowcast()].
#' @returns A one-row `data.frame`.
#' @seealso [auto_nowcast()], [comparison_scores()], [best_model_name()]
#' @export
best_score <- function(nc) {
  cmp    <- .auto_comparison(nc)
  scores <- cmp$scores
  row    <- scores[scores$model == cmp$chosen, , drop = FALSE]
  rownames(row) <- NULL
  row
}

#' Fitting times recorded by `auto_nowcast()`
#'
#' @param nc A result returned by [auto_nowcast()].
#' @returns A list with `backtest` (one row per attempted retrospective fit),
#'   `refit` (the full-data refit attempts), and `total_seconds` for the complete
#'   automatic-selection call.
#' @seealso [auto_nowcast()], [comparison_scores()]
#' @export
selection_timings <- function(nc) {
  .auto_comparison(nc)$timings
}
