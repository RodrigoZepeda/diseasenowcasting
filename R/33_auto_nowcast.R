# =============================================================================
# auto_nowcast() -- pick the best model by backtesting, then fit it
# =============================================================================
# Builds a candidate grid of (epidemic process x delay family) models sized to
# the amount of data, backtests them over several historical dates, scores them
# (WIS or interval coverage), selects the winner, and refits it on the full data.
# =============================================================================

#' Automatically select and fit the best nowcasting model
#'
#' Takes a `tbl_now` and **chooses a model for you**: it builds a grid of
#' candidate models (epidemic process x reporting-delay family) sized to how much
#' data you have, [backtest()]s them over several historical dates, [score()]s
#' them, keeps the best one, and refits it on the full data.  The returned object
#' is an ordinary [nowcast()] result (so `autoplot()`, `predict()`, etc. work),
#' with the ranked scoreboard attached in its `comparison` slot.
#'
#' @details
#' **Candidate epidemic processes are chosen by series length** (`max_time`, the
#' number of event-times): the SIR process needs the least data, the HSGP the
#' most.  With the default thresholds:
#' \itemize{
#'   \item `max_time < min_ar`         -> compares `{SIR}`;
#'   \item `min_ar <= max_time < min_hsgp` -> compares `{SIR, AR(1)}`;
#'   \item `max_time >= min_hsgp`      -> compares `{AR(1), HSGP}`.
#' }
#' Any process you pass explicitly via `sir` / `ar` / `hsgp` is *always* included
#' (regardless of length), which is how you make a prior compete: e.g. pass
#' `sir = sir_epidemic(R0 = lognormal_prior(log(3), 0.2))` and the SIR candidate
#' will use that R0 prior throughout the comparison.
#'
#' **Candidate delays** default to LogNormal, Generalized-Gamma and Dirichlet;
#' override with `delays`.
#'
#' **Speed.** The grid is backtested with a fast configuration
#' (`n_draws_select` posterior draws over `n_dates` dates); only the winning model
#' is refit with the full `n_draws`.  Backtesting is the expensive step -- set a
#' `future::plan()` (e.g. `future::plan(multisession)`) for parallel speed-up.
#'
#' @param data A `tbl_now` object (`tbl.now::tbl_now()`).
#' @param metric Selection criterion. `"wis"` (default, lowest Weighted Interval
#'   Score), `"ape"` (lowest absolute percentage error of the median), `"mse"`
#'   (lowest mean squared error), or one of the calibration criteria, which pick
#'   the model whose empirical interval coverage is closest to nominal:
#'   `"coverage_50"` (smallest `|0.50 - coverage_50|`), `"coverage_90"` (smallest
#'   `|0.90 - coverage_90|`), or `"coverage"` (smallest
#'   `|0.50 - coverage_50| + |0.90 - coverage_90|`, i.e. both intervals jointly).
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
#' @param K Delay imputations for two-stage fits (default 25).
#' @param min_ar,min_hsgp Series-length thresholds (in event-times) at which
#'   AR(1) and HSGP become candidates (defaults 15 and 30).
#' @param now As-of date for the final fit (default: the `tbl_now`'s `now`).
#' @param seed RNG seed.
#' @param verbose Print progress and the chosen model (default `TRUE`).
#' @param ... Passed through to [backtest()] and [nowcast()] (e.g.
#'   `temporal_effects`).
#'
#' @returns A `nowcast_class` (as from [nowcast()]) for the selected model, with
#'   the model-selection scoreboard in its `comparison` slot:
#'   `list(scores, chosen, metric, max_time)`.
#'
#' @seealso [nowcast()], [backtest()], [score()]
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
                         metric = c("wis", "ape", "mse", "coverage",
                                    "coverage_50", "coverage_90"),
                         type   = c("auto", "two_stage", "one_stage"),
                         sir = NULL, ar = NULL, hsgp = NULL,
                         delays = NULL, likelihood = nb_likelihood(),
                         models = NULL,
                         n_dates = 6L, n_draws_select = 500L,
                         n_draws = 2000L, K = 25L,
                         min_ar = 15L, min_hsgp = 30L,
                         now = NULL, seed = sample.int(.Machine$integer.max, 1),
                         verbose = TRUE, ...) {
  metric <- match.arg(metric)
  type   <- match.arg(type)
  if (!is.null(seed)) set.seed(seed)

  # -- 1. candidate epidemic processes, sized to the series length -------------
  max_time <- prepare_from_tbl_now(data, diseasenowcasting::model(), now = now)$max_time
  epis <- list()
  if (max_time < min_ar) {
    epis[["SIR"]]  <- sir  %||% sir_epidemic()
  } else if (max_time < min_hsgp) {
    epis[["SIR"]]  <- sir  %||% sir_epidemic()
    epis[["AR1"]]  <- ar   %||% ar1_epidemic()
  } else {
    epis[["AR1"]]  <- ar   %||% ar1_epidemic()
    epis[["HSGP"]] <- hsgp %||% hsgp_epidemic()
  }
  # Any explicitly supplied process is forced in (so its priors compete).
  if (!is.null(sir)  && is.null(epis[["SIR"]]))  epis[["SIR"]]  <- sir
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

  # -- 4. backtest the grid (fast config) and score ----------------------------
  bt <- backtest(data, models = grid, type = type, n_dates = n_dates,
                 n_draws = n_draws_select, K = K, seed = seed, ...)
  # score() returns one row per model with wis, ape, mse, coverage_50/90.
  scores <- score(bt, metric = "wis", report = FALSE)

  # -- 5. pick the winner ------------------------------------------------------
  # wis / ape / mse: smaller is better.  Coverage metrics: smallest miss from the
  # nominal level -- coverage_50 |0.50 - cov50|, coverage_90 |0.90 - cov90|, and
  # coverage the combined |0.50 - cov50| + |0.90 - cov90|.
  pick <- switch(metric,
    wis         = which.min(scores$wis),
    ape         = which.min(scores$ape),
    mse         = which.min(scores$mse),
    coverage_50 = which.min(abs(scores$coverage_50 - 0.50)),
    coverage_90 = which.min(abs(scores$coverage_90 - 0.90)),
    coverage    = which.min(abs(scores$coverage_50 - 0.50) +
                            abs(scores$coverage_90 - 0.90)))
  winner_label <- if (length(pick) == 1L && !is.na(pick)) scores$model[pick] else NA_character_
  idx <- match(winner_label, grid_labels)
  if (is.na(idx)) {                       # scoring inconclusive -> safe fallback
    idx <- 1L; winner_label <- grid_labels[1L]
    cli::cli_warn("auto_nowcast: scoring was inconclusive; falling back to {.val {winner_label}}.")
  }
  winner_model <- grid[[idx]]
  if (verbose)
    cli::cli_inform(c("v" = "auto_nowcast: selected {.strong {winner_label}} (best {metric})."))

  # -- 6. refit the winner on the full data, full draws ------------------------
  nc <- nowcast(data, model = winner_model, type = type, n_draws = n_draws,
                now = now, seed = seed, ...)
  nc@comparison <- list(scores = scores, chosen = winner_label,
                        metric = metric, max_time = max_time)
  nc
}

# -----------------------------------------------------------------------------
# Accessors for an auto_nowcast() result
# -----------------------------------------------------------------------------

# Pull the `comparison` slot, erroring clearly if `nc` is a plain nowcast().
.auto_comparison <- function(nc) {
  if (!S7::S7_inherits(nc, nowcast_class))
    cli::cli_abort("{.arg nc} must be a {.cls nowcast_class} object (from {.fn auto_nowcast}).")
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
  if (!S7::S7_inherits(nc, nowcast_class))
    cli::cli_abort("{.arg nc} must be a {.cls nowcast_class} object.")
  nc@model
}

#' The model-selection scoreboard from `auto_nowcast()`
#'
#' The ranked table of candidate models that [auto_nowcast()] backtested, one row
#' per model, best-first by the selection `metric`.  Columns include `model` (the
#' label), `wis` (and its decomposition), `ape`, `mse`, and `coverage_50` /
#' `coverage_90` (see [score()]).
#'
#' @param nc A `nowcast_class` returned by [auto_nowcast()].
#' @returns A `data.frame`, one row per candidate model.
#' @seealso [auto_nowcast()], [best_score()], [score()]
#' @export
comparison_scores <- function(nc) {
  .auto_comparison(nc)$scores
}

#' The metric `auto_nowcast()` used to pick the winner
#'
#' @param nc A `nowcast_class` returned by [auto_nowcast()].
#' @returns A string: `"wis"`, `"ape"`, `"mse"`, `"coverage"`, `"coverage_50"`,
#'   or `"coverage_90"`.
#' @seealso [auto_nowcast()], [comparison_scores()]
#' @export
selection_metric <- function(nc) {
  .auto_comparison(nc)$metric
}

#' The scoreboard row for the model `auto_nowcast()` chose
#'
#' The single [comparison_scores()] row belonging to the winning model -- its
#' WIS, APE, MSE and interval coverage -- rather than the whole table.
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
