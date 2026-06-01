# =============================================================================
# score() -- WIS / APE / MSE for a backtest, with multi-model ranking
# =============================================================================
# Scores the d*=0 nowcast (the newest event per as-of date) against the eventual
# truth.  WIS via scoringutils; APE (mean absolute percentage error) and MSE
# (mean squared error of the median point forecast) computed directly.  With
# several models, ranks them by the chosen metric and reports via cli.
# =============================================================================

#' Score a backtest: WIS, APE, MSE per model (and rank them)
#'
#' @param object A `backtest_class`.
#' @param metric Metric to RANK models by: `"wis"` (default), `"ape"`, or `"mse"`.
#'   All three are always reported; `metric` only chooses the ranking.
#' @param report If TRUE (default), print the ranked comparison via cli.
#' @returns A data.frame, one row per model, with `wis`, `ape`, `mse`,
#'   `coverage_50`, `coverage_90`, sorted best-first by `metric`.
#' @export
score <- function(object, metric = c("wis", "ape", "mse"), report = TRUE) {
  stopifnot(S7::S7_inherits(object, backtest_class))
  metric <- match.arg(metric)
  results <- object@results
  if (is.null(results) || nrow(results) == 0) cli::cli_abort("Backtest has no results to score.")

  quantile_names <- c("q2.5", "q5", "q10", "q25", "q50", "q75", "q90", "q95", "q97.5")
  quantile_levels <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)

  # d*=0 target: the newest event per (model, date_run).
  results <- results[is.finite(results$final), , drop = FALSE]
  newest <- do.call(rbind, by(results, list(results$model, results$date_run), function(block) {
    block[which.max(block$.event_num), , drop = FALSE]
  }))

  # -- WIS via scoringutils ----------------------------------------------------
  long <- do.call(rbind, lapply(seq_along(quantile_names), function(j) {
    data.frame(model = newest$model, date_run = newest$date_run, observed = newest$final,
               predicted = newest[[quantile_names[j]]], quantile_level = quantile_levels[j])
  }))
  wis_tbl <- tryCatch({
    forecast <- scoringutils::as_forecast_quantile(long, observed = "observed",
      predicted = "predicted", quantile_level = "quantile_level",
      forecast_unit = c("model", "date_run"))
    scored <- scoringutils::score(forecast)
    agg <- scoringutils::summarise_scores(scored, by = "model")
    data.frame(model = agg$model, wis = agg$wis,
               coverage_50 = agg$interval_coverage_50, coverage_90 = agg$interval_coverage_90)
  }, error = function(e) {
    cli::cli_warn("scoringutils WIS failed: {conditionMessage(e)}")
    data.frame(model = unique(newest$model), wis = NA_real_, coverage_50 = NA_real_, coverage_90 = NA_real_)
  })

  # -- APE (mean abs % error) and MSE (of the median point forecast) -----------
  point_tbl <- do.call(rbind, by(newest, newest$model, function(block) {
    point <- block$q50
    ape <- mean(abs(point - block$final) / pmax(block$final, 1), na.rm = TRUE)
    mse <- mean((point - block$final)^2, na.rm = TRUE)
    data.frame(model = block$model[1], ape = ape, mse = mse, n = nrow(block))
  }))

  out <- merge(wis_tbl, point_tbl, by = "model")
  out <- out[order(out[[metric]]), , drop = FALSE]
  rownames(out) <- NULL

  if (report) {
    cli::cli_h2("Backtest scores (d*=0, ranked by {metric}, n = {max(out$n)} dates)")
    for (i in seq_len(nrow(out))) {
      marker <- if (i == 1) cli::col_green(cli::symbol$tick) else " "
      cli::cli_text("{marker} {out$model[i]}: WIS {round(out$wis[i], 2)} | APE {round(out$ape[i], 3)} | ",
                    "MSE {round(out$mse[i], 1)} | cov50 {round(out$coverage_50[i], 2)} cov90 {round(out$coverage_90[i], 2)}")
    }
    cli::cli_alert_success("Best by {metric}: {out$model[1]}")
  }
  out
}
