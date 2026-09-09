#' Native and cross-engine workflows
#'
#' `diseasenowcasting` owns the statistical model and RTMB fit;
#' [tbl.now::tbl_nowcast] owns the common result grammar. The public result of
#' [nowcast()] and [auto_nowcast()] therefore supports both layers without an
#' explicit conversion.
#'
#' @section Use the native layer for modelling:
#'
#' Use [model()], [nowcast()], and [auto_nowcast()] when you need this package's
#' epidemic, delay, likelihood, revision, or cumulative-process components.
#' Use [fit_check()] and [nowcast_diagnostic()] for RTMB-specific convergence and
#' fit diagnostics. These functions automatically unwrap the retained native
#' fit from the common result.
#'
#' [backtest()] is a native model-specification convenience, not a second
#' evaluation system: it translates each [model()] into a
#' [tbl.now::engine_diseasenowcasting()] and returns
#' [tbl.now::nowcast_backtest()] directly.
#'
#' @section Use the common layer for results and comparison:
#'
#' Use [tbl.now::run_nowcast()] with
#' [tbl.now::engine_diseasenowcasting()] when diseasenowcasting is one of several
#' engines. Once a fit exists, use the common methods for `tidy()`, `autoplot()`,
#' predictive scoring, forecast conversion, ensembling, and persistence.
#'
#' In particular, a backtest can be passed directly to
#' [scoringutils::as_forecast_quantile()],
#' [scoringutils::as_forecast_point()], or, when draws were retained,
#' [scoringutils::as_forecast_sample()]. Relative WIS and additional scores then
#' come from [scoringutils::score()] and
#' [scoringutils::add_relative_skill()], rather than a package-local scorer.
#'
#' @examples
#' \dontrun{
#' fit <- nowcast(data, model = model())
#' autoplot(fit)                  # common result plot
#' fit_check(fit)                 # native RTMB diagnostics
#'
#' bt <- backtest(data, models = list(default = model()))
#' relative <- bt |>
#'   scoringutils::as_forecast_quantile() |>
#'   scoringutils::score() |>
#'   scoringutils::add_relative_skill(metric = "wis")
#' }
#'
#' @name diseasenowcasting_workflows
NULL
