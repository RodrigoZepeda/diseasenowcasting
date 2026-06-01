# =============================================================================
# nowcast S7 class + the main user-facing nowcast() call
# =============================================================================
# nowcast() takes a tbl_now and a model(), FITS the nowcast model (one- or
# two-stage), and returns a `nowcast` object holding the fit(s).  It does NOT
# draw the posterior-predictive nowcast -- that is deferred to predict() (and the
# latent-incidence summaries mean()/median()/quantile()).
# =============================================================================

#' The fitted nowcast object
#' @keywords internal
#' @noRd
nowcast_class <- S7::new_class(
  "nowcast",
  properties = list(
    model  = model_class,
    data   = S7::class_any,      # the input tbl_now
    now    = S7::class_any,      # as-of date
    type   = S7::class_character,
    fits   = S7::class_list,     # list of underlying RTMB fit objects
    rung   = S7::class_character,
    target = S7::class_numeric,  # newest event-time index
    engine = S7::class_list,     # prepared-data list (prepare_data output)
    priors = S7::class_list,
    phi    = S7::class_any,      # NB overdispersion prior (for update())
    n_draws = S7::class_numeric  # default posterior draws for predict/summaries
  )
)

#' Fit a nowcast model to censored reporting data
#'
#' The main entry point.  Takes a `tbl_now` (from the tbl.now package) and a
#' [model()], fits the latent-epidemic + reporting-delay model as of a given
#' date, and returns a `nowcast_class` object.  Fitting only -- the
#' posterior-predictive nowcast is produced lazily by [predict()]; the latent
#' incidence by [mean()]/[median()]/[quantile()]; the parameter estimates by
#' [coef()].
#'
#' @param data A `tbl_now` object (`tbl.now::tbl_now()`).
#' @param model A [model()] object.  Default: `model()` (NB + HSGP + Dirichlet).
#' @param type `"two_stage"` (default; delay-imputation pooling) or `"one_stage"`
#'   (a single joint fit).
#' @param now As-of date; only events/reports up to `now` are used.  Default:
#'   `tbl.now::get_now(data)`, falling back to the latest report date.
#' @param K Number of delay imputations for the two-stage path.
#' @param n_draws Default number of posterior draws used by [predict()] and the
#'   latent-incidence summaries.
#' @param delay_window Recent window length for the parametric Stage-1 delay fit.
#' @param np_spread Dirichlet simplex imputation covariance inflation (default 1).
#' @param floor_mu,floor_sig_frac Imputation-spread floors (parametric families).
#' @param phi NB overdispersion prior (default `lognormal_prior(log(20), 0.5)`).
#' @param seed Optional RNG seed (imputation draws).
#' @param ... Passed to [prepare_data()] (e.g. `gp_boundary_frac`).
#' @returns A `nowcast_class` object.
#'
#' @examples
#' if (requireNamespace("tbl.now", quietly = TRUE)) {
#'   # data <- tbl.now::tbl_now(my_linelist, event_date = onset, report_date = reported)
#'   # nc <- nowcast(data, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()))
#'   # predict(nc); median(nc); coef(nc)
#' }
#' @export
nowcast <- function(data, model = dcast3::model(),
                    type = c("two_stage", "one_stage"), now = NULL,
                    K = 25L, n_draws = 2000L, delay_window = 120L, np_spread = 1,
                    floor_mu = 0.15, floor_sig_frac = 0.25,
                    phi = lognormal_prior(log(20), 0.5), seed = NULL, ...) {
  type <- match.arg(type)
  if (!is.null(seed)) set.seed(seed)
  prepared <- prepare_from_tbl_now(data, model, now = now, delay_only = FALSE, ...)
  engine   <- prepared$data
  priors   <- default_priors(model, engine, phi = phi)
  collected <- .collect_nowcast_fits(model, engine, priors, type = type, K = K,
                                     floor_mu = floor_mu, floor_sig_frac = floor_sig_frac,
                                     np_spread = np_spread, delay_window = delay_window)
  # Augment the engine list with date/strata metadata so autoplot() can recover
  # real calendar dates and stratum names without needing the tbl_now.
  engine$min_event    <- prepared$min_event
  engine$event_unit   <- as.character(prepared$event_unit)
  engine$strata_levels <- prepared$strata_levels
  nowcast_class(model = model, data = data, now = prepared$now, type = type,
                fits = collected$fits, rung = collected$rung, target = collected$target,
                engine = engine, priors = priors, phi = phi, n_draws = as.integer(n_draws))
}

#' The posterior-predictive nowcast (returned by [predict()] on a nowcast)
#' @keywords internal
#' @noRd
nowcast_prediction_class <- S7::new_class(
  "nowcast_prediction",
  properties = list(
    draws        = S7::class_any,    # [n_draws x max_time] predictive count matrix
    target       = S7::class_numeric,
    observed     = S7::class_numeric,
    event_index  = S7::class_numeric # 0-indexed event numbers (columns of draws)
  )
)
