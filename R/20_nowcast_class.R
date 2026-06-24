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
    n_draws = S7::class_numeric, # default posterior draws for predict/summaries
    # Model-selection scoreboard, set by auto_nowcast() (NULL for a plain fit):
    # list(scores = <ranked data.frame>, chosen = <label>, metric = <chr>).
    comparison = S7::new_property(S7::class_any, default = NULL)
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
#' @param type `"two_stage"` (default; delay-imputation pooling), `"one_stage"`
#'   (a single joint fit), or `"auto"` (per delay: dirichlet one-stage, all other
#'   delays two-stage -- the better choice for each in our experiments).
#' @param now As-of date; only events/reports up to `now` are used.  Default:
#'   `tbl.now::get_now(data)`, falling back to the latest report date.
#' @param K Number of delay imputations for the two-stage path.
#' @param n_draws Default number of posterior draws used by [predict()] and the
#'   latent-incidence summaries.
#' @param delay_window Recent window length for the parametric Stage-1 delay fit.
#' @param np_spread Dirichlet simplex imputation covariance inflation (default 1).
#' @param floor_mu,floor_sig_frac Imputation-spread floors (parametric families).
#' @param temporal_effects Controls automatic seasonal / day-of-week covariates.
#'   `"auto"` (default) adds sensible effects based on the data's time unit
#'   (weekly -> 52-period seasonality; daily -> day-of-week + 52-period
#'   seasonality; monthly -> 12-period seasonality) **only if the `tbl_now`
#'   does not already carry computed temporal effects**.  Use `"none"` (or
#'   `"None"`) to disable, or pre-attach your own effects to the `tbl_now` with
#'   `tbl.now::add_temporal_effects()` + `tbl.now::compute_temporal_effects()`.
#' @param prior_only If `TRUE`, ignore the likelihood and draw the epidemic
#'   parameters from their **priors** only, returning the prior-predictive latent
#'   incidence.  Useful for understanding what a prior implies *before* seeing
#'   data (e.g. how the SIR `R0` prior or the AR(1) `phi` prior reshapes the
#'   epidemic).  The result is a normal `nowcast_class`, so `predict()` /
#'   `autoplot()` / `median()` / `quantile()` all work; `data` only supplies the
#'   time grid.  Default `FALSE`.
#' @param seed Optional RNG seed (imputation draws).
#' @param ... Passed to [prepare_data()] (e.g. `gp_boundary_frac`).
#' @returns A `nowcast_class` object.
#'
#' @section Overdispersion (`phi`):
#' The negative-binomial overdispersion prior is **not** an argument of
#' `nowcast()`.  Set it on the likelihood instead, e.g.
#' `model(nb_likelihood(phi = lognormal_prior(log(5), 0.5)), ...)`.  The default
#' `nb_likelihood()` already uses `lognormal_prior(log(20), 0.5)`.
#'
#' @examples
#' if (requireNamespace("tbl.now", quietly = TRUE)) {
#'   # data <- tbl.now::tbl_now(my_linelist, event_date = onset, report_date = reported)
#'   # nc <- nowcast(data, model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()))
#'   # predict(nc); median(nc); coef(nc)
#' }
#' @export
nowcast <- function(data, model = diseasenowcasting::model(),
                    type = c("two_stage", "one_stage", "auto"), now = NULL,
                    K = 25L, n_draws = 2000L, delay_window = 120L, np_spread = 1,
                    floor_mu = 0.08, floor_sig_frac = 0.08,
                    temporal_effects = "auto", prior_only = FALSE,
                    seed = sample.int(.Machine$integer.max, 1), ...) {
  type <- match.arg(type)
  if (!is.null(seed)) set.seed(seed)
  # The NB overdispersion prior lives on the likelihood, not on nowcast().
  phi <- .likelihood_phi(model)
  # prior_only: don't auto-add temporal effects (keep the prior epidemic clean).
  data <- .apply_default_temporal_effects(data, if (isTRUE(prior_only)) "none" else temporal_effects)
  prepared <- prepare_from_tbl_now(data, model, now = now, delay_only = FALSE, ...)
  engine   <- prepared$data
  priors   <- default_priors(model, engine)

  if (isTRUE(prior_only)) {
    # Draw epidemic parameters from the PRIORS (no fitting) and reconstruct the
    # prior-predictive latent incidence -- "what does this prior yield?".
    hb   <- .hsgp_basis_for_engine(engine)
    sims <- .simulate_prior_draws(engine, priors, n_draws = as.integer(n_draws),
                                  Bmat = hb$Bmat, freq = hb$freq, seed = seed)
    prior_fit <- list(data = engine, priors = priors, prior_only = TRUE,
                      prior_sims = sims, Bmat = hb$Bmat, freq = hb$freq,
                      delay_mu = NA_real_, delay_sigma = NA_real_, phi_nb = NA_real_,
                      parList = list())
    collected <- list(fits = list(prior_fit), rung = "prior", target = engine$max_time)
  } else {
    collected <- .collect_nowcast_fits(model, engine, priors, type = type, K = K,
                                       floor_mu = floor_mu, floor_sig_frac = floor_sig_frac,
                                       np_spread = np_spread, delay_window = delay_window)
  }
  # Augment the engine list with date/strata metadata so autoplot() can recover
  # real calendar dates and stratum names without needing the tbl_now.
  engine$min_event    <- prepared$min_event
  engine$event_unit   <- as.character(prepared$event_unit)
  engine$strata_levels <- prepared$strata_levels
  nowcast_class(model = model, data = data, now = prepared$now,
                type = if (isTRUE(prior_only)) "prior_only" else type,
                fits = collected$fits, rung = collected$rung, target = collected$target,
                engine = engine, priors = priors, phi = phi, n_draws = as.integer(n_draws))
}

#' The NB overdispersion prior carried by a model's likelihood (or `NULL`).
#' @keywords internal
#' @noRd
.likelihood_phi <- function(model) {
  lik <- model@likelihood
  if (S7::S7_inherits(lik, nb_likelihood_class)) lik@phi else NULL
}

#' Apply default seasonal / day-of-week temporal effects to a tbl_now
#'
#' Foolproof default: unless the user opts out (`temporal_effects = "none"`) or
#' has already attached their own effects, sensible covariates are added based
#' on the data's time unit and a message is emitted.
#' @keywords internal
#' @noRd
.apply_default_temporal_effects <- function(data, temporal_effects = "auto") {
  # Opt-out
  if (is.character(temporal_effects) &&
      length(temporal_effects) == 1L &&
      tolower(temporal_effects) %in% c("none", "off", "no", "false")) {
    return(data)
  }
  if (!tbl.now::is_tbl_now(data)) return(data)

  # Respect any temporal effects the user has attached -- whether already
  # COMPUTED (effect columns present) or merely SPECIFIED (a spec attached via
  # `tbl_now(t_effects = ...)` / `add_temporal_effects()` but not yet computed).
  existing_cols <- tryCatch(tbl.now::get_temporal_effect_cols(data), error = function(e) character(0))
  if (length(existing_cols) > 0L) return(data)            # already computed -> use as-is
  existing_spec <- tryCatch(tbl.now::get_temporal_effects(data), error = function(e) NULL)
  if (!is.null(existing_spec) && length(existing_spec) > 0L) {
    # Spec attached but not computed: materialise the columns so the engine can
    # use them.  No defaults are added and no message is emitted.
    return(tryCatch(tbl.now::compute_temporal_effects(data), error = function(e) data))
  }

  unit <- tryCatch(as.character(tbl.now::get_event_units(data)), error = function(e) "day")

  # Choose effects by time unit
  if (grepl("^day", unit)) {
    eff   <- tbl.now::temporal_effects(day_of_week = TRUE)
    descr <- "day-of-week + 52-period seasonality (daily data)"
  } else if (grepl("^week", unit)) {
    eff   <- tbl.now::temporal_effects(seasons = 52)
    descr <- "52-period seasonality (weekly data)"
  } else if (grepl("^month", unit)) {
    eff   <- tbl.now::temporal_effects(seasons = 12)
    descr <- "12-period seasonality (monthly data)"
  } 

  out <- tryCatch({
    data |>
      tbl.now::add_temporal_effects(eff) |>
      tbl.now::compute_temporal_effects()
  }, error = function(e) {
    cli::cli_warn("Could not add default temporal effects: {conditionMessage(e)}")
    data
  })

  cli::cli_inform(c(
    "i" = "Added default temporal effects: {descr}.",
    "*" = "To use your own effects, attach them to the {.cls tbl_now} with {.fn tbl.now::add_temporal_effects} + {.fn tbl.now::compute_temporal_effects} before calling {.fn nowcast}.",
    "*" = "To disable, call {.code nowcast(..., temporal_effects = \"none\")}."
  ))
  out
}

#' The posterior-predictive nowcast (returned by [predict()] on a nowcast)
#' @keywords internal
#' @noRd
nowcast_prediction_class <- S7::new_class(
  "nowcast_prediction",
  properties = list(
    draws            = S7::class_any,    # [n_draws x max_time] TOTAL predictive count matrix
    target           = S7::class_numeric,
    observed         = S7::class_numeric, # observed total at the target event
    event_index      = S7::class_numeric, # 0-indexed event numbers (columns of draws)
    strata_draws     = S7::class_any,     # [n_draws x max_time x n_strata] per-stratum (or NULL)
    strata_levels    = S7::class_any,     # character vector of stratum labels (or NULL)
    event_dates      = S7::class_any,     # Date vector length max_time (or NULL)
    observed_series  = S7::class_any,     # [max_time] observed total per event-time (or NULL)
    observed_strata  = S7::class_any      # [max_time x n_strata] observed per stratum (or NULL)
  )
)
