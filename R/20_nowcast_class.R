# =============================================================================
# nowcast S7 class + the main user-facing nowcast() call
# =============================================================================
# nowcast() takes a tbl_now and a model(), fits the native model, eagerly draws
# its public prediction, and returns a diseasenowcasting subclass of
# tbl.now::tbl_nowcast. The untouched native object is retained in `@fit`.
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
    fit_diagnostics = S7::new_property(S7::class_list, default = list()),
    # Which outcomes the fit modelled: "none", "confirmation_only",
    # "retraction_only" or "both".  Recorded rather than re-derived so a saved fit
    # reports the mode it was FITTED under, even if its data are later subset.
    revision_mode = S7::new_property(S7::class_character, default = "none"),
    # Model-selection scoreboard, set by auto_nowcast() (NULL for a plain fit):
    # list(scores = <ranked data.frame>, chosen = <label>, metric = <chr>).
    comparison = S7::new_property(S7::class_any, default = NULL)
  )
)

#' Fit a nowcast model to censored reporting data
#'
#' The main entry point.  Takes a `tbl_now` (from the tbl.now package) and a
#' [model()], fits the latent-epidemic + reporting-delay model as of a given
#' date, and returns the common [tbl.now::tbl_nowcast] result. Predictive draws
#' and quantiles are materialised in that result; the untouched native fit is
#' retained in `@fit` for [predict()], [mean()]/[median()]/[quantile()],
#' [coef()], and model-specific diagnostics.
#'
#' @param data A `tbl_now` object (`tbl.now::tbl_now()`).
#' @param model A [model()] object.  Default: `model()` (NB + HSGP + Dirichlet).
#' @param type `"two_stage"` (default; reporting-delay imputation pooling),
#'   `"one_stage"` (a single joint fit), or `"auto"` (per reporting-delay
#'   family: Dirichlet one-stage, all other delays two-stage -- the better choice
#'   for each in our experiments).
#' @param now As-of date; only events/reports up to `now` are used.  Default:
#'   `tbl.now::get_now(data)`, falling back to the latest report date.
#' @param K Number of delay imputations for the two-stage path.
#' @param n_draws Default number of posterior draws used by [predict()] and the
#'   latent-incidence summaries.
#' @param delay_window Recent window length for the parametric Stage-1 delay fit.
#' @param np_spread Dirichlet simplex imputation covariance inflation (default 1).
#' @param floor_mu,floor_sig_frac Imputation-spread floors (parametric families).
#' @section Revision processes:
#' A revision process -- reports that are later **confirmed** or **retracted** --
#' is a row-level mechanism for linelist and count-incidence data. It is detected
#' from the data, not requested by an argument. `nowcast()` attaches one when the
#' `tbl_now` carries `revision_date` / `revision_type` (see
#' `tbl.now::add_revision_date()`).  The mode
#' (`confirmation_only` / `retraction_only` / `both`) is read from
#' `unique(revision_type)` over the full data, so it is stable across as-of
#' dates.  Configure `p` and the lag with
#' `model(revision = revision_process(...))`, which always wins over the
#' detected default; assert the mode with `revision_process(mode = )`.
#' Revision-date censoring is likewise data metadata: set
#' `is_censored_revision` when constructing the `tbl_now`. There is no
#' `nowcast()` column-name argument for it.
#' Count-cumulative revisions instead use [cumulative_process()] and do
#' not estimate a separate revision probability `p`.
#'
#' @section One-stage and two-stage revision:
#' With `type = "one_stage"`, the epidemic process, event-to-report delay,
#' report-to-revision delay, and revision probability `p` are estimated in
#' one joint objective. With `type = "two_stage"`, Stage 1 estimates the
#' event-to-report delay and draws `K` imputations from its Laplace
#' approximation. Each Stage-2 fit conditions on one imputed reporting-delay
#' distribution and jointly estimates the epidemic process, revision delay,
#' and `p`. Posterior draws within a Stage-2 fit propagate revision uncertainty;
#' pooling across the fits additionally propagates reporting-delay uncertainty.
#' This is the same stepwise boundary used by the original event-to-report model:
#' `two_stage` separates the reporting process from the downstream model, while
#' the revision block remains downstream of a report.
#'
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
#'   epidemic). The result uses the same common grammar as an ordinary fit, so
#'   `predict()` / `autoplot()` / `median()` / `quantile()` all work; `data`
#'   only supplies the time grid. Default `FALSE`.
#' @param quantile_levels Probabilities at which to summarise the predictive
#'   draws in the returned [tbl.now::tbl_nowcast].
#' @param seed Optional RNG seed (imputation draws).
#' @param ... Passed to [prepare_data()] (e.g. `gp_boundary_frac`).
#' @returns A diseasenowcasting subclass of [tbl.now::tbl_nowcast]. The native
#'   fitted model is retained in `@fit`; diseasenowcasting operations unwrap it
#'   automatically. The `@fit_diagnostics` property (also available at
#'   `@metadata$diseasenowcasting$fit_diagnostics`) records the resolved fitting
#'   stage, imputation retention, optimizer adequacy, curvature, and any
#'   Laplace-precision regularization used for prediction.
#'
#' @section Overdispersion (`phi`):
#' The negative-binomial overdispersion prior is **not** an argument of
#' `nowcast()`.  Set it on the likelihood instead, e.g.
#' `model(nb_likelihood(phi = lognormal_prior(log(5), 0.5)), ...)`.  The default
#' `nb_likelihood()` already uses `lognormal_prior(log(20), 0.5)`.
#'
#' @seealso [diseasenowcasting_workflows] for when to use native modelling
#'   operations versus the shared `tbl.now` result workflow; [backtest()] and
#'   [auto_nowcast()] for retrospective comparison and automatic selection.
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
                    temporal_effects = "auto",
                    prior_only = FALSE,
                    quantile_levels = tbl.now::nowcast_quantile_levels(),
                    seed = sample.int(.Machine$integer.max, 1), ...) {
  if ("revision_censored" %in% names(list(...))) {
    cli::cli_abort(c(
      "{.arg revision_censored} is not a {.fn nowcast} argument.",
      "i" = "Attach the censoring column to the {.cls tbl_now} with {.arg is_censored_revision}."
    ))
  }
  type <- match.arg(type)
  if (!is.null(seed)) set.seed(seed)
  # The NB overdispersion prior lives on the likelihood, not on nowcast().
  phi <- .likelihood_phi(model)
  # -- revision process: detected, not requested ----------------------------
  # Report-level revision is detected from revision_date/revision_type.
  # Count-cumulative down-revisions use the separate cumulative_process;
  # they do not identify a report-level `p`.
  is_cumulative  <- identical(tbl.now::get_data_type(data), "count-cumulative")
  has_revision <- isTRUE(.tblnow_has_revision(data))
  revision_mode <- "none"

  if (has_revision) {
    revision_mode <- .resolve_revision_mode(data, model@revision)
    # Count-cumulative cannot see confirmations: eq. `noconfirmcum` sets
    # g^val_{D+} = 0, because a confirmation does not change a cumulative count.
    if (is_cumulative && !revision_mode %in% c("retraction_only", "none"))
      cli::cli_abort(c(
        "A count-cumulative stream cannot carry {.val confirmed} revisions.",
        "x" = "A confirmation does not change a cumulative count, so the confirmation-delay parameters are unidentifiable.",
        "i" = "Only the DOWN-revisions are informative here; keep the retractions and drop the confirmations, or model the data as {.val count-incidence}."))
  }

  if (!is_cumulative && isTRUE(model@cumulative@active)) {
    cli::cli_abort(c(
      "A {.fn cumulative_process} can only be used with {.val count-cumulative} data.",
      "x" = "The supplied data type is {.val {tbl.now::get_data_type(data)}}."
    ))
  }

  if (is_cumulative && isTRUE(model@revision@active)) {
    cli::cli_abort(c(
      "A {.fn revision_process} cannot be used with {.val count-cumulative} data.",
      "i" = "Count-cumulative data consume signed changes in the cumulative trajectory only.",
      "*" = "Use {.code model(cumulative = cumulative_process(...))}."
    ))
  }

  if (is_cumulative && !isTRUE(model@cumulative@active)) {
    cumulative_process <- cumulative_process(
      observation = "hurdle_ztnb",
      retraction_delay = lognormal_delay(),
      settlement = 26L,
      retraction_mass = beta_prior(1.5, 20)
    )
    model <- model_class(
      likelihood = model@likelihood,
      epidemic = model@epidemic,
      delay = model@delay,
      revision = no_revision(),
      covariate_prior = model@covariate_prior,
      strata_pooling = model@strata_pooling,
      cumulative = cumulative_process
    )
    cli::cli_inform(c(
      "i" = "Using the default signed hurdle--ZTNB count-cumulative model with a 26-step settlement horizon.",
      "*" = "Configure it with {.code model(cumulative = cumulative_process(...))}."
    ))
  }

  promote <- !is_cumulative && has_revision &&
    !identical(revision_mode, "none")
  if (promote && !isTRUE(model@revision@active)) {
    model <- model_class(likelihood = model@likelihood, epidemic = model@epidemic,
                         delay = model@delay, revision = revision_process(),
                         covariate_prior = model@covariate_prior,
                         strata_pooling = model@strata_pooling,
                         cumulative = model@cumulative)
    promoted_for <- switch(revision_mode,
      "confirmation_only" = "confirmations", "retraction_only" = "retractions",
      "both" = "confirmations and retractions", "down-revisions")
    cli::cli_inform(c("i" = "Modelling {promoted_for} with a default {.fn revision_process} (lognormal revision lag, data-informed default for {.arg p}).",
                      "*" = "Pass {.code model(revision = revision_process(...))} to configure it."))
  }

  # prior_only: don't auto-add temporal effects (keep the prior epidemic clean).
  data <- .apply_default_temporal_effects(data, if (isTRUE(prior_only)) "none" else temporal_effects)
  prepared <- prepare_from_tbl_now(data, model, now = now, delay_only = FALSE,
                                   revision_mode = revision_mode, ...)
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
  native_fit <- nowcast_class(
    model = model, data = data, now = prepared$now,
    type = if (isTRUE(prior_only)) "prior_only" else type,
    fits = collected$fits, rung = collected$rung, target = collected$target,
    engine = engine, priors = priors, phi = phi, n_draws = as.integer(n_draws),
    fit_diagnostics = collected$diagnostics %||% list(),
    revision_mode = revision_mode
  )
  .as_diseasenowcasting_result(
    native_fit,
    n_draws = n_draws,
    seed = seed,
    quantile_levels = quantile_levels
  )
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
  } else {
    return(data)
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
    observed_strata  = S7::class_any,     # [max_time x n_strata] observed per stratum (or NULL)
    estimand = S7::new_property(S7::class_any, default = NULL),
    cumulative_reconstruction = S7::new_property(S7::class_any, default = NULL),
    negative_projection_count = S7::new_property(S7::class_numeric, default = 0),
    laplace_sampling = S7::new_property(S7::class_list, default = list())
  )
)
