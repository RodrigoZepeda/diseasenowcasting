# =============================================================================
# update() -- warm-started refit of a nowcast when new data arrive
# =============================================================================
# Merges the new data into the stored tbl_now (via tbl.now::update, which
# handles "just-new" or "old + new"), then refits warm-started from the previous
# estimates instead of cold.  Faster than calling nowcast() afresh because the
# scalar hyperparameters and (length-adapted) latent coefficients seed the
# optimiser, skipping the cold init ladder and the cold Stage-A warm fit.
# =============================================================================

#' Adapt a fitted parameter list to a new engine's dimensions (warm-start init)
#'
#' Scalars carry over unchanged; the latent vectors are resized to the new
#' series length / basis count (padded with zeros when the series grew, the
#' usual case for new data; truncated if it shrank).
#' @keywords internal
#' @noRd
.adapt_init <- function(old_parlist, new_engine, model) {
  n_strata <- as.integer(new_engine$num_strata %||% 1L)
  resize <- function(vec, n) {                      # plain vector resize (zero-pad / truncate)
    if (is.null(vec)) return(rep(0, n))
    if (length(vec) >= n) vec[seq_len(n)] else c(vec, rep(0, n - length(vec)))
  }
  # Resize a per-stratum latent block to `n_rows` x num_strata, preserving columns
  # (num_strata is fixed by the data, so column count carries over unchanged).
  resize_strata <- function(block, n_rows) {
    mat <- matrix(as.numeric(block), ncol = n_strata)
    out <- matrix(0, n_rows, n_strata)
    keep <- min(nrow(mat), n_rows)
    out[seq_len(keep), ] <- mat[seq_len(keep), , drop = FALSE]
    out
  }
  init <- list()
  # Per-stratum vectors (length num_strata) and scalars carry over unchanged.
  for (nm in intersect(c("mu_intercept", "delay_mu", "log_delay_sigma_excess", "delay_Q",
                         "log_phi_nb", "log_gp_alpha", "log_gp_ell",
                         "ar_phi_unc", "log_ar_sigma_unc", "log_R0", "u_gamma", "u_neff"),
                       names(old_parlist)))
    init[[nm]] <- old_parlist[[nm]]
  if (!is.null(old_parlist$gamma) && new_engine$P > 0)
    init$gamma <- resize_strata(old_parlist$gamma, new_engine$P)
  if (!is.null(old_parlist$ar_innov))
    init$ar_innov <- resize_strata(old_parlist$ar_innov, new_engine$max_time)
  if (!is.null(old_parlist$basis_coefs))
    init$basis_coefs <- resize_strata(old_parlist$basis_coefs, new_engine$num_basis)
  if (!is.null(old_parlist$delay_logits))
    init$delay_logits <- resize(old_parlist$delay_logits, as.integer(new_engine$np_model_length))
  init
}

#' Update a fitted nowcast with new data (warm-started refit)
#'
#' @param object A `nowcast_class` object.
#' @param new_data A `tbl_now` (or data.frame) with either just the new rows or
#'   the full old + new data; merged into the existing data via
#'   `tbl.now::update()`.
#' @param now As-of date for the refit; default the latest report date in the
#'   merged data.
#' @param K Delay imputations (two-stage).
#' @param np_spread Dirichlet simplex imputation covariance inflation.
#' @param compute_surprise If `TRUE` (default), score the new counts and delays
#'   against the previous fit and warn about anything surprising.
#' @param surprise_level Credible level for the surprise flags (default `0.99`).
#'   A count outside this predictive interval, or a delay whose tail probability
#'   is below `1 - surprise_level`, raises a warning.  Retrieve the full table
#'   with [surprise_result()].
#' @param ... Unused.
#' @returns A new `nowcast_class` object fit to the merged data.
#' @noRd
S7::method(update, nowcast_class) <- function(object, new_data, now = NULL,
                                              K = 25L, np_spread = 1,
                                              compute_surprise = TRUE,
                                              surprise_level = 0.99, ...) {
  # The stored data may carry computed temporal effects (added by default in
  # nowcast()).  Strip them before merging so a raw `new_data` can be combined,
  # then re-derive them on the merged tbl_now.
  base_data <- object@data
  te_cols   <- tryCatch(tbl.now::get_temporal_effect_cols(base_data), error = function(e) character(0))
  had_effects <- length(te_cols) > 0L
  if (had_effects)
    base_data <- tryCatch(tbl.now::remove_temporal_effects(base_data), error = function(e) base_data)

  merged <- stats::update(base_data, new_data = new_data)
  if (had_effects)
    merged <- .apply_default_temporal_effects(merged, "auto")
  prepared <- prepare_from_tbl_now(merged, object@model, now = now, delay_only = FALSE)
  engine   <- prepared$data
  priors   <- default_priors(object@model, engine, phi = object@phi)
  warm     <- .adapt_init(object@fits[[1]]$parList, engine, object@model)

  # -- Compute count + delay surprise for new data BEFORE the refit -----------
  # Both are scored against the OLD fit (what we believed before the new data),
  # at the requested credible level.  Surprising findings raise a warning that
  # says exactly what was unexpected (count too high/low, delay too long/short).
  surprise_result <- NULL
  if (isTRUE(compute_surprise)) {
    surprise_result <- tryCatch(
      .update_surprise(object, engine, merged, surprise_level), error = function(e) NULL)
    if (!is.null(surprise_result)) .warn_surprise(surprise_result, surprise_level)
  }

  collected <- if (object@type == "one_stage")
      list(fits = list(fit(object@model, engine, priors = priors, init = warm)),
           rung = "onestage", target = engine$max_time)
    else
      .collect_nowcast_fits(object@model, engine, priors, type = "two_stage",
                            K = K, np_spread = np_spread, warm_inits = warm)

  new_nc <- nowcast_class(model = object@model, data = merged, now = prepared$now, type = object@type,
                          fits = collected$fits, rung = collected$rung, target = collected$target,
                          engine = engine, priors = priors, phi = object@phi, n_draws = object@n_draws)
  if (!is.null(surprise_result)) {
    attr(new_nc, "surprise") <- surprise_result
    cli::cli_inform(c("i" = "Surprise scores computed; access via {.code attr(nc, \"surprise\")} or {.code surprise_result(nc)}."))
  }
  new_nc
}

#' Retrieve the surprise scores computed during the last `update()`
#' @param nc A `nowcast_class` object returned by [update()].
#' @returns A `diseasenowcasting_surprise` object, or `NULL` if no surprise was computed.
#' @export
surprise_result <- function(nc) attr(nc, "surprise")

#' Score count + delay surprise of the new data against the previous fit.
#' @keywords internal
#' @noRd
.update_surprise <- function(object, engine, merged, level) {
  old_engine <- object@fits[[1]]$data
  old_T <- as.integer(old_engine$max_time)
  new_T <- as.integer(engine$max_time)

  # ── Count surprise: REVISIONS to event-times the old model predicted ──────
  # Only event-times within the old fit's horizon are scored: the old model has
  # a posterior-predictive distribution for them, so a revised total can be
  # "surprising".  Brand-new event-times beyond the old horizon are skipped
  # (the model never forecast them, so it cannot be surprised by them).
  count_df <- NULL
  old_obs <- rowSums(if (is.matrix(old_engine$case_counts)) old_engine$case_counts
                      else matrix(old_engine$case_counts, old_T, 1))
  new_obs <- rowSums(if (is.matrix(engine$case_counts)) engine$case_counts
                      else matrix(engine$case_counts, new_T, 1))
  # Only score the recent "still accruing" window: event-times whose maximum
  # observable delay at the OLD as-of date was within the bulk of the delay
  # distribution.  Long-settled older times are essentially fully reported and
  # are not re-flagged on every update.
  old_delays   <- if (nrow(old_engine$m) > 0) old_engine$m[, 3] - 1 else 0   # 0-indexed delays
  delay_horizon <- max(1, ceiling(stats::quantile(old_delays, 0.99, names = FALSE)))
  recent_lo <- max(1L, old_T - as.integer(delay_horizon))
  in_range  <- recent_lo:old_T
  revised   <- in_range[abs(new_obs[in_range] - old_obs[in_range]) > 1e-8]
  if (length(revised) > 0L) {
    count_df <- data.frame(event_index = revised - 1L, count = new_obs[revised])
    if (nrow(count_df) == 0L) count_df <- NULL
  }

  # ── Delay surprise: delays of reports that arrived since the last `now` ───
  delay_df <- tryCatch({
    ev  <- tbl.now::get_event_date(merged); rp <- tbl.now::get_report_date(merged)
    eu  <- tbl.now::get_event_units(merged); mn <- min(merged[[ev]], na.rm = TRUE)
    new_rep <- which(merged[[rp]] > object@now & merged[[rp]] <= engine$now %||% max(merged[[rp]], na.rm = TRUE))
    if (length(new_rep) == 0L) new_rep <- which(merged[[rp]] > object@now)
    if (length(new_rep) == 0L) NULL else {
      d_u <- .unit_steps(mn, merged[[rp]][new_rep], eu) - .unit_steps(mn, merged[[ev]][new_rep], eu)
      d_u <- d_u[is.finite(d_u) & d_u >= 0]
      if (length(d_u) == 0L) NULL else {
        tab <- as.data.frame(table(delay = d_u), stringsAsFactors = FALSE)
        data.frame(delay = as.numeric(tab$delay), weight = as.numeric(tab$Freq))
      }
    }
  }, error = function(e) NULL)

  if (is.null(count_df) && is.null(delay_df)) return(NULL)

  # Combine into one new_data frame and score both types in one pass.
  if (!is.null(count_df) && !is.null(delay_df)) {
    res_c <- surprise(object, new_data = count_df, type = "count", level = level, n_draws = 200L)
    res_d <- surprise(object, new_data = delay_df, type = "delay", level = level, n_draws = 200L)
    structure(list(count_surprise = res_c$count_surprise, delay_surprise = res_d$delay_surprise,
                   type = "both", level = level, n_draws = 200L),
              class = "diseasenowcasting_surprise")
  } else if (!is.null(count_df)) {
    surprise(object, new_data = count_df, type = "count", level = level, n_draws = 200L)
  } else {
    surprise(object, new_data = delay_df, type = "delay", level = level, n_draws = 200L)
  }
}

#' Emit cli warnings describing any surprising new data.
#' @keywords internal
#' @noRd
.warn_surprise <- function(s, level) {
  pct <- paste0(round(level * 100), "%")
  cs <- s$count_surprise
  if (!is.null(cs)) {
    bad <- cs[which(cs$is_surprising), , drop = FALSE]
    for (i in seq_len(nrow(bad))) {
      dir <- if (bad$direction[i] == "high") "higher" else "lower"
      cli::cli_warn(c(
        "!" = "Surprising case count at event index {bad$event_index[i]}: observed {.val {bad$observed[i]}} is {dir} than the {pct} predictive interval (median {bad$posterior_median[i]}).",
        "i" = "Check for a data-entry error, a reporting backlog, or a real epidemiological signal."))
    }
  }
  ds <- s$delay_surprise
  if (!is.null(ds)) {
    bad <- ds[which(ds$is_surprising), , drop = FALSE]
    for (i in seq_len(nrow(bad))) {
      dir <- if (bad$direction[i] == "long") "longer" else "shorter"
      cli::cli_warn(c(
        "!" = "Surprising reporting delay of {.val {bad$delay[i]}} event unit{?s} ({bad$weight[i]} report{?s}): {dir} than the model expects (P(D >= d) = {signif(bad$mean_tail_prob[i], 2)}).",
        "i" = "If this is an outlier, treat it as censored with {.fn censor_delays_above} and re-fit."))
    }
  }
  invisible(NULL)
}
