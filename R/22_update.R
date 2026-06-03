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
#'   with [extreme_values()].
#' @param ... Unused.
#' @returns A new `nowcast_class` object fit to the merged data.
#' @noRd
S7::method(update, nowcast_class) <- function(object, new_data, now = NULL,
                                              K = 25L, np_spread = 1,
                                              compute_surprise = TRUE,
                                              surprise_level = 0.99, ...) {
  # Temporal effects must match on both sides of the tbl.now merge, otherwise it
  # errors with "Cannot handle different temporal_effects".  The stored data and
  # `new_data` may carry effects in different states (computed vs. only
  # specified), so we strip BOTH sides, merge, then re-attach the ORIGINAL effect
  # specification from the fitted object (preserving the user's choice rather
  # than substituting the package defaults).  tbl.now's internal data-shaping
  # warnings (e.g. non-unique rows) are not actionable here, so they are muffled.
  orig_spec   <- tryCatch(tbl.now::get_temporal_effects(object@data), error = function(e) NULL)
  had_effects <- !is.null(orig_spec) && length(orig_spec) > 0L
  strip_te <- function(d) {
    if (!tbl.now::is_tbl_now(d)) return(d)
    tryCatch(tbl.now::remove_temporal_effects(d), error = function(e) d)
  }

  merged <- suppressWarnings({
    m <- stats::update(strip_te(object@data), new_data = strip_te(new_data))
    if (had_effects) {
      m <- tryCatch({
        for (s in orig_spec) m <- tbl.now::add_temporal_effects(m, s$t_effects)
        tbl.now::compute_temporal_effects(m)
      }, error = function(e) m)
    }
    m
  })
  prepared <- prepare_from_tbl_now(merged, object@model, now = now, delay_only = FALSE)
  engine   <- prepared$data
  priors   <- default_priors(object@model, engine, phi = object@phi)
  warm     <- .adapt_init(object@fits[[1]]$parList, engine, object@model)

  # -- Surprise: upper-censored (too-long) reporting delays only --------------
  # Scored against the OLD fit (what we believed before the new data).  We do NOT
  # score the epidemic/count process (too noisy) and we do NOT flag delays that
  # are shorter than expected -- only reports that arrived LATER than the fitted
  # delay distribution leads us to expect.  All surprises are collapsed into a
  # single warning.
  extreme_values <- NULL
  if (isTRUE(compute_surprise)) {
    extreme_values <- tryCatch(
      .update_surprise(object, merged, surprise_level), error = function(e) NULL)
    if (!is.null(extreme_values)) {
      unit <- tryCatch(as.character(tbl.now::get_event_units(merged)),
                       error = function(e) "event units")
      .warn_surprise(extreme_values, surprise_level, unit)
    }
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
  if (!is.null(extreme_values)) attr(new_nc, "surprise") <- extreme_values
  new_nc
}

#' Surprising (extreme) values flagged during the last `update()`
#'
#' Returns a tidy `data.frame` with one row per flagged surprise -- currently the
#' reporting delays that arrived later than the model expects (`surprise =
#' "delay"`, `direction = "long"`) -- together with their tail probability and
#' the `level` used.  Returns `NULL` when nothing was flagged (or surprise was
#' not computed).
#' @param nc A `nowcast_class` object returned by [update()].
#' @returns A `data.frame` of flagged surprises, or `NULL`.
#' @export
extreme_values <- function(nc) {
  surprises <- attr(nc, "surprise")
  if (is.null(surprises)) return(NULL)

  parts <- list()
  ds <- surprises$delay_surprise
  if (!is.null(ds) && nrow(ds) > 0) {
    keep <- ds[which(ds$is_surprising & ds$direction == "long"), , drop = FALSE]
    if (nrow(keep) > 0) { keep$is_surprising <- NULL; keep$surprise <- "delay"
                          parts[[length(parts) + 1L]] <- keep }
  }
  cs <- surprises$count_surprise
  if (!is.null(cs) && nrow(cs) > 0) {
    keep <- cs[which(cs$is_surprising), , drop = FALSE]
    if (nrow(keep) > 0) { keep$is_surprising <- NULL; keep$surprise <- "epidemic"
                          parts[[length(parts) + 1L]] <- keep }
  }
  if (length(parts) == 0L) return(NULL)

  # Column-filling rbind (delay and count tables have different columns).
  cols <- unique(unlist(lapply(parts, names)))
  out  <- do.call(rbind, lapply(parts, function(d) {
    for (cc in setdiff(cols, names(d))) d[[cc]] <- NA
    d[cols]
  }))
  out$level <- surprises$level
  out
}

#' Score the reporting-delay surprise of the new reports against the previous fit.
#'
#' Only the delay process is scored (the epidemic/count process is too noisy to
#' flag reliably on every update).  Returns a `diseasenowcasting_surprise` whose
#' `delay_surprise` table covers the delays of reports that arrived after the
#' fitted object's `now`, or `NULL` if there are none.
#' @keywords internal
#' @noRd
.update_surprise <- function(object, merged, level) {
  delay_df <- tryCatch({
    ev  <- tbl.now::get_event_date(merged); rp <- tbl.now::get_report_date(merged)
    eu  <- tbl.now::get_event_units(merged); mn <- min(merged[[ev]], na.rm = TRUE)
    new_rep <- which(merged[[rp]] > object@now)
    if (length(new_rep) == 0L) NULL else {
      d_u <- .unit_steps(mn, merged[[rp]][new_rep], eu) - .unit_steps(mn, merged[[ev]][new_rep], eu)
      d_u <- d_u[is.finite(d_u) & d_u >= 0]
      if (length(d_u) == 0L) NULL else {
        tab <- as.data.frame(table(delay = d_u), stringsAsFactors = FALSE)
        data.frame(delay = as.numeric(tab$delay), weight = as.numeric(tab$Freq))
      }
    }
  }, error = function(e) NULL)

  if (is.null(delay_df)) return(NULL)
  surprise(object, new_data = delay_df, type = "delay", level = level, n_draws = 200L)
}

#' Emit ONE warning listing every surprisingly-long (upper-censored) delay.
#'
#' Only reports that arrived LATER than the fitted delay distribution leads us to
#' expect are flagged (`direction == "long"`); delays shorter than expected
#' (where `P(D >= d) = 1`) are ignored.  All surprises are collapsed into a
#' single cli warning, with delays expressed in the data's time `unit`.
#' @keywords internal
#' @noRd
.warn_surprise <- function(s, level, unit = "event units") {
  ds <- s$delay_surprise
  if (is.null(ds)) return(invisible(NULL))
  bad <- ds[which(ds$is_surprising & ds$direction == "long"), , drop = FALSE]
  if (nrow(bad) == 0L) return(invisible(NULL))

  bad <- bad[order(-bad$delay), , drop = FALSE]
  bullets <- vapply(seq_len(nrow(bad)), function(i) {
    rpt <- bad$weight[i]
    paste0("Surprising reporting delay of ", bad$delay[i], " ", unit,
           " (", rpt, " report", if (rpt != 1) "s" else "", "): longer than the ",
           "model expects (P(D >= d) = ", signif(bad$mean_tail_prob[i], 2), ").")
  }, character(1))

  msg <- stats::setNames(
    c(bullets,
      "If these are outliers, treat them as censored with `censor_delays_above()` and re-fit.",
      "See all flagged delays with `extreme_values(nc)`."),
    c(rep("!", length(bullets)), "i", "i"))
  cli::cli_warn(msg)
  invisible(NULL)
}
