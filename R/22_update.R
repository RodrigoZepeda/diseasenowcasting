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
#' @param ... Unused.
#' @returns A new `nowcast_class` object fit to the merged data.
#' @noRd
S7::method(update, nowcast_class) <- function(object, new_data, now = NULL,
                                              K = 25L, np_spread = 1,
                                              compute_surprise = TRUE, ...) {
  merged   <- stats::update(object@data, new_data = new_data)
  prepared <- prepare_from_tbl_now(merged, object@model, now = now, delay_only = FALSE)
  engine   <- prepared$data
  priors   <- default_priors(object@model, engine, phi = object@phi)
  warm     <- .adapt_init(object@fits[[1]]$parList, engine, object@model)

  # -- Compute surprise for new data BEFORE the refit -------------------------
  surprise_result <- NULL
  if (isTRUE(compute_surprise)) {
    surprise_result <- tryCatch({
      old_fit    <- object@fits[[1]]
      old_engine <- old_fit$data
      old_T      <- as.integer(old_engine$max_time)
      new_T      <- as.integer(engine$max_time)
      if (new_T > old_T) {
        # Count surprise: new event-times t+1 .. T_new that weren't in the old fit
        new_idxs   <- seq(old_T, new_T - 1L)   # 0-indexed event indices
        obs_new_total <- rowSums(if (is.matrix(engine$case_counts)) engine$case_counts
                                  else matrix(engine$case_counts, new_T, 1))
        count_df <- data.frame(event_index = new_idxs,
                               count = obs_new_total[new_idxs + 1L])
        count_df <- count_df[count_df$count > 0, , drop = FALSE]
        if (nrow(count_df) > 0)
          surprise(object, new_data = count_df, type = "count", n_draws = 200L)
        else NULL
      } else NULL
    }, error = function(e) NULL)
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
#' @returns A `dcast3_surprise` object, or `NULL` if no surprise was computed.
#' @export
surprise_result <- function(nc) attr(nc, "surprise")
