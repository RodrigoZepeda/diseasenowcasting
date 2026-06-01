# =============================================================================
# tidy() -- parameter estimates in tidy data-frame format
# =============================================================================
# Returns a data.frame (tibble-compatible) with one row per estimated parameter:
#   term, estimate, std.error, conf.low, conf.high, type
# Suitable for use with ggplot2, broom-style workflows, and CSV export.
# =============================================================================

#' Tidy parameter estimates from a fitted nowcast
#'
#' Returns all estimated parameters as a long data frame with credible intervals
#' derived from the Laplace approximation posterior precision matrix.
#'
#' @param x A `nowcast_class` object.
#' @param conf.level Credible level for the interval (default 0.95).
#' @param ... Unused.
#' @returns A `data.frame` with columns `term`, `estimate`, `std.error`,
#'   `conf.low`, `conf.high`, `type`.
#' @export
tidy <- function(x, conf.level = 0.95, ...) UseMethod("tidy")

#' @method tidy default
#' @export
tidy.default <- function(x, conf.level = 0.95, ...) {
  cli::cli_abort("No `tidy()` method for objects of class {.cls {class(x)}}.")
}

#' @noRd
S7::method(tidy, nowcast_class) <- function(x, conf.level = 0.95, ...) {
  fit    <- x@fits[[1]]
  data   <- fit$data
  priors <- fit$priors

  # -- posterior precision from the Laplace mode ------------------------------
  obj        <- fit$obj
  mode_vec   <- obj$env$last.par.best
  par_names  <- names(mode_vec)
  estimates  <- as.numeric(mode_vec)

  # Posterior SDs via diagonal of H^{-1} (sparse Cholesky)
  std_errors <- tryCatch({
    H <- methods::as(obj$he(mode_vec), "sparseMatrix")
    # Add small ridge if non-PD
    for (ridge_exp in c(0, -6:-1)) {
      chol <- tryCatch(Matrix::Cholesky(H + Matrix::Diagonal(nrow(H), 1e-8 * 10^ridge_exp),
                                        super = TRUE), error = function(e) NULL)
      if (!is.null(chol)) break
    }
    if (is.null(chol)) rep(NA_real_, length(estimates))
    else sqrt(diag(Matrix::solve(chol, Matrix::Diagonal(nrow(H)))))
  }, error = function(e) rep(NA_real_, length(estimates)))

  z <- stats::qnorm((1 + conf.level) / 2)

  # -- categorise parameters --------------------------------------------------
  type_of <- function(nm) {
    if (grepl("^delay|^simplex|^logit", nm))                "delay"
    else if (grepl("^log_phi|^nb", nm))                     "likelihood"
    else if (grepl("^log_gp|^basis_coefs|^gp", nm))         "epidemic_hsgp"
    else if (grepl("^ar_|^log_ar", nm))                     "epidemic_ar1"
    else if (grepl("^log_R0|^u_gamma|^u_neff", nm))         "epidemic_sir"
    else if (grepl("^mu_intercept|^mu_global|^delta|^log_tau_intercept", nm)) "epidemic_intercept"
    else if (grepl("^gamma", nm))                           "covariate"
    else                                                    "other"
  }

  # -- back-transform to interpretable scale ---------------------------------
  human_name <- function(nm) {
    nm <- sub("^log_", "log(", nm)
    if (grepl("log\\(", nm)) paste0(nm, ")")
    else nm
  }

  out <- data.frame(
    term       = par_names,
    estimate   = estimates,
    std.error  = as.numeric(std_errors),
    conf.low   = estimates - z * as.numeric(std_errors),
    conf.high  = estimates + z * as.numeric(std_errors),
    type       = vapply(par_names, type_of, character(1)),
    stringsAsFactors = FALSE
  )

  # Average delay parameters across two-stage imputations
  if (length(x@fits) > 1L) {
    delay_cols <- grepl("^delay_mu$|^delay_sigma$|^delay_Q$", out$term)
    if (any(delay_cols)) {
      all_delay <- lapply(x@fits, function(f) {
        pl <- f$parList; list(delay_mu = pl$delay_mu, delay_sigma = f$delay_sigma)
      })
      avg_mu  <- mean(vapply(all_delay, function(f) f$delay_mu %||% NA_real_, numeric(1)), na.rm = TRUE)
      avg_sig <- mean(vapply(all_delay, function(f) f$delay_sigma %||% NA_real_, numeric(1)), na.rm = TRUE)
      if ("delay_mu" %in% out$term && is.finite(avg_mu))
        out$estimate[out$term == "delay_mu"] <- avg_mu
      if (any(grepl("delay_sigma|log_delay_sigma", out$term)) && is.finite(avg_sig))
        out$estimate[grepl("delay_sigma|log_delay_sigma", out$term)][1] <- log(max(avg_sig - 0.01, 1e-6))
    }
  }

  out <- out[order(out$type, out$term), ]
  rownames(out) <- NULL
  out
}
