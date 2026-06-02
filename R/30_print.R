# =============================================================================
# Pretty cli printing for the model menu (ported & polished from diseasenowcast2)
# =============================================================================
# Inline, coloured one-liners for each likelihood / epidemic / delay / prior,
# and a full model_class printer that shows every component WITH its priors.
# =============================================================================

#' Format a prior_class inline: "Dist(p1, p2)"
#' @keywords internal
#' @noRd
.prior_inline <- function(p) {
  args <- if (length(p@stan_params) > 0) paste(format(p@stan_params, digits = 4), collapse = ", ") else ""
  paste0(cli::col_blue(p@name), "(", args, ")")
}

#' Format a parameter slot inline: "p ~ Dist()", "p = num", or just "p"
#' @keywords internal
#' @noRd
.fmt_slot <- function(param_name, value) {
  if (S7::S7_inherits(value, prior_class)) {
    paste0("{.emph ", param_name, "} ~ ", .prior_inline(value))
  } else if (length(value) > 0) {
    paste0("{.emph ", param_name, "} = {.val ", paste(format(value, digits = 4), collapse = ","), "}")
  } else {
    paste0("{.emph ", param_name, "}")
  }
}

# ── Likelihoods ───────────────────────────────────────────────────────────────

#' @noRd
S7::method(print, poisson_likelihood_class) <- function(x, ..., digits = 4) {
  cli::cli_text(paste0(cli::col_cyan("Poisson"), "(", .fmt_slot("mu", x@mu), ")"))
  invisible(x)
}

#' @noRd
S7::method(print, nb_likelihood_class) <- function(x, ..., digits = 4) {
  cli::cli_text(paste0(cli::col_cyan("NegBin"), "(",
                       .fmt_slot("mu", x@mu), ", ", .fmt_slot("phi", x@phi), ")"))
  invisible(x)
}

# ── Priors ────────────────────────────────────────────────────────────────────

#' @noRd
S7::method(print, prior_class) <- function(x, ..., digits = 4) {
  args <- if (length(x@stan_params) > 0) paste(format(x@stan_params, digits = 4), collapse = ", ") else ""
  cli::cli_text(paste0(cli::col_blue(x@name), "({args})"))
  invisible(x)
}

# ── Delay processes ───────────────────────────────────────────────────────────

.seasons_lbl <- function(x) {
  if (x@num_delay_seasons == 1L) "" else
    paste0("; {.emph seasons} = {.val ", x@num_delay_seasons, "}, ",
           .fmt_slot("season_effect", x@season_distribution))
}

#' @noRd
S7::method(print, lognormal_delay_class) <- function(x, ..., digits = 4) {
  cli::cli_text(paste0(cli::col_green("LogNormal"), "(",
                       .fmt_slot("mu", x@mu), ", ", .fmt_slot("sigma", x@sigma),
                       .seasons_lbl(x), ")"))
  invisible(x)
}

#' @noRd
S7::method(print, gamma_delay_class) <- function(x, ..., digits = 4) {
  cli::cli_text(paste0(cli::col_green("Gamma"), "(",
                       .fmt_slot("shape", x@shape), ", ", .fmt_slot("rate", x@rate),
                       .seasons_lbl(x), ")"))
  invisible(x)
}

#' @noRd
S7::method(print, generalized_gamma_delay_class) <- function(x, ..., digits = 4) {
  cli::cli_text(paste0(cli::col_green("GeneralizedGamma"), "(",
                       .fmt_slot("mu", x@mu), ", ", .fmt_slot("sigma", x@sigma), ", ",
                       .fmt_slot("Q", x@Q), .seasons_lbl(x), ")"))
  invisible(x)
}

#' @noRd
S7::method(print, dirichlet_delay_class) <- function(x, ..., digits = 4) {
  bins_lbl <- if (length(x@bins) > 0 && !is.na(x@bins) && x@bins > 0) x@bins else "auto"
  cli::cli_text(paste0(cli::col_green("Dirichlet"), "(", .fmt_slot("alpha", x@alpha),
                       "; {.emph bins} = {.val ", bins_lbl, "})"))
  invisible(x)
}

# ── Epidemic processes ────────────────────────────────────────────────────────

.gp_kernel_label <- function(k) c("1" = "sq_exp", "2" = "matern32", "3" = "matern52")[[as.character(k)]]

#' @noRd
S7::method(print, hsgp_epidemic_class) <- function(x, ..., digits = 4) {
  cli::cli_text(paste0(cli::col_magenta("HSGP"), "(",
    .fmt_slot("alpha", x@alpha), ", ", .fmt_slot("ell", x@ell), ", ",
    .fmt_slot("basis_coefs", x@basis_coefs),
    " ; {.emph kernel} = {.val ", .gp_kernel_label(x@gp_kernel),
    "}, {.emph num_basis} = {.val ", ifelse(x@num_basis > 0, x@num_basis, "auto"),
    "}, {.emph tmax} = {.val ", ifelse(x@tmax_model > 0, x@tmax_model, "auto"), "})"))
  invisible(x)
}

#' @noRd
S7::method(print, ar1_epidemic_class) <- function(x, ..., digits = 4) {
  cli::cli_text(paste0(cli::col_magenta("AR(1)"), "(",
                       .fmt_slot("phi", x@phi), ", ", .fmt_slot("sigma", x@sigma),
                       " | ", .fmt_slot("error", x@error), ")"))
  invisible(x)
}

#' @noRd
S7::method(print, sir_epidemic_class) <- function(x, ..., digits = 4) {
  rw_lbl <- if (isTRUE(x@use_beta_rw_trend)) "TRUE" else "FALSE"
  cli::cli_text(paste0(cli::col_magenta("SIR"), "(",
    .fmt_slot("R0", x@R0), ", ", .fmt_slot("gamma", x@gamma), ", ", .fmt_slot("N_eff", x@N_eff),
    " ; {.emph N_pop} = {.val ", format(x@N_pop, big.mark = ",", scientific = FALSE),
    "}, {.emph beta_rw} = {.val ", rw_lbl, "})"))
  invisible(x)
}

#' @noRd
S7::method(print, spline_epidemic_class) <- function(x, ..., digits = 4) {
  cli::cli_text(paste0(cli::col_magenta("B-Splines"), "(",
    .fmt_slot("tau", x@tau), ", ", .fmt_slot("basis_coefs", x@basis_coefs),
    "; {.emph degree} = {.val ", x@spline_degree,
    "}, {.emph num_basis} = {.val ", ifelse(x@num_basis > 0, x@num_basis, "auto"), "})"))
  invisible(x)
}

# ── Model ─────────────────────────────────────────────────────────────────────

#' Print a Bayesian nowcast model specification
#'
#' Pretty-prints a [model()] object: the likelihood, epidemic process, delay
#' process, covariate prior, and strata pooling, each shown with its priors.
#' @param x A `model_class` object.
#' @param ... Unused.
#' @param digits Significant digits for numeric values.
#' @returns `x`, invisibly.
#' @noRd
S7::method(print, model_class) <- function(x, ..., digits = 4) {
  cli::cli_h1("Bayesian Nowcast Model")
  cli::cli_h3("Likelihood")
  print(x@likelihood, digits = digits)
  cli::cli_h3("Epidemic process")
  print(x@epidemic, digits = digits)
  cli::cli_h3("Delay process")
  print(x@delay, digits = digits)
  cli::cli_h3("Covariate prior")
  print(x@covariate_prior, digits = digits)
  pool <- x@strata_pooling
  cli::cli_text("{.emph Strata pooling}: {.val {pool}}")
  cli::cli_rule()
  invisible(x)
}

#' Compact one-line model spec ("NegBin / HSGP / LogNormal").
#' @keywords internal
#' @noRd
.model_oneline <- function(model) {
  lik <- switch(model@likelihood@name, nb = "NegBin", poisson = "Poisson", model@likelihood@name)
  paste0(cli::col_cyan(lik), " / ",
         cli::col_magenta(model@epidemic@name), " / ",
         cli::col_green(model@delay@name))
}
