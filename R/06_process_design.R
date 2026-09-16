# =============================================================================
# Design matrices for the observation-process regressions
# =============================================================================
# Everything the reporting and revision hazards regress on goes through here, so
# that one set of rules governs contrasts, centring, degeneracy and rank.  The
# rules matter more than usual because these matrices are rebuilt at every
# backtest date and on every `update()`: if the columns are allowed to drift,
# `delay_beta[3]` silently stops meaning the same thing from one as-of date to
# the next, and a warm start writes a coefficient into the wrong slot.
#
# The contract is therefore a SCHEMA -- levels, contrasts, centring constants
# and the surviving column names -- which is built once, stored on the engine,
# and replayed verbatim afterwards.  `.process_design()` builds one; passing the
# result back as `schema =` reproduces it exactly.
# =============================================================================

#' Encode one covariate column against a fixed level set
#'
#' An UNORDERED factor or character column becomes reference-level dummies: its
#' levels have no distance between them, so any numeric coding would invent one.
#' An ORDERED factor keeps a single ordinal score, because its levels do have an
#' order and spending a coefficient per level throws that away.
#'
#' The dummies are built by comparison rather than through `model.matrix()`,
#' which reads the global `options("contrasts")`: the encoding a coefficient
#' refers to must not depend on a session setting.
#' @keywords internal
#' @noRd
.encode_design_column <- function(values, column, levels_for_column,
                                  ordered = FALSE) {
  if (is.null(levels_for_column)) {
    return(matrix(as.numeric(values), ncol = 1L,
                  dimnames = list(NULL, column)))
  }
  unseen <- setdiff(stats::na.omit(unique(as.character(values))), levels_for_column)
  if (length(unseen)) {
    cli::cli_abort(c(
      "Covariate {.val {column}} has level{?s} {.val {unseen}} that {?was/were} not in the fitted design.",
      "i" = "Make the column a factor whose {.code levels()} cover every value the series can take, so the design is stable across as-of dates.",
      "*" = "An unseen level cannot silently become a new coefficient: it would renumber every other one."
    ))
  }
  encoded <- factor(as.character(values), levels = levels_for_column)
  if (isTRUE(ordered)) {
    return(matrix(as.numeric(encoded), ncol = 1L,
                  dimnames = list(NULL, column)))
  }
  contrast_levels <- levels_for_column[-1L]
  if (!length(contrast_levels)) return(NULL)
  design <- vapply(contrast_levels, function(level) as.numeric(encoded == level),
                   numeric(length(encoded)))
  if (is.null(dim(design))) design <- matrix(design, ncol = length(contrast_levels))
  colnames(design) <- paste0(column, "[", contrast_levels, "]")
  design
}

#' Level set for a covariate, taken from its declaration where it has one
#'
#' A factor's declared `levels()` are the user's contract and are honoured even
#' when the as-of view has not seen them all yet, which is what keeps the first
#' level (the reference) from moving as a backtest walks forward.  A character
#' column has no declaration, so its levels can only be observed -- that is
#' reproducible within one fit but not across as-of dates, hence the warning.
#' @keywords internal
#' @noRd
.design_levels <- function(values, column) {
  if (is.factor(values)) return(levels(values))
  if (!is.character(values)) return(NULL)
  cli::cli_warn(c(
    "Covariate {.val {column}} is character, so its reference level is whichever value sorts first {.emph in this as-of view}.",
    "i" = "Convert it to a {.cls factor} with explicit {.code levels} to keep coefficients comparable across backtest dates and {.fn update}."
  ))
  sort(unique(as.character(values[!is.na(values)])))
}

#' Build a process design matrix and the schema that reproduces it
#'
#' @param frame Data frame holding the covariate columns (the as-of view).
#' @param covariate_cols Column names to encode.
#' @param role Role name, used in messages.
#' @param schema Schema from an earlier fit to replay, or `NULL` to build one.
#' @param standardize Whether continuous columns are centred and scaled.
#' @returns A list with the `design` matrix and the `schema` that rebuilds it.
#' @keywords internal
#' @noRd
.process_design <- function(frame, covariate_cols, role, schema = NULL,
                            standardize = TRUE) {
  frame <- as.data.frame(frame)
  covariate_cols <- intersect(covariate_cols, names(frame))
  if (!is.null(schema)) covariate_cols <- schema$columns
  if (!length(covariate_cols)) {
    return(list(design = matrix(0.0, nrow(frame), 0L),
                schema = .empty_design_schema()))
  }

  levels_by_column <- schema$levels %||% lapply(
    stats::setNames(covariate_cols, covariate_cols),
    function(column) .design_levels(frame[[column]], column)
  )
  ordered_by_column <- schema$ordered %||% vapply(
    stats::setNames(covariate_cols, covariate_cols),
    function(column) is.ordered(frame[[column]]), logical(1)
  )
  encoded <- lapply(covariate_cols, function(column) {
    .encode_design_column(frame[[column]], column, levels_by_column[[column]],
                          ordered_by_column[[column]])
  })
  encoded <- Filter(Negate(is.null), encoded)
  if (!length(encoded)) {
    return(list(design = matrix(0.0, nrow(frame), 0L),
                schema = .empty_design_schema()))
  }
  design <- do.call(cbind, encoded)
  storage.mode(design) <- "double"

  if (is.null(schema)) {
    schema <- .build_design_schema(design, covariate_cols, levels_by_column,
                                   ordered_by_column, role, standardize)
  }
  list(design = .apply_design_schema(design, schema, role), schema = schema)
}

#' @keywords internal
#' @noRd
.empty_design_schema <- function() {
  list(columns = character(0), levels = list(), ordered = logical(0),
       terms = character(0), center = numeric(0), scale = numeric(0),
       dropped = character(0))
}

#' Decide which columns survive, and how they are standardized
#'
#' Degenerate columns are removed before the tape is built, because no
#' likelihood can tell them apart from the baseline hazard (a constant), from
#' each other (a duplicate), or from a combination of the others (an exact
#' alias).  Leaving them in would produce a singular Hessian and an
#' uninterpretable warning instead of a named one.
#' @keywords internal
#' @noRd
.build_design_schema <- function(design, covariate_cols, levels_by_column,
                                 ordered_by_column, role, standardize) {
  terms <- colnames(design)
  dropped <- character(0)
  note <- function(term, reason) dropped[[term]] <<- reason

  keep <- rep(TRUE, ncol(design))
  unexposed <- character(0)
  for (index in seq_along(terms)) {
    column <- design[, index]
    finite <- column[is.finite(column)]
    if (!length(finite)) {
      keep[index] <- FALSE; note(terms[index], "no finite values")
    } else if (all(finite == 0)) {
      # A declared factor level the as-of view has not seen yet.  The column is
      # kept so that the coefficient vector does not renumber between as-of
      # dates; it contributes nothing to the linear predictor, so its posterior
      # is exactly its (zero-centred) prior until the level appears.
      unexposed <- c(unexposed, terms[index])
    } else if (stats::sd(finite) == 0) {
      # A genuinely constant non-zero column is aliased with the baseline
      # hazard, which already owns the timing intercept.
      keep[index] <- FALSE; note(terms[index], "constant, so aliased with the baseline hazard")
    }
  }
  if (length(unexposed)) {
    count <- length(unexposed)
    cli::cli_inform(c(
      "i" = "{count} {role} design {cli::qty(count)}column{?s} ({.val {unexposed}}) {cli::qty(count)}{?has/have} no exposed rows at this as-of date.",
      "*" = "{cli::qty(count)}The coefficient{?s} {?is/are} carried by the prior and the column{?s} {?keeps/keep} {?its/their} slot, so the design stays comparable across dates."
    ))
  }
  for (index in which(keep)) {
    earlier <- which(keep)[which(keep) < index]
    if (terms[index] %in% unexposed) next
    duplicate <- earlier[vapply(earlier, function(other)
      isTRUE(all.equal(design[, other], design[, index])), logical(1))]
    if (length(duplicate)) {
      keep[index] <- FALSE
      note(terms[index], paste0("duplicate of ", terms[duplicate[1L]]))
    }
  }

  center <- stats::setNames(numeric(ncol(design)), terms)
  scale <- stats::setNames(rep(1, ncol(design)), terms)
  if (standardize) for (index in which(keep)) {
    column <- design[, index]
    column <- column[is.finite(column)]
    # Binary columns (every dummy, and 0/1 numerics) are left alone: their
    # coefficient is already a clean log hazard-odds ratio between two states.
    if (length(unique(column)) > 2L) {
      center[index] <- mean(column)
      spread <- stats::sd(column)
      scale[index] <- if (is.finite(spread) && spread > 0) spread else 1
    }
  }

  # The rank check excludes the unexposed all-zero columns: they are trivially
  # rank-deficient, and they are being kept on purpose.
  testable <- keep & !(terms %in% unexposed)
  if (any(testable)) {
    standardized <- .standardize_design(design[, testable, drop = FALSE],
                                        center[testable], scale[testable])
    aliased <- .aliased_design_terms(standardized)
    for (term in aliased) note(term, "exactly collinear with the terms kept before it")
    keep[terms %in% aliased] <- FALSE
  }

  if (length(dropped)) {
    n_dropped <- length(dropped)
    cli::cli_warn(c(
      "Dropped {n_dropped} {role} design {cli::qty(n_dropped)}column{?s} that the likelihood cannot identify.",
      stats::setNames(paste0("{.val ", names(dropped), "}: ", unlist(dropped)),
                      rep("*", length(dropped))),
      "i" = "The remaining coefficients are unchanged; see {.code fit_check()} for the recorded schema."
    ))
  }
  list(columns = covariate_cols, levels = levels_by_column,
       ordered = ordered_by_column,
       terms = terms[keep], center = center[keep], scale = scale[keep],
       dropped = unlist(dropped) %||% character(0))
}

#' @keywords internal
#' @noRd
.standardize_design <- function(design, center, scale) {
  for (index in seq_len(ncol(design))) {
    design[, index] <- (design[, index] - center[index]) / scale[index]
  }
  design
}

#' Terms a pivoted QR finds to be exact linear combinations of earlier ones
#'
#' The pivot order is forced to the natural column order so the answer does not
#' depend on how LAPACK happens to rank equally-informative columns: the term a
#' user listed first is the one that survives.
#'
#' Collinearity is judged on the rows that are complete, since that is where the
#' likelihood can see it. With fewer complete rows than columns the question is
#' unanswerable, so nothing is dropped and the fit's own curvature diagnostic is
#' left to report the problem.
#' @keywords internal
#' @noRd
.aliased_design_terms <- function(design) {
  if (!ncol(design)) return(character(0))
  design <- design[stats::complete.cases(design), , drop = FALSE]
  if (nrow(design) < ncol(design)) return(character(0))
  decomposition <- qr(design, tol = 1e-7, LAPACK = FALSE)
  if (decomposition$rank >= ncol(design)) return(character(0))
  colnames(design)[sort(decomposition$pivot[seq.int(decomposition$rank + 1L,
                                                    ncol(design))])]
}

#' Replay a schema on a freshly encoded design
#' @keywords internal
#' @noRd
.apply_design_schema <- function(design, schema, role) {
  missing_terms <- setdiff(schema$terms, colnames(design))
  if (length(missing_terms)) {
    cli::cli_abort(c(
      "The {role} design is missing term{?s} {.val {missing_terms}} the fitted schema expects.",
      "i" = "This usually means a covariate changed type or lost a factor level between fits."
    ))
  }
  .standardize_design(design[, schema$terms, drop = FALSE],
                      schema$center, schema$scale)
}
