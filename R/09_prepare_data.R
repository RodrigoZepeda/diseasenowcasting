# =============================================================================
# prepare_data() -- build the RTMB engine inputs from an observation matrix
# =============================================================================
# Analogue of diseasenowcast2::data_to_stan(), but emits the (Stan-free) data
# list the RTMB objective consumes.  Reproduces the FIXED delay-only censoring:
# in delay_only mode each event at time t is censored at c_t = max_time - t + 1
# (the corrected `delay_routing[t] = t` behaviour), and num_delay_seasons = 1
# keeps the delay log-mean constant across t.
# =============================================================================

#' The softplus ceiling on the latent log-incidence
#'
#' The objective caps `log_mean` with `ub - log1p(exp(ub - log_mean))` so a bad
#' optimiser step cannot overflow `exp()`.  That is the whole of its job: it is
#' a numerical guard, NOT a belief about how large incidence can be.
#'
#' The ceiling is nevertheless sized from `casemax`, the largest count REPORTED
#' so far.  The latent incidence exceeds the reported count by `1/Gstar`, the
#' reciprocal of the reporting fraction -- which is exactly the quantity a
#' nowcast exists to estimate.  On a stream that is growing while only a percent
#' or two has arrived, a ceiling at `log1p(casemax)` sits BELOW the answer, the
#' fit pins to it, and (because the softplus gradient vanishes once saturated)
#' the trend goes flat above it.  On `covid_us` in March 2020 that put the
#' median nowcast at 8.7% of the settled count at every as-of date and for every
#' epidemic process, because the cap is applied downstream of all of them.
#'
#' `log(100)` of headroom decouples the guard from the observed scale: it admits
#' a hundredfold reporting inflation, which covers the 37-83x that early-2020
#' `covid_us` needs, while `exp(16)` (~8.9M) remains the hard overflow stop.
#' Widening a bound is free wherever it does not bind -- and the softplus is
#' within 5% of the identity three log units below the ceiling, so on the
#' datasets that were already clear of it nothing moves.
#'
#' @param casemax Largest observed count over (event time, stratum).
#' @param override Optional user-supplied bound; returned as given.
#' @returns A single finite bound on the natural-log scale.
#' @keywords internal
#' @noRd
.mu_log_upper_bound <- function(casemax, override = NULL) {
  if (!is.null(override)) {
    value <- suppressWarnings(as.numeric(override))
    if (length(value) != 1L || !is.finite(value) || value <= 0) {
      cli::cli_abort(c(
        "`mu_log_upper_bound` must be a single finite positive number.",
        "x" = "Got {.val {override}}.",
        "i" = "It is a bound on the NATURAL LOG of incidence: {.code log(1e6)} is a ceiling of a million."
      ))
    }
    return(value)
  }
  min(max(6, log1p(casemax)) + log(100), 16)
}

#' Warn when the HSGP has more basis functions than the series can support
#'
#' The basis count sets the shortest wavelength the trend can resolve: with the
#' boundary factor `gp_L`, basis `j` carries a wavelength of about
#' `2 * gp_L * max_time / j`.  Once `j` approaches `max_time / 2` the trend can
#' wiggle on a two-step scale, and the place it does so is the right-hand edge,
#' where reporting is least complete and the likelihood constrains it least.
#' The result is a nowcast that extrapolates the last few censored points
#' instead of the epidemic.
#'
#' Measured on mpox at `now = 2022-08-09` (33 event-times, settled truth 64),
#' one-stage, NB + log-normal:
#'
#' | num_basis | median nowcast | 90% band | covers? |
#' |---|---|---|---|
#' | 8  | 84   | [9, 484]        | yes |
#' | 12 (auto) | 186  | [19, 1913]  | yes |
#' | 20 | 1540 | [180, 13373]    | no  |
#'
#' This used to be invisible: the softplus ceiling on `log_mean` clipped the
#' runaway to something plausible-looking.  With the ceiling decoupled from the
#' observed scale (see `.mu_log_upper_bound()`) the over-flexible basis shows
#' through, so it is named here rather than silently absorbed.
#'
#' @section Only a count the caller chose:
#'
#' The warning fires for an EXPLICIT `num_basis` only.  The automatic ladder has
#' a floor of 12, which is itself more than half of a 20-step series, so warning
#' on it would fire on the package's own default path for every short series --
#' 11 times across this package's own test suite, none of them a choice anyone
#' made.  Short series are already handled elsewhere and better:
#' `auto_nowcast()` keeps HSGP out of its candidate grid below
#' `min_hsgp = 30`.  What is actionable is a number the caller supplied, which
#' is usually one carried over from a longer series (the `num_basis = 20L` that
#' `?diseasenowcasting` recommends for COVID-length daily data is exactly the
#' value that breaks a 33-day one).
#'
#' @param num_basis The resolved basis count.
#' @param max_time Number of event-times the model spans.
#' @param explicit `TRUE` when the caller set `num_basis`.  An automatic count
#'   never warns; see above.
#' @returns `TRUE` (invisibly) when a warning was emitted.
#' @keywords internal
#' @noRd
.warn_hsgp_basis_fraction <- function(num_basis, max_time, explicit = FALSE) {
  if (!isTRUE(explicit)) return(invisible(FALSE))
  num_basis <- as.integer(num_basis)
  max_time <- as.integer(max_time)
  if (!length(num_basis) || !length(max_time) ||
      is.na(num_basis) || is.na(max_time) || max_time <= 0L) {
    return(invisible(FALSE))
  }
  # Half the event-times is where the shortest resolvable wavelength reaches
  # the two-step scale.  Below that the basis is smoothing; above it, it can
  # interpolate the noise.
  if (num_basis <= max_time %/% 2L) return(invisible(FALSE))

  fraction <- round(100 * num_basis / max_time)
  automatic <- .auto_hsgp_num_basis(max_time)
  cli::cli_warn(c(
    "{.val {num_basis}} HSGP basis function{?s} for {.val {max_time}} event-time{?s} ({fraction}%) is more flexibility than the series supports.",
    "x" = "The basis can then fit the most recent, least-reported points instead of smoothing them, and the nowcast extrapolates from them.",
    "i" = "Drop {.code num_basis} (leave it at {.code 0L} for the automatic count, {.val {automatic}} here), or use a process whose flexibility does not scale with the basis count: {.fn ar1_epidemic}, {.fn random_walk_epidemic}, {.fn sts_epidemic}.",
    "i" = "Check {.code reporting_fraction(nc)} and {.code fit_check()} before trusting the result."
  ))
  invisible(TRUE)
}

#' The automatic HSGP basis count for a series length
#'
#' `prepare_data()`'s ladder, factored out so the warning can quote the number
#' the user would get by leaving `num_basis` alone without the two drifting.
#' @keywords internal
#' @noRd
.auto_hsgp_num_basis <- function(max_time) {
  if (max_time < 10) return(3L)
  if (max_time < 20) return(8L)
  min(150L, max(12L, as.integer(ceiling(1.5 * sqrt(max_time)))))
}

#' Prepare data for the RTMB nowcast engine
#'
#' @param model A [model()] object.
#' @param m Observation matrix: columns `[event_time, count, delay, strata...]`,
#'   delays 1-indexed (single stratum supported in this version).
#' @param m_censored Optional censored-observation matrix (same layout).
#' @param X Optional covariate matrix (`max_time` rows, P columns).
#' @param report_calendar Optional report-date design matrix over the full
#'   calendar grid. These columns affect reporting timing, not incidence.
#' @param report_cohort Optional event-time by stratum by covariate array for
#'   cohort-level reporting covariates.
#' @param revision_calendar Optional revision-date design matrix over the full
#'   calendar grid. These columns affect revision timing, not incidence.
#' @param covariate_roles Named list recording the event, delay, and revision
#'   covariate column names discovered on the source data.
#' @param design_schema Named list of per-role design schemas (levels,
#'   contrasts, centring constants, surviving and dropped terms) so later fits
#'   on the same series reproduce the same columns.
#' @param d_star Optional max-observable-delay vector; if NULL, computed as
#'   `rev(seq_len(max_time)) - 1`.
#' @param delay_only If TRUE, only the delay process is prepared/fit.
#' @param max_time Time-window length; defaults to `max(m[, 1])`.
#' @param num_strata Number of stratum cells (the K-way product of the strata
#'   levels). If `NULL`, inferred from column 4 of `m`. The likelihood is summed
#'   over all `max_time x num_strata` (time, stratum) cells; `1` is unstratified.
#' @param gp_L HSGP boundary factor (> 1). Default 1.5.
#' @param gp_boundary_frac Fraction of the HSGP domain placed left of the data.
#'   Default 0.62.
#' @param ar_sigma_max Upper bound on the AR/beta RW innovation SD. Default 1.
#' @param mu_log_upper_bound Optional override for the softplus ceiling on the
#'   latent log-incidence.  `NULL` (default) derives it from the observed counts
#'   with headroom for the reporting fraction; see `.mu_log_upper_bound()`.
#' @param is_confirmation Deprecated legacy switch.  `TRUE` now errors; use the
#'   dedicated `count_cumulative` model component and cumulative arguments.
#' @param cumulative_levels Optional cumulative levels aligned row-for-row with
#'   `m`; used only by the dedicated count-cumulative composites.
#' @param cumulative_previous_nonzero Optional indicators, aligned with `m`,
#'   that the preceding signed update was non-zero.
#' @param cumulative_settlement Optional positive integer settlement horizon
#'   `H`. It is required for count-cumulative preparation.
#' @param resolution_mode `0L` when the resolution observed is a RETRACTION (the
#'   default), `1L` when it is a CONFIRMATION. Sets the support of the resolution
#'   lag and what a resolved row means for the nowcast target.
#' @param retraction Optional list of linelist-retraction sufficient statistics
#'   from `.linelist_retraction_stats()`. When supplied, the engine carries the
#'   cure-model observation block (see 31_retraction_likelihood.R). Default NULL.
#' @param ... Reserved.
#' @returns A named list of engine inputs.
#' @export
prepare_data <- function(model, m, m_censored = NULL, X = NULL, d_star = NULL,
                         delay_only = FALSE, max_time = NULL, num_strata = NULL,
                         report_calendar = NULL, report_cohort = NULL,
                         revision_calendar = NULL,
                         covariate_roles = NULL, design_schema = NULL,
                         gp_L = 1.5, gp_boundary_frac = 0.62,
                         ar_sigma_max = 1, mu_log_upper_bound = NULL,
                         is_confirmation = FALSE,
                         cumulative_levels = NULL,
                         cumulative_previous_nonzero = NULL,
                         cumulative_settlement = NULL,
                         retraction = NULL, resolution_mode = 0L, ...) {
  if (isTRUE(is_confirmation)) {
    cli::cli_abort(c(
      "The legacy count-cumulative `is_confirmation` engine has been removed.",
      "x" = "It estimated a fixed-`p` Skellam/SkNB model that is not identified by cumulative streams.",
      "i" = "Use `model(cumulative = cumulative_process(...))` and prepare through `nowcast()` or `prepare_from_tbl_now()`."
    ))
  }
  if (!S7::S7_inherits(model, model_class))
    cli::cli_abort("`model` must be a model_class object (use model()).")
  if (!is.matrix(m) || ncol(m) < 3L)
    cli::cli_abort("`m` must be a matrix with >= 3 columns [event_time, count, delay].")
  if (is.null(m_censored)) m_censored <- matrix(0L, nrow = 0L, ncol = ncol(m))
  if (is.null(max_time)) max_time <- max(m[, 1])
  max_time <- as.integer(max_time)

  epi <- model@epidemic; dly <- model@delay; lik <- model@likelihood

  # -- strata -------------------------------------------------------------------
  # Column 4 of `m` holds the 1-indexed stratum-cell of each observation (all 1
  # when unstratified).  `num_strata` is the number of cells (= prod of strata
  # levels); pass it in so empty-in-the-as-of-view cells are still counted.
  cell_of <- if (ncol(m) >= 4L) as.integer(m[, 4]) else rep(1L, nrow(m))
  if (is.null(num_strata)) num_strata <- max(c(1L, cell_of))
  num_strata <- as.integer(num_strata)

  # -- covariates -------------------------------------------------------------
  if (is.null(X)) { X_mat <- matrix(0.0, max_time, 0L); P_val <- 0L }
  else { X_mat <- as.matrix(X); P_val <- ncol(X_mat) }
  report_calendar_mat <- if (is.null(report_calendar))
    matrix(0.0, max_time, 0L) else as.matrix(report_calendar)
  revision_calendar_mat <- if (is.null(revision_calendar))
    matrix(0.0, max_time, 0L) else as.matrix(revision_calendar)
  report_cohort_array <- if (is.null(report_cohort))
    array(0.0, c(max_time, num_strata, 0L)) else as.array(report_cohort)
  if (nrow(report_calendar_mat) < max_time ||
      nrow(revision_calendar_mat) < max_time) {
    cli::cli_abort("Process calendar matrices must have at least `max_time` rows.")
  }
  if (length(dim(report_cohort_array)) != 3L ||
      !identical(as.integer(dim(report_cohort_array)[1:2]),
                 c(max_time, num_strata))) {
    cli::cli_abort("`report_cohort` must be a max_time by num_strata by covariate array.")
  }

  # -- d_star [max_time x num_strata] (same reporting horizon across strata) ----
  d_star_mat <- if (is.null(d_star)) matrix(rev(seq_len(max_time)) - 1L, max_time, num_strata)
    else { dd <- as.matrix(d_star)
           if (ncol(dd) == 1L) matrix(dd[, 1], max_time, num_strata) else dd }

  # -- per-time delay aggregation (FIXED censoring routing) ----------------------
  # Builds a [n_observed_delays x max_time] count matrix and returns its marginals.
  # row_sums[d] = total cases with delay d (summed over all event-times)
  # col_sums[t] = total cases at event-time t (summed over all observed delays)
  aggregate_by_delay_and_time <- function(obs_mat) {
    if (nrow(obs_mat) == 0L)
      return(list(obs_delays = numeric(0), row_sums = numeric(0), col_sums = rep(0, max_time)))

    df <- data.frame(time  = as.integer(obs_mat[, 1]),
                     delay = as.integer(obs_mat[, 3]),
                     count = as.numeric(obs_mat[, 2]))

    agg <- df |>
      dplyr::group_by(.data$delay, .data$time) |>
      dplyr::summarise(count = sum(.data$count), .groups = "drop")

    unique_delays <- sort(unique(agg$delay))
    cases_mat <- matrix(0.0, length(unique_delays), max_time)
    cases_mat[cbind(match(agg$delay, unique_delays), agg$time)] <- agg$count

    list(obs_delays = as.numeric(unique_delays),
         row_sums   = rowSums(cases_mat),
         col_sums   = colSums(cases_mat))
  }
  exact_agg    <- aggregate_by_delay_and_time(m)
  censored_agg <- aggregate_by_delay_and_time(m_censored)

  # censoring point per event-time t (1-indexed delays): c_t = max_time - t + 1
  censoring_col <- as.numeric(max_time - seq_len(max_time) + 1L)

  # -- per-(time, stratum) case counts [max_time x num_strata] ------------------
  # Returns a [max_time x num_strata] matrix; missing (time, stratum) pairs are zero.
  count_matrix <- function(obs_mat) {
    if (nrow(obs_mat) == 0L) return(matrix(0.0, max_time, num_strata))

    df <- data.frame(
      time   = as.integer(obs_mat[, 1]),
      count  = as.numeric(obs_mat[, 2]),
      strata = if (ncol(obs_mat) >= 4L) as.integer(obs_mat[, 4]) else 1L
    )

    agg <- df |>
      dplyr::group_by(.data$time, .data$strata) |>
      dplyr::summarise(count = sum(.data$count), .groups = "drop")

    mat <- matrix(0.0, max_time, num_strata)
    mat[cbind(agg$time, agg$strata)] <- agg$count
    mat
  }
  case_counts <- count_matrix(m) + count_matrix(m_censored)
  casemax <- max(abs(case_counts), na.rm = TRUE)

  # Legacy storage remains inert for compatibility with old serialized engine
  # lists.  New cumulative paths use the explicit arrays and mask below.
  increment_array   <- NULL
  max_conf_delay    <- 0L
  if (isTRUE(is_confirmation) && nrow(m) > 0) {
    max_conf_delay  <- as.integer(max(m[, 3]))            # 1-indexed max delay observed
    increment_array <- array(0.0, dim = c(max_time, max_conf_delay, num_strata))
    # Columns of `m` are (1) 1-indexed event-time, (2) signed increment m_t^d,
    # (3) 1-indexed delay, (4) stratum cell.
    for (row_index in seq_len(nrow(m))) {
      time_index      <- as.integer(m[row_index, 1])
      delay_index     <- as.integer(m[row_index, 3])
      strata_index    <- if (ncol(m) >= 4L) as.integer(m[row_index, 4]) else 1L
      increment_value <- m[row_index, 2]
      increment_array[time_index, delay_index, strata_index] <- increment_value
    }
  }


  # -- revised count-cumulative observation arrays -----------------------------
  # Only rows explicitly completed inside the as-of triangle are marked observed.
  # Every other array cell remains masked; its numeric zero is storage only and
  # must never contribute to a likelihood.
  is_count_cumulative <- !is.null(cumulative_levels)
  settlement_horizon <- if (is_count_cumulative) {
    .validate_settlement_horizon(cumulative_settlement)
  } else 0L
  cumulative_level_array <- signed_update_array <-
    previous_nonzero_array <- age_array <- observation_mask <- NULL
  if (is_count_cumulative) {
    if (length(cumulative_levels) != nrow(m) ||
        length(cumulative_previous_nonzero) != nrow(m)) {
      cli::cli_abort("Count-cumulative level/update metadata must align one-to-one with `m` rows.")
    }
    array_dim <- c(max_time, settlement_horizon + 1L, num_strata)
    cumulative_level_array <- array(0.0, dim = array_dim)
    signed_update_array <- array(0.0, dim = array_dim)
    previous_nonzero_array <- array(0.0, dim = array_dim)
    age_array <- array(-1L, dim = array_dim)
    observation_mask <- array(FALSE, dim = array_dim)
    for (row_index in seq_len(nrow(m))) {
      time_index <- as.integer(m[row_index, 1L])
      delay_index <- as.integer(m[row_index, 3L])
      strata_index <- if (ncol(m) >= 4L) as.integer(m[row_index, 4L]) else 1L
      if (delay_index < 1L || delay_index > settlement_horizon + 1L) next
      cumulative_level_array[time_index, delay_index, strata_index] <-
        cumulative_levels[row_index]
      signed_update_array[time_index, delay_index, strata_index] <- m[row_index, 2L]
      previous_nonzero_array[time_index, delay_index, strata_index] <-
        cumulative_previous_nonzero[row_index]
      age_array[time_index, delay_index, strata_index] <- delay_index - 1L
      observation_mask[time_index, delay_index, strata_index] <- TRUE
    }
  }

  # -- num_basis (auto) ---------------------------------------------------------
  nb_model <- if (S7::S7_inherits(epi, hsgp_epidemic_class)) epi@num_basis else 0L
  num_basis_val <- if (nb_model > 0L) as.integer(nb_model)
                   else .auto_hsgp_num_basis(max_time)
  if (S7::S7_inherits(epi, hsgp_epidemic_class) && !isTRUE(delay_only)) {
    .warn_hsgp_basis_fraction(num_basis_val, max_time, explicit = nb_model > 0L)
  }

  # -- tmax_model (HSGP time normalisation) -------------------------------------
  tmax_model_val <- if (S7::S7_inherits(epi, hsgp_epidemic_class)) {
    if (epi@tmax_model > 0) as.integer(epi@tmax_model) else max(3L, max_time)
  } else 100L

  # -- np_model_length (Dirichlet) ----------------------------------------------
  np_len <- if (S7::S7_inherits(dly, dirichlet_delay_class)) {
    if (length(dly@bins) == 1 && !is.na(dly@bins)) as.integer(dly@bins) else as.integer(max(m[, 3]))
  } else 1L

  list(
    # dimensions / config
    max_time = max_time, num_strata = num_strata, P = P_val, X = X_mat,
    event_coef_names = colnames(X_mat) %||% character(0),
    P_delay_calendar = ncol(report_calendar_mat),
    P_delay_cohort = dim(report_cohort_array)[3L],
    P_delay = ncol(report_calendar_mat) + dim(report_cohort_array)[3L],
    delay_coef_names = c(
      colnames(report_calendar_mat) %||% character(0),
      dimnames(report_cohort_array)[[3L]] %||% character(0)
    ),
    P_revision_calendar = ncol(revision_calendar_mat),
    P_revision_row = ncol(retraction$revision_row_design %||%
      matrix(0.0, 0L, 0L)),
    P_revision = ncol(revision_calendar_mat) +
      ncol(retraction$revision_row_design %||% matrix(0.0, 0L, 0L)),
    revision_coef_names = c(
      colnames(revision_calendar_mat) %||% character(0),
      colnames(retraction$revision_row_design %||% matrix(0.0, 0L, 0L)) %||%
        character(0)
    ),
    report_calendar = report_calendar_mat,
    report_cohort = report_cohort_array,
    revision_calendar = revision_calendar_mat,
    revision_rows = retraction$revision_rows,
    revision_row_design = retraction$revision_row_design,
    design_schema = c(
      design_schema %||% list(),
      list(revision_row = retraction$revision_row_schema)
    ),
    covariate_roles = covariate_roles %||% list(
      event = colnames(X_mat) %||% character(0),
      delay = character(0), revision = character(0)
    ),
    delay_only = isTRUE(delay_only),
    delay_family = as.integer(dly@num_id),
    epidemic_model = as.integer(epi@num_id),
    epidemic_name = as.character(epi@name),
    is_negative_binomial = as.integer(lik@num_id),
    num_delay_seasons = as.integer(dly@num_delay_seasons),
    np_model_length = np_len,
    m = m, m_censored = m_censored,
    # confirmation (count-cumulative) signed-increment likelihood
    is_confirmation = as.integer(isTRUE(is_confirmation)),
    increment_array = increment_array, max_conf_delay = max_conf_delay,
    is_count_cumulative = as.integer(is_count_cumulative),
    count_cumulative_observation = if (is_count_cumulative &&
      isTRUE(model@cumulative@active)) switch(
        model@cumulative@observation,
        cumulative = 1L, hurdle_ztnb = 2L, hurdle_ztpoisson = 3L
      ) else 0L,
    settlement_horizon = settlement_horizon,
    cumulative_level_array = cumulative_level_array,
    signed_update_array = signed_update_array,
    observation_mask = observation_mask,
    age_array = age_array,
    previous_nonzero_array = previous_nonzero_array,
    # linelist retractions (cure-model block).  `case_counts` above already counts
    # EVERY row -- standing and already-retracted alike -- which is exactly the
    # k_t the count block models; `standing_counts` is the smaller, currently-on-
    # the-books total that the posterior predictive starts from.
    is_linelist_retraction = as.integer(!is.null(retraction)),
    resolution_mode        = as.integer(resolution_mode),
    retract_table          = retraction$retract_table,
    standing_table         = retraction$standing_table,
    censored_patterns      = retraction$censored_patterns,
    n_retracted_by_stratum = retraction$n_retracted_by_stratum %||% numeric(0),
    n_retracted            = retraction$n_retracted            %||% 0,
    n_positive_by_stratum  = retraction$n_positive_by_stratum  %||% numeric(0),
    n_negative_by_stratum  = retraction$n_negative_by_stratum  %||% numeric(0),
    n_positive             = retraction$n_positive             %||% 0,
    n_negative             = retraction$n_negative             %||% 0,
    n_standing             = retraction$n_standing             %||% 0,
    n_censored             = retraction$n_censored             %||% 0,
    standing_rows          = retraction$standing_rows,
    standing_censored_rows = retraction$standing_censored_rows,
    standing_counts        = retraction$standing_counts,
    resolved_counts        = retraction$resolved_counts,
    max_report_age         = retraction$max_report_age         %||% 0L,
    retract_grid_max       = retraction$max_grid               %||% 0L,
    # delay aggregation (per-time censoring)
    obs_delays = exact_agg$obs_delays, row_sums_exact = exact_agg$row_sums, col_sums_exact = exact_agg$col_sums,
    obs_delays_cens = censored_agg$obs_delays, row_sums_cens = censored_agg$row_sums, col_sums_cens = censored_agg$col_sums,
    censoring_col = censoring_col,
    max_delay_obs = if (nrow(m) > 0) max(m[, 3]) else 1,
    # epidemic
    case_counts = case_counts, d_star = d_star_mat, casemax = casemax,
    # hsgp config
    num_basis = num_basis_val, gp_kernel = if (S7::S7_inherits(epi, hsgp_epidemic_class)) epi@gp_kernel else 2L,
    gp_basis = if (S7::S7_inherits(epi, hsgp_epidemic_class)) epi@gp_basis else 1L,
    tmax_model = tmax_model_val,
    gp_L = gp_L,
    gp_L_left  = 2 * gp_L * gp_boundary_frac,
    gp_L_right = max(2 * gp_L * (1 - gp_boundary_frac), 1e-6),
    # SIR
    N_pop = if (S7::S7_inherits(epi, sir_epidemic_class)) epi@N_pop else 1e6,
    use_beta_rw_trend = if (S7::S7_inherits(epi, sir_epidemic_class)) as.integer(epi@use_beta_rw_trend) else 1L,
    # ARIMA (num_id 5)
    arima_p = if (S7::S7_inherits(epi, arima_epidemic_class)) as.integer(epi@p) else 0L,
    arima_d = if (S7::S7_inherits(epi, arima_epidemic_class)) as.integer(epi@d) else 0L,
    arima_q = if (S7::S7_inherits(epi, arima_epidemic_class)) as.integer(epi@q) else 0L,
    arima_include_drift = if (S7::S7_inherits(epi, arima_epidemic_class)) as.integer(epi@include_drift) else 0L,
    # ETS family: exponential smoothing, random walk, naive, Theta (num_id 6)
    ets_has_slope = if (S7::S7_inherits(epi, ets_epidemic_class)) as.integer(epi@trend == "additive") else 0L,
    ets_damped = if (S7::S7_inherits(epi, ets_epidemic_class)) as.integer(epi@damped) else 0L,
    ets_include_drift = if (S7::S7_inherits(epi, ets_epidemic_class)) as.integer(epi@include_drift) else 0L,
    # Structural time series (num_id 7)
    sts_has_slope = if (S7::S7_inherits(epi, sts_epidemic_class)) as.integer(epi@trend != "local_level") else 0L,
    sts_reverting_slope = if (S7::S7_inherits(epi, sts_epidemic_class)) as.integer(epi@trend == "semilocal") else 0L,
    # Custom epidemic
    custom_epidemic_n_params = if (S7::S7_inherits(epi, custom_epidemic_class)) as.integer(epi@n_params) else 0L,
    # bounds
    mu_log_upper_bound = .mu_log_upper_bound(casemax, mu_log_upper_bound),
    # The pre-2.5.0 bound, kept so a cap warning can name the tighter ceiling
    # concretely.  It is NOT used by the objective; see `.mu_log_upper_bound()`.
    mu_log_upper_bound_legacy = min(max(6, log1p(casemax)), 16),
    ar_sigma_max = ar_sigma_max
  )
}
