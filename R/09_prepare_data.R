# =============================================================================
# prepare_data() -- build the RTMB engine inputs from an observation matrix
# =============================================================================
# Analogue of diseasenowcast2::data_to_stan(), but emits the (Stan-free) data
# list the RTMB objective consumes.  Reproduces the FIXED delay-only censoring:
# in delay_only mode each event at time t is censored at c_t = max_time - t + 1
# (the corrected `delay_routing[t] = t` behaviour), and num_delay_seasons = 1
# keeps the delay log-mean constant across t.
# =============================================================================

#' Prepare data for the RTMB nowcast engine
#'
#' @param model A [model()] object.
#' @param m Observation matrix: columns `[event_time, count, delay, strata...]`,
#'   delays 1-indexed (single stratum supported in this version).
#' @param m_censored Optional censored-observation matrix (same layout).
#' @param X Optional covariate matrix (`max_time` rows, P columns).
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
#' @param ... Reserved.
#' @returns A named list of engine inputs.
#' @export
prepare_data <- function(model, m, m_censored = NULL, X = NULL, d_star = NULL,
                         delay_only = FALSE, max_time = NULL, num_strata = NULL,
                         gp_L = 1.5, gp_boundary_frac = 0.62,
                         ar_sigma_max = 1, ...) {
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
      dplyr::group_by(delay, time) |>
      dplyr::summarise(count = sum(count), .groups = "drop")

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
      dplyr::group_by(time, strata) |>
      dplyr::summarise(count = sum(count), .groups = "drop")

    mat <- matrix(0.0, max_time, num_strata)
    mat[cbind(agg$time, agg$strata)] <- agg$count
    mat
  }
  case_counts <- count_matrix(m) + count_matrix(m_censored)
  casemax <- max(case_counts, na.rm = TRUE)

  # -- num_basis (auto) ---------------------------------------------------------
  nb_model <- if (S7::S7_inherits(epi, hsgp_epidemic_class)) epi@num_basis else 0L
  num_basis_val <- if (nb_model > 0L) as.integer(nb_model)
                   else if (max_time < 10) 3L
                   else if (max_time < 20) 8L
                   else min(150L, max(12L, as.integer(ceiling(1.5 * sqrt(max_time)))))

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
    delay_only = isTRUE(delay_only),
    delay_family = as.integer(dly@num_id),
    epidemic_model = as.integer(epi@num_id),
    is_negative_binomial = as.integer(lik@num_id),
    num_delay_seasons = as.integer(dly@num_delay_seasons),
    np_model_length = np_len,
    m = m,
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
    # Custom process
    custom_process_n_params = if (S7::S7_inherits(epi, custom_process_class)) as.integer(epi@n_params) else 0L,
    # bounds
    mu_log_upper_bound = min(max(6, log1p(casemax)), 16),
    ar_sigma_max = ar_sigma_max
  )
}
