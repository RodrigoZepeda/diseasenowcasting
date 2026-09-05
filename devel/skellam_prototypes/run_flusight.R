#!/usr/bin/env Rscript

# End-to-end prototype. Examples:
#   Rscript devel/skellam_prototypes/run_flusight.R
#   STATE=California EPIDEMIC=hsgp N_ORIGINS=4 Rscript devel/skellam_prototypes/run_flusight.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tbl.now)
  library(RTMB)
  pkgload::load_all(".", quiet = TRUE)
})

source("devel/skellam_prototypes/flusight_data.R")
source("devel/skellam_prototypes/rtmb_count_cumulative.R")

state <- Sys.getenv("STATE", "Texas")
epidemic <- Sys.getenv("EPIDEMIC", "ar")
delay_family <- Sys.getenv("DELAY", "lognormal")
n_origins <- as.integer(Sys.getenv("N_ORIGINS", "3"))
n_draws <- as.integer(Sys.getenv("N_DRAWS", "1000"))
horizon <- as.integer(Sys.getenv("HORIZON", "4"))
seed <- as.integer(Sys.getenv("SEED", "20260904"))
set.seed(seed)

prepared <- prepare_flusight(states = state)
panel <- state_panel(prepared, state)

cat(sprintf("Prepared %s: %d compressed publication weeks, %d cumulative cells, %d explicit zero cells added.\n",
            state, nrow(prepared$clock), nrow(panel$cells), prepared$n_zeroes_added))
cat(sprintf("Calendar gaps are one model step each; largest gap: %d days.\n",
            max(prepared$calendar_gaps$calendar_days)))
cat(sprintf("Empirical fixed p = %.4f (first=%g, later up=%g, down=%g).\n\n",
            panel$p_empirical, panel$first_total, panel$up, panel$down))

fit_one <- function(panel, observation) {
  dat <- make_prototype_data(panel, epidemic = epidemic)
  built <- build_rtmb_prototype(
    dat,
    observation = observation,
    appearance_delay = delay_family,
    retraction_delay = delay_family
  )
  fit_rtmb_prototype(built)
}

cat("Fitting full-window mechanistic Skellam...\n")
fit_skellam <- fit_one(panel, "skellam")
cat(sprintf("  convergence=%d, nll=%.2f, max|gradient|=%.3g\n",
            fit_skellam$fit$convergence, fit_skellam$nll,
            max(abs(fit_skellam$obj$gr(fit_skellam$fit$par)))))

cat("Fitting full-window signed ZINB...\n")
fit_zinb <- fit_one(panel, "zinb")
cat(sprintf("  convergence=%d, nll=%.2f, max|gradient|=%.3g\n",
            fit_zinb$fit$convergence, fit_zinb$nll,
            max(abs(fit_zinb$obj$gr(fit_zinb$fit$par)))))

zero_diagnostic <- function(fit) {
  r <- reconstruct_prototype(fit)
  x <- fit$data
  keep <- x$age > 0L
  expected <- numeric(sum(keep))
  ii <- which(keep)
  for (j in seq_along(ii)) {
    i <- ii[j]
    alpha <- max(r$mu[x$event_slot[i]] * r$g_d[x$age[i] + 1L], 1e-10)
    omega <- max(r$mu[x$event_slot[i]] * (1 - r$p) * r$g_w[x$age[i] + 1L], 1e-10)
    if (fit$observation == "skellam") {
      expected[j] <- exp(diseasenowcasting:::.log_skellam_increment(
        0, alpha, omega, 1L
      ))
    } else {
      lp <- r$par$zi_intercept + r$par$zi_age * log1p(x$age[i]) +
        r$par$zi_previous * x$previous_moved[i]
      zeta <- plogis(lp)
      expected[j] <- zeta + (1 - zeta) * dnbinom(
        0, size = exp(r$par$log_nb_size),
        mu = (alpha + omega) / (1 - zeta + 1e-8)
      )
    }
  }
  data.frame(model = fit$observation,
             observed_zero_rate = mean(x$increment[keep] == 0),
             expected_zero_rate = mean(expected))
}

zero_table <- bind_rows(zero_diagnostic(fit_skellam), zero_diagnostic(fit_zinb))
cat("\nPost-first-publication zero diagnostic:\n")
print(zero_table, row.names = FALSE, digits = 4)

pinball_wis <- function(q, y, levels) {
  2 * mean(ifelse(y >= q, levels * (y - q), (1 - levels) * (q - y)))
}

empirical_draws <- function(training_cells, age0, age1, current, n) {
  wide <- training_cells |>
    filter(.data$age %in% c(age0, age1)) |>
    select("event_index", "age", "cumulative") |>
    tidyr::pivot_wider(names_from = .data$age, values_from = .data$cumulative,
                       names_prefix = "age_")
  c0 <- wide[[paste0("age_", age0)]]
  c1 <- wide[[paste0("age_", age1)]]
  ratio <- c1 / c0
  ratio <- ratio[is.finite(ratio) & ratio >= 0]
  if (length(ratio) < 5L) return(rep(current, n))
  # Repeat the empirical support deterministically so every fitted model is
  # compared with exactly the same baseline quantiles.
  current * rep_len(ratio, n)
}

forecast_existing <- function(fit, training_panel, full_panel, origin, h, n) {
  current <- full_panel$cells |>
    filter(.data$report_index == origin, .data$cumulative >= 0) |>
    select("event_index", age0 = "age", current = "cumulative",
           previous_moved = "previous_moved")
  truth <- full_panel$cells |>
    filter(.data$report_index == origin + h) |>
    select("event_index", age1 = "age", truth = "cumulative")
  targets <- inner_join(current, truth, by = "event_index") |>
    filter(.data$age1 <= full_panel$cells$age |> max(), .data$age1 > .data$age0)
  if (!nrow(targets)) return(NULL)

  event_match <- match(targets$event_index, fit$data$event_levels)
  targets <- targets[!is.na(event_match), , drop = FALSE]
  event_match <- event_match[!is.na(event_match)]
  out <- vector("list", nrow(targets))
  for (i in seq_len(nrow(targets))) {
    cur <- rep(targets$current[i], n)
    moved <- rep(targets$previous_moved[i], n)
    for (age in seq.int(targets$age0[i] + 1L, targets$age1[i])) {
      delta <- simulate_update(fit, event_match[i], age,
                               previous_moved = moved, n = n)
      cur <- pmax(cur + delta, 0)
      moved <- as.numeric(delta != 0)
    }
    empirical <- empirical_draws(training_panel$cells, targets$age0[i],
                                  targets$age1[i], targets$current[i], n)
    out[[i]] <- data.frame(
      event_index = targets$event_index[i], truth = targets$truth[i],
      model_draw = cur, empirical_draw = empirical
    )
  }
  bind_rows(out)
}

report_indices <- sort(unique(panel$cells$report_index))
eligible <- report_indices[report_indices + horizon <= max(report_indices)]
origins <- if (n_origins > 0L) utils::tail(eligible, n_origins) else integer()
levels <- c(0.05, seq(0.1, 0.9, 0.1), 0.95)
scores <- list()

cat(sprintf("\nRolling-origin prototype: %d origins x horizons 1..%d.\n",
            length(origins), horizon))
for (origin in origins) {
  training_panel <- state_panel(prepared, state, report_cut = origin)
  cat(sprintf("  origin %d: fitting", origin))
  fits <- list(skellam = fit_one(training_panel, "skellam"),
               zinb = fit_one(training_panel, "zinb"))
  cat("; forecasting\n")
  for (h in seq_len(horizon)) {
    for (model_name in names(fits)) {
      draws <- forecast_existing(fits[[model_name]], training_panel, panel,
                                 origin, h, n_draws)
      if (is.null(draws)) next
      for (event in unique(draws$event_index)) {
        z <- draws[draws$event_index == event, ]
        q_model <- quantile(z$model_draw, levels, names = FALSE)
        q_emp <- quantile(z$empirical_draw, levels, names = FALSE)
        scores[[length(scores) + 1L]] <- data.frame(
          origin = origin, horizon = h, event_index = event,
          model = model_name, truth = z$truth[1L],
          wis_model = pinball_wis(q_model, z$truth[1L], levels),
          wis_empirical = pinball_wis(q_emp, z$truth[1L], levels),
          coverage90 = z$truth[1L] >= q_model[1L] &&
            z$truth[1L] <= q_model[length(q_model)]
        )
      }
    }
  }
}

scores <- bind_rows(scores)
summary_scores <- if (nrow(scores)) {
  scores |>
    group_by(.data$model) |>
    summarise(n = dplyr::n(), wis = mean(.data$wis_model),
              empirical_wis = mean(.data$wis_empirical),
              skill_vs_empirical = 1 - .data$wis / .data$empirical_wis,
              coverage90 = mean(.data$coverage90), .groups = "drop")
} else {
  data.frame(model = character(), n = integer(), wis = numeric(),
             empirical_wis = numeric(), skill_vs_empirical = numeric(),
             coverage90 = numeric())
}

cat("\nMatched rolling-origin scores:\n")
print(summary_scores, row.names = FALSE, digits = 4)

dir.create("devel/skellam_prototypes/results", showWarnings = FALSE)
saveRDS(
  list(config = list(state = state, epidemic = epidemic,
                     delay = delay_family, seed = seed),
       zero_diagnostic = zero_table, scores = scores,
       summary = summary_scores,
       fits = list(skellam = fit_skellam$fit, zinb = fit_zinb$fit)),
  file.path("devel/skellam_prototypes/results",
            paste0("flusight_", gsub(" ", "_", tolower(state)), "_",
                   epidemic, ".rds"))
)
