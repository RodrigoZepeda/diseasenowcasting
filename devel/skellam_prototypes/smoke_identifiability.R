#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(RTMB)
  pkgload::load_all(".", quiet = TRUE)
})
source("devel/skellam_prototypes/flusight_asof_data.R")
source("devel/skellam_prototypes/rtmb_identifiability_models.R")

prepared <- prepare_flusight_asof("Texas", settlement_horizon = 4L)
for (clock in c("calendar", "compressed")) {
  panel <- asof_panel(prepared, "Texas", as.Date("2025-01-02"), clock)
  stopifnot(panel$latest_report_date <= as.Date("2025-01-02"),
            max(panel$cells$event_date_actual) <= as.Date("2025-01-02"),
            max(panel$cells$report_date_actual) <= as.Date("2025-01-02"))
}

panel <- asof_panel(prepared, "Texas", as.Date("2025-01-02"), "compressed")
for (epidemic in c("ar", "hsgp", "sir")) {
  data <- make_identifiability_data(panel, epidemic, num_basis = 5L)
  for (delay in c("lognormal", "gamma", "generalized_gamma")) {
    for (model in c("cumulative_poisson", "hurdle_ztnb")) {
      built <- build_identifiability_model(data, model, delay, delay)
      value <- built$obj$fn(built$obj$par)
      gradient <- built$obj$gr(built$obj$par)
      stopifnot(is.finite(value), all(is.finite(gradient)))
    }
  }
}

# The mechanistic identity behind both composite likelihoods.
data <- make_identifiability_data(panel, "ar")
built <- build_identifiability_model(data, "hurdle_ztnb")
built$par_list <- built$obj$env$parList(built$obj$par)
components <- identifiability_components(built)
expected_level <- cumsum(components$alpha_unit - components$omega_unit)
stopifnot(max(abs(expected_level - components$q_C)) < 1e-10)

# Verify that nu indexes the mean of the TRUNCATED distribution, not its parent.
for (size in c(0.05, 0.5, 1, 10)) {
  for (nu in c(1.01, 1.2, 2, 10)) {
    parent <- ztnb_parent_mean_numeric(nu, size)
    p0 <- (size / (size + parent))^size
    stopifnot(abs(parent / (1 - p0) - nu) < 1e-6)
  }
}

set.seed(1)
alpha <- 1.4
omega <- 0.6
total <- alpha + omega
nonnull <- (1 - exp(-total)) * plogis(-0.5)
moved <- runif(2e5) < nonnull
update <- numeric(length(moved))
magnitude <- simulate_ztnb(sum(moved), total / nonnull, 0.8)
update[moved] <- ifelse(runif(sum(moved)) < alpha / total,
                        magnitude, -magnitude)
stopifnot(abs(mean(update) - (alpha - omega)) < 0.03)

# The optimized cumulative path integrates the high-dimensional epidemic
# innovations with Laplace rather than leaving them in the outer MAP problem.
laplace_fit <- build_identifiability_model(
  data, "cumulative_poisson", use_random = TRUE
) |>
  fit_identifiability_model(control = list(iter.max = 300, eval.max = 800))
stopifnot(laplace_fit$fit$convergence == 0L,
          is.finite(laplace_fit$max_gradient),
          laplace_fit$max_gradient < 0.1)

cat("All as-of, RTMB tape, cumulative-mean, and hurdle-ZTNB checks passed.\n")
