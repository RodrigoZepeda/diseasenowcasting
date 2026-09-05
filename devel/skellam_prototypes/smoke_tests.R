#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr); library(tbl.now); library(RTMB)
  pkgload::load_all(".", quiet = TRUE)
})
source("devel/skellam_prototypes/flusight_data.R")
source("devel/skellam_prototypes/rtmb_count_cumulative.R")

prepared <- prepare_flusight("Texas", max_delay = 6L)
panel <- state_panel(prepared, "Texas", report_cut = 12L)
checks <- list()
for (epidemic in c("ar", "hsgp", "sir")) {
  for (delay in c("lognormal", "gamma", "generalized_gamma")) {
    data <- make_prototype_data(panel, epidemic = epidemic)
    built <- build_rtmb_prototype(data, "zinb", delay, delay)
    value <- built$obj$fn(built$obj$par)
    gradient <- built$obj$gr(built$obj$par)
    stopifnot(is.finite(value), all(is.finite(gradient)))
    checks[[length(checks) + 1L]] <- data.frame(
      epidemic = epidemic, delay = delay, nll = value,
      gradient_finite = all(is.finite(gradient))
    )
  }
}

# Directly verify the signed-ZINB mean preservation used in the likelihood.
alpha <- 3.2; omega <- 1.1; zeta <- 0.7; size <- 0.4
nb_mean <- (alpha + omega) / (1 - zeta)
k <- 0:10000
pm <- dnbinom(k, size = size, mu = nb_mean)
theta <- alpha / (alpha + omega)
numeric_mean <- (1 - zeta) * sum(k * pm) * (2 * theta - 1)
stopifnot(abs(numeric_mean - (alpha - omega)) < 1e-6)

print(bind_rows(checks), row.names = FALSE)
cat(sprintf("\nSigned-ZINB mean: numeric %.8f, target alpha-omega %.8f\n",
            numeric_mean, alpha - omega))
