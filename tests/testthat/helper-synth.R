# Test-local LogNormal native params (inlined, so the synthetic-data helpers do
# not depend on a non-exported package internal — see scoring/validation note).
# log_mean = log of the natural-scale mean; sd = natural-scale SD.
lognormal_native <- function(log_mean, sd) {
  natural_mean <- exp(log_mean)
  log_scale    <- sqrt(log1p(sd^2 / natural_mean^2))
  log_location <- log_mean - 0.5 * log_scale^2
  list(log_location = log_location, log_scale = log_scale)
}

# Shared synthetic bell-curve epidemic with LogNormal reporting delays, censored
# to the observation horizon.  Visible to every test file (helper-*.R is sourced
# into the test environment), so .make_synth is defined once here.
.make_synth <- function(Tn = 80L, peak = 80, ctr = 45, wid = 14,
                        base = 8, mu_log = log(5), sigma = 4, seed = 7) {
  set.seed(seed)
  lambda <- peak * exp(-0.5 * ((seq_len(Tn) - ctr) / wid)^2) + base
  native <- lognormal_native(mu_log, sigma)
  rows <- list()
  for (t in seq_len(Tn)) {
    n_cases <- rpois(1, lambda[t])
    if (n_cases > 0) {
      delays <- pmax(1L, ceiling(rlnorm(n_cases, native$log_location, native$log_scale)))
      observable <- delays <= (Tn - t + 1)
      if (any(observable)) {
        delay_counts <- table(delays[observable])
        for (k in seq_along(delay_counts))
          rows[[length(rows) + 1]] <- c(t, as.integer(delay_counts[k]),
                                        as.integer(names(delay_counts)[k]), 1L)
      }
    }
  }
  m <- do.call(rbind, rows); colnames(m) <- c("event", "count", "delay", "strata")
  list(m = m, Tn = Tn, lambda = lambda)
}
