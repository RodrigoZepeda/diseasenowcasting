# =============================================================================
# Phase 1, steps 2-3: how wrong is the saddlepoint blend, and is it normalised?
# =============================================================================
# Deliverables A (accuracy, incl. z = 0) and B (normalisation) of the plan.
#
# NOTE on d = 0.  The production likelihood does NOT approximate there: bin_type
# 0 returns an exact `dpois(z, alpha)`, which is correct by construction because
# g_C(0) = 0 means no retraction can land at delay 0.  So "zero increments" split
# into two populations, and only the d >= 1 ones (bin_type 1) involve any
# approximation at all.  Everything below is bin_type 1.
# =============================================================================

suppressMessages(pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE))
invisible(capture.output(source("devel/spa_diagnostics/01_exact_reference.R")))

spa <- function(z, alpha, omega) {
  diseasenowcasting:::.log_skellam_increment(z, alpha, omega, 1L)
}
# The production function adds 1e-8 to both rates internally; give the reference
# the same rates so the comparison isolates the APPROXIMATION, not the epsilon.
exact <- function(z, alpha, omega) log_skellam_exact(z, alpha + 1e-8, omega + 1e-8)

grid <- list(c(0.1, 0.1), c(1, 1), c(10, 9), c(10, 10),
             c(100, 99), c(100, 100), c(1000, 999), c(1000, 1000),
             c(0.5, 0.05), c(5, 0.5), c(50, 5), c(500, 50),
             c(2, 8), c(20, 80))

cat("\n=== A. SPA error e = logP_SPA - logP_exact, by (alpha, omega) ===\n")
cat(sprintf("%8s %8s %10s %10s %10s %10s %10s\n",
            "alpha", "omega", "e(z=0)", "max|e|", "med|e|", "p99|e|", "sum e"))
rows <- list()
for (params in grid) {
  alpha <- params[1]; omega <- params[2]
  # Centre the window on the MEAN (alpha - omega), not on zero: at alpha=500,
  # omega=50 the mass sits around 450 and a zero-centred window misses it entirely.
  centre <- round(alpha - omega)
  span   <- ceiling(8 * sqrt(alpha + omega)) + 15
  z_seq  <- seq.int(centre - span, centre + span)
  e <- vapply(z_seq, function(z) spa(z, alpha, omega) - exact(z, alpha, omega), numeric(1))
  # Weight the "total signed error" by how often each z actually occurs: an error
  # in the far tail is irrelevant, one at the mode is not.
  w <- exp(vapply(z_seq, exact, numeric(1), alpha, omega))
  # z = 0 may lie outside the window when the mean is far from zero, so compute it
  # directly rather than looking it up.
  e_at_zero <- spa(0, alpha, omega) - exact(0, alpha, omega)
  rows[[length(rows) + 1L]] <- data.frame(
    alpha = alpha, omega = omega, e0 = e_at_zero,
    max_abs = max(abs(e)), med_abs = median(abs(e)),
    p99_abs = quantile(abs(e), 0.99), weighted_signed = sum(w * e))
  cat(sprintf("%8.1f %8.1f %10.2e %10.2e %10.2e %10.2e %10.2e\n",
              alpha, omega, e_at_zero, max(abs(e)), median(abs(e)),
              quantile(abs(e), 0.99), sum(w * e)))
}

cat("\n=== B. SPA normalisation: sum_z P_SPA(z) ===\n")
cat("A parameter-DEPENDENT normalisation error acts like an extra likelihood term.\n")
cat(sprintf("%8s %8s %16s %14s\n", "alpha", "omega", "sum_z P_SPA(z)", "log(sum)"))
for (params in grid) {
  alpha <- params[1]; omega <- params[2]
  centre <- round(alpha - omega)
  span   <- ceiling(12 * sqrt(alpha + omega)) + 30
  z_seq  <- seq.int(centre - span, centre + span)
  total  <- sum(exp(vapply(z_seq, spa, numeric(1), alpha, omega)))
  cat(sprintf("%8.1f %8.1f %16.10f %14.2e\n", alpha, omega, total, log(total)))
}

cat("\n=== B2. Does the normalisation error VARY along the alpha ~ omega ridge? ===\n")
cat("This is the direction the pathological fit moves in: alpha ~ omega, both large.\n")
cat(sprintf("%10s %16s %14s\n", "alpha=omega", "sum_z P_SPA(z)", "log(sum)"))
for (rate in c(0.5, 1, 2, 5, 10, 15, 20, 22, 24, 26, 28, 30, 35, 40, 50,
               75, 100, 250, 500, 1000)) {
  span  <- ceiling(12 * sqrt(2 * rate)) + 30
  z_seq <- seq.int(-span, span)          # alpha = omega, so the mean IS zero
  total <- sum(exp(vapply(z_seq, spa, numeric(1), rate, rate)))
  cat(sprintf("%10.1f %16.10f %14.2e\n", rate, total, log(total)))
}
saveRDS(do.call(rbind, rows), "devel/spa_diagnostics/spa_accuracy.rds")
