# Which is wrong on the worst cells -- the SPA or my reference?  Adjudicate with
# an independent Monte-Carlo estimate, which needs no analysis at all.
suppressMessages(pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE))
invisible(capture.output(source("devel/spa_diagnostics/01_exact_reference.R")))

res <- readRDS("devel/spa_diagnostics/flusight_cells.rds")
mixed <- res$low$mixed
mixed$abs_e <- abs(mixed$e)
worst <- head(mixed[order(-mixed$abs_e), ], 6)

cat("=== worst cells under the low-p solution ===\n")
cat(sprintf("%6s %6s %10s %10s %12s %12s %10s\n",
            "z", "d", "alpha", "omega", "logP_SPA", "logP_exact", "e"))
set.seed(3)
for (i in seq_len(nrow(worst))) {
  z <- worst$z[i]; a <- worst$alpha[i]; w <- worst$omega[i]
  l_spa   <- diseasenowcasting:::.log_skellam_increment(z, a, w, 1L)
  l_exact <- log_skellam_exact(z, a + 1e-8, w + 1e-8)
  cat(sprintf("%6.0f %6.0f %10.2f %10.2f %12.4f %12.4f %10.4f\n",
              z, d <- worst$d[i], a, w, l_spa, l_exact, l_spa - l_exact))
}

cat("\n=== independent Monte-Carlo adjudication ===\n")
cat("Draw A ~ Pois(alpha), W ~ Pois(omega), count how often A - W == z.\n")
for (i in seq_len(min(3L, nrow(worst)))) {
  z <- worst$z[i]; a <- worst$alpha[i]; w <- worst$omega[i]
  n_draw <- 4e7
  hits <- sum((rpois(n_draw, a) - rpois(n_draw, w)) == z)
  mc    <- hits / n_draw
  l_spa   <- diseasenowcasting:::.log_skellam_increment(z, a, w, 1L)
  l_exact <- log_skellam_exact(z, a + 1e-8, w + 1e-8)
  cat(sprintf("  z=%.0f alpha=%.1f omega=%.1f : MC logP = %s (%d hits)  SPA %.4f  exact %.4f\n",
              z, a, w, if (hits > 0) sprintf("%.4f", log(mc)) else "< -17.5",
              hits, l_spa, l_exact))
}

cat("\n=== where does the SPA break? a controlled sweep at fixed alpha/omega ratio ===\n")
cat("Mean is alpha - omega; z is swept away from it in units of sd = sqrt(alpha+omega).\n")
for (params in list(c(500, 450), c(2000, 1800), c(4000, 3500))) {
  a <- params[1]; w <- params[2]
  sd <- sqrt(a + w); mu <- a - w
  cat(sprintf("\n  alpha=%.0f omega=%.0f  (mean %.0f, sd %.1f)\n", a, w, mu, sd))
  cat(sprintf("  %10s %8s %12s %12s %10s\n", "z", "(z-mu)/sd", "SPA", "exact", "e"))
  for (k in c(0, 1, 2, 4, 8, 16, 32)) {
    z <- round(mu + k * sd)
    l_spa <- diseasenowcasting:::.log_skellam_increment(z, a, w, 1L)
    l_ex  <- log_skellam_exact(z, a + 1e-8, w + 1e-8)
    cat(sprintf("  %10.0f %8.1f %12.4f %12.4f %10.4f\n", z, k, l_spa, l_ex, l_spa - l_ex))
  }
}
