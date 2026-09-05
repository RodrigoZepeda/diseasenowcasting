# =============================================================================
# Phase 1, step 1: an EXACT Skellam log-pmf that shares no machinery with the
# production likelihood, and proof that it is right.
# =============================================================================
# The production `.log_skellam_increment()` blends a saddlepoint approximation
# with an ascending series -- and that series IS the modified-Bessel series, so a
# Bessel-based reference would not be independent of it.  This reference is built
# from a different identity entirely:
#
#   A ~ Pois(alpha), W ~ Pois(omega), T = A + W ~ Pois(alpha + omega),
#   A | T = n ~ Binom(n, alpha / (alpha + omega)),
#   Delta = A - W = 2A - T,  so Delta = z  <=>  A = (n + z) / 2.
#
#   P(Delta = z) = sum_{n >= |z|, n = z mod 2}
#                    Pois(n; alpha + omega) Binom((n + z)/2; n, alpha/(alpha+omega))
#
# Truncation is controlled, not guessed: n_max = qpois(1 - eps, alpha + omega),
# so the omitted mass is bounded by eps.
# =============================================================================

# Exact log P(Delta = z). Scalar in, scalar out. Log space throughout.
#
# Summed over the RETRACTION count w rather than over the total n:
#
#   P(Delta = z) = sum_{w >= 0} Pois(w + z; alpha) Pois(w; omega)      (z >= 0)
#
# with the z < 0 case obtained by the symmetry P(z; alpha, omega) = P(-z; omega, alpha).
#
# WHY NOT n = a + w.  Truncating that sum at qpois(1 - eps, alpha + omega) is
# WRONG, and silently so.  That quantile bounds the MARGINAL total, but conditional
# on an extreme Delta the mass sits much further out: the dominant term is at
#
#   w* = (-|z| + sqrt(z^2 + 4 alpha omega)) / 2,
#
# so n* = |z| + 2 w*.  For a real FluSight cell (z = 2512, alpha = 1204,
# omega = 974) that is n* = 3317 against a marginal quantile of 2528 -- the sum was
# cut off before reaching its own maximum and understated logP by 182 nats.  The
# window below is centred on w* instead, and widened until the edge terms are
# negligible.
log_skellam_exact <- function(z, alpha, omega, log_tol = 40) {
  if (alpha + omega <= 0) return(if (z == 0) 0 else -Inf)
  # Work with z >= 0; the other side is the same sum with the rates swapped.
  if (z < 0) { tmp <- alpha; alpha <- omega; omega <- tmp; z <- -z }

  # Mode of Pois(w + z; alpha) Pois(w; omega) in w.
  w_mode <- (-z + sqrt(z^2 + 4 * alpha * omega)) / 2
  # Widen generously around it; the terms fall off faster than a Poisson of the
  # same mean, so this is conservative.
  half_width <- ceiling(12 * sqrt(w_mode + z + 1) + 60)
  w_lo <- max(0, floor(w_mode - half_width))
  w_hi <- ceiling(w_mode + half_width)

  repeat {
    w  <- seq.int(w_lo, w_hi)
    lt <- stats::dpois(w + z, alpha, log = TRUE) + stats::dpois(w, omega, log = TRUE)
    peak <- max(lt)
    if (!is.finite(peak)) return(-Inf)
    # Both edges must be negligible relative to the peak, or the window is too narrow.
    edge_ok <- (w_lo == 0 || lt[1] < peak - log_tol) && (lt[length(lt)] < peak - log_tol)
    if (edge_ok) break
    half_width <- half_width * 2
    w_lo <- max(0, floor(w_mode - half_width))
    w_hi <- ceiling(w_mode + half_width)
  }
  peak + log(sum(exp(lt - peak)))
}

# =============================================================================
# Validation.  A reference that is itself wrong is worse than no reference, so
# check it three independent ways before using it to judge anything.
# =============================================================================

cat("=== V1: does the exact pmf sum to 1? ===\n")
for (params in list(c(0.1, 0.1), c(1, 1), c(10, 9), c(10, 10),
                    c(100, 99), c(100, 100), c(1000, 999))) {
  alpha <- params[1]; omega <- params[2]
  span  <- ceiling(10 * sqrt(alpha + omega)) + 20
  z_seq <- seq.int(-span, span)
  total <- sum(exp(vapply(z_seq, log_skellam_exact, numeric(1), alpha, omega)))
  cat(sprintf("  alpha=%7.1f omega=%7.1f   sum = %.12f   (err %.2e)\n",
              alpha, omega, total, abs(total - 1)))
}

cat("\n=== V2: mean and variance match the Skellam moments? ===\n")
# E[Delta] = alpha - omega,  Var[Delta] = alpha + omega.
for (params in list(c(2, 1), c(10, 9), c(50, 48))) {
  alpha <- params[1]; omega <- params[2]
  span  <- ceiling(12 * sqrt(alpha + omega)) + 30
  z_seq <- seq.int(-span, span)
  pmf   <- exp(vapply(z_seq, log_skellam_exact, numeric(1), alpha, omega))
  m1 <- sum(z_seq * pmf); m2 <- sum((z_seq - m1)^2 * pmf)
  cat(sprintf("  alpha=%5.1f omega=%5.1f  mean %8.4f (exp %8.4f)  var %9.4f (exp %9.4f)\n",
              alpha, omega, m1, alpha - omega, m2, alpha + omega))
}

cat("\n=== V3: agreement with a large Monte-Carlo sample ===\n")
set.seed(11)
for (params in list(c(3, 2), c(20, 19))) {
  alpha <- params[1]; omega <- params[2]
  draws <- rpois(4e6, alpha) - rpois(4e6, omega)
  tab   <- table(draws)
  z_vals <- as.integer(names(tab))
  keep   <- abs(z_vals - (alpha - omega)) <= 3 * sqrt(alpha + omega)
  empirical <- as.numeric(tab)[keep] / length(draws)
  analytic  <- exp(vapply(z_vals[keep], log_skellam_exact, numeric(1), alpha, omega))
  cat(sprintf("  alpha=%5.1f omega=%5.1f  max |MC - exact| = %.2e  (MC se ~ %.2e)\n",
              alpha, omega, max(abs(empirical - analytic)), 1 / sqrt(4e6)))
}

cat("\n=== V4: reduces to Poisson when omega = 0 ===\n")
for (z in 0:4) {
  a <- log_skellam_exact(z, 5, 0)
  b <- dpois(z, 5, log = TRUE)
  cat(sprintf("  z=%d  exact %12.8f   dpois %12.8f   diff %.2e\n", z, a, b, abs(a - b)))
}
