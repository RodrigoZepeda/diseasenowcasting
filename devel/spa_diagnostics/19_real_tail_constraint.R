# =============================================================================
# 19 -- REAL-DATA tail-constraint discriminator (response steps 2-3, on FluSight).
#
# Script 18 proved in simulation that a LONG retraction tail (mass beyond the
# observation horizon) makes p non-identifiable and collapses p_hat toward ~0.15.
# The airtight test on real data: profile p on the real Texas interval data under
#   (FREE)  g_C tail free           -> reproduces the ~0.4 real optimum
#   (SHORT) g_C tail pinned short   -> Gbar(horizon) ~ 0, all retraction observed
# If pinning the tail short pushes the optimum p up toward the mature-cohort ~0.95,
# the real-data low-p is finite-horizon confounding (Result C), not an estimator
# defect and not (solely) revision-process misspecification.
#
# Run:  NOT_CRAN=true Rscript devel/spa_diagnostics/19_real_tail_constraint.R
# =============================================================================
Sys.setenv(PR_MODE = "none")                       # source 18 for machinery + real cadence only
suppressMessages(source("devel/spa_diagnostics/18_param_recovery.R"))

# real Texas interval observations (cadence-given intervals), as in script 17
real_obs <- local({
  out <- vector("list", e$max_time)
  for (t in seq_len(e$max_time)) {
    h <- min(as.integer(e$d_star[t, 1]), cD); if (h < 0L) { out[[t]] <- NULL; next }
    cum <- cumsum(inc[t, seq_len(h + 1L), 1])
    obs_d <- (0:h)[(ev[1] + (t - 1 + 0:h) * 7) %in% asof]
    if (!length(obs_d)) { out[[t]] <- NULL; next }
    a <- c(-1L, head(obs_d, -1L)); z <- cum[obs_d + 1L] - c(0, cum[head(obs_d, -1L) + 1L])
    out[[t]] <- list(a = a, b = obs_d, z = z)
  }
  out
})

# ---- retraction-fixed likelihood: optimise delay only, g_C pinned -------------
neg_loglik_fixret <- function(theta2, p_val, fl, cD, rmu, rsig) {
  gD <- mk_gD(theta2[1], exp(theta2[2]), cD); gC <- mk_gC(rmu, rsig, cD)
  if (any(!is.finite(gD)) || any(!is.finite(gC))) return(1e12)
  Q <- q_tab(gD, gC, p_val, cD)
  qp <- Q$qp[cbind(fl$a + 2L, fl$b + 1L)]; qm <- Q$qm[cbind(fl$a + 2L, fl$b + 1L)]
  nT <- fl$nTg
  M1 <- .grid_M(fl, qp, qm, p_val, fl$start, GCOARSE); j1 <- .argmax_na(M1)
  if (any(j1 == 0L)) return(1e12)
  M <- .grid_M(fl, qp, qm, p_val, fl$start + GCOARSE[j1], GFINE)
  jm <- .argmax_na(M); if (any(jm == 0L)) return(1e12)
  G <- ncol(M); total <- 0
  for (t in seq_len(nT)) {
    j <- jm[t]; y2 <- M[t, j]
    if (j > 1L && j < G) { y1 <- M[t, j-1L]; y3 <- M[t, j+1L]
      if (is.finite(y1) && is.finite(y3)) { den <- y1 - 2*y2 + y3
        if (den < 0) { corr <- -0.125*(y1-y3)^2/den   # >= 0; UNGUARDED this
          # explodes when the mu profile is flat (den -> 0-) or when the grid
          # values are wildly asymmetric.  A refinement over a grid of step
          # 0.125 in log mu cannot be worth more than a fraction of a nat.
          if (is.finite(corr) && corr <= 1) { total <- total + y2 + corr; next } } } }
    total <- total + y2
  }
  if (!is.finite(total) || total > 0) 1e12 else -total
}

PG <- c(0.10, 0.20, 0.30, 0.40, 0.55, 0.70, 0.85, 0.95)
profile_fixret <- function(obs, cD, rmu, rsig) {
  fl <- flatten_obs(obs); ll <- rep(-Inf, length(PG)); prev <- c(0, log(0.5))
  for (i in order(PG, decreasing = TRUE)) {
    f <- stats::optim(prev, neg_loglik_fixret, p_val = PG[i], fl = fl, cD = cD,
                      rmu = rmu, rsig = rsig, method = "Nelder-Mead",
                      control = list(maxit = 300, reltol = 1e-8))
    ll[i] <- -f$value; prev <- f$par
  }
  ll
}

# FREE fit (retraction tail free): full 4-param profile, both passes.
# Seed from a FEASIBLE short-tail start: the default long-tail start is entirely
# infeasible on the real negative batches, so the optimiser cannot move from it.
cat("FREE g_C tail (retraction params optimised) -- reproduces the real optimum:\n")
fr <- profile_p(real_obs, cD, passes = 2L, start = c(0, log(0.5), log(1.5), log(1.0)))
names(fr$ll) <- P_GRID
print(round(fr$ll - max(fr$ll), 1))
cat(sprintf("  optimum p = %.3f\n\n", fr$phat))

# SHORT fit: retraction pinned to a short tail so Gbar(horizon) ~ 0
for (short in list(c(rmu = log(1.2), rsig = 0.8), c(rmu = log(2.0), rsig = 1.5))) {
  rh <- { gC <- mk_gC(short["rmu"], short["rsig"], cD); 1 - sum(gC) }
  ll <- profile_fixret(real_obs, cD, short["rmu"], short["rsig"])
  names(ll) <- PG
  cat(sprintf("SHORT g_C pinned  rmu=%.2f rsig=%.2f  Gbar(horizon)=%.4f\n",
              short["rmu"], short["rsig"], rh))
  print(round(ll - max(ll), 1))
  cat(sprintf("  optimum p = %.3f\n\n", PG[which.max(ll)]))
}

cat("Interpretation: if pinning g_C short lifts the optimum p from ~0.4 toward ~0.9,\n")
cat("the real-data low-p is finite-horizon p <-> Gbar(a) confounding (Result C).\n")
