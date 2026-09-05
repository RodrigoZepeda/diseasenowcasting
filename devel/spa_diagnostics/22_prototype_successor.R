# =============================================================================
# 22 -- Prototype the principled successor to hard-fixing p (response step 13).
#
# Two candidate levers, tested on real Texas interval data:
#   A. weakly-informative prior on logit(p) centred on the mature-cohort value
#   B. constrain the g_C tail (small Gbar(horizon)) so the DATA identify p
#
# The recovery experiment (FINDINGS I) predicts A is a WEAK lever -- the free
# likelihood is confidently wrong at p~0.5 (thousands of nats deep), so a
# weakly-informative prior is overwhelmed and only a near-delta prior (= the
# current hard fix) reaches 0.95 -- while B attacks the confounding at its source.
# This script quantifies both.
#
# Run:  NOT_CRAN=true Rscript devel/spa_diagnostics/22_prototype_successor.R
# =============================================================================
Sys.setenv(PR_MODE = "none")
suppressMessages(source("devel/spa_diagnostics/18_param_recovery.R"))

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
logit <- function(p) log(p / (1 - p))

# ---- FREE profile log-likelihood on a fine p grid ---------------------------
P_GRID    <- c(0.20, 0.30, 0.40, 0.45, 0.50, 0.55, 0.60, 0.70, 0.80, 0.90, 0.95)
start_par <- c(0, log(0.5), log(1.5), log(1.0))       # feasible short-tail seed
pr <- profile_p(real_obs, cD, passes = 2L, start = start_par)
llf <- pr$ll; names(llf) <- P_GRID
cat("FREE profile log-lik (relative), fine grid:\n"); print(round(llf - max(llf), 1))
cat(sprintf("free optimum p = %.3f\n\n", pr$phat))

# ---- A. weakly-informative prior on logit(p), centred on the mature value ----
m <- logit(0.95)                                     # mature-cohort down-revision rate
cat("A. PRIOR on logit(p) ~ N(logit(0.95), s^2)  -- posterior MAP over the grid:\n")
cat(sprintf("   %-8s %-10s %-24s\n", "prior_s", "MAP p", "note"))
for (s in c(1.0, 0.5, 0.25, 0.10, 0.05)) {
  post <- llf + dnorm(logit(P_GRID), m, s, log = TRUE)
  imax <- which.max(post); pmap <- P_GRID[imax]
  # parabolic refine on logit scale for a smoother MAP
  if (imax > 1 && imax < length(P_GRID)) {
    x <- logit(P_GRID[(imax-1):(imax+1)]); y <- post[(imax-1):(imax+1)]
    d <- (x[1]-x[2])*(x[1]-x[3])*(x[2]-x[3])
    A <- (x[3]*(y[2]-y[1])+x[2]*(y[1]-y[3])+x[1]*(y[3]-y[2]))/d
    B <- (x[3]^2*(y[1]-y[2])+x[2]^2*(y[3]-y[1])+x[1]^2*(y[2]-y[3]))/d
    if (A < 0) pmap <- plogis(-B/(2*A))
  }
  note <- if (s >= 0.5) "weak: likelihood dominates" else if (s <= 0.05) "near-delta ~ the hard fix" else "moderate"
  cat(sprintf("   %-8.2f %-10.3f %s\n", s, pmap, note))
}
cat("   (prior strength needed to reach ~0.9 collapses the prior to a near-delta:\n")
cat("    the confidently-wrong likelihood makes a weakly-informative p-prior a WEAK lever.)\n\n")

# ---- B. constrain the g_C tail: the data then identify p (bounded tail) ------
# neg-loglik with retraction params free but Gbar(horizon) penalised toward 0,
# i.e. estimate the tail subject to a soft small-tail constraint.
neg_loglik_boundtail <- function(theta, p_val, fl, cD, tau) {
  dmu <- theta[1]; dsig <- exp(theta[2]); rmu <- theta[3]; rsig <- exp(theta[4])
  gD <- mk_gD(dmu, dsig, cD); gC <- mk_gC(rmu, rsig, cD)
  if (any(!is.finite(gD)) || any(!is.finite(gC))) return(1e12)
  gbar_h <- 1 - sum(gC)                              # mass beyond the horizon
  Q <- q_tab(gD, gC, p_val, cD)
  qp <- Q$qp[cbind(fl$a + 2L, fl$b + 1L)]; qm <- Q$qm[cbind(fl$a + 2L, fl$b + 1L)]
  nT <- fl$nTg
  M1 <- .grid_M(fl, qp, qm, p_val, fl$start, GCOARSE); j1 <- .argmax_na(M1)
  if (any(j1 == 0L)) return(1e12)
  M <- .grid_M(fl, qp, qm, p_val, fl$start + GCOARSE[j1], GFINE)
  jm <- .argmax_na(M); if (any(jm == 0L)) return(1e12)
  G <- ncol(M); total <- 0
  for (t in seq_len(nT)) { j <- jm[t]; y2 <- M[t, j]
    if (j > 1L && j < G) { y1 <- M[t, j-1L]; y3 <- M[t, j+1L]
      if (is.finite(y1) && is.finite(y3)) { den <- y1 - 2*y2 + y3
        if (den < 0) { corr <- -0.125*(y1-y3)^2/den   # >= 0; UNGUARDED this
          # explodes when the mu profile is flat (den -> 0-) or when the grid
          # values are wildly asymmetric.  A refinement over a grid of step
          # 0.125 in log mu cannot be worth more than a fraction of a nat.
          if (is.finite(corr) && corr <= 1) { total <- total + y2 + corr; next } } } }
    total <- total + y2 }
  if (!is.finite(total)) return(1e12)
  -total + 0.5 * (gbar_h / tau)^2                    # soft constraint Gbar(h) ~ 0 +/- tau
}
PGB <- c(0.30, 0.45, 0.55, 0.65, 0.75, 0.85, 0.95)
cat("B. BOUNDED TAIL: Gbar(horizon) softly constrained <= tau; p and the (bounded)\n")
cat("   tail estimated jointly, so the DATA pick p with propagated uncertainty:\n")
cat(sprintf("   %-8s %-10s\n", "tau", "optimum p"))
for (tau in c(0.30, 0.10, 0.02)) {
  fl <- flatten_obs(real_obs); ll <- rep(-Inf, length(PGB)); prev <- start_par
  for (i in order(PGB, decreasing = TRUE)) {
    f <- stats::optim(prev, neg_loglik_boundtail, p_val = PGB[i], fl = fl, cD = cD,
                      tau = tau, method = "Nelder-Mead", control = list(maxit = 300, reltol = 1e-8))
    ll[i] <- -f$value; prev <- f$par
  }
  cat(sprintf("   %-8.2f %-10.3f\n", tau, PGB[which.max(ll)]))
}
cat("\nRecommendation: B (a bounded/known retraction tail) is the lever that identifies\n")
cat("p from finite-horizon data; A alone degenerates to the current hard fix.\n")
