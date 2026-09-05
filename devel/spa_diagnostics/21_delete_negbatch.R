# =============================================================================
# 21 -- Isolate the RESIDUAL: delete the negative administrative batches (event
#       weeks 15-17) from the real interval fit, tail pinned short, and re-profile.
#
# FINDINGS I.3: on real Texas the pinned-short optimum is p ~ 0.70, short of the
# mature-cohort 0.95.  The confounding (Result C) explains 0.50 -> 0.70; the
# residual 0.70 -> 0.95 should be the negative administrative batches (weeks
# 15-17: fully-published, ~20%/wk down; handoff B.4).  If deleting ONLY those
# weeks moves the pinned-short optimum from ~0.70 toward ~0.9, the negatives ARE
# the residual term, on top of the confounding.
#
# Run:  NOT_CRAN=true Rscript devel/spa_diagnostics/21_delete_negbatch.R
# =============================================================================
Sys.setenv(PR_MODE = "none")
suppressMessages(source("devel/spa_diagnostics/18_param_recovery.R"))

# real Texas interval observations, optionally excluding a set of event times
make_real_obs <- function(drop = integer(0)) {
  out <- vector("list", e$max_time)
  for (t in seq_len(e$max_time)) {
    if (t %in% drop) { out[[t]] <- NULL; next }
    h <- min(as.integer(e$d_star[t, 1]), cD); if (h < 0L) { out[[t]] <- NULL; next }
    cum <- cumsum(inc[t, seq_len(h + 1L), 1])
    obs_d <- (0:h)[(ev[1] + (t - 1 + 0:h) * 7) %in% asof]
    if (!length(obs_d)) { out[[t]] <- NULL; next }
    a <- c(-1L, head(obs_d, -1L)); z <- cum[obs_d + 1L] - c(0, cum[head(obs_d, -1L) + 1L])
    out[[t]] <- list(a = a, b = obs_d, z = z)
  }
  out
}

# identify the negative-batch event times empirically (largest single down-revision)
full <- make_real_obs()
negtab <- do.call(rbind, lapply(seq_along(full), function(t) {
  o <- full[[t]]; if (is.null(o)) return(NULL)
  data.frame(t = t, event = as.character(ev[t]), min_z = min(o$z), n_down = sum(o$z < 0))
}))
negtab <- negtab[order(negtab$min_z), ]
cat("Event times with the largest single down-revision (candidate negative batches):\n")
print(head(negtab, 10), row.names = FALSE)
DROP <- head(negtab$t, 3)                          # the three worst negative weeks
cat(sprintf("\nDropping the 3 worst: event times %s (dates %s)\n\n",
            paste(DROP, collapse = ", "), paste(ev[DROP], collapse = ", ")))

# retraction-fixed likelihood (delay optimised, g_C pinned), from script 19
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
PG <- c(0.10, 0.20, 0.30, 0.40, 0.55, 0.70, 0.80, 0.90, 0.95)
profile_fixret <- function(obs, cD, rmu, rsig) {
  fl <- flatten_obs(obs); ll <- rep(-Inf, length(PG)); prev <- c(0, log(0.5))
  for (i in order(PG, decreasing = TRUE)) {
    f <- stats::optim(prev, neg_loglik_fixret, p_val = PG[i], fl = fl, cD = cD,
                      rmu = rmu, rsig = rsig, method = "Nelder-Mead",
                      control = list(maxit = 300, reltol = 1e-8))
    ll[i] <- -f$value; prev <- f$par
  }
  imax <- which.max(ll); phat <- PG[imax]
  if (imax > 1 && imax < length(PG)) { x <- PG[(imax-1):(imax+1)]; y <- ll[(imax-1):(imax+1)]
    d <- (x[1]-x[2])*(x[1]-x[3])*(x[2]-x[3])
    A <- (x[3]*(y[2]-y[1])+x[2]*(y[1]-y[3])+x[1]*(y[3]-y[2]))/d
    B <- (x[3]^2*(y[1]-y[2])+x[2]^2*(y[3]-y[1])+x[1]^2*(y[2]-y[3]))/d
    if (A < 0) phat <- min(max(-B/(2*A), 0.02), 0.995) }
  list(ll = ll, phat = phat)
}

SHORT <- c(rmu = log(2.0), rsig = 1.5)             # Gbar(horizon) ~ 0.0004
gbar_h <- { gC <- mk_gC(SHORT["rmu"], SHORT["rsig"], cD); 1 - sum(gC) }
cat(sprintf("Retraction pinned short: rmu=%.2f rsig=%.2f  Gbar(horizon)=%.4f\n\n",
            SHORT["rmu"], SHORT["rsig"], gbar_h))

for (lab in c("FULL", "DROP 15-17")) {
  obs <- if (lab == "FULL") full else make_real_obs(DROP)
  pr <- profile_fixret(obs, cD, SHORT["rmu"], SHORT["rsig"])
  names(pr$ll) <- PG
  cat(sprintf("-- %s --  optimum p = %.3f\n", lab, pr$phat))
  print(round(pr$ll - max(pr$ll), 1)); cat("\n")
}
cat("If DROP 15-17 lifts the optimum from ~0.70 toward ~0.9, the negative batches\n")
cat("are the residual term on top of the finite-horizon confounding.\n")
