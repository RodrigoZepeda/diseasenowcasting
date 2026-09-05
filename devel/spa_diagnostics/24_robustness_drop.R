# =============================================================================
# 24 -- ROBUSTNESS ARM: ask the model to explain only what it CAN represent.
#
# Script 23 showed that once follow-up runs past cD=15 the likelihood becomes
# structurally infeasible: 65 positive revisions need g_D mass at ages 16-169
# (alpha(a,b)/mu = sum_{r=a+1}^{b} gD(r)[...] -- no choice of g_C enters), and
# 113 negatives need g_C mass at comparable lags.  cD=15 had ZERO such cells; the
# truncation was hiding every observation the model cannot express.
#
# Two deletion rules, both applied to the interval observation set:
#   impossible -- drop intervals still infeasible under the MOST PERMISSIVE
#                 parameters the model allows (long-tail g_C).  These are the
#                 ones no parameter choice can rescue.  Non-circular.
#   admin      -- additionally drop intervals ENDING at a snapshot that moved
#                 >=5 cohorts simultaneously (the administrative signature:
#                 sd of relative change << sd of absolute change).  Detected
#                 from the data, not hand-picked.
#
# Dropping an interval is exact, not an approximation: (a,b] and (b,c] are each
# marginally Skellam on their own, so removing (a,b] leaves the rest valid.
#
# Run: NOT_CRAN=true ARM=all DROP=impossible Rscript devel/spa_diagnostics/24_robustness_drop.R
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})
ARM   <- Sys.getenv("ARM", "wfull")        # wfull | all
DROP  <- Sys.getenv("DROP", "impossible")  # none | impossible | admin
STATE <- Sys.getenv("STATE", "Texas")
START <- as.Date("2023-09-23")

vskel <- function(z, a, b) {
  out <- numeric(length(z))
  pure_add <- b <= 1e-9; pure_ret <- a <= 1e-9 & !pure_add; mixed <- !pure_add & !pure_ret
  if (any(pure_add)) out[pure_add] <- ifelse(z[pure_add] < 0, -Inf,
    stats::dpois(pmax(z[pure_add], 0), a[pure_add] + 1e-10, log = TRUE))
  if (any(pure_ret)) out[pure_ret] <- ifelse(z[pure_ret] > 0, -Inf,
    stats::dpois(pmax(-z[pure_ret], 0), b[pure_ret] + 1e-10, log = TRUE))
  if (any(mixed)) {
    zz <- z[mixed]; aa <- a[mixed]; bb <- b[mixed]
    root <- sqrt(zz^2 + 4*aa*bb); u <- ifelse(zz >= 0, (zz+root)/(2*aa), 2*bb/(root-zz))
    out[mixed] <- aa*(u-1) + bb*(1/u-1) - log(u)*zz - 0.5*log(2*pi*(aa*u + bb/u))
  }
  out
}
mk_gD <- function(dmu, dsig, cD) {
  f <- diseasenowcasting:::.delay_distribution_functions(1L, dmu, 0.01 + dsig)
  acdf <- as.numeric(f$cdf(seq_len(cD + 1L))); c(acdf[1], diff(acdf)) }
mk_gC <- function(rmu, rsig, cD) {
  f <- diseasenowcasting:::.delay_distribution_functions(1L, rmu, 0.01 + rsig)
  c(0, diff(c(0, as.numeric(f$cdf(seq_len(cD)))))) }
q_pairs <- function(gD, gC, p, a, b) {
  GD <- cumsum(gD); GC <- cumsum(gC); nD <- length(GD); nC <- length(GC)
  cl <- function(V, k, n) { out <- numeric(length(k)); ok <- k >= 0
                            out[ok] <- V[pmin(k[ok], n - 1L) + 1L]; out }
  Gf <- function(k) cl(GC, k, nC); GDf <- function(k) cl(GD, k, nD)
  L_ab <- L_aa <- Kb <- numeric(length(a))
  for (r in 0:min(nD - 1L, max(b))) {
    g <- gD[r + 1L]; if (!is.finite(g) || g < 1e-14) next
    inA <- a >= r
    if (any(inA)) { L_ab[inA] <- L_ab[inA] + g*Gf(b[inA]-r); L_aa[inA] <- L_aa[inA] + g*Gf(a[inA]-r) }
    inB <- b >= r; if (any(inB)) Kb[inB] <- Kb[inB] + g*(1 - Gf(b[inB]-r))
  }
  list(qp = pmax(p*(GDf(b)-GDf(a)) + (1-p)*(Kb - GDf(a) + L_ab), 0),
       qm = pmax((1-p)*(L_ab - L_aa), 0)) }

GCOARSE <- seq(-13, 13, length.out = 27L); GFINE <- seq(-1.5, 1.5, length.out = 13L)
.grid_M <- function(fl, qp, qm, p_val, centre, off) {
  G <- length(off); nO <- fl$nObs; nT <- fl$nTg
  ec <- rep.int(seq_len(nO), G); gc <- rep(seq_len(G), each = nO)
  mu <- exp(centre[fl$grp[ec]] + off[gc]) / p_val
  v <- vskel(fl$z[ec], mu*qp[ec], mu*qm[ec]); v[!is.finite(v)] <- NA_real_
  agg <- rowsum(v, fl$grp[ec] + (gc-1L)*nT, na.rm = FALSE)
  Sv <- rep(NA_real_, nT*G); Sv[as.integer(rownames(agg))] <- agg; matrix(Sv, nT, G) }
.argmax_na <- function(M) { Mf <- M; Mf[is.na(Mf)] <- -Inf
  jm <- max.col(Mf, ties.method = "first")
  jm[!is.finite(Mf[cbind(seq_len(nrow(M)), jm)])] <- 0L; jm }
neg_loglik <- function(theta, p_val, fl, cD) {
  gD <- mk_gD(theta[1], exp(theta[2]), cD); gC <- mk_gC(theta[3], exp(theta[4]), cD)
  if (any(!is.finite(gD)) || any(!is.finite(gC))) return(1e12)
  Q <- q_pairs(gD, gC, p_val, fl$a, fl$b)
  M1 <- .grid_M(fl, Q$qp, Q$qm, p_val, fl$start, GCOARSE)
  j1 <- .argmax_na(M1); if (any(j1 == 0L)) return(1e12)
  M <- .grid_M(fl, Q$qp, Q$qm, p_val, fl$start + GCOARSE[j1], GFINE)
  jm <- .argmax_na(M); if (any(jm == 0L)) return(1e12)
  G <- ncol(M); total <- 0
  for (t in seq_len(fl$nTg)) { j <- jm[t]; y2 <- M[t, j]
    if (j > 1L && j < G) { y1 <- M[t, j-1L]; y3 <- M[t, j+1L]
      if (is.finite(y1) && is.finite(y3)) { den <- y1 - 2*y2 + y3
        if (den < 0) { corr <- -0.125*(y1-y3)^2/den   # >= 0; UNGUARDED this
          # explodes when the mu profile is flat (den -> 0-) or when the grid
          # values are wildly asymmetric.  A refinement over a grid of step
          # 0.125 in log mu cannot be worth more than a fraction of a nat.
          if (is.finite(corr) && corr <= 1) { total <- total + y2 + corr; next } } } }
    total <- total + y2 }
  if (!is.finite(total) || total > 0) 1e12 else -total }
P_GRID <- c(0.15, 0.30, 0.45, 0.60, 0.75, 0.85, 0.92, 0.97)

# ---- observations inferred from published rows ------------------------------
raw <- tbl.now::flusight |> filter(location_name == STATE) |> arrange(target_end_date, as_of)
if (ARM != "all") raw <- raw |> filter(target_end_date >= START)
raw <- raw |> mutate(d = floor(as.numeric(as_of - target_end_date)/7))
rec <- list(); k <- 0
for (E in unique(raw$target_end_date)) {
  x <- raw |> filter(target_end_date == E) |> arrange(d); x <- x[!duplicated(x$d), ]; k <- k + 1
  rec[[k]] <- data.frame(t = k, snap = x$as_of, a = c(-1L, head(x$d,-1L)), b = x$d,
                         z = x$observation - c(0, head(x$observation,-1L)),
                         prev = c(0, head(x$observation,-1L))) }
R <- do.call(rbind, rec); cD <- max(R$b); n0 <- nrow(R)

keep <- rep(TRUE, nrow(R))
if (DROP %in% c("impossible", "admin", "placebo")) {
  # most permissive parameters the model allows: long g_C tail, diffuse g_D
  gDp <- mk_gD(0, 0.5, cD); gCp <- mk_gC(log(30), 20, cD)
  Qp <- q_pairs(gDp, gCp, 0.6, R$a, R$b)
  imp <- (Qp$qp <= 1e-9 & R$z > 0) | (Qp$qm <= 1e-9 & R$z < 0)
  keep <- keep & !imp
  cat(sprintf("drop 'impossible': %d intervals (%+.0f net mass)\n", sum(imp), sum(R$z[imp])))
}
if (DROP == "admin") {
  # administrative snapshots: >=5 cohorts past the reporting phase moved together
  adm <- R |> filter(a >= 2, prev > 0) |> group_by(snap) |>
    summarise(n_moved = sum(z != 0), .groups = "drop") |> filter(n_moved >= 5)
  hit <- R$snap %in% adm$snap
  keep <- keep & !hit
  cat(sprintf("drop 'admin': %d snapshots flagged, %d further intervals removed\n",
              nrow(adm), sum(hit & !((Qp$qp <= 1e-9 & R$z > 0) | (Qp$qm <= 1e-9 & R$z < 0)))))
}
if (DROP == "placebo") {
  # CONTROL: the admin rule removes ~22% of intervals.  Does dropping that MUCH
  # data at ARBITRARY snapshots move p just as far?  If it does, the admin result
  # is an artefact of volume, not of which snapshots were removed.
  set.seed(as.integer(Sys.getenv("PLSEED", "1")))
  adm0 <- R |> filter(a >= 2, prev > 0) |> group_by(snap) |>
    summarise(n_moved = sum(z != 0), .groups = "drop") |> filter(n_moved >= 5)
  n_target <- sum(R$snap %in% adm0$snap)
  pool <- setdiff(unique(as.character(R$snap)), as.character(adm0$snap))
  pick <- character(0); got <- 0
  for (s in sample(pool)) { pick <- c(pick, s)
    got <- sum(as.character(R$snap) %in% pick); if (got >= n_target) break }
  keep <- keep & !(as.character(R$snap) %in% pick)
  cat(sprintf("drop 'placebo': %d random snapshots, %d intervals (admin rule drops %d)\n",
              length(pick), got, n_target))
}
R <- R[keep, ]
R <- R |> group_by(t) |> filter(n() >= 1) |> ungroup()
tt <- match(R$t, sort(unique(R$t)))
fl <- list(a = R$a, b = R$b, z = R$z, grp = tt, nTg = max(tt), nObs = nrow(R),
           start = as.numeric(tapply(pmax(R$z,0), tt, function(v) log(max(sum(v),1)))))

cat(sprintf("\n=== ARM %s | DROP %s | %s ===\n", ARM, DROP, STATE))
cat(sprintf("intervals %d of %d kept (%.1f%%) | event weeks %d | cD %d\n",
            fl$nObs, n0, 100*fl$nObs/n0, fl$nTg, cD))

SEED <- Sys.getenv("SEED", "long")   # short-tail seed is infeasible once late negatives are in
start0 <- if (SEED == "short") c(0, log(0.5), log(1.5), log(0.5)) else c(0, log(0.5), log(30), log(20))
if (neg_loglik(start0, 0.97, fl, cD) >= 1e11) cat("WARNING: seed is infeasible at p=0.97\n")
t0 <- Sys.time(); ll <- rep(-Inf, length(P_GRID)); prev <- start0
for (i in order(P_GRID, decreasing = TRUE)) {
  fit <- stats::optim(prev, neg_loglik, p_val = P_GRID[i], fl = fl, cD = cD,
                      method = "Nelder-Mead", control = list(maxit = 300, reltol = 1e-8))
  ll[i] <- -fit$value; prev <- fit$par
  cat(sprintf("  p=%.2f  logL=%.1f  gC=(mu %.2f sd %.2f)\n", P_GRID[i], ll[i],
              exp(fit$par[3]), exp(fit$par[4]))) }
cat(sprintf("fit %.0f s\n", as.numeric(Sys.time()-t0, units="secs")))
names(ll) <- P_GRID; cat("\nprofile logL (rel):\n"); print(round(ll - max(ll), 1))
imax <- which.max(ll); phat <- P_GRID[imax]
if (imax > 1 && imax < length(P_GRID)) { x <- P_GRID[(imax-1):(imax+1)]; y <- ll[(imax-1):(imax+1)]
  d <- (x[1]-x[2])*(x[1]-x[3])*(x[2]-x[3])
  A <- (x[3]*(y[2]-y[1])+x[2]*(y[1]-y[3])+x[1]*(y[3]-y[2]))/d
  B <- (x[3]^2*(y[1]-y[2])+x[2]^2*(y[3]-y[1])+x[1]^2*(y[2]-y[3]))/d
  if (A < 0) phat <- min(max(-B/(2*A), 0.02), 0.995) }
cat(sprintf("\np_hat = %.3f\n", phat))
