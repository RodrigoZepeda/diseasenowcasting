# =============================================================================
# 25 -- THE b_c REPARAMETERISATION (reviewer note sections 1, 2, 7, 30.1).
#
# Write the retraction masses that the data can actually see,
#
#     b_c = (1 - p) g_C(c),      Bcum(m) = sum_{c<=m} b_c = (1-p) G_C(m),
#
# so that the age-a retention probability is
#
#     r(a) = p + (1-p) Gbar_C(a) = 1 - Bcum(a),
#
# and the cadence interval rates lose p entirely:
#
#     alpha_t(a,b)/mu_t = sum_{r=a+1}^{b} gD(r) [1 - Bcum(b-r)]
#     omega_t(a,b)/mu_t = sum_{r=0}^{a}   gD(r) [Bcum(b-r) - Bcum(a-r)]
#
# provided the epidemic scale is carried by the GROSS report rate mu_t and not by
# lambda_t = p mu_t.  Only b_1..b_H for H = max delay enter; p = 1 - sum_{c=1}^inf
# b_c needs the mass beyond H, which no snapshot has had time to show.
#
# Three parts:
#   A  numeric equivalence -- b-form vs the (p, g_C) code path of script 23, on
#      the real Texas (a,b) pairs, over a grid of p and retraction tails.
#   B  exact flatness -- hold b_1..b_H fixed, move the leftover retraction mass
#      past the horizon to buy any p <= r(H), and profile.  If the algebra is
#      right the profile logL is CONSTANT, which is the identification statement
#      in its sharpest form.  Printed against the parametric profile of 23.
#   C  empirical support -- how many interval observations actually involve b_c,
#      by lag (reviewer section 9's n_risk).
#
# Run: NOT_CRAN=true Rscript devel/spa_diagnostics/25_bc_reparam.R
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})
STATE <- Sys.getenv("STATE", "Texas")
START <- as.Date("2023-09-23")

# ---- estimator core, verbatim from 23 ---------------------------------------
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
  acdf <- as.numeric(f$cdf(seq_len(cD + 1L))); c(acdf[1], diff(acdf))
}
mk_gC <- function(rmu, rsig, cD) {
  f <- diseasenowcasting:::.delay_distribution_functions(1L, rmu, 0.01 + rsig)
  c(0, diff(c(0, as.numeric(f$cdf(seq_len(cD))))))
}
q_pairs <- function(gD, gC, p, a, b) {          # the (p, g_C) path, unchanged
  GD <- cumsum(gD); GC <- cumsum(gC); nD <- length(GD); nC <- length(GC)
  cl <- function(V, k, n) { out <- numeric(length(k)); ok <- k >= 0
                            out[ok] <- V[pmin(k[ok], n - 1L) + 1L]; out }
  Gf  <- function(k) cl(GC, k, nC)
  GDf <- function(k) cl(GD, k, nD)
  L_ab <- L_aa <- Kb <- numeric(length(a))
  for (r in 0:min(nD - 1L, max(b))) {
    g <- gD[r + 1L]; if (!is.finite(g) || g < 1e-14) next
    inA <- a >= r
    if (any(inA)) { L_ab[inA] <- L_ab[inA] + g * Gf(b[inA] - r)
                    L_aa[inA] <- L_aa[inA] + g * Gf(a[inA] - r) }
    inB <- b >= r
    if (any(inB)) Kb[inB] <- Kb[inB] + g * (1 - Gf(b[inB] - r))
  }
  list(qp = pmax(p * (GDf(b) - GDf(a)) + (1 - p) * (Kb - GDf(a) + L_ab), 0),
       qm = pmax((1 - p) * (L_ab - L_aa), 0))
}

# ---- the b-form, written independently (interval by interval, no shortcuts) --
# bvec is 0-indexed: bvec[k+1] = b_k, with b_0 = 0 (no retraction at lag 0).
q_bform <- function(gD, bvec, a, b) {
  Bc <- cumsum(bvec); nB <- length(Bc)
  Bat <- function(m) ifelse(m < 0, 0, Bc[pmin(pmax(m, 0), nB - 1L) + 1L])
  qp <- qm <- numeric(length(a))
  for (i in seq_along(a)) {
    ai <- a[i]; bi <- b[i]
    r1 <- (ai + 1L):bi                                   # positive part
    qp[i] <- sum(gD[r1 + 1L] * (1 - Bat(bi - r1)))
    if (ai >= 0) { r2 <- 0:ai                            # negative part
      qm[i] <- sum(gD[r2 + 1L] * (Bat(bi - r2) - Bat(ai - r2))) }
  }
  list(qp = qp, qm = qm)
}

# ---- lambda_t / mu_t profiled out by the two-stage grid, but on mu directly --
GCOARSE <- seq(-13, 13, length.out = 27L)
GFINE   <- seq(-1.5, 1.5, length.out = 13L)
.grid_M <- function(fl, qp, qm, centre, off) {
  G <- length(off); nO <- fl$nObs; nT <- fl$nTg
  ec <- rep.int(seq_len(nO), G); gc <- rep(seq_len(G), each = nO)
  mu <- exp(centre[fl$grp[ec]] + off[gc])                # GROSS rate, no /p
  v <- vskel(fl$z[ec], mu * qp[ec], mu * qm[ec]); v[!is.finite(v)] <- NA_real_
  agg <- rowsum(v, fl$grp[ec] + (gc - 1L) * nT, na.rm = FALSE)
  Sv <- rep(NA_real_, nT * G); Sv[as.integer(rownames(agg))] <- agg
  matrix(Sv, nT, G)
}
.argmax_na <- function(M) {
  Mf <- M; Mf[is.na(Mf)] <- -Inf
  jm <- max.col(Mf, ties.method = "first")
  jm[!is.finite(Mf[cbind(seq_len(nrow(M)), jm)])] <- 0L; jm
}
loglik_q <- function(fl, qp, qm) {              # profiled over free mu_t
  M1 <- .grid_M(fl, qp, qm, fl$start, GCOARSE)
  j1 <- .argmax_na(M1); if (any(j1 == 0L)) return(-1e12)
  M <- .grid_M(fl, qp, qm, fl$start + GCOARSE[j1], GFINE)
  jm <- .argmax_na(M); if (any(jm == 0L)) return(-1e12)
  G <- ncol(M); total <- 0
  for (t in seq_len(fl$nTg)) {
    j <- jm[t]; y2 <- M[t, j]
    if (j > 1L && j < G) {
      y1 <- M[t, j-1L]; y3 <- M[t, j+1L]
      if (is.finite(y1) && is.finite(y3)) { den <- y1 - 2*y2 + y3
        if (den < 0) { corr <- -0.125*(y1-y3)^2/den   # >= 0; UNGUARDED this
          # explodes when the mu profile is flat (den -> 0-) or when the grid
          # values are wildly asymmetric.  A refinement over a grid of step
          # 0.125 in log mu cannot be worth more than a fraction of a nat.
          if (is.finite(corr) && corr <= 1) { total <- total + y2 + corr; next } } } }
    total <- total + y2
  }
  if (is.finite(total)) total else -1e12
}

# ---- interval observations, exactly as script 23 builds them ----------------
build_fl <- function(arm) {
  raw <- tbl.now::flusight |> filter(location_name == STATE) |> arrange(target_end_date, as_of)
  if (arm != "all") raw <- raw |> filter(target_end_date >= START)
  raw <- raw |> mutate(d = floor(as.numeric(as_of - target_end_date) / 7))
  cap <- if (arm == "w15") 15L else Inf
  obs <- list()
  for (E in unique(raw$target_end_date)) {
    x <- raw |> filter(target_end_date == E, d <= cap) |> arrange(d)
    if (!nrow(x)) next
    x <- x[!duplicated(x$d), ]
    obs[[length(obs) + 1L]] <- list(a = as.integer(c(-1L, head(x$d, -1L))), b = as.integer(x$d),
      z = as.numeric(x$observation - c(0, head(x$observation, -1L))))
  }
  fl <- list(a = unlist(lapply(obs, `[[`, "a")), b = unlist(lapply(obs, `[[`, "b")),
             z = unlist(lapply(obs, `[[`, "z")),
             grp = rep(seq_along(obs), vapply(obs, function(o) length(o$z), 1L)),
             start = vapply(obs, function(o) log(max(sum(pmax(o$z, 0)), 1)), 1.0),
             nTg = length(obs))
  fl$nObs <- length(fl$z); fl
}

# =============================================================================
# A -- numeric equivalence of the two parameterisations
# =============================================================================
cat("=== A. b-form vs (p, g_C) form, real Texas (a,b) pairs ===\n\n")
cat(sprintf("%-6s %6s %6s %6s %6s  %11s %11s\n",
            "arm", "p", "gCmu", "gCsd", "H", "max|d qp|", "max|d qm|"))
for (arm in c("w15", "wfull", "all")) {
  fl <- build_fl(arm); cD <- max(fl$b)
  for (pv in c(0.15, 0.60, 0.95)) for (rp in list(c(1.5, 0.5), c(6, 2), c(30, 20))) {
    gD <- mk_gD(0, 0.5, cD); gC <- mk_gC(rp[1], rp[2], cD)
    Q1 <- q_pairs(gD, gC, pv, fl$a, fl$b)
    Q2 <- q_bform(gD, (1 - pv) * gC, fl$a, fl$b)
    cat(sprintf("%-6s %6.2f %6.1f %6.1f %6d  %11.2e %11.2e\n", arm, pv, rp[1], rp[2], cD,
                max(abs(Q1$qp - Q2$qp)), max(abs(Q1$qm - Q2$qm))))
  }
}

# =============================================================================
# B -- p is exactly flat once the retraction tail beyond H is free
# =============================================================================
cat("\n=== B. profile of p with b_1..b_H HELD FIXED ===\n")
fl <- build_fl("w15"); cD <- max(fl$b); H <- cD
gD <- mk_gD(0, 0.5, cD)
bref <- (1 - 0.90) * mk_gC(1.5, 0.5, cD)        # reference within-horizon masses
Bsum <- sum(bref); pmax_id <- 1 - Bsum
cat(sprintf("reference b_1..b_%d: sum = %.4f  ->  r(H) = 1 - sum b = %.4f\n", H, Bsum, pmax_id))
cat(sprintf("so any p in (0, %.3f] is reachable by putting mass (1-p) - %.4f at lags > H\n\n",
            pmax_id, Bsum))
P_GRID <- c(0.15, 0.30, 0.45, 0.60, 0.75, 0.85, 0.92, 0.97)
ll_flat <- rep(NA_real_, length(P_GRID))
for (i in seq_along(P_GRID)) {
  pv <- P_GRID[i]; if (pv > pmax_id) next
  gCp <- c(bref / (1 - pv), 1 - Bsum / (1 - pv))   # residual parked at lag H+1
  Q <- q_pairs(gD, gCp, pv, fl$a, fl$b)            # ORIGINAL code path, not q_bform
  ll_flat[i] <- loglik_q(fl, Q$qp, Q$qm)
}
par23 <- readRDS("devel/spa_diagnostics/censint_w15_Texas.rds")$ll
cmp <- data.frame(p = P_GRID,
                  free_tail = round(ll_flat - max(ll_flat, na.rm = TRUE), 4),
                  lognormal_gC_23 = round(par23 - max(par23), 1))
print(cmp, row.names = FALSE)
cat(sprintf("\nrange of free-tail profile: %.2e nats over %d feasible p values\n",
            diff(range(ll_flat, na.rm = TRUE)), sum(!is.na(ll_flat))))

# =============================================================================
# C -- which b_c the data can see, and how often
# =============================================================================
cat("\n=== C. empirical support n_risk(c): intervals whose rate involves b_c ===\n")
GD <- cumsum(gD)
GDf <- function(k) ifelse(k < 0, 0, GD[pmin(pmax(k, 0), length(GD) - 1L) + 1L])
sup <- lapply(seq_len(H), function(cc) {
  dqp <- ifelse(fl$b - cc >= fl$a + 1L, GDf(fl$b - cc) - GDf(fl$a), 0)          # |d qp / d b_c|
  dqm <- pmax(GDf(pmin(fl$a, fl$b - cc)) - GDf(fl$a - cc), 0)                   #  d qm / d b_c
  w <- dqp + dqm
  data.frame(lag = cc, n_risk = sum(w > 1e-8), weight = sum(w))
})
sup <- do.call(rbind, sup)
sup$share <- round(sup$weight / max(sup$weight), 3)
print(sup, row.names = FALSE, max = 1e4)
cat(sprintf("\nretention curve implied by the reference b (reviewer section 4 format):\n"))
Bc <- cumsum(bref)
for (a in c(1, 2, 4, 8, 15)) cat(sprintf("  r(%2d) = %.4f\n", a, 1 - Bc[a + 1L]))
