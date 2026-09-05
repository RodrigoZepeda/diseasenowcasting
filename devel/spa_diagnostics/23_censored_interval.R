# =============================================================================
# 23 -- PROTOTYPE: cadence-inferred censored interval observations.
#
# Everything before this script fed the likelihood a DENSE (event-time x delay)
# grid in which delays carrying no published snapshot entered as observed zeros.
# For Texas that is 2649 of 5671 cells (46.7%), and 34 event times are told
# "exactly zero for up to 28 weeks, then the whole count" when the truth is only
# that reporting finished BY that delay.
#
# flusight carries NO is_censored_report flag, so the censoring is INFERRED here,
# from the only thing that is actually observable: which (event week, snapshot)
# rows exist.  A published row is an observation; an absent one is not an
# observation of zero.  For an event week with published ages d_1 < d_2 < ... the
# observations are the interval increments
#
#     Delta_t^(a,b] = C_t(b) - C_t(a) ~ Skellam(alpha_t(a,b), omega_t(a,b))
#     alpha_t(a,b) = mu_t sum_{r=a+1}^{b} gD(r)[p + (1-p) Gbar_C(b-r)]
#     omega_t(a,b) = mu_t (1-p) sum_{r=0}^{a} gD(r)[G_C(b-r) - G_C(a-r)]
#
# with (a,b) = (-1, d_1) for the first.  Left-censoring of the reporting delay is
# then just the a = -1 case -- it needs no separate mechanism, because
# alpha_t(-1,d_1) already marginalises over WHEN in [0, d_1] the reports arrived.
#
# Three arms, so the two changes are separable:
#   w15   -- windowed, cD=15: reproduces scripts 17/19 (hiatus cohorts dropped)
#   wfull -- windowed, full cD: hiatus cohorts enter as left-censored intervals
#   all   -- unwindowed, full cD: + the 85 mature pre-2023 cohorts (tail info)
#
# Run: NOT_CRAN=true ARM=w15 Rscript devel/spa_diagnostics/23_censored_interval.R
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})

ARM   <- Sys.getenv("ARM", "w15")
STATE <- Sys.getenv("STATE", "Texas")
START <- as.Date("2023-09-23")

# ---- estimator core, verbatim from 17/18 ------------------------------------
vskel <- function(z, a, b) {
  out <- numeric(length(z))
  pure_add <- b <= 1e-9; pure_ret <- a <= 1e-9 & !pure_add
  mixed <- !pure_add & !pure_ret
  if (any(pure_add)) out[pure_add] <- ifelse(z[pure_add] < 0, -Inf,
    stats::dpois(pmax(z[pure_add], 0), a[pure_add] + 1e-10, log = TRUE))
  if (any(pure_ret)) out[pure_ret] <- ifelse(z[pure_ret] > 0, -Inf,
    stats::dpois(pmax(-z[pure_ret], 0), b[pure_ret] + 1e-10, log = TRUE))
  if (any(mixed)) {
    zz <- z[mixed]; aa <- a[mixed]; bb <- b[mixed]
    root <- sqrt(zz^2 + 4 * aa * bb)
    u <- ifelse(zz >= 0, (zz + root) / (2 * aa), 2 * bb / (root - zz))
    s <- log(u)
    out[mixed] <- aa * (u - 1) + bb * (1 / u - 1) - s * zz -
                  0.5 * log(2 * pi * (aa * u + bb / u))
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

# ---- q evaluated at the OBSERVED (a,b) pairs only ---------------------------
# The dense (cD+2)x(cD+1) q_tab of scripts 17/18 is O(cD^3) to build and is
# hopeless once cD runs to ~190 weeks.  Only the pairs that actually occur are
# needed, and both sums collapse onto L(a,m) = sum_{r=0}^{a} gD(r) G_C(m-r):
#   alpha/mu = p[G_D(b)-G_D(a)] + (1-p)[K(b) - G_D(a) + L(a,b)]
#   omega/mu = (1-p)[L(a,b) - L(a,a)],   K(b) = sum_{r=0}^{b} gD(r) Gbar_C(b-r)
q_pairs <- function(gD, gC, p, a, b) {
  GD <- cumsum(gD); GC <- cumsum(gC); nD <- length(GD); nC <- length(GC)
  cl <- function(V, k, n) { out <- numeric(length(k)); ok <- k >= 0
                            out[ok] <- V[pmin(k[ok], n - 1L) + 1L]; out }
  Gf  <- function(k) cl(GC, k, nC)
  GDf <- function(k) cl(GD, k, nD)
  L_ab <- L_aa <- Kb <- numeric(length(a))
  for (r in 0:min(nD - 1L, max(b))) {
    g <- gD[r + 1L]; if (!is.finite(g) || g < 1e-14) next
    inA <- a >= r
    if (any(inA)) {
      L_ab[inA] <- L_ab[inA] + g * Gf(b[inA] - r)
      L_aa[inA] <- L_aa[inA] + g * Gf(a[inA] - r)
    }
    inB <- b >= r
    if (any(inB)) Kb[inB] <- Kb[inB] + g * (1 - Gf(b[inB] - r))
  }
  list(qp = pmax(p * (GDf(b) - GDf(a)) + (1 - p) * (Kb - GDf(a) + L_ab), 0),
       qm = pmax((1 - p) * (L_ab - L_aa), 0))
}

# ---- lambda_t profiled out by the two-stage vectorised grid (script 18) ------
GCOARSE <- seq(-13, 13, length.out = 27L)
GFINE   <- seq(-1.5, 1.5, length.out = 13L)
.grid_M <- function(fl, qp, qm, p_val, centre, off) {
  G <- length(off); nO <- fl$nObs; nT <- fl$nTg
  ec <- rep.int(seq_len(nO), G); gc <- rep(seq_len(G), each = nO)
  mu <- exp(centre[fl$grp[ec]] + off[gc]) / p_val
  v <- vskel(fl$z[ec], mu * qp[ec], mu * qm[ec])
  v[!is.finite(v)] <- NA_real_
  agg <- rowsum(v, fl$grp[ec] + (gc - 1L) * nT, na.rm = FALSE)
  Sv <- rep(NA_real_, nT * G); Sv[as.integer(rownames(agg))] <- agg
  matrix(Sv, nT, G)
}
.argmax_na <- function(M) {
  Mf <- M; Mf[is.na(Mf)] <- -Inf
  jm <- max.col(Mf, ties.method = "first")
  jm[!is.finite(Mf[cbind(seq_len(nrow(M)), jm)])] <- 0L; jm
}
neg_loglik <- function(theta, p_val, fl, cD) {
  gD <- mk_gD(theta[1], exp(theta[2]), cD); gC <- mk_gC(theta[3], exp(theta[4]), cD)
  if (any(!is.finite(gD)) || any(!is.finite(gC))) return(1e12)
  Q <- q_pairs(gD, gC, p_val, fl$a, fl$b)
  M1 <- .grid_M(fl, Q$qp, Q$qm, p_val, fl$start, GCOARSE)
  j1 <- .argmax_na(M1); if (any(j1 == 0L)) return(1e12)
  M <- .grid_M(fl, Q$qp, Q$qm, p_val, fl$start + GCOARSE[j1], GFINE)
  jm <- .argmax_na(M); if (any(jm == 0L)) return(1e12)
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
          if (is.finite(corr) && corr <= 1) { total <- total + y2 + corr; next } } }
    }
    total <- total + y2
  }
  if (!is.finite(total) || total > 0) 1e12 else -total
}
P_GRID <- c(0.15, 0.30, 0.45, 0.60, 0.75, 0.85, 0.92, 0.97)
profile_p <- function(fl, cD, start) {
  ll <- rep(-Inf, length(P_GRID)); prev <- start
  for (i in order(P_GRID, decreasing = TRUE)) {          # warm-start continuation
    fit <- stats::optim(prev, neg_loglik, p_val = P_GRID[i], fl = fl, cD = cD,
                        method = "Nelder-Mead", control = list(maxit = 300, reltol = 1e-8))
    ll[i] <- -fit$value; prev <- fit$par
  }
  imax <- which.max(ll); phat <- P_GRID[imax]
  if (imax > 1 && imax < length(P_GRID)) {               # parabolic refine
    x <- P_GRID[(imax-1):(imax+1)]; y <- ll[(imax-1):(imax+1)]
    d <- (x[1]-x[2])*(x[1]-x[3])*(x[2]-x[3])
    A <- (x[3]*(y[2]-y[1]) + x[2]*(y[1]-y[3]) + x[1]*(y[3]-y[2])) / d
    B <- (x[3]^2*(y[1]-y[2]) + x[2]^2*(y[3]-y[1]) + x[1]^2*(y[2]-y[3])) / d
    if (A < 0) phat <- min(max(-B/(2*A), 0.02), 0.995)
  }
  list(phat = phat, ll = ll)
}

# =============================================================================
# DATA: observations inferred from published rows.  No dense grid, no zero-fill.
# =============================================================================
raw <- tbl.now::flusight |> filter(location_name == STATE) |> arrange(target_end_date, as_of)
if (ARM != "all") raw <- raw |> filter(target_end_date >= START)
# floor() is the correct week index: 60 snapshots are Saturdays (offset 0), 4 are
# Wednesdays (offset 4 days); both land in the intended week.
raw <- raw |> mutate(d = floor(as.numeric(as_of - target_end_date) / 7))
CD_CAP <- if (ARM == "w15") 15L else Inf

obs <- list(); ev_keep <- c()
for (E in unique(raw$target_end_date)) {
  x <- raw |> filter(target_end_date == E, d <= CD_CAP) |> arrange(d)
  if (!nrow(x)) next
  x <- x[!duplicated(x$d), ]                      # one observation per delay week
  a <- c(-1L, head(x$d, -1L)); b <- x$d
  z <- x$observation - c(0, head(x$observation, -1L))
  obs[[length(obs) + 1L]] <- list(a = as.integer(a), b = as.integer(b), z = as.numeric(z))
  ev_keep <- c(ev_keep, as.character(E))
}
fl <- list(a = unlist(lapply(obs, `[[`, "a")), b = unlist(lapply(obs, `[[`, "b")),
           z = unlist(lapply(obs, `[[`, "z")),
           grp = rep(seq_along(obs), vapply(obs, function(o) length(o$z), 1L)),
           start = vapply(obs, function(o) log(max(sum(pmax(o$z, 0)), 1)), 1.0),
           nTg = length(obs))
fl$nObs <- length(fl$z)
cD <- max(fl$b)

cat(sprintf("=== ARM %s | %s ===\n", ARM, STATE))
cat(sprintf("event weeks kept      : %d\n", fl$nTg))
cat(sprintf("interval observations : %d   (dense grid would have been %d cells)\n",
            fl$nObs, sum(vapply(obs, function(o) max(o$b) + 1L, 1L))))
cat(sprintf("cD (max delay)        : %d weeks\n", cD))
cat(sprintf("left-censored starts (first interval b>=2): %d event weeks, max b=%d\n",
            sum(vapply(obs, function(o) o$b[1] >= 2, TRUE)),
            max(vapply(obs, function(o) o$b[1], 1L))))
cat(sprintf("negative increments   : %d   total down-revision %.0f\n",
            sum(fl$z < 0), sum(pmin(fl$z, 0))))

t0 <- Sys.time()
pr <- profile_p(fl, cD, start = c(0, log(0.5), log(1.5), log(0.5)))
cat(sprintf("\nfit %.0f s\n", as.numeric(Sys.time() - t0, units = "secs")))
names(pr$ll) <- P_GRID
cat("profile logL (relative to max):\n"); print(round(pr$ll - max(pr$ll), 1))
cat(sprintf("\np_hat = %.3f\n", pr$phat))
saveRDS(list(arm = ARM, state = STATE, ll = pr$ll, phat = pr$phat, nT = fl$nTg,
             nObs = fl$nObs, cD = cD),
        sprintf("devel/spa_diagnostics/censint_%s_%s.rds", ARM, gsub(" ", "", STATE)))
