# =============================================================================
# 26 -- FIT THE OPERATIONAL FINITE-HORIZON MODEL (reviewer note sections 7, 8,
#       28, 30.2).
#
# Script 25 proved that the cadence interval likelihood depends on (p, g_C) only
# through the within-horizon retraction masses b_c, and that with the tail beyond
# H free the profile in p is EXACTLY flat.  So stop estimating p and estimate
# what the data hold: the gross report rate mu_t, the reporting delay g_D, and
# the retention curve
#
#     r(a) = 1 - sum_{c<=a} b_c,     a = 1..H.
#
# b_1..b_15 unrestricted is 15 parameters against a support that thins from 761
# intervals at lag 1 to 49 at lag 15 (script 25 part C), so the masses are pooled
# into five blocks -- {1}, {2}, {3,4}, {5..8}, {9..15} -- which are the ages the
# reviewer's section 4 table asks about.  Total within-horizon mass and its split
# are free:
#
#     S = plogis(s0),  f = softmax(0, v1..v4),  block mass = S f_k,
#     b_c = S f_k / |block k|  for c in block k.
#
# b_c >= 0 and sum b_c <= 1 hold by construction, so there is no boundary to
# handle.  q_pairs(gD, bvec, p = 0, ...) IS the b-form (feed b in place of g_C
# and set p = 0; verified against the independent q_bform in script 25 part A),
# so the vectorised interval-rate path is reused unchanged.
#
# The blocks are only a starting point: stage 2 frees all 15 masses,
#
#     b_c = S softmax(0, w_1..w_14)_c,
#
# and refines with BFGS from the stage-1 optimum (Nelder-Mead stalls badly here
# -- the four block starts spread over 200 nats).
#
# Stage 3 is the check that matters for interpretation.  Under this model the
# gross retraction flow is sum_i mu_t qm_i, and the data only ever show the NET
# increment, so the fit is free to invent churn: large alpha and large omega give
# the same net movement with more variance.  Compare the fitted gross flows with
# the observed net movements.  If retraction flow comes back an order of
# magnitude above the observed down-movement, the fitted b_c are a variance dial,
# not a retention curve -- and that is a statement about the mean/variance
# structure, quite separate from the p-tail identification question.
#
# Reported against script 23's lognormal-g_C profile on the same w15 data.
#
# Run: NOT_CRAN=true Rscript devel/spa_diagnostics/26_free_retention.R
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})
STATE <- Sys.getenv("STATE", "Texas")
START <- as.Date("2023-09-23")
set.seed(20260902)

# ---- estimator core, verbatim from 23/25 ------------------------------------
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
    if (any(inA)) { L_ab[inA] <- L_ab[inA] + g * Gf(b[inA] - r)
                    L_aa[inA] <- L_aa[inA] + g * Gf(a[inA] - r) }
    inB <- b >= r
    if (any(inB)) Kb[inB] <- Kb[inB] + g * (1 - Gf(b[inB] - r))
  }
  list(qp = pmax(p * (GDf(b) - GDf(a)) + (1 - p) * (Kb - GDf(a) + L_ab), 0),
       qm = pmax((1 - p) * (L_ab - L_aa), 0))
}
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
loglik_q <- function(fl, qp, qm) {
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

# ---- interval observations, arm w15, exactly as script 23 builds them -------
raw <- tbl.now::flusight |> filter(location_name == STATE, target_end_date >= START) |>
  arrange(target_end_date, as_of) |>
  mutate(d = floor(as.numeric(as_of - target_end_date) / 7))
obs <- list()
for (E in unique(raw$target_end_date)) {
  x <- raw |> filter(target_end_date == E, d <= 15L) |> arrange(d)
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
fl$nObs <- length(fl$z); H <- max(fl$b)
cat(sprintf("=== %s | w15 | %d event weeks, %d intervals, H = %d ===\n\n",
            STATE, fl$nTg, fl$nObs, H))

# ---- block retention parameterisation ---------------------------------------
BLK <- list(1L, 2L, 3:4, 5:8, 9:15)
blk_lab <- vapply(BLK, function(k) if (length(k) == 1) sprintf("%d", k)
                  else sprintf("%d-%d", min(k), max(k)), "")
mk_b_blk <- function(par) {                        # par = (s0, v1..v4) -> b vector 0..H
  S <- stats::plogis(par[1]); f <- exp(c(0, par[-1])); f <- f / sum(f)
  b <- numeric(H + 1L)
  for (k in seq_along(BLK)) b[BLK[[k]] + 1L] <- S * f[k] / length(BLK[[k]])
  b
}
neg_ll_blk <- function(theta) {
  gD <- mk_gD(theta[1], exp(theta[2]), H)
  if (any(!is.finite(gD))) return(1e12)
  b <- mk_b_blk(theta[-(1:2)])
  Q <- q_pairs(gD, b, 0, fl$a, fl$b)           # p = 0 + b in place of g_C = the b-form
  -loglik_q(fl, Q$qp, Q$qm)
}

# ---- stage 1: block fit from several starts ---------------------------------
starts <- list(c(0, log(0.5), stats::qlogis(0.05), 0, 0, 0, 0),
               c(0, log(0.5), stats::qlogis(0.30), 1, 0, -1, -1),
               c(0, log(0.5), stats::qlogis(0.50), -1, -1, 0, 1),
               c(0.5, log(1.0), stats::qlogis(0.10), 0, 1, 1, 0))
t0 <- Sys.time(); best <- NULL
for (i in seq_along(starts)) {
  fit <- stats::optim(starts[[i]], neg_ll_blk, method = "Nelder-Mead",
                      control = list(maxit = 2000, reltol = 1e-10))
  cat(sprintf("stage 1 start %d: logL = %.2f  (S = %.4f)\n", i, -fit$value,
              stats::plogis(fit$par[3])))
  if (is.null(best) || fit$value < best$value) best <- fit
}

# ---- stage 2: all 15 masses free, BFGS --------------------------------------
mk_b_free <- function(par) {                   # par = (s0, w_1..w_14)
  S <- stats::plogis(par[1]); f <- exp(c(0, par[-1])); f <- f / sum(f)
  c(0, S * f)
}
neg_ll_free <- function(theta) {
  gD <- mk_gD(theta[1], exp(theta[2]), H)
  if (any(!is.finite(gD))) return(1e12)
  Q <- q_pairs(gD, mk_b_free(theta[-(1:2)]), 0, fl$a, fl$b)
  -loglik_q(fl, Q$qp, Q$qm)
}
b0 <- mk_b_blk(best$par[-(1:2)])[-1]
th0 <- c(best$par[1:2], stats::qlogis(min(sum(b0), 0.999)),
         log(pmax(b0[-1], 1e-8) / max(b0[1], 1e-8)))
free <- NULL
for (i in 1:3) {
  st <- if (i == 1) th0 else th0 + c(0, 0, stats::rnorm(15, 0, 0.5))
  ft <- stats::optim(st, neg_ll_free, method = "BFGS",
                     control = list(maxit = 400, reltol = 1e-12))
  ft <- stats::optim(ft$par, neg_ll_free, method = "BFGS",
                     control = list(maxit = 400, reltol = 1e-14))
  cat(sprintf("stage 2 start %d: logL = %.2f  (S = %.4f)\n", i, -ft$value,
              stats::plogis(ft$par[3])))
  if (is.null(free) || ft$value < free$value) free <- ft
}
cat(sprintf("fit %.0f s\n\n", as.numeric(Sys.time() - t0, units = "secs")))

b <- mk_b_free(free$par[-(1:2)]); Bc <- cumsum(b)
gD <- mk_gD(free$par[1], exp(free$par[2]), H)
cat(sprintf("g_D: mean %.2f wk, P(delay 0) = %.3f, P(delay <= 1) = %.3f\n",
            sum((0:H) * gD), gD[1], sum(gD[1:2])))
cat(sprintf("total within-horizon retraction mass  sum_{c<=%d} b_c = %.4f\n", H, sum(b)))
cat(sprintf("=> r(H) = %.4f: p is FLAT on (0, %.4f] and cannot exceed it\n\n",
            1 - sum(b), 1 - sum(b)))
cat("fitted retraction masses b_c and retention r(a) = 1 - sum_{c<=a} b_c:\n")
print(data.frame(lag = 1:H, b = round(b[-1], 5), r = round(1 - Bc[-1], 4)),
      row.names = FALSE, max = 1e4)
cat("\nreviewer section 4 table:\n")
for (a in c(1, 2, 4, 8, 15)) cat(sprintf("  r(%2d) = %.4f\n", a, 1 - Bc[a + 1L]))

# ---- stage 3: is b a retention curve or a variance dial? --------------------
Q <- q_pairs(gD, b, 0, fl$a, fl$b)
mu <- vapply(seq_len(fl$nTg), function(t) {
  k <- which(fl$grp == t)
  stats::optimize(function(lm) -sum(vskel(fl$z[k], exp(lm) * Q$qp[k], exp(lm) * Q$qm[k])),
                  c(fl$start[t] - 13, fl$start[t] + 13))$minimum
}, 1.0)
E_add <- sum(exp(mu[fl$grp]) * Q$qp); E_ret <- sum(exp(mu[fl$grp]) * Q$qm)
obs_up <- sum(pmax(fl$z, 0)); obs_dn <- -sum(pmin(fl$z, 0))
cat(sprintf("\n=== stage 3: gross flow vs observed net movement ===\n"))
cat(sprintf("model expected gross additions   : %10.0f\n", E_add))
cat(sprintf("model expected gross retractions : %10.0f\n", E_ret))
cat(sprintf("observed net up-movement         : %10.0f\n", obs_up))
cat(sprintf("observed net down-movement       : %10.0f   (a LOWER bound on gross)\n", obs_dn))
cat(sprintf("invented churn ratio E_ret / obs_dn = %.1f x\n", E_ret / obs_dn))
cat(sprintf("model net (E_add - E_ret) = %.0f vs observed net %.0f\n",
            E_add - E_ret, obs_up - obs_dn))

par23 <- readRDS("devel/spa_diagnostics/censint_w15_Texas.rds")
cat(sprintf("\nlogL free retention (17 par)      : %.2f\n", -free$value))
cat(sprintf("logL lognormal g_C + p (5 par, 23): %.2f  at p_hat = %.3f\n",
            max(par23$ll), par23$phat))
cat(sprintf("difference: %.2f nats for 12 extra parameters\n", -free$value - max(par23$ll)))

saveRDS(list(state = STATE, arm = "w15", par = free$par, logL = -free$value,
             b = b, r = 1 - Bc, gD = gD, mu = exp(mu), H = H,
             E_add = E_add, E_ret = E_ret, obs_up = obs_up, obs_dn = obs_dn),
        sprintf("devel/spa_diagnostics/freeret_%s.rds", gsub(" ", "", STATE)))
