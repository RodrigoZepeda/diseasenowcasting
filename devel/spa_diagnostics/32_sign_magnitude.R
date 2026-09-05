# =============================================================================
# 32 -- STAGE 1d: decouple the SIGN of a revision from its MAGNITUDE.
#
# Scripts 29-31 kept the revision inside the Skellam family and could not pass
# gates 2 and 3.  Script 31 settles why: psi(a) bought 0.40 nats for its
# parameter and came back at psi = 1.00 at every age, so the likelihood does not
# want an age tilt.  The obstruction is structural.
#
# A Skellam(alpha + psi*lam, lam) has mean alpha + (psi-1)lam, and the SAME lam
# sets both the spread and the balance of signs.  The Texas non-zero moves are
#
#     144 up (78%) vs 41 down (22%),   |move| median 5, max 615 (ratio 123),
#
# so the revision must be strongly asymmetric in SIGN and enormous in RANGE.
# Inside the Skellam those two demands fight: covering +-615 needs a large lam
# (the fit chose 5.65 x level), and at lam = 56,500 for a cohort of 10,000,
# moving P(up) from 0.50 to 0.78 needs psi ~ 1.02, which shifts the mean by
# ~1,130 counts.  Gates 2 and 3 are not two failures, they are one.
#
# So drop the Skellam for the revision and model sign and magnitude separately:
#
#   pi(a)     = plogis(p0 + p1 log(1+a))          does this cohort move?
#   theta(a)  = plogis(t0 + t1 log(1+a))          if it moves, is the move UP?
#   log M     ~ Normal(log kappa + beta log(level), s)   discretised on {1,2,...}
#
#   Delta = 0   w.p. 1 - pi(a)
#         = +M  w.p. pi(a) theta(a)
#         = -M  w.p. pi(a) (1 - theta(a))
#
# A lognormal magnitude spans median 5 to max 615 with one shape parameter, and
# beta lets the scale grow sub-linearly with the cohort level rather than being
# forced to either pure-additive or pure-multiplicative (FINDINGS L.3).
#
# The first interval stays Poisson(mu_t G_D(b)) and is what identifies mu_t.
# LIMITATION, stated deliberately: post-first arrivals are dropped, which is
# justified here because g_D fits P(delay 0) = 0.998 -- reporting is complete at
# first publication for FluSight.  A dataset with genuine late reporting needs
# the arrival term convolved back in, and this prototype does not do that.
#
# No Bessel anywhere: this is pnorm only, so it is ~50x cheaper than 29-31.
#
# Run: COLD=1 START=1 NOT_CRAN=true Rscript devel/spa_diagnostics/32_sign_magnitude.R
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})
STATE <- Sys.getenv("STATE", "Texas")
START <- as.Date("2023-09-23")
set.seed(20260902)

mk_gD <- function(dmu, dsig, cD) {
  f <- diseasenowcasting:::.delay_distribution_functions(1L, dmu, 0.01 + dsig)
  acdf <- as.numeric(f$cdf(seq_len(cD + 1L))); c(acdf[1], diff(acdf))
}
lpois <- function(z, a) ifelse(z < 0, -Inf, stats::dpois(pmax(z, 0), a, log = TRUE))

# ---- data, identical construction to scripts 23/26/29 -----------------------
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
fl$nObs <- length(fl$z); H <- max(fl$b); fl$post <- fl$a >= 0
cat(sprintf("=== %s | w15 | %d event weeks, %d intervals (%d post-first), H = %d ===\n\n",
            STATE, fl$nTg, fl$nObs, sum(fl$post), H))

# ---- the law ----------------------------------------------------------------
# theta = (dmu, log dsig, p0, p1, t0, t1, log kappa, beta, log s)
unpack <- function(th) list(gD = mk_gD(th[1], exp(th[2]), H),
  p0 = th[3], p1 = th[4], t0 = th[5], t1 = th[6],
  lkap = th[7], beta = th[8], s = exp(th[9]))

# log P(M = k) for M discretised on {1,2,...} from LogNormal(m, s).
# Computed from UPPER tails in log space.  The naive form
#   log(Phi(hi) - Phi(lo)) - log(1 - Phi((log 0.5 - m)/s))
# underflows to log(1e-300) - log(1e-300) = 0 when the median is driven far
# below 1 -- a log-probability of ZERO, i.e. certainty -- and the optimiser
# exploits exactly that (it found beta = -1.08, s = 0.39, logL = -110 on 736
# observations).  Upper tails via pnorm(lower.tail = FALSE, log.p = TRUE) stay
# accurate in the far tail, and the bin mass is a subset of the normalising
# mass by construction, so this form cannot exceed 0.
lsf <- function(x) stats::pnorm(x, lower.tail = FALSE, log.p = TRUE)
ldiff <- function(a, b) a + log1p(-exp(pmin(b - a, -1e-12)))   # log(e^a - e^b), a > b
lmag <- function(k, m, s) {
  lo <- lsf((log(pmax(k - 0.5, 1e-8)) - m) / s)     # log P(M > k-0.5)
  hi <- lsf((log(k + 0.5) - m) / s)                 # log P(M > k+0.5)
  nrm <- lsf((log(0.5) - m) / s)                    # log P(M > 0.5)
  pmin(ldiff(lo, hi) - nrm, 0)
}
cell_ll <- function(z, mu, aa, bb, post, P) {
  GD <- cumsum(P$gD)
  GDf <- function(k) ifelse(k < 0, 0, GD[pmin(pmax(k, 0), length(GD) - 1L) + 1L])
  out <- numeric(length(z))
  first <- !post
  if (any(first)) out[first] <- lpois(z[first], pmax(mu[first] * GDf(bb[first]), 1e-10))
  if (any(post)) {
    zz <- z[post]; lev <- pmax(mu[post] * GDf(aa[post]), 1)
    lpi <- stats::plogis(P$p0 + P$p1 * log1p(bb[post]), log.p = TRUE)
    l1pi <- stats::plogis(-(P$p0 + P$p1 * log1p(bb[post])), log.p = TRUE)
    lth <- stats::plogis(P$t0 + P$t1 * log1p(bb[post]), log.p = TRUE)
    l1th <- stats::plogis(-(P$t0 + P$t1 * log1p(bb[post])), log.p = TRUE)
    m <- P$lkap + P$beta * log(lev)
    v <- numeric(length(zz))
    zero <- zz == 0; up <- zz > 0; dn <- zz < 0
    v[zero] <- l1pi[zero]
    if (any(up)) v[up] <- lpi[up] + lth[up]  + lmag(zz[up], m[up], P$s)
    if (any(dn)) v[dn] <- lpi[dn] + l1th[dn] + lmag(-zz[dn], m[dn], P$s)
    out[post] <- v
  }
  out
}

# ---- mu_t profiled by the two-stage grid (now cheap: pnorm only) ------------
GCOARSE <- seq(-6, 6, length.out = 13L); GFINE <- seq(-0.5, 0.5, length.out = 9L)
.gridM <- function(P, centre, off) {
  G <- length(off); nO <- fl$nObs; nT <- fl$nTg
  ec <- rep.int(seq_len(nO), G); gc <- rep(seq_len(G), each = nO)
  v <- cell_ll(fl$z[ec], exp(centre[fl$grp[ec]] + off[gc]), fl$a[ec], fl$b[ec], fl$post[ec], P)
  v[!is.finite(v)] <- NA_real_
  agg <- rowsum(v, fl$grp[ec] + (gc - 1L) * nT, na.rm = FALSE)
  Sv <- rep(NA_real_, nT * G); Sv[as.integer(rownames(agg))] <- agg
  matrix(Sv, nT, G)
}
.amax <- function(M) { Mf <- M; Mf[is.na(Mf)] <- -Inf
  jm <- max.col(Mf, ties.method = "first")
  jm[!is.finite(Mf[cbind(seq_len(nrow(M)), jm)])] <- 0L; jm }
profile_mu <- function(P) {
  M1 <- .gridM(P, fl$start, GCOARSE); j1 <- .amax(M1); if (any(j1 == 0L)) return(NULL)
  M <- .gridM(P, fl$start + GCOARSE[j1], GFINE); jm <- .amax(M); if (any(jm == 0L)) return(NULL)
  list(M = M, jm = jm, centre = fl$start + GCOARSE[j1])
}
neg_ll <- function(th) {
  if (any(!is.finite(th)) || th[2] > 3 || th[9] > 3) return(1e12)
  P <- unpack(th); if (any(!is.finite(P$gD))) return(1e12)
  pr <- profile_mu(P); if (is.null(pr)) return(1e12)
  G <- ncol(pr$M); total <- 0
  for (t in seq_len(fl$nTg)) {
    j <- pr$jm[t]; y2 <- pr$M[t, j]
    if (j > 1L && j < G) { y1 <- pr$M[t, j-1L]; y3 <- pr$M[t, j+1L]
      if (is.finite(y1) && is.finite(y3)) { den <- y1 - 2*y2 + y3
        # vertex of the parabola through (-1,y1) (0,y2) (1,y3), in grid units.
        # UNGUARDED this explodes when the profile is flat (den -> 0-), which
        # silently produced POSITIVE log-likelihoods in script 33.
        if (den < 0) { corr <- -0.125 * (y1 - y3)^2 / den
          if (is.finite(corr) && corr <= 1) { total <- total + y2 + corr; next } } } }
    total <- total + y2
  }
  if (is.finite(total) && total <= 0) -total else 1e12
}

CKPT <- sprintf("devel/spa_diagnostics/signmagfit_%s.rds", gsub(" ", "", STATE))
starts <- list(c(-3.5, 0.5,  2.2, -2.0,  1.3, 0.0, log(5), 0.00, log(1.5)),
               c(-3.5, 0.5,  2.2, -2.0,  1.0, -0.2, log(2), 0.20, log(1.2)),
               c(-2.0, 0.5,  1.8, -1.7,  1.5, 0.2, log(8), -0.1, log(2.0)))
t0 <- Sys.time(); best <- NULL
if (nzchar(Sys.getenv("GATESONLY"))) { best <- readRDS(CKPT) } else {
  ONE <- as.integer(Sys.getenv("START", "0"))
  for (i in (if (ONE > 0) ONE else seq_along(starts))) {
    ft <- stats::optim(starts[[i]], neg_ll, method = "BFGS", control = list(maxit = 300, reltol = 1e-12))
    ft <- stats::optim(ft$par, neg_ll, method = "Nelder-Mead", control = list(maxit = 1500, reltol = 1e-13))
    cat(sprintf("start %d: logL = %.2f  (%.0f s)\n", i, -ft$value, as.numeric(Sys.time()-t0, units="secs")))
    old <- if (file.exists(CKPT)) readRDS(CKPT) else NULL
    if (is.null(old) || ft$value < old$value) { saveRDS(list(par = ft$par, value = ft$value), CKPT)
      best <- list(par = ft$par, value = ft$value) } else best <- old
  }
}
P <- unpack(best$par); pr <- profile_mu(P); mu <- exp(pr$centre + GFINE[pr$jm])
cat(sprintf("\nlogL = %.2f\n", -best$value))
cat(sprintf("g_D     : P(0) = %.3f\n", P$gD[1]))
cat(sprintf("pi(a)   : %.3f at age 1, %.3f at 4, %.3f at 15   (empirical .58 / .12 / .023)\n",
  stats::plogis(P$p0+P$p1*log(2)), stats::plogis(P$p0+P$p1*log(5)), stats::plogis(P$p0+P$p1*log(16))))
cat(sprintf("theta(a): P(move is UP) = %.3f at age 1, %.3f at 4, %.3f at 15   (empirical overall .78)\n",
  stats::plogis(P$t0+P$t1*log(2)), stats::plogis(P$t0+P$t1*log(5)), stats::plogis(P$t0+P$t1*log(16))))
cat(sprintf("|move|  : median = %.2f x level^%.3f, log-sd %.3f\n", exp(P$lkap), P$beta, P$s))

# ---- GATES ------------------------------------------------------------------
GD <- cumsum(P$gD); GDf <- function(k) ifelse(k < 0, 0, GD[pmin(pmax(k,0), length(GD)-1L)+1L])
mu_i <- mu[fl$grp]; k <- which(fl$post)
lev <- pmax(mu_i * GDf(fl$a), 1); m_i <- P$lkap + P$beta * log(lev)
pi_i <- stats::plogis(P$p0 + P$p1 * log1p(fl$b)); th_i <- stats::plogis(P$t0 + P$t1 * log1p(fl$b))
pmf <- function(z, i) {
  out <- numeric(length(z))
  out[z == 0] <- 1 - pi_i[i]
  up <- z > 0; dn <- z < 0
  if (any(up)) out[up] <- pi_i[i] * th_i[i] * exp(lmag(z[up], m_i[i], P$s))
  if (any(dn)) out[dn] <- pi_i[i] * (1-th_i[i]) * exp(lmag(-z[dn], m_i[i], P$s))
  out
}
cat("\n=== GATE 1: exact zeros ===\n")
cat(sprintf("observed %d of %d (%.0f%%) | model %.0f (%.0f%%) | retention model 85 (11%%)\n",
    sum(fl$z[k]==0), length(k), 100*mean(fl$z[k]==0), sum(1-pi_i[k]), 100*mean(1-pi_i[k])))

# exact CDF: P(Z < z) in closed form.  Smag(x) = P(M > x) for the discretised
# lognormal, normalised over {1,2,...}.
Smag <- function(x, m) exp(lsf((log(x + 0.5) - m) / P$s) - lsf((log(0.5) - m) / P$s))
cdf_lower <- function(z, i) {
  if (z > 0)  return(1 - pi_i[i] * th_i[i] * Smag(z - 1, m_i[i]))
  if (z == 0) return(pi_i[i] * (1 - th_i[i]))
  pi_i[i] * (1 - th_i[i]) * Smag(-z - 1 + 1e-9, m_i[i])
}
Fl <- vapply(k, function(i) cdf_lower(fl$z[i], i), 1.0)
Pa <- vapply(k, function(i) pmf(fl$z[i], i), 1.0)
U <- Fl + stats::runif(length(k)) * Pa
# GATE 2, REDEFINED.  The original form -- "of the cells with |z| > 50, what
# fraction land inside the central 95% predictive?" -- is not a valid test:
# conditioning on the OBSERVATION being extreme selects cells whose PIT is
# necessarily near an end, so even a perfectly calibrated model fails it, and
# the 95% target is meaningless.  The proper posterior predictive check is the
# unconditional one: how many cells does the model EXPECT to exceed the
# threshold, against how many actually do?
cat("\n=== GATE 2: the tail, as a posterior predictive check ===\n")
for (thr in c(20, 50, 100, 300)) {
  exp_n <- sum(pi_i[k] * Smag(thr, m_i[k]))
  cat(sprintf("  |z| > %3d : observed %3d   expected %6.1f   ratio %.2f\n",
              thr, sum(abs(fl$z[k]) > thr), exp_n, sum(abs(fl$z[k]) > thr) / max(exp_n, 1e-9)))
}
cat(sprintf("  PIT beyond the central 95%% (all cells): %.1f%%, nominal 5%%\n",
            100 * mean(U < 0.025 | U > 0.975)))
ks <- stats::ks.test(U, "punif")
cat("\n=== GATE 3: randomised PIT ===\n")
cat(sprintf("n = %d | KS D = %.4f, p = %.3g | deciles:\n", length(U), ks$statistic, ks$p.value))
print(round(as.numeric(table(cut(U, seq(0,1,0.1)))) / length(U), 3))
cat("\n=== GATE 4: log-likelihood ===\n")
cat(sprintf("sign x magnitude (9 par)      : %.2f\n", -best$value))
cat(sprintf("psi(a) two-scale (9 par, sc31): -1521.35\n"))
cat(sprintf("two-scale hurdle (8 par, sc30): -1521.75\n"))
cat(sprintf("free retention  (17 par, sc26): -3055.41\n"))
saveRDS(list(state=STATE, par=best$par, logL=-best$value, P=P, mu=mu, U=U, ks=ks),
        sprintf("devel/spa_diagnostics/signmag_%s.rds", gsub(" ", "", STATE)))
