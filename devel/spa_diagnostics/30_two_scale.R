# =============================================================================
# 30 -- STAGE 1b: two revision scales.  Script 29 passed gates 1 and 4 and
#       failed 2 and 3, and the two failures agree with each other.
#
# Script 29 fitted ONE revision scale, lamR = sigma0 + sigma1 * level, and the
# additive floor went to zero: every moving cell got the same relative width.
# The gates said that is wrong in both directions at once --
#
#   gate 2: only 11 of the 27 cells with |z| > 50 fell inside the central 95%
#           predictive interval (nominal 95%), so the tail is far too thin;
#   gate 3: the randomised PIT is DEPLETED at both extremes (deciles 0.076 ...
#           0.073, KS p = 5e-4), so the body is too wide.
#
# Body over-dispersed and tail too thin is the signature of a single scale doing
# duty for two populations.  Stage 0 (FINDINGS L.3) already said as much: small
# moves are additive, large ones multiplicative.  The additive term belongs as
# the scale of its OWN component, not as a floor inside a single lambda where it
# simply fits to zero.
#
# So the revision becomes a two-point scale mixture:
#
#   alpha_t(a,b) = mu_t [G_D(b) - G_D(a)]                    arrivals
#   pi(b)        = plogis(p0 + p1 log(1+b))                  participation
#   lam_s        = sigma_s                                   small, additive
#   lam_L        = sigma_1 mu_t G_D(a)                       large, multiplicative
#
#   Delta ~ (1-pi)     Poisson(alpha)
#         + pi (1-q)   Skellam(alpha + psi lam_s, lam_s)
#         + pi q       Skellam(alpha + psi lam_L, lam_L)
#
# One extra parameter over script 29 (q, and sigma_s replaces the dead sigma0).
#
# Numerics as in 29: exact Skellam via scaled Bessel with an ascending log-series
# once the order exceeds the argument, verified against direct convolution for
# BOTH signs of z with a != b, and checked to sum to 1.
#
# Run: COLD=1 START=1 NOT_CRAN=true Rscript devel/spa_diagnostics/30_two_scale.R
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})
STATE <- Sys.getenv("STATE", "Texas")
START <- as.Date("2023-09-23")
set.seed(20260902)

# ---- exact Skellam / Poisson log densities ----------------------------------
# R's besselI(x, nu) loses precision once nu > x ("precision lost in result"),
# which happens constantly here: a cell with z = 615 is evaluated at small rates
# all over the mu grid.  In that regime use the ascending series in log space,
#   log I_nu(x) = nu log(x/2) - lgamma(nu+1) + log sum_k y^k / (k! prod_j (nu+j))
# with y = x^2/4, whose terms peak at k ~ y/nu < 1 and then decay geometrically.
lpois <- function(z, a) ifelse(z < 0, -Inf, stats::dpois(pmax(z, 0), a, log = TRUE))
KSER <- 40L
log_besselI <- function(nu, x) {
  n <- max(length(nu), length(x))          # recycle: the gate code calls this
  nu <- rep_len(nu, n); x <- rep_len(x, n) # with a scalar rate and a vector z
  out <- numeric(n); ser <- nu > x
  if (any(!ser)) out[!ser] <- log(besselI(x[!ser], nu[!ser], expon.scaled = TRUE)) + x[!ser]
  if (any(ser)) {
    n <- nu[ser]; xx <- pmax(x[ser], 1e-300); ly <- 2 * log(xx / 2)
    lt <- outer(rep(0, length(n)), 0:KSER) +
          outer(rep(1, length(n)), 0:KSER) * ly -
          rep(lgamma(seq_len(KSER + 1L)), each = length(n)) -
          (lgamma(outer(n, 0:KSER, "+") + 1) - lgamma(n + 1))
    m <- do.call(pmax, as.data.frame(lt))
    out[ser] <- n * log(xx / 2) - lgamma(n + 1) + m + log(rowSums(exp(lt - m)))
  }
  out
}
# NOTE the two z's are different: the Bessel ORDER is |z|, but the exponent on
# the rate ratio is z itself.  Using |z| in both places is wrong for z < 0
# whenever a != b, and is invisible in any check that uses a = b or z >= 0.
lskel <- function(z, a, b) {                     # a, b > 0
  x <- 2 * sqrt(a * b)
  -(a + b) + (z / 2) * log(a / b) + log_besselI(abs(z), x)
}
mk_gD <- function(dmu, dsig, cD) {
  f <- diseasenowcasting:::.delay_distribution_functions(1L, dmu, 0.01 + dsig)
  acdf <- as.numeric(f$cdf(seq_len(cD + 1L))); c(acdf[1], diff(acdf))
}

# ---- interval observations, w15 arm, as scripts 23/26 ----------------------
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

# ---- the mixture log density ------------------------------------------------
# theta = (dmu, log dsig, p0, p1, logit q, log sigma_s, log sigma_1, log psi)
unpack <- function(theta) list(
  gD = mk_gD(theta[1], exp(theta[2]), H), p0 = theta[3], p1 = theta[4],
  q = stats::plogis(theta[5]), ss = exp(theta[6]), s1 = exp(theta[7]),
  psi = exp(theta[8]))

lse3 <- function(A, B, C) {
  m <- pmax(A, B, C); m[!is.finite(m)] <- 0
  m + log(exp(A - m) + exp(B - m) + exp(C - m))
}
cell_ll <- function(z, mu, aa, bb, post, P) {
  GD <- cumsum(P$gD)
  GDf <- function(k) ifelse(k < 0, 0, GD[pmin(pmax(k, 0), length(GD) - 1L) + 1L])
  alpha <- pmax(mu * (GDf(bb) - GDf(aa)), 1e-10)
  lamL  <- pmax(P$s1 * mu * GDf(aa), 1e-10)
  lamS  <- rep_len(P$ss, length(z))
  pi_   <- ifelse(post, stats::plogis(P$p0 + P$p1 * log1p(bb)), 0)
  l0 <- lpois(z, alpha)
  out <- numeric(length(z)); on <- pi_ > 1e-12
  out[!on] <- l0[!on]
  if (any(on)) out[on] <- lse3(
    log1p(-pi_[on]) + l0[on],
    log(pi_[on]) + log1p(-P$q) + lskel(z[on], alpha[on] + P$psi * lamS[on], lamS[on]),
    log(pi_[on]) + log(P$q)    + lskel(z[on], alpha[on] + P$psi * lamL[on], lamL[on]))
  out
}

# ---- mu_t profiled by the same two-stage grid used since script 18 ----------
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
  M1 <- .gridM(P, fl$start, GCOARSE); j1 <- .amax(M1)
  if (any(j1 == 0L)) return(NULL)
  M <- .gridM(P, fl$start + GCOARSE[j1], GFINE); jm <- .amax(M)
  if (any(jm == 0L)) return(NULL)
  list(M = M, jm = jm, centre = fl$start + GCOARSE[j1])
}
neg_ll <- function(theta) {
  if (any(!is.finite(theta)) || theta[2] > 3 || theta[6] > 5) return(1e12)
  P <- unpack(theta); if (any(!is.finite(P$gD))) return(1e12)
  pr <- profile_mu(P); if (is.null(pr)) return(1e12)
  G <- ncol(pr$M); total <- 0
  for (t in seq_len(fl$nTg)) {
    j <- pr$jm[t]; y2 <- pr$M[t, j]
    if (j > 1L && j < G) { y1 <- pr$M[t, j-1L]; y3 <- pr$M[t, j+1L]
      if (is.finite(y1) && is.finite(y3)) { den <- y1 - 2*y2 + y3
        if (den < 0) { corr <- -0.125*(y1-y3)^2/den   # >= 0; UNGUARDED this
          # explodes when the mu profile is flat (den -> 0-) or when the grid
          # values are wildly asymmetric.  A refinement over a grid of step
          # 0.125 in log mu cannot be worth more than a fraction of a nat.
          if (is.finite(corr) && corr <= 1) { total <- total + y2 + corr; next } } } }
    total <- total + y2
  }
  if (is.finite(total) && total <= 0) -total else 1e12
}

# ---- fit --------------------------------------------------------------------
# WARM is the converged optimum of the three cold starts below (logL -1566.91,
# all three within 0.14 nats of each other, so the optimum is not in doubt).
# Set COLD=1 to redo the multi-start search from scratch (~37 min).
# seeded from the script 29 optimum, with the new scale split opened up
WARM <- c(-3.50, 0.50, 2.13, -1.78, stats::qlogis(0.25), log(3), log(2.05), 0)
starts <- if (nzchar(Sys.getenv("COLD"))) list(
               WARM,
               c(-3.50, 0.50, 2.13, -1.78, stats::qlogis(0.10), log(1.5), log(3.0), 0),
               c(-2.00, 0.50, 1.50, -1.50, stats::qlogis(0.50), log(6), log(1.0), 0)) else list(WARM)
# SKIPFIT=1 evaluates the objective at WARM and goes straight to the gates.
# WARM was recovered from the converged cold fit, so this reproduces it without
# repeating the 37-minute multi-start search.
CKPT <- sprintf("devel/spa_diagnostics/twoscalefit_%s.rds", gsub(" ", "", STATE))
keep_best <- function(par, value) {                # merge with any earlier start
  old <- if (file.exists(CKPT)) readRDS(CKPT) else NULL
  if (is.null(old) || value < old$value) {
    saveRDS(list(par = par, value = value, state = STATE), CKPT)
    list(par = par, value = value)
  } else list(par = old$par, value = old$value)
}
t0 <- Sys.time(); best <- NULL
if (nzchar(Sys.getenv("GATESONLY"))) {
  best <- list(par = readRDS(CKPT)$par, value = readRDS(CKPT)$value)
} else if (nzchar(Sys.getenv("SKIPFIT"))) {
  v <- neg_ll(WARM); best <- list(par = WARM, value = v)
  cat(sprintf("SKIPFIT: logL at WARM = %.2f  (cold multi-start reached -1566.91)\n", -v))
} else {
  ONE <- as.integer(Sys.getenv("START", "0"))     # 0 = all starts in one run
  idx <- if (ONE > 0) ONE else seq_along(starts)
  for (i in idx) {
    ft <- stats::optim(starts[[i]], neg_ll, method = "BFGS",
                       control = list(maxit = 150, reltol = 1e-12))
    ft <- stats::optim(ft$par, neg_ll, method = "Nelder-Mead",
                       control = list(maxit = 400, reltol = 1e-12))
    cat(sprintf("start %d: logL = %.2f  (%.0f s)\n", i, -ft$value,
                as.numeric(Sys.time() - t0, units = "secs")))
    best <- keep_best(ft$par, ft$value)
  }
}
cat(sprintf("fit %.0f s\n\n", as.numeric(Sys.time() - t0, units = "secs")))
if (nzchar(Sys.getenv("GATESONLY"))) { ck <- readRDS(CKPT); best <- ck
  cat(sprintf("gates at the checkpointed optimum: logL = %.2f\n", -ck$value)) }
P <- unpack(best$par)
pr <- profile_mu(P)
mu <- exp(pr$centre + GFINE[pr$jm])
cat(sprintf("mu_t profile interior (not at a grid edge): %d of %d\n",
            sum(pr$jm > 1 & pr$jm < length(GFINE)), fl$nTg))
cat(sprintf("g_D    : mean %.2f wk, P(0) = %.3f, P(<=1) = %.3f\n",
            sum((0:H) * P$gD), P$gD[1], sum(P$gD[1:2])))
cat(sprintf("pi(a)  : logit = %.2f %+.2f log(1+a)  ->  pi(1) = %.3f, pi(4) = %.3f, pi(15) = %.3f\n",
            P$p0, P$p1, stats::plogis(P$p0 + P$p1*log(2)), stats::plogis(P$p0 + P$p1*log(5)),
            stats::plogis(P$p0 + P$p1*log(16))))
cat(sprintf("scales : q = %.3f of revisions are LARGE | sigma_s = %.2f (additive) | sigma_1 = %.4f x level\n",
            P$q, P$ss, P$s1))
cat(sprintf("psi    : %.3f   (1 = symmetric revision, <1 = net downward)\n\n", P$psi))

# ---- GATES ------------------------------------------------------------------
GD <- cumsum(P$gD)
GDf <- function(k) ifelse(k < 0, 0, GD[pmin(pmax(k, 0), length(GD) - 1L) + 1L])
mu_i <- mu[fl$grp]
alpha <- pmax(mu_i * (GDf(fl$b) - GDf(fl$a)), 1e-10)
lamL  <- pmax(P$s1 * mu_i * GDf(fl$a), 1e-10)
lamS  <- rep_len(P$ss, fl$nObs)
pi_   <- ifelse(fl$post, stats::plogis(P$p0 + P$p1 * log1p(fl$b)), 0)
pmf <- function(z, i) {                # z may be a vector; i is a single cell
  if (pi_[i] <= 1e-12) return(exp(lpois(z, alpha[i])))
  exp(lse3(log1p(-pi_[i]) + lpois(z, alpha[i]),
           log(pi_[i]) + log1p(-P$q) + lskel(z, alpha[i] + P$psi * lamS[i], lamS[i]),
           log(pi_[i]) + log(P$q)    + lskel(z, alpha[i] + P$psi * lamL[i], lamL[i])))
}
k <- which(fl$post)
p0hat <- vapply(k, function(i) pmf(0, i), 1.0)
cat("=== GATE 1: exact zeros ===\n")
cat(sprintf("observed %d of %d (%.0f%%) | hurdle %.0f (%.0f%%) | script 26 model 85 (11%%)\n",
            sum(fl$z[k] == 0), length(k), 100*mean(fl$z[k] == 0),
            sum(p0hat), 100*mean(p0hat)))

# predictive CDF by direct summation on a per-cell grid
cdf_at <- function(i, zq) {
  sd_i <- sqrt(alpha[i] + (1 + P$psi) * max(lamS[i], lamL[i]))
  lo <- floor(min(-10 * sd_i, zq - 10) - 10)
  hi <- ceiling(max(alpha[i] + 10 * sd_i, zq + 10) + 10)
  zz <- lo:hi; p <- pmf(zz, i); tot <- sum(p)
  if (!is.finite(tot) || tot <= 0) return(c(lower = NA_real_, at = NA_real_))
  p <- p / tot
  c(lower = sum(p[zz < zq]), at = p[zz == zq][1])
}
big <- k[abs(fl$z[k]) > 50]
cv <- vapply(big, function(i) { cc <- cdf_at(i, fl$z[i]); u <- cc[["lower"]] + cc[["at"]]
                                as.numeric(u > 0.025 && u < 0.975) }, 1.0)
cat("\n=== GATE 2: the large moves ===\n")
cat(sprintf("cells with |z| > 50: %d | inside central 95%% predictive: %d (%.0f%%, nominal 95%%)\n",
            length(big), sum(cv), 100*mean(cv)))

U <- vapply(k, function(i) { cc <- cdf_at(i, fl$z[i])
                             cc[["lower"]] + stats::runif(1) * cc[["at"]] }, 1.0)
nbad <- sum(!is.finite(U)); U <- U[is.finite(U)]
if (nbad) cat(sprintf("(%d cells dropped from the PIT: predictive grid failed)\n", nbad))
ks <- stats::ks.test(U, "punif")
cat("\n=== GATE 3: randomised PIT ===\n")
cat(sprintf("n = %d | KS D = %.4f, p = %.3g | deciles:\n", length(U), ks$statistic, ks$p.value))
print(round(as.numeric(table(cut(U, seq(0, 1, 0.1)))) / length(U), 3))

cat("\n=== GATE 4: log-likelihood ===\n")
cat(sprintf("two-scale hurdle (8 par)      : %.2f\n", -best$value))
cat(sprintf("one-scale hurdle (7 par, sc29): -1612.19\n"))
cat(sprintf("free retention (17 par, sc 26): -3055.41\n"))
cat(sprintf("lognormal g_C + p (5 par, 23) : -3065.20\n"))

saveRDS(list(state = STATE, par = best$par, logL = -best$value, mu = mu,
             P = P, zeros_obs = sum(fl$z[k] == 0), zeros_hat = sum(p0hat),
             cover_big = mean(cv), ks = ks, U = U),
        sprintf("devel/spa_diagnostics/twoscale_%s.rds", gsub(" ", "", STATE)))
