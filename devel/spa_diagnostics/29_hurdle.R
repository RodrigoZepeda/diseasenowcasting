# =============================================================================
# 29 -- STAGE 1 of PLAN_increment_nowcasting.md: the hurdle increment law.
#
# Stage 0 (script 28) changed the specification in three ways:
#
#   * participation is driven by cohort AGE, not by snapshot sparsity: 58% of
#     cohorts move at age 1, 25% at age 2, 2.3% at age 15, and that decay is
#     consistent across all 53 locations (q10-q90 32-85% at age 1, 0-4% at 15).
#     The apparent snapshot concentration of movement MASS is mostly a season
#     effect -- pooled participation only ranges 0.10 to 0.47 across snapshots,
#     and only 1 of 62 snapshots sits above 3x the median.
#   * moves are additive when small and multiplicative when large (CV of the
#     relative change beats the absolute only once the cell's median move
#     exceeds ~25 counts), so the revision scale needs BOTH: sigma0 + sigma1 L.
#   * early positives are arrivals (age 1: 47% up, median +7) and late moves are
#     revisions (age >=8: ~4% up, ~2% down, median down -8). The two components
#     separate by age, so the retention curve b_c is not needed at all -- down
#     moves belong to the revision component.
#
# The law, for a cadence interval (a,b] on event week t:
#
#   alpha_t(a,b) = mu_t [G_D(b) - G_D(a)]                        arrivals
#   pi(b)        = plogis(p0 + p1 log(1+b))                      participation
#   lamR_t(a)    = sigma0 + sigma1 mu_t G_D(a)                   revision scale
#
#   Delta ~  (1 - pi) Skellam(alpha, 0)          = Poisson(alpha)   ordinary
#          +      pi  Skellam(alpha + psi lamR, lamR)              revised
#
# The ordinary component has omega = 0, so an exact zero costs nothing whenever
# arrivals are finished -- which is what the current model cannot do (K.3: 85
# zeros predicted against 620 observed). Dispersion is bought on the pi fraction
# of cells that actually move, not globally. pi = 0 on the first interval, where
# there is nothing yet to revise.
#
# The mixture is a 2-term logsumexp per cell: exact, cheap, and it keeps the
# discrete part out of any Laplace approximation later.
#
# The Skellam pmf here is EXACT (scaled Bessel), not the saddlepoint: verified
# to 1e-13 against direct Poisson convolution across the full range of rates in
# play, at 2.6 ms per 900 cells. Mixture weights depend on relative densities,
# so an unnormalised SPA is not safe here.
#
# Run: NOT_CRAN=true Rscript devel/spa_diagnostics/29_hurdle.R
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
# theta = (dmu, log dsig, p0, p1, log sigma0, log sigma1, log psi)
unpack <- function(theta) list(
  gD = mk_gD(theta[1], exp(theta[2]), H), p0 = theta[3], p1 = theta[4],
  s0 = exp(theta[5]), s1 = exp(theta[6]), psi = exp(theta[7]))

cell_ll <- function(z, mu, aa, bb, post, P) {
  GD <- cumsum(P$gD)
  GDf <- function(k) ifelse(k < 0, 0, GD[pmin(pmax(k, 0), length(GD) - 1L) + 1L])
  alpha <- pmax(mu * (GDf(bb) - GDf(aa)), 1e-10)
  lam   <- P$s0 + P$s1 * mu * GDf(aa)
  pi_   <- ifelse(post, stats::plogis(P$p0 + P$p1 * log1p(bb)), 0)
  l0 <- lpois(z, alpha)
  l1 <- lskel(z, alpha + P$psi * lam, lam)
  out <- numeric(length(z))
  on <- pi_ > 1e-12
  out[!on] <- l0[!on]
  if (any(on)) {
    A <- log1p(-pi_[on]) + l0[on]; B <- log(pi_[on]) + l1[on]
    m <- pmax(A, B); m[!is.finite(m)] <- 0
    out[on] <- m + log(exp(A - m) + exp(B - m))
  }
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
WARM <- c(-3.50, 0.50, 1.92, -1.99, -8, log(1.9501), log(1.021))
starts <- if (nzchar(Sys.getenv("COLD"))) list(
               c(0, log(0.5), -1, -1.0, log(2), log(0.02), 0),
               c(0, log(0.8),  0, -1.5, log(5), log(0.05), 0),
               c(0.5, log(0.4), -2, -0.5, log(1), log(0.01), 0)) else list(WARM)
# SKIPFIT=1 evaluates the objective at WARM and goes straight to the gates.
# WARM was recovered from the converged cold fit, so this reproduces it without
# repeating the 37-minute multi-start search.
CKPT <- sprintf("devel/spa_diagnostics/hurdlefit_%s.rds", gsub(" ", "", STATE))
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
cat(sprintf("lamR   : sigma0 = %.2f (additive floor) + sigma1 = %.4f x level\n", P$s0, P$s1))
cat(sprintf("psi    : %.3f   (1 = symmetric revision, <1 = net downward)\n\n", P$psi))

# ---- GATES ------------------------------------------------------------------
GD <- cumsum(P$gD)
GDf <- function(k) ifelse(k < 0, 0, GD[pmin(pmax(k, 0), length(GD) - 1L) + 1L])
mu_i <- mu[fl$grp]
alpha <- pmax(mu_i * (GDf(fl$b) - GDf(fl$a)), 1e-10)
lam   <- P$s0 + P$s1 * mu_i * GDf(fl$a)
pi_   <- ifelse(fl$post, stats::plogis(P$p0 + P$p1 * log1p(fl$b)), 0)
pmf <- function(z, i) {                # z may be a vector; i is a single cell
  if (pi_[i] <= 1e-12) return(exp(lpois(z, alpha[i])))
  A <- log1p(-pi_[i]) + lpois(z, alpha[i])
  B <- log(pi_[i]) + lskel(z, alpha[i] + P$psi * lam[i], lam[i])
  m <- pmax(A, B); m[!is.finite(m)] <- 0
  exp(m + log(exp(A - m) + exp(B - m)))
}
k <- which(fl$post)
p0hat <- vapply(k, function(i) pmf(0, i), 1.0)
cat("=== GATE 1: exact zeros ===\n")
cat(sprintf("observed %d of %d (%.0f%%) | hurdle %.0f (%.0f%%) | script 26 model 85 (11%%)\n",
            sum(fl$z[k] == 0), length(k), 100*mean(fl$z[k] == 0),
            sum(p0hat), 100*mean(p0hat)))

# predictive CDF by direct summation on a per-cell grid
cdf_at <- function(i, zq) {
  sd_i <- sqrt(alpha[i] + (1 + P$psi) * lam[i])
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
cat(sprintf("hurdle (7 par + mu_t)         : %.2f\n", -best$value))
cat(sprintf("free retention (17 par, sc 26): -3055.41\n"))
cat(sprintf("lognormal g_C + p (5 par, 23) : -3065.20\n"))

saveRDS(list(state = STATE, par = best$par, logL = -best$value, mu = mu, P = P,
             zeros_obs = sum(fl$z[k] == 0), zeros_hat = sum(p0hat),
             cover_big = mean(cv), ks = ks, U = U),
        sprintf("devel/spa_diagnostics/hurdle_%s.rds", gsub(" ", "", STATE)))
