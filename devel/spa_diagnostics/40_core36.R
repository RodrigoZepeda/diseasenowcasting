mk_gD <- function(dmu, dsig, cD) {
  f <- diseasenowcasting:::.delay_distribution_functions(1L, dmu, 0.01 + dsig)
  acdf <- as.numeric(f$cdf(seq_len(cD + 1L))); c(acdf[1], diff(acdf))
}
lpois <- function(z, a) ifelse(z < 0, -Inf, stats::dpois(pmax(z, 0), a, log = TRUE))


# log P(M = k) for a ZERO-TRUNCATED negative binomial on {1,2,...}.
# The normaliser 1 - NB(0) depends on (size, mu) but NOT on any discretisation
# boundary, so unlike the discretised lognormal it cannot silently absorb a
# level-dependent share of the mass.
lmag <- function(k, m, size) {
  lp0 <- stats::dnbinom(0, size = size, mu = m, log = TRUE)
  pmin(stats::dnbinom(k, size = size, mu = m, log = TRUE) - log1p(-exp(pmin(lp0, -1e-12))), 0)
}
# P(M > x) under the same law
Smag_nb <- function(x, m, size) {
  lp0 <- stats::dnbinom(0, size = size, mu = m, log = TRUE)
  stats::pnbinom(x, size = size, mu = m, lower.tail = FALSE) / (1 - exp(pmin(lp0, -1e-12)))
}

QL <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
HMAX <- 4L; NDRAW <- 2000L

# theta = (dmu, log dsig, p0, p1, gamma, t0, t1, log kappa, beta, log s)
unpack <- function(th, H) list(gD = mk_gD(th[1], exp(th[2]), H),
  p0 = th[3], p1 = th[4], gam = th[5], t0 = th[6], t1 = th[7],
  lkap = th[8], beta = th[9], size = exp(th[10]))

# ---- interval observations, now carrying the previous move indicator --------
build_fl <- function(rows) {
  obs <- list()
  for (E in unique(rows$target_end_date)) {
    x <- rows |> dplyr::filter(target_end_date == E, d <= 15L, d >= 0) |> dplyr::arrange(d)
    if (!nrow(x)) next
    x <- x[!duplicated(x$d), ]
    z <- as.numeric(x$observation - c(0, head(x$observation, -1L)))
    # pm[i] = did the PREVIOUS increment move?  cell 1 is the arrival interval;
    # cell 2 has no comparable history, so it takes pm = 0.
    pm <- c(0, 0, as.numeric(z[-c(1, length(z))] != 0))[seq_along(z)]
    obs[[length(obs) + 1L]] <- list(a = as.integer(c(-1L, head(x$d, -1L))),
                                    b = as.integer(x$d), z = z, pm = pm)
  }
  fl <- list(a = unlist(lapply(obs, `[[`, "a")), b = unlist(lapply(obs, `[[`, "b")),
             z = unlist(lapply(obs, `[[`, "z")), pm = unlist(lapply(obs, `[[`, "pm")),
             grp = rep(seq_along(obs), vapply(obs, function(o) length(o$z), 1L)),
             start = vapply(obs, function(o) log(max(sum(pmax(o$z, 0)), 1)), 1.0),
             nTg = length(obs))
  fl$nObs <- length(fl$z); fl$post <- fl$a >= 0; fl
}
GCOARSE <- seq(-6, 6, length.out = 13L); GFINE <- seq(-0.5, 0.5, length.out = 9L)
make_nll <- function(fl) {
  H <- max(fl$b)
  cell_ll <- function(z, mu, aa, bb, post, pm, P) {
    GD <- cumsum(P$gD)
    GDf <- function(k) ifelse(k < 0, 0, GD[pmin(pmax(k, 0), length(GD) - 1L) + 1L])
    out <- numeric(length(z)); first <- !post
    if (any(first)) out[first] <- lpois(z[first], pmax(mu[first] * GDf(bb[first]), 1e-10))
    if (any(post)) {
      zz <- z[post]; lev <- pmax(mu[post] * GDf(aa[post]), 1)
      lin_p <- P$p0 + P$p1 * log1p(bb[post]) + P$gam * pm[post]
      lin_t <- P$t0 + P$t1 * log1p(bb[post])
      m <- exp(P$lkap + P$beta * log(lev)); v <- numeric(length(zz))
      zero <- zz == 0; up <- zz > 0; dn <- zz < 0
      v[zero] <- stats::plogis(-lin_p[zero], log.p = TRUE)
      if (any(up)) v[up] <- stats::plogis(lin_p[up], log.p = TRUE) +
        stats::plogis(lin_t[up], log.p = TRUE) + lmag(zz[up], m[up], P$size)
      if (any(dn)) v[dn] <- stats::plogis(lin_p[dn], log.p = TRUE) +
        stats::plogis(-lin_t[dn], log.p = TRUE) + lmag(-zz[dn], m[dn], P$size)
      out[post] <- v
    }
    out
  }
  gridM <- function(P, centre, off) {
    G <- length(off); nO <- fl$nObs; nT <- fl$nTg
    ec <- rep.int(seq_len(nO), G); gc <- rep(seq_len(G), each = nO)
    v <- cell_ll(fl$z[ec], exp(centre[fl$grp[ec]] + off[gc]), fl$a[ec], fl$b[ec],
                 fl$post[ec], fl$pm[ec], P)
    v[!is.finite(v)] <- NA_real_
    agg <- rowsum(v, fl$grp[ec] + (gc - 1L) * nT, na.rm = FALSE)
    Sv <- rep(NA_real_, nT * G); Sv[as.integer(rownames(agg))] <- agg
    matrix(Sv, nT, G)
  }
  amax <- function(M) { Mf <- M; Mf[is.na(Mf)] <- -Inf
    jm <- max.col(Mf, ties.method = "first")
    jm[!is.finite(Mf[cbind(seq_len(nrow(M)), jm)])] <- 0L; jm }
  list(H = H, cell_ll = cell_ll, gridM = gridM, amax = amax,
       nll = function(th) {
    if (any(!is.finite(th)) || th[2] > 3 || th[10] > 4 || th[10] < -6) return(1e12)
    P <- unpack(th, H); if (any(!is.finite(P$gD))) return(1e12)
    M1 <- gridM(P, fl$start, GCOARSE); j1 <- amax(M1); if (any(j1 == 0L)) return(1e12)
    M <- gridM(P, fl$start + GCOARSE[j1], GFINE); jm <- amax(M); if (any(jm == 0L)) return(1e12)
    G <- ncol(M); tot <- 0
    for (t in seq_len(fl$nTg)) {
      j <- jm[t]; y2 <- M[t, j]
      if (j > 1L && j < G) { y1 <- M[t, j-1L]; y3 <- M[t, j+1L]
        if (is.finite(y1) && is.finite(y3)) { den <- y1 - 2*y2 + y3
          if (den < 0) { corr <- -0.125 * (y1 - y3)^2 / den      # >= 0
            # The |vertex| <= 1 guard is NOT enough: with wildly asymmetric grid
            # values den and (y1-y3) are both astronomical and the "correction"
            # reached +1e151.  A refinement over a grid of step 0.125 in log mu
            # cannot be worth more than a fraction of a nat, so cap it.
            if (is.finite(corr) && corr <= 1) { tot <- tot + y2 + corr; next } } } }
      tot <- tot + y2
    }
    # A sum of log-probabilities cannot be positive.  This assertion would have
    # caught the parabolic explosion AND the lmag underflow immediately.
    if (is.finite(tot) && tot <= 0) -tot else 1e12 },
       profile = function(P) {
    M1 <- gridM(P, fl$start, GCOARSE); j1 <- amax(M1); if (any(j1 == 0L)) return(NULL)
    M <- gridM(P, fl$start + GCOARSE[j1], GFINE); jm <- amax(M); if (any(jm == 0L)) return(NULL)
    exp(fl$start + GCOARSE[j1] + GFINE[jm]) })
}
fit_one <- function(fl, start) {
  E <- make_nll(fl)
  ft <- stats::optim(start, E$nll, method = "BFGS", control = list(maxit = 200, reltol = 1e-11))
  ft <- stats::optim(ft$par, E$nll, method = "Nelder-Mead", control = list(maxit = 900, reltol = 1e-12))
  list(par = ft$par, logL = -ft$value, H = E$H, env = E)
}
# forward simulation now carries the move STATE
sim_forward <- function(C0, state0, ages, P, n) {
  cur <- rep(as.numeric(C0), n); st <- rep(as.numeric(state0), n)
  for (ag in ages) {
    pi_ <- stats::plogis(P$p0 + P$p1 * log1p(ag) + P$gam * st)
    th_ <- stats::plogis(P$t0 + P$t1 * log1p(ag))
    mv <- stats::runif(n) < pi_
    if (any(mv)) {
      m <- exp(P$lkap + P$beta * log(pmax(cur[mv], 1)))
      p0 <- stats::dnbinom(0, size = P$size, mu = m)
      M <- stats::qnbinom(stats::runif(sum(mv), p0, 1), size = P$size, mu = m)
      sgn <- ifelse(stats::runif(sum(mv)) < th_, 1, -1)
      cur[mv] <- pmax(cur[mv] + sgn * M, 0)
    }
    st <- as.numeric(mv)
  }
  cur
}
wis <- function(qs, y) 2 * mean(ifelse(y >= qs, QL * (y - qs), (1 - QL) * (qs - y)))

