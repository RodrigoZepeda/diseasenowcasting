QL <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
HMAX <- 4L; NDRAW <- 2000L
set.seed(20260903)

# exact Skellam log-pmf (verified in script 32: both signs, a != b, sums to 1)
lsf <- function(x) stats::pnorm(x, lower.tail = FALSE, log.p = TRUE)
KSER <- 40L
log_besselI <- function(nu, x) {
  n <- max(length(nu), length(x)); nu <- rep_len(nu, n); x <- rep_len(x, n)
  out <- numeric(n); ser <- nu > x
  if (any(!ser)) out[!ser] <- log(besselI(x[!ser], nu[!ser], expon.scaled = TRUE)) + x[!ser]
  if (any(ser)) {
    nn <- nu[ser]; xx <- pmax(x[ser], 1e-300); ly <- 2 * log(xx / 2)
    lt <- outer(rep(1, length(nn)), 0:KSER) * ly -
          rep(lgamma(seq_len(KSER + 1L)), each = length(nn)) -
          (lgamma(outer(nn, 0:KSER, "+") + 1) - lgamma(nn + 1))
    m <- do.call(pmax, as.data.frame(lt))
    out[ser] <- nn * log(xx / 2) - lgamma(nn + 1) + m + log(rowSums(exp(lt - m)))
  }
  out
}
lskel <- function(z, a, b) {
  both <- a > 1e-12 & b > 1e-12
  out <- numeric(length(z))
  if (any(!both)) { i <- !both
    out[i] <- ifelse(b[i] <= 1e-12, ifelse(z[i] < 0, -Inf, stats::dpois(pmax(z[i],0), a[i]+1e-10, log=TRUE)),
                                    ifelse(z[i] > 0, -Inf, stats::dpois(pmax(-z[i],0), b[i]+1e-10, log=TRUE))) }
  if (any(both)) { i <- both; x <- 2*sqrt(a[i]*b[i])
    out[i] <- -(a[i]+b[i]) + (z[i]/2)*log(a[i]/b[i]) + log_besselI(abs(z[i]), x) }
  out
}
mk_gD <- function(dmu, dsig, cD) {
  f <- diseasenowcasting:::.delay_distribution_functions(1L, dmu, 0.01 + dsig)
  acdf <- as.numeric(f$cdf(seq_len(cD + 1L))); c(acdf[1], diff(acdf))
}
# hR is 0-indexed: hR[l+1] = h_R(l), hR[1] = 0 (no same-delay withdrawal)
mk_hR <- function(par, H) {
  tot <- stats::plogis(par[1]); w <- exp(c(0, par[-1])); w <- w / sum(w)
  c(0, tot * w)
}
# qp, qm for the observed (a,b] pairs; S_R = 1 - cumsum(hR)
q_pairs <- function(gD, hR, a, b) {
  GD <- cumsum(gD); HR <- cumsum(hR); nD <- length(GD); nH <- length(HR)
  SR <- function(k) { out <- numeric(length(k)); ok <- k >= 0
                      out[ok] <- 1 - HR[pmin(k[ok], nH - 1L) + 1L]; out }
  qp <- qm <- numeric(length(a))
  for (r in 0:min(nD - 1L, max(b))) {
    g <- gD[r + 1L]; if (!is.finite(g) || g < 1e-14) next
    inP <- b >= r & a < r                       # arrived in (a,b]
    if (any(inP)) qp[inP] <- qp[inP] + g * SR(b[inP] - r)
    inM <- a >= r                               # arrived by a
    if (any(inM)) qm[inM] <- qm[inM] + g * (SR(a[inM] - r) - SR(b[inM] - r))
  }
  list(qp = pmax(qp, 0), qm = pmax(qm, 0))
}
build_fl <- function(rows) {
  obs <- list()
  for (E in unique(rows$target_end_date)) {
    x <- rows |> filter(target_end_date == E, d <= 15L, d >= 0) |> arrange(d)
    if (!nrow(x)) next
    x <- x[!duplicated(x$d), ]
    zz <- as.numeric(x$observation - c(0, head(x$observation,-1L)))
    pm <- c(0, 0, as.numeric(zz[-c(1, length(zz))] != 0))[seq_along(zz)]
    obs[[length(obs)+1L]] <- list(a = as.integer(c(-1L, head(x$d,-1L))), b = as.integer(x$d),
      z = zz, pm = pm)
  }
  fl <- list(a = unlist(lapply(obs,`[[`,"a")), b = unlist(lapply(obs,`[[`,"b")),
             z = unlist(lapply(obs,`[[`,"z")), pm = unlist(lapply(obs,`[[`,"pm")),
             grp = rep(seq_along(obs), vapply(obs, function(o) length(o$z), 1L)),
             start = vapply(obs, function(o) log(max(sum(pmax(o$z,0)),1)), 1.0),
             nTg = length(obs))
  fl$nObs <- length(fl$z); fl$post <- fl$a >= 0; fl
}
GCOARSE <- seq(-6, 6, length.out = 13L); GFINE <- seq(-0.5, 0.5, length.out = 9L)

# zero-truncated NB on {1,2,...}
lmag <- function(k, m, size) {
  lp0 <- stats::dnbinom(0, size = size, mu = m, log = TRUE)
  pmin(stats::dnbinom(k, size = size, mu = m, log = TRUE) - log1p(-exp(pmin(lp0, -1e-12))), 0)
}
Smag_nb <- function(x, m, size) {
  lp0 <- stats::dnbinom(0, size = size, mu = m, log = TRUE)
  stats::pnbinom(x, size = size, mu = m, lower.tail = FALSE) / (1 - exp(pmin(lp0, -1e-12)))
}
NH <- 15L                      # free retraction masses h_R(1..15)
# theta = (dmu, log dsig, h0, v1..v14, p0, p1, gam, log size)   -> 2+15+3+1 = 21
unpack <- function(th, H) {
  hp <- th[3:(2 + NH)]
  list(gD = mk_gD(th[1], exp(th[2]), H), hR = mk_hR(hp, H),
       c0 = th[NH + 3], c1 = th[NH + 4], c2 = th[NH + 5], size = exp(th[NH + 6]))
}
log_m  <- function(P, age, pm) P$c0 + P$c1 * log1p(age) + P$c2 * pm
Psi    <- function(m, s) m / (1 - stats::dnbinom(0, size = s, mu = m))
# nu = E[M] in closed form; pi derived so that E[Delta] = alpha - omega exactly
derive <- function(P, age, pm, tot) {
  m  <- pmin(pmax(exp(log_m(P, age, pm)), 1e-10), 1e8)
  nu <- Psi(m, P$size)
  list(m = m, nu = nu, pi = pmin(tot / pmax(nu, 1 + 1e-12), 1))
}

cell_ll <- function(z, mu, aa, bb, post, pm, P, Q) {
  al <- pmax(mu * Q$qp, 1e-10); om <- pmax(mu * Q$qm, 1e-10)
  out <- numeric(length(z)); first <- !post
  if (any(first)) out[first] <- ifelse(z[first] < 0, -Inf,
    stats::dpois(pmax(z[first], 0), al[first], log = TRUE))
  if (any(post)) {
    i <- which(post); zz <- z[i]
    tot <- al[i] + om[i]
    th_ <- al[i] / tot                                   # STRUCTURAL sign
    D <- derive(P, bb[i], pm[i], tot)                    # m parent, nu = E[M], pi derived
    pi_ <- D$pi
    v <- numeric(length(zz))
    zero <- zz == 0; up <- zz > 0; dn <- zz < 0
    v[zero] <- log(pmax(1 - pi_[zero], 1e-300))
    if (any(up)) v[up] <- log(pmax(pi_[up], 1e-300)) + log(pmax(th_[up], 1e-12)) +
      lmag(zz[up], D$m[up], P$size)
    if (any(dn)) v[dn] <- log(pmax(pi_[dn], 1e-300)) + log(pmax(1 - th_[dn], 1e-12)) +
      lmag(-zz[dn], D$m[dn], P$size)
    out[i] <- v
  }
  out
}
make_env <- function(fl) {
  H <- max(fl$b)
  gridM <- function(P, Q, centre, off) {
    G <- length(off); nO <- fl$nObs; nT <- fl$nTg
    ec <- rep.int(seq_len(nO), G); gc <- rep(seq_len(G), each = nO)
    mu <- exp(centre[fl$grp[ec]] + off[gc])
    v <- cell_ll(fl$z[ec], mu, fl$a[ec], fl$b[ec], fl$post[ec], fl$pm[ec], P,
                 list(qp = Q$qp[ec], qm = Q$qm[ec]))
    v[!is.finite(v)] <- NA_real_
    agg <- rowsum(v, fl$grp[ec] + (gc - 1L) * nT, na.rm = FALSE)
    Sv <- rep(NA_real_, nT * G); Sv[as.integer(rownames(agg))] <- agg
    matrix(Sv, nT, G)
  }
  amax <- function(M) { Mf <- M; Mf[is.na(Mf)] <- -Inf
    jm <- max.col(Mf, ties.method = "first")
    jm[!is.finite(Mf[cbind(seq_len(nrow(M)), jm)])] <- 0L; jm }
  prof <- function(th) {
    P <- unpack(th, H)
    if (any(!is.finite(P$gD)) || any(!is.finite(P$hR))) return(NULL)
    Q <- q_pairs(P$gD, P$hR, fl$a, fl$b)
    M1 <- gridM(P, Q, fl$start, GCOARSE); j1 <- amax(M1); if (any(j1 == 0L)) return(NULL)
    M <- gridM(P, Q, fl$start + GCOARSE[j1], GFINE); jm <- amax(M); if (any(jm == 0L)) return(NULL)
    list(M = M, jm = jm, mu = exp(fl$start + GCOARSE[j1] + GFINE[jm]), P = P, Q = Q)
  }
  list(H = H, prof = prof, nll = function(th) {
    if (any(!is.finite(th)) || th[2] > 3 || th[NH + 6] > 4 || th[NH + 6] < -6) return(1e12)
    p <- prof(th); if (is.null(p)) return(1e12)
    G <- ncol(p$M); tot <- 0
    for (t in seq_len(fl$nTg)) {
      j <- p$jm[t]; y2 <- p$M[t, j]
      if (j > 1L && j < G) { y1 <- p$M[t, j-1L]; y3 <- p$M[t, j+1L]
        if (is.finite(y1) && is.finite(y3)) { den <- y1 - 2*y2 + y3
          if (den < 0) { corr <- -0.125*(y1-y3)^2/den
            if (is.finite(corr) && corr <= 1) { tot <- tot + y2 + corr; next } } } }
      tot <- tot + y2
    }
    if (is.finite(tot) && tot <= 0) -tot else 1e12 })
}
fit_m <- function(fl, start) {
  E <- make_env(fl)
  ft <- stats::optim(start, E$nll, method = "BFGS", control = list(maxit = 150, reltol = 1e-10))
  list(par = ft$par, logL = -ft$value, env = E)
}
wis <- function(qs, y) 2 * mean(ifelse(y >= qs, QL * (y - qs), (1 - QL) * (qs - y)))

