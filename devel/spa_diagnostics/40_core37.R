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
    obs[[length(obs)+1L]] <- list(a = as.integer(c(-1L, head(x$d,-1L))), b = as.integer(x$d),
      z = as.numeric(x$observation - c(0, head(x$observation,-1L))))
  }
  fl <- list(a = unlist(lapply(obs,`[[`,"a")), b = unlist(lapply(obs,`[[`,"b")),
             z = unlist(lapply(obs,`[[`,"z")),
             grp = rep(seq_along(obs), vapply(obs, function(o) length(o$z), 1L)),
             start = vapply(obs, function(o) log(max(sum(pmax(o$z,0)),1)), 1.0),
             nTg = length(obs))
  fl$nObs <- length(fl$z); fl
}
GCOARSE <- seq(-6, 6, length.out = 13L); GFINE <- seq(-0.5, 0.5, length.out = 9L)
make_env <- function(fl) {
  H <- max(fl$b)
  gridM <- function(Q, centre, off) {
    G <- length(off); nO <- fl$nObs; nT <- fl$nTg
    ec <- rep.int(seq_len(nO), G); gc <- rep(seq_len(G), each = nO)
    mu <- exp(centre[fl$grp[ec]] + off[gc])
    v <- lskel(fl$z[ec], mu * Q$qp[ec], mu * Q$qm[ec]); v[!is.finite(v)] <- NA_real_
    agg <- rowsum(v, fl$grp[ec] + (gc - 1L) * nT, na.rm = FALSE)
    Sv <- rep(NA_real_, nT * G); Sv[as.integer(rownames(agg))] <- agg
    matrix(Sv, nT, G)
  }
  amax <- function(M) { Mf <- M; Mf[is.na(Mf)] <- -Inf
    jm <- max.col(Mf, ties.method = "first")
    jm[!is.finite(Mf[cbind(seq_len(nrow(M)), jm)])] <- 0L; jm }
  prof <- function(th) {
    gD <- mk_gD(th[1], exp(th[2]), H); hR <- mk_hR(th[-(1:2)], H)
    if (any(!is.finite(gD)) || any(!is.finite(hR))) return(NULL)
    Q <- q_pairs(gD, hR, fl$a, fl$b)
    M1 <- gridM(Q, fl$start, GCOARSE); j1 <- amax(M1); if (any(j1 == 0L)) return(NULL)
    M <- gridM(Q, fl$start + GCOARSE[j1], GFINE); jm <- amax(M); if (any(jm == 0L)) return(NULL)
    list(M = M, jm = jm, mu = exp(fl$start + GCOARSE[j1] + GFINE[jm]), gD = gD, hR = hR)
  }
  list(H = H, prof = prof, nll = function(th) {
    if (any(!is.finite(th)) || th[2] > 3) return(1e12)
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
fit_tex <- function(fl, start) {
  E <- make_env(fl)
  ft <- stats::optim(start, E$nll, method = "BFGS", control = list(maxit = 120, reltol = 1e-10))
  list(par = ft$par, logL = -ft$value, env = E)
}
wis <- function(qs, y) 2 * mean(ifelse(y >= qs, QL * (y - qs), (1 - QL) * (qs - y)))
