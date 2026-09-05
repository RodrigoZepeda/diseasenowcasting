# =============================================================================
# 38 -- THE ZTNB VERSION OF THE p-FREE MODEL: h_R primitive, hurdle increments.
#
# FINDINGS T showed the two problems are orthogonal.  The tex model
# (main_identifiability_update.tex) fixes IDENTIFIABILITY -- h_R(l) primitive,
# no p, exactly section J's theorem -- but forecasts at -29.4% against a ratio
# lookup, because its Poisson-Skellam increments cannot put mass on the 77% of
# increments that are exactly zero (90% coverage 0.979).  The sign x magnitude
# law (script 36) fixes FORECASTING (+10.6%) but is a descriptive law with no
# retention structure at all.
#
# This script takes both.  The trick is to preserve the tex model's MEAN exactly
# while redistributing its mass.  With alpha = mu_t q_+(a,b), omega = mu_t q_-(a,b)
# from the tex model, put
#
#     theta = alpha / (alpha + omega)                  sign, STRUCTURAL
#     E[M]  = (alpha + omega) / pi                     magnitude mean, STRUCTURAL
#     Delta = 0  w.p. 1 - pi ;  +M w.p. pi*theta ;  -M w.p. pi*(1-theta)
#
# Then
#     E[Delta] = pi E[M] (2 theta - 1) = (alpha+omega) * (alpha-omega)/(alpha+omega)
#              = alpha - omega
# for ANY pi.  So the mean is the tex model's mu_t q_C(d) untouched -- the whole
# identifiability argument carries over verbatim -- while pi is free to absorb
# the zeros and the NB size is free to absorb the dispersion.  Nothing here
# estimates p; the primitive is still h_R.
#
# M is zero-truncated NB on {1,2,...} (FINDINGS R: it beat the discretised
# lognormal by 9.4 nats and does not lean on a level-dependent renormalisation).
# pi carries the age term and the Markov move state of script 34.
#
# One thing to watch in the fit: with reporting complete at first publication,
# q_+ at post-first ages is near zero, so theta -> 0 and the model would predict
# almost all DOWN moves -- but 78% of observed moves are UP.  The model can only
# reconcile that by giving g_D genuine late mass.  In the old Poisson-Skellam
# that was impossible, because late g_D mass destroyed the zeros; with the hurdle
# absorbing the zeros, g_D is free to carry it.  Whether it does is a real test.
#
# First interval stays Poisson(mu_t q_C(b)) and identifies mu_t.
#
# Run: NORIG=12 NOT_CRAN=true Rscript devel/spa_diagnostics/38_tex_ztnb.R
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})
STATE <- Sys.getenv("STATE", "Texas"); NORIG <- as.integer(Sys.getenv("NORIG", "16"))
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
       p0 = th[NH + 3], p1 = th[NH + 4], gam = th[NH + 5], size = exp(th[NH + 6]))
}
lin_pi <- function(P, age, pm) P$p0 + P$p1 * log1p(age) + P$gam * pm

cell_ll <- function(z, mu, aa, bb, post, pm, P, Q) {
  al <- pmax(mu * Q$qp, 1e-10); om <- pmax(mu * Q$qm, 1e-10)
  out <- numeric(length(z)); first <- !post
  if (any(first)) out[first] <- ifelse(z[first] < 0, -Inf,
    stats::dpois(pmax(z[first], 0), al[first], log = TRUE))
  if (any(post)) {
    i <- which(post); zz <- z[i]
    tot <- al[i] + om[i]
    th_ <- al[i] / tot                                   # STRUCTURAL sign
    lp <- lin_pi(P, bb[i], pm[i])
    pi_ <- stats::plogis(lp)
    mM <- pmax(tot / pmax(pi_, 1e-8), 1e-8)              # STRUCTURAL magnitude mean
    v <- numeric(length(zz))
    zero <- zz == 0; up <- zz > 0; dn <- zz < 0
    v[zero] <- stats::plogis(-lp[zero], log.p = TRUE)
    if (any(up)) v[up] <- stats::plogis(lp[up], log.p = TRUE) + log(pmax(th_[up], 1e-12)) +
      lmag(zz[up], mM[up], P$size)
    if (any(dn)) v[dn] <- stats::plogis(lp[dn], log.p = TRUE) + log(pmax(1 - th_[dn], 1e-12)) +
      lmag(-zz[dn], mM[dn], P$size)
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

# =============================================================================
# PART A -- in-sample fit and the stage-1 gates
# =============================================================================
STARTD <- as.Date("2023-09-23")
raw_all <- tbl.now::flusight |> filter(location_name == STATE, !is.na(observation)) |>
  mutate(d = floor(as.numeric(as_of - target_end_date) / 7)) |> arrange(as_of, target_end_date)
fl <- build_fl(raw_all |> filter(target_end_date >= STARTD))
cat(sprintf("=== %s | w15 | %d event weeks, %d intervals (%d post-first) ===\n\n",
            STATE, fl$nTg, fl$nObs, sum(fl$post)))
S0 <- c(-1, log(0.5), stats::qlogis(0.5), rep(0, 14), 2.2, -2.0, 0.6, log(0.2))
starts <- list(S0,
  c(-0.3, log(0.8), stats::qlogis(0.4), rep(0, 14), 2.0, -1.8, 0.6, log(0.1)),
  c(-2.0, log(0.4), stats::qlogis(0.6), rep(0, 14), 2.4, -2.2, 0.8, log(0.4)))
best <- NULL
for (st in starts) { ft <- fit_m(fl, st); cat(sprintf("start: logL = %.2f\n", ft$logL))
  if (is.null(best) || ft$logL > best$logL) best <- ft }
pr <- best$env$prof(best$par); P <- pr$P; mu <- pr$mu
cat(sprintf("\nlogL = %.2f\n", best$logL))
cat(sprintf("  (tex Poisson-Skellam, sc37 same data: -3173.2 | ZTNB descriptive, sc36: -1496.34)\n"))
cat(sprintf("S_R(15) = %.3f   (tex model sc37: 0.474-0.483 | free retention sc26: 0.467)\n", 1 - sum(P$hR)))
cat(sprintf("g_D: P(0) = %.3f, P(<=1) = %.3f, mean = %.2f wk   <- does it take LATE mass?\n",
            P$gD[1], sum(P$gD[1:2]), sum((seq_along(P$gD)-1) * P$gD)))
cat(sprintf("pi(a,quiet): %.3f at 1, %.3f at 4, %.3f at 15 | gamma %+.3f | NB size %.3f\n",
  stats::plogis(lin_pi(P,1,0)), stats::plogis(lin_pi(P,4,0)), stats::plogis(lin_pi(P,15,0)),
  P$gam, P$size))
Qf <- q_pairs(P$gD, P$hR, fl$a, fl$b)
mu_i <- mu[fl$grp]; al <- pmax(mu_i*Qf$qp, 1e-10); om <- pmax(mu_i*Qf$qm, 1e-10)
th_i <- al/(al+om); pi_i <- stats::plogis(lin_pi(P, fl$b, fl$pm))
mM <- pmax((al+om)/pmax(pi_i,1e-8), 1e-8); k <- which(fl$post)
cat(sprintf("structural P(move is UP), mean over post-first cells: %.3f  (observed 0.78)\n",
            mean(th_i[k])))
cat("\n=== GATE 1: exact zeros ===\n")
cat(sprintf("observed %d of %d (%.0f%%) | model %.0f (%.0f%%) | tex Poisson-Skellam ~118 (15%%)\n",
    sum(fl$z[k]==0), length(k), 100*mean(fl$z[k]==0), sum(1-pi_i[k]), 100*mean(1-pi_i[k])))
cat("\n=== GATE 2: the tail ===\n")
for (thr in c(20,50,100,300)) { e <- sum(pi_i[k]*Smag_nb(thr, mM[k], P$size))
  cat(sprintf("  |z| > %3d : observed %3d   expected %6.1f   ratio %.2f\n",
      thr, sum(abs(fl$z[k])>thr), e, sum(abs(fl$z[k])>thr)/max(e,1e-9))) }
cdf_lower <- function(i, z) { if (z > 0) return(1 - pi_i[i]*th_i[i]*Smag_nb(z-1, mM[i], P$size))
  if (z == 0) return(pi_i[i]*(1-th_i[i]))
  pi_i[i]*(1-th_i[i])*Smag_nb(-z, mM[i], P$size) }
pmf1 <- function(i, z) { if (z == 0) return(1-pi_i[i])
  if (z > 0) return(pi_i[i]*th_i[i]*exp(lmag(z, mM[i], P$size)))
  pi_i[i]*(1-th_i[i])*exp(lmag(-z, mM[i], P$size)) }
U <- vapply(k, function(i) cdf_lower(i, fl$z[i]) + stats::runif(1)*pmf1(i, fl$z[i]), 1.0)
U <- U[is.finite(U)]; ks <- stats::ks.test(U, "punif")
cat(sprintf("\n=== GATE 3: PIT ===\nn = %d | KS D = %.4f, p = %.3g | outside central 95%%: %.1f%%\n",
            length(U), ks$statistic, ks$p.value, 100*mean(U<0.025|U>0.975)))
print(round(as.numeric(table(cut(U, seq(0,1,0.1))))/length(U), 3))

# =============================================================================
# PART B -- rolling-origin backtest, matched to scripts 36/37
# =============================================================================
snaps <- sort(unique(raw_all$as_of))
origins <- utils::head(utils::tail(snaps, NORIG + HMAX), NORIG)
cat(sprintf("\n\n=== PART B: %d origins ===\n", length(origins)))
sim_fwd <- function(C0, st0, mu_t, ages, P, n) {
  cur <- rep(as.numeric(C0), n); st <- rep(as.numeric(st0), n)
  for (g in ages) {
    Q <- q_pairs(P$gD, P$hR, g - 1L, g)
    a_ <- max(mu_t*Q$qp, 1e-10); o_ <- max(mu_t*Q$qm, 1e-10)
    th_ <- a_/(a_+o_)
    pi_ <- stats::plogis(lin_pi(P, g, st))
    mM_ <- pmax((a_+o_)/pmax(pi_,1e-8), 1e-8)
    mv <- stats::runif(n) < pi_
    if (any(mv)) {
      p0 <- stats::dnbinom(0, size = P$size, mu = mM_[mv])
      M <- stats::qnbinom(stats::runif(sum(mv), p0, 1), size = P$size, mu = mM_[mv])
      sg <- ifelse(stats::runif(sum(mv)) < th_, 1, -1)
      cur[mv] <- pmax(cur[mv] + sg*M, 0)
    }
    st <- as.numeric(mv)
  }
  cur
}
rows <- list(); warm <- best$par
for (s0 in origins) {
  tr <- raw_all |> filter(as_of <= s0); flb <- build_fl(tr)
  ftb <- fit_m(flb, warm); warm <- ftb$par
  prb <- ftb$env$prof(ftb$par); Pb <- prb$P; mub <- prb$mu
  ev <- unique((tr |> filter(d <= 15, d >= 0))$target_end_date)
  cur <- raw_all |> filter(as_of == s0, d >= 0, d <= 15) |>
    select(target_end_date, a0 = d, C0 = observation) |> filter(C0 > 0)
  st0 <- raw_all |> filter(as_of <= s0, d >= 0, d <= 15) |> group_by(target_end_date) |>
    arrange(d, .by_group = TRUE) |>
    summarise(st = if (n() >= 3) as.numeric(tail(diff(observation),1) != 0) else 0, .groups="drop")
  cur <- cur |> left_join(st0, by = "target_end_date") |> mutate(st = ifelse(is.na(st), 0, st))
  fut <- snaps[snaps > s0][seq_len(HMAX)]
  for (h in seq_along(fut)) {
    s_star <- fut[h]
    tgt <- raw_all |> filter(as_of == s_star) |> select(target_end_date, truth = observation)
    jj <- cur |> inner_join(tgt, by = "target_end_date") |>
      mutate(a_star = as.integer(floor(as.numeric(s_star - target_end_date)/7))) |>
      filter(a_star <= 15, a_star > a0)
    if (!nrow(jj)) next
    for (r in seq_len(nrow(jj))) {
      kk <- match(jj$target_end_date[r], ev); if (is.na(kk)) next
      dr <- sim_fwd(jj$C0[r], jj$st[r], mub[kk], (jj$a0[r]+1L):jj$a_star[r], Pb, NDRAW)
      q <- as.numeric(stats::quantile(dr, QL, names = FALSE))
      spans <- tr |> group_by(target_end_date) |> arrange(d, .by_group = TRUE) |>
        summarise(rr = { i <- which(d == jj$a0[r]); j <- which(d == jj$a_star[r])
          if (length(i) && length(j) && observation[i] > 0) observation[j]/observation[i] else NA_real_ },
          .groups="drop") |> pull(rr)
      spans <- spans[is.finite(spans)]
      q_em <- if (length(spans) >= 5) as.numeric(stats::quantile(jj$C0[r]*spans, QL, names=FALSE))
              else rep(jj$C0[r], length(QL))
      rows[[length(rows)+1L]] <- data.frame(origin = s0, h = h, a0 = jj$a0[r],
        a_star = jj$a_star[r], truth = jj$truth[r],
        wis_hyb = wis(q, jj$truth[r]), wis_emp = wis(q_em, jj$truth[r]),
        wis_persist = wis(rep(jj$C0[r], length(QL)), jj$truth[r]),
        cov50 = as.numeric(jj$truth[r] >= q[8] & jj$truth[r] <= q[16]),
        cov90 = as.numeric(jj$truth[r] >= q[3] & jj$truth[r] <= q[21]),
        cov95 = as.numeric(jj$truth[r] >= q[2] & jj$truth[r] <= q[22]))
    }
  }
  cat(sprintf("origin %s: logL %.1f, S_R(15) %.3f, %d scored\n", as.character(s0),
              ftb$logL, 1 - sum(Pb$hR), length(rows)))
}
R <- do.call(rbind, rows)
saveRDS(R, sprintf("devel/spa_diagnostics/backtest_hybrid_%s.rds", gsub(" ","",STATE)))
cat(sprintf("\n=== %d triples ===\n", nrow(R)))
cat("\n--- WIS by horizon ---\n")
print(as.data.frame(R |> group_by(h) |> summarise(n=n(), hybrid=mean(wis_hyb),
  empirical=mean(wis_emp), persist=mean(wis_persist),
  skill=1-mean(wis_hyb)/mean(wis_emp), cov90=mean(cov90)) |>
  mutate(across(where(is.numeric), ~round(.x,3)))), row.names=FALSE)
cat("\n--- WIS by TARGET AGE ---\n")
print(as.data.frame(R |> mutate(age=cut(a_star,c(-1,1,2,4,8,15))) |> group_by(age) |>
  summarise(n=n(), hybrid=mean(wis_hyb), empirical=mean(wis_emp), persist=mean(wis_persist),
  skill=1-mean(wis_hyb)/mean(wis_emp), cov90=mean(cov90)) |>
  mutate(across(where(is.numeric), ~round(.x,3)))), row.names=FALSE)
cat("\n--- FOUR-WAY, same triples ---\n")
cat(sprintf("h_R + ZTNB hurdle  (38, this) : WIS %.3f | skill %+.1f%%\n",
    mean(R$wis_hyb), 100*(1-mean(R$wis_hyb)/mean(R$wis_emp))))
cat(sprintf("ZTNB descriptive   (36)       : WIS 1.959 | skill +10.6%%\n"))
cat(sprintf("empirical baseline            : WIS %.3f\n", mean(R$wis_emp)))
cat(sprintf("persistence                   : WIS %.3f\n", mean(R$wis_persist)))
cat(sprintf("tex Poisson-Skellam (37)      : WIS 2.838 | skill -29.4%%\n"))
cat(sprintf("\ncoverage 50%%: %.3f (nom .50) | 90%%: %.3f (nom .90) | 95%%: %.3f (nom .95)\n",
    mean(R$cov50), mean(R$cov90), mean(R$cov95)))
