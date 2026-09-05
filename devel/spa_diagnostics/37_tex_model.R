# =============================================================================
# 37 -- THE p-FREE COUNT-CUMULATIVE MODEL OF main_identifiability_update.tex,
#       put through the stage-2 rolling-origin backtest.
#
# The tex takes the retraction kernel h_R(l) = P(R = l) as PRIMITIVE, with
# survival S_R(a) = 1 - sum_{l<=a} h_R(l) and pi_inf = 1 - sum_l h_R(l), and
# does not estimate p or g_C separately.  That is exactly the b_c
# reparameterisation of FINDINGS J with h_R == b_c, and its identifiability
# claim is J.3's result: finite-horizon data pin h_R(1..A) and S_R(A), not p.
#
# Its per-delay marginals are
#     alpha_t^d = mu_t g_D(d)                                  (all arrivals at d)
#     omega_t^d = mu_t sum_{d1<d} g_D(d1) h_R(d - d1)          (all withdrawals at d)
#     Delta_t^d ~ Skellam(alpha_t^d, omega_t^d).
#
# For a CADENCE interval (a,b] -- which is what FluSight actually gives, since
# snapshots are missing -- the product-over-d composite is not available.  The
# exact marginal of C(b)-C(a) under the same model comes from the disjoint
# trajectory classes:
#     +1 : arrived in (a,b] and still present at b
#     -1 : arrived by a, present at a, withdrawn by b
#      0 : arrived and withdrawn inside (a,b], or arrived after b
# giving Skellam(mu*qp, mu*qm) with
#     qp(a,b) = sum_{r=a+1}^{b} g_D(r) S_R(b-r)
#     qm(a,b) = sum_{r=0}^{a}   g_D(r) [S_R(a-r) - S_R(b-r)].
# For unit delays this reduces to the tex's alpha/omega exactly.  It differs from
# a naive sum of the per-delay marginals over the window, which would double
# count an arrive-and-withdraw pair inside the window as two independent draws
# instead of a deterministic zero -- i.e. it is the tex's own acknowledged
# cross-delay dependence, handled exactly rather than ignored.
#
# So the Poisson version of the tex model IS script 26's free-retention fit,
# which was only ever evaluated in-sample.  This script forecasts with it, on the
# same 16 origins, the same cohorts and the same target definition as scripts 33
# and 36, and scores it against ZTNB + Markov and the empirical baseline.
#
# Forecast: condition on the published C_t(a0) -- the operational estimand of
# PLAN section 1 -- and add the model's interval increment,
#     C_t(a*) = C_t(a0) + Skellam(mu_t qp(a0,a*), mu_t qm(a0,a*)).
#
# Run: NOT_CRAN=true Rscript devel/spa_diagnostics/37_tex_model.R
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

# =============================================================================
raw_all <- tbl.now::flusight |> filter(location_name == STATE, !is.na(observation)) |>
  mutate(d = floor(as.numeric(as_of - target_end_date) / 7)) |> arrange(as_of, target_end_date)
snaps <- sort(unique(raw_all$as_of))
origins <- utils::head(utils::tail(snaps, NORIG + HMAX), NORIG)
cat(sprintf("=== tex model (free h_R, no p) | %s | %d origins ===\n\n", STATE, length(origins)))

warm <- c(-1, log(0.5), stats::qlogis(0.5), rep(0, 14))
rows_out <- list()
for (s0 in origins) {
  tr <- raw_all |> filter(as_of <= s0)
  fl <- build_fl(tr); ft <- fit_tex(fl, warm); warm <- ft$par
  pr <- ft$env$prof(ft$par); mu <- pr$mu
  ev <- unique((tr |> filter(d <= 15, d >= 0))$target_end_date)
  cur <- raw_all |> filter(as_of == s0, d >= 0, d <= 15) |>
    select(target_end_date, a0 = d, C0 = observation) |> filter(C0 > 0)
  fut <- snaps[snaps > s0][seq_len(HMAX)]
  for (h in seq_along(fut)) {
    s_star <- fut[h]
    tgt <- raw_all |> filter(as_of == s_star) |> select(target_end_date, truth = observation)
    jj <- cur |> inner_join(tgt, by = "target_end_date") |>
      mutate(a_star = as.integer(floor(as.numeric(s_star - target_end_date)/7))) |>
      filter(a_star <= 15, a_star > a0)
    if (!nrow(jj)) next
    for (r in seq_len(nrow(jj))) {
      k <- match(jj$target_end_date[r], ev); if (is.na(k)) next
      Q <- q_pairs(pr$gD, pr$hR, jj$a0[r], jj$a_star[r])
      dr <- pmax(jj$C0[r] + stats::rpois(NDRAW, mu[k]*Q$qp) - stats::rpois(NDRAW, mu[k]*Q$qm), 0)
      q_tx <- as.numeric(stats::quantile(dr, QL, names = FALSE))
      spans <- tr |> group_by(target_end_date) |> arrange(d, .by_group = TRUE) |>
        summarise(rr = { i <- which(d == jj$a0[r]); j <- which(d == jj$a_star[r])
          if (length(i) && length(j) && observation[i] > 0) observation[j]/observation[i] else NA_real_ },
          .groups="drop") |> pull(rr)
      spans <- spans[is.finite(spans)]
      q_em <- if (length(spans) >= 5) as.numeric(stats::quantile(jj$C0[r]*spans, QL, names=FALSE))
              else rep(jj$C0[r], length(QL))
      rows_out[[length(rows_out)+1L]] <- data.frame(origin = s0, h = h, a0 = jj$a0[r],
        a_star = jj$a_star[r], truth = jj$truth[r],
        wis_tex = wis(q_tx, jj$truth[r]), wis_emp = wis(q_em, jj$truth[r]),
        wis_persist = wis(rep(jj$C0[r], length(QL)), jj$truth[r]),
        cov50 = as.numeric(jj$truth[r] >= q_tx[8] & jj$truth[r] <= q_tx[16]),
        cov90 = as.numeric(jj$truth[r] >= q_tx[3] & jj$truth[r] <= q_tx[21]),
        cov95 = as.numeric(jj$truth[r] >= q_tx[2] & jj$truth[r] <= q_tx[22]))
    }
  }
  cat(sprintf("origin %s: logL %.1f, S_R(15) = %.3f, %d scored\n", as.character(s0),
              ft$logL, 1 - sum(pr$hR), length(rows_out)))
}
R <- do.call(rbind, rows_out)
saveRDS(R, sprintf("devel/spa_diagnostics/backtest_tex_%s.rds", gsub(" ","",STATE)))
z <- readRDS(sprintf("devel/spa_diagnostics/backtest_ztnb_%s.rds", gsub(" ","",STATE)))
cat(sprintf("\n=== %d triples (ZTNB run scored %d) ===\n", nrow(R), nrow(z)))
cat("\n--- WIS by horizon ---\n")
print(as.data.frame(R |> group_by(h) |> summarise(n=n(), tex=mean(wis_tex),
  empirical=mean(wis_emp), persist=mean(wis_persist),
  skill=1-mean(wis_tex)/mean(wis_emp), cov90=mean(cov90)) |>
  mutate(across(where(is.numeric), ~round(.x,3)))), row.names=FALSE)
cat("\n--- WIS by TARGET AGE ---\n")
print(as.data.frame(R |> mutate(age=cut(a_star,c(-1,1,2,4,8,15))) |> group_by(age) |>
  summarise(n=n(), tex=mean(wis_tex), empirical=mean(wis_emp), persist=mean(wis_persist),
  skill=1-mean(wis_tex)/mean(wis_emp), cov90=mean(cov90)) |>
  mutate(across(where(is.numeric), ~round(.x,3)))), row.names=FALSE)
cat("\n--- COMPARISON ---\n")
cat(sprintf("tex model (free h_R, no p) : WIS %.3f | skill vs empirical %+.1f%%\n",
    mean(R$wis_tex), 100*(1-mean(R$wis_tex)/mean(R$wis_emp))))
cat(sprintf("ZTNB + Markov (script 36)  : WIS %.3f | skill vs empirical %+.1f%%\n",
    mean(z$wis_markov), 100*(1-mean(z$wis_markov)/mean(z$wis_emp))))
cat(sprintf("empirical ratio baseline   : WIS %.3f\n", mean(R$wis_emp)))
cat(sprintf("persistence                : WIS %.3f\n", mean(R$wis_persist)))
cat(sprintf("\ntex coverage  50%%: %.3f (nom .50) | 90%%: %.3f (nom .90) | 95%%: %.3f (nom .95)\n",
    mean(R$cov50), mean(R$cov90), mean(R$cov95)))
