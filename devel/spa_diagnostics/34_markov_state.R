# =============================================================================
# 34 -- STAGE 2b: a two-state Markov move indicator, to fix the over-dispersion.
#
# Script 33 beat both baselines out of sample (+9.2% WIS vs the empirical ratio
# lookup, +20.8% vs persistence) but was OVER-dispersed: 90% coverage 0.977
# against a nominal 0.90, and at target ages 9-15 plain persistence still won
# (1.042 vs 1.333).  The in-sample marginal law is well calibrated (PIT p 0.096,
# 4.7% outside the central 95%), so the defect is in COMPOUNDING: the forward
# simulation drew move / no-move independently at every future age, and reality
# has state.
#
# Measured, on all 53 locations, 37,907 cells with a previous increment:
#
#     target age   P(move | prev moved)   P(move | prev didn't)   odds ratio
#        1-2            0.320                   0.088                4.9
#        3-4            0.291                   0.085                4.5
#        5-8            0.256                   0.051                6.4
#       9-15            0.259                   0.027               12.5
#
# Persistence is real, survives conditioning on age, and STRENGTHENS with age --
# exactly the regime where persistence beat the model.  So the move indicator
# becomes a two-state Markov chain, one extra parameter:
#
#     logit pi(a, prev) = p0 + p1 log(1+a) + gamma * 1{previous increment != 0}
#
# A cohort's first post-first interval has no comparable history and takes
# gamma = 0; that conflates "no history" with "did not move", which is a
# deliberate simplification worth revisiting.
#
# In forecasting this also makes the cohort's CURRENT state informative: one that
# just moved is likely to move again, which a marginal model cannot express.
#
# Magnitudes are persistent too (cor(log|z|, log|z_prev|) = 0.358) and that is
# NOT modelled here -- one change at a time, so the effect is attributable.
#
# Part A refits in-sample and re-runs the stage-1 gates; part B repeats the
# stage-2 rolling-origin backtest.
#
# Run: NOT_CRAN=true Rscript devel/spa_diagnostics/34_markov_state.R
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})
STATE <- Sys.getenv("STATE", "Texas")
NORIG <- as.integer(Sys.getenv("NORIG", "16"))
START <- as.Date("2023-09-23")
set.seed(20260902)

mk_gD <- function(dmu, dsig, cD) {
  f <- diseasenowcasting:::.delay_distribution_functions(1L, dmu, 0.01 + dsig)
  acdf <- as.numeric(f$cdf(seq_len(cD + 1L))); c(acdf[1], diff(acdf))
}
lpois <- function(z, a) ifelse(z < 0, -Inf, stats::dpois(pmax(z, 0), a, log = TRUE))


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

QL <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
HMAX <- 4L; NDRAW <- 2000L

# theta = (dmu, log dsig, p0, p1, gamma, t0, t1, log kappa, beta, log s)
unpack <- function(th, H) list(gD = mk_gD(th[1], exp(th[2]), H),
  p0 = th[3], p1 = th[4], gam = th[5], t0 = th[6], t1 = th[7],
  lkap = th[8], beta = th[9], s = exp(th[10]))

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
      m <- P$lkap + P$beta * log(lev); v <- numeric(length(zz))
      zero <- zz == 0; up <- zz > 0; dn <- zz < 0
      v[zero] <- stats::plogis(-lin_p[zero], log.p = TRUE)
      if (any(up)) v[up] <- stats::plogis(lin_p[up], log.p = TRUE) +
        stats::plogis(lin_t[up], log.p = TRUE) + lmag(zz[up], m[up], P$s)
      if (any(dn)) v[dn] <- stats::plogis(lin_p[dn], log.p = TRUE) +
        stats::plogis(-lin_t[dn], log.p = TRUE) + lmag(-zz[dn], m[dn], P$s)
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
    if (any(!is.finite(th)) || th[2] > 3 || th[10] > 3) return(1e12)
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
      m <- P$lkap + P$beta * log(pmax(cur[mv], 1))
      M <- pmax(round(exp(stats::rnorm(sum(mv), m, P$s))), 1)
      sgn <- ifelse(stats::runif(sum(mv)) < th_, 1, -1)
      cur[mv] <- pmax(cur[mv] + sgn * M, 0)
    }
    st <- as.numeric(mv)
  }
  cur
}
wis <- function(qs, y) 2 * mean(ifelse(y >= qs, QL * (y - qs), (1 - QL) * (qs - y)))

# =============================================================================
# PART A -- in-sample refit and the stage-1 gates
# =============================================================================
raw_all <- tbl.now::flusight |> dplyr::filter(location_name == STATE, !is.na(observation)) |>
  dplyr::mutate(d = floor(as.numeric(as_of - target_end_date) / 7)) |>
  dplyr::arrange(as_of, target_end_date)
tr_full <- raw_all |> dplyr::filter(target_end_date >= START)
fl <- build_fl(tr_full)
cat(sprintf("=== %s | w15 | %d event weeks, %d intervals (%d post-first) ===\n\n",
            STATE, fl$nTg, fl$nObs, sum(fl$post)))
S0 <- c(-3.5, 0.5, 2.2, -2.0, 1.5, 1.3, 0.0, log(5), 0.00, log(1.5))
starts <- list(S0, c(-3.5, 0.5, 1.8, -1.8, 2.0, 1.0, -0.2, log(2), 0.20, log(1.2)),
                   c(-2.0, 0.5, 2.5, -2.2, 1.0, 1.5, 0.2, log(8), -0.1, log(2.0)))
best <- NULL
for (st in starts) { ft <- fit_one(fl, st)
  cat(sprintf("start: logL = %.2f\n", ft$logL))
  if (is.null(best) || ft$logL > best$logL) best <- ft }
P <- unpack(best$par, best$H); mu <- best$env$profile(P)
cat(sprintf("\nlogL = %.2f   (marginal-pi model, script 32: -1500.94)\n", best$logL))
cat(sprintf("gamma = %+.3f  -> odds of moving are %.1fx higher after a move\n",
            P$gam, exp(P$gam)))
cat(sprintf("pi(a, quiet) : %.3f at age 1, %.3f at 4, %.3f at 15\n",
  stats::plogis(P$p0+P$p1*log(2)), stats::plogis(P$p0+P$p1*log(5)), stats::plogis(P$p0+P$p1*log(16))))
cat(sprintf("pi(a, active): %.3f at age 1, %.3f at 4, %.3f at 15\n",
  stats::plogis(P$p0+P$p1*log(2)+P$gam), stats::plogis(P$p0+P$p1*log(5)+P$gam),
  stats::plogis(P$p0+P$p1*log(16)+P$gam)))
cat(sprintf("theta: %.3f | |move| median %.3f x level^%.3f, log-sd %.3f\n",
  stats::plogis(P$t0+P$t1*log(5)), exp(P$lkap), P$beta, P$s))

GD <- cumsum(P$gD); GDf <- function(k) ifelse(k<0,0,GD[pmin(pmax(k,0),length(GD)-1L)+1L])
mu_i <- mu[fl$grp]; kk <- which(fl$post)
lev <- pmax(mu_i * GDf(fl$a), 1); m_i <- P$lkap + P$beta * log(lev)
pi_i <- stats::plogis(P$p0 + P$p1*log1p(fl$b) + P$gam*fl$pm)
th_i <- stats::plogis(P$t0 + P$t1*log1p(fl$b))
Smag <- function(x, m) exp(lsf((log(x + 0.5) - m)/P$s) - lsf((log(0.5) - m)/P$s))
cat("\n=== GATE 1: exact zeros ===\n")
cat(sprintf("observed %d of %d (%.0f%%) | model %.0f (%.0f%%)\n", sum(fl$z[kk]==0), length(kk),
            100*mean(fl$z[kk]==0), sum(1-pi_i[kk]), 100*mean(1-pi_i[kk])))
cat("\n=== GATE 2: the tail ===\n")
for (thr in c(20,50,100,300)) cat(sprintf("  |z| > %3d : observed %3d   expected %6.1f   ratio %.2f\n",
  thr, sum(abs(fl$z[kk])>thr), sum(pi_i[kk]*Smag(thr,m_i[kk])),
  sum(abs(fl$z[kk])>thr)/max(sum(pi_i[kk]*Smag(thr,m_i[kk])),1e-9)))
cdf_lower <- function(z,i) { if (z>0) return(1 - pi_i[i]*th_i[i]*Smag(z-1,m_i[i]))
  if (z==0) return(pi_i[i]*(1-th_i[i])); pi_i[i]*(1-th_i[i])*Smag(-z-1+1e-9,m_i[i]) }
pmf1 <- function(z,i) { if (z==0) return(1-pi_i[i])
  if (z>0) return(pi_i[i]*th_i[i]*exp(lmag(z,m_i[i],P$s)))
  pi_i[i]*(1-th_i[i])*exp(lmag(-z,m_i[i],P$s)) }
U <- vapply(kk, function(i) cdf_lower(fl$z[i],i) + stats::runif(1)*pmf1(fl$z[i],i), 1.0)
ks <- stats::ks.test(U, "punif")
cat(sprintf("\n=== GATE 3: PIT ===\nn = %d | KS D = %.4f, p = %.3g | outside central 95%%: %.1f%%\n",
            length(U), ks$statistic, ks$p.value, 100*mean(U<0.025|U>0.975)))
print(round(as.numeric(table(cut(U, seq(0,1,0.1))))/length(U), 3))

# =============================================================================
# PART B -- the rolling-origin backtest, identical setup to script 33
# =============================================================================
snaps <- sort(unique(raw_all$as_of))
origins <- utils::head(utils::tail(snaps, NORIG + HMAX), NORIG)
cat(sprintf("\n\n=== PART B: %d origins, %s to %s ===\n", length(origins),
            min(origins), max(origins)))
rows_out <- list(); warm <- best$par
for (s0 in origins) {
  tr <- raw_all |> dplyr::filter(as_of <= s0)
  flb <- build_fl(tr); ftb <- fit_one(flb, warm); warm <- ftb$par
  Pb <- unpack(ftb$par, ftb$H)
  # current value AND current move state for every visible cohort
  cur <- raw_all |> dplyr::filter(as_of <= s0, d >= 0, d <= 15) |>
    dplyr::group_by(target_end_date) |> dplyr::arrange(d, .by_group = TRUE) |>
    dplyr::summarise(a0 = dplyr::last(d), C0 = dplyr::last(observation),
                     st0 = if (dplyr::n() >= 3) as.numeric(dplyr::last(diff(observation)) != 0) else 0,
                     .groups = "drop") |>
    dplyr::filter(C0 > 0, a0 == max(a0) | TRUE)
  cur <- cur |> dplyr::filter(target_end_date %in%
    (raw_all |> dplyr::filter(as_of == s0) |> dplyr::pull(target_end_date)))
  fut <- snaps[snaps > s0][seq_len(HMAX)]
  for (h in seq_along(fut)) {
    s_star <- fut[h]
    tgt <- raw_all |> dplyr::filter(as_of == s_star) |> dplyr::select(target_end_date, truth = observation)
    jj <- cur |> dplyr::inner_join(tgt, by = "target_end_date") |>
      dplyr::mutate(a_star = as.integer(floor(as.numeric(s_star - target_end_date)/7))) |>
      dplyr::filter(a_star <= 15, a_star > a0)
    if (!nrow(jj)) next
    for (r in seq_len(nrow(jj))) {
      dr <- sim_forward(jj$C0[r], jj$st0[r], (jj$a0[r]+1L):jj$a_star[r], Pb, NDRAW)
      q_sm <- as.numeric(stats::quantile(dr, QL, names = FALSE))
      spans <- tr |> dplyr::group_by(target_end_date) |> dplyr::arrange(d, .by_group = TRUE) |>
        dplyr::summarise(r = { i <- which(d == jj$a0[r]); j <- which(d == jj$a_star[r])
          if (length(i) && length(j) && observation[i] > 0) observation[j]/observation[i] else NA_real_ },
          .groups = "drop") |> dplyr::pull(r)
      spans <- spans[is.finite(spans)]
      q_em <- if (length(spans) >= 5) as.numeric(stats::quantile(jj$C0[r]*spans, QL, names = FALSE))
              else rep(jj$C0[r], length(QL))
      rows_out[[length(rows_out)+1L]] <- data.frame(origin = s0, h = h, a0 = jj$a0[r],
        a_star = jj$a_star[r], truth = jj$truth[r],
        wis_markov = wis(q_sm, jj$truth[r]), wis_emp = wis(q_em, jj$truth[r]),
        wis_persist = wis(rep(jj$C0[r], length(QL)), jj$truth[r]),
        cov50 = as.numeric(jj$truth[r] >= q_sm[8]  & jj$truth[r] <= q_sm[16]),
        cov90 = as.numeric(jj$truth[r] >= q_sm[3]  & jj$truth[r] <= q_sm[21]),
        cov95 = as.numeric(jj$truth[r] >= q_sm[2]  & jj$truth[r] <= q_sm[22]),
        ecov90 = as.numeric(jj$truth[r] >= q_em[3] & jj$truth[r] <= q_em[21]),
        ecov95 = as.numeric(jj$truth[r] >= q_em[2] & jj$truth[r] <= q_em[22]))
    }
  }
  cat(sprintf("origin %s: logL %.1f, %d scored\n", as.character(s0), ftb$logL, length(rows_out)))
}
R <- do.call(rbind, rows_out)
saveRDS(R, sprintf("devel/spa_diagnostics/backtest_markov_%s.rds", gsub(" ", "", STATE)))
old <- readRDS(sprintf("devel/spa_diagnostics/backtest_%s.rds", gsub(" ", "", STATE)))
cat(sprintf("\n=== %d triples (script 33 scored %d) ===\n", nrow(R), nrow(old)))
cat("\n--- WIS by horizon ---\n")
print(as.data.frame(R |> dplyr::group_by(h) |> dplyr::summarise(n = dplyr::n(),
  markov = mean(wis_markov), empirical = mean(wis_emp), persist = mean(wis_persist),
  skill = 1 - mean(wis_markov)/mean(wis_emp), cov90 = mean(cov90)) |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 3)))), row.names = FALSE)
cat("\n--- WIS by TARGET AGE ---\n")
print(as.data.frame(R |> dplyr::mutate(age = cut(a_star, c(-1,1,2,4,8,15))) |>
  dplyr::group_by(age) |> dplyr::summarise(n = dplyr::n(), markov = mean(wis_markov),
  empirical = mean(wis_emp), persist = mean(wis_persist),
  skill = 1 - mean(wis_markov)/mean(wis_emp),
  cov50 = mean(cov50), cov90 = mean(cov90), cov95 = mean(cov95)) |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 3)))), row.names = FALSE)
cat("\n--- overall ---\n")
cat(sprintf("markov %.3f | empirical %.3f | persist %.3f | skill vs empirical %+.1f%%\n",
    mean(R$wis_markov), mean(R$wis_emp), mean(R$wis_persist),
    100*(1-mean(R$wis_markov)/mean(R$wis_emp))))
cat(sprintf("coverage  50%%: %.3f (nom 0.50) | 90%%: %.3f (nom 0.90) | 95%%: %.3f (nom 0.95)\n",
    mean(R$cov50), mean(R$cov90), mean(R$cov95)))
cat(sprintf("empirical 90%%: %.3f | 95%%: %.3f\n", mean(R$ecov90), mean(R$ecov95)))
cat(sprintf("script 33 overall WIS was %.3f -> improvement %+.1f%%\n",
    mean(old$wis_signmag), 100*(1 - mean(R$wis_markov)/mean(old$wis_signmag))))
