# =============================================================================
# 33 -- STAGE 2 of PLAN_increment_nowcasting.md: does it FORECAST better?
#
# Everything in FINDINGS M and N is in-sample.  It establishes that the sign x
# magnitude law can REPRESENT what the retention model provably could not (620
# exact zeros against 85), which was the question K raised -- but not that it
# predicts anything.  This is the out-of-sample test.
#
# ESTIMAND (plan section 1): the finite-horizon operational target, keyed to a
# future PUBLICATION DATE rather than to "h snapshots ahead", so it is invariant
# to skipped snapshots.  For an origin snapshot s0 and a cohort t visible there
# at age a, the target is the value that cohort will carry in the snapshot
# published at s*, i.e. C_t(a*) with a* = floor((s* - t)/7).
#
# Rolling origin: refit at every origin on rows with as_of <= s0 only, so
# nothing from the future leaks into either the parameters or the level.
#
# Methods scored on identical (cohort, origin, horizon) triples:
#   signmag  -- the script 32 law, simulated forward snapshot by snapshot
#   empirical-- for the same (current age -> target age) pair, the empirical
#               distribution of C_t(a*)/C_t(a) ratios seen in TRAINING.  This is
#               the strong baseline for this data: 77% of increments are exactly
#               zero, so "nothing will change, and here is how often that is
#               wrong" is hard to beat.
#   persist  -- point mass at the current value.  A floor, not a real rival.
#
# WIS from the 23 FluSight quantile levels, via the pinball identity
# WIS = 2 * mean_tau pinball_tau.  Reported BY TARGET AGE as well as by horizon,
# because support falls from 761 intervals at lag 1 to 49 at lag 15 (FINDINGS
# J.4) and a horizon-only table hides where the model is extrapolating.
#
# Run: NOT_CRAN=true Rscript devel/spa_diagnostics/33_backtest.R
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})
STATE   <- Sys.getenv("STATE", "Texas")
NORIG   <- as.integer(Sys.getenv("NORIG", "16"))
NDRAW   <- 2000L
HMAX    <- 4L
QL      <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
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
unpack <- function(th, H) list(gD = mk_gD(th[1], exp(th[2]), H),
  p0 = th[3], p1 = th[4], t0 = th[5], t1 = th[6],
  lkap = th[7], beta = th[8], s = exp(th[9]))

# ---- build interval observations from a set of raw rows --------------------
build_fl <- function(rows) {
  obs <- list()
  for (E in unique(rows$target_end_date)) {
    x <- rows |> filter(target_end_date == E, d <= 15L, d >= 0) |> arrange(d)
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
  fl$nObs <- length(fl$z); fl$post <- fl$a >= 0; fl
}
GCOARSE <- seq(-6, 6, length.out = 13L); GFINE <- seq(-0.5, 0.5, length.out = 9L)
fit_signmag <- function(fl, start) {
  H <- max(fl$b)
  cell_ll <- function(z, mu, aa, bb, post, P) {
    GD <- cumsum(P$gD)
    GDf <- function(k) ifelse(k < 0, 0, GD[pmin(pmax(k, 0), length(GD) - 1L) + 1L])
    out <- numeric(length(z)); first <- !post
    if (any(first)) out[first] <- lpois(z[first], pmax(mu[first] * GDf(bb[first]), 1e-10))
    if (any(post)) {
      zz <- z[post]; lev <- pmax(mu[post] * GDf(aa[post]), 1)
      lin_p <- P$p0 + P$p1 * log1p(bb[post]); lin_t <- P$t0 + P$t1 * log1p(bb[post])
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
    v <- cell_ll(fl$z[ec], exp(centre[fl$grp[ec]] + off[gc]), fl$a[ec], fl$b[ec], fl$post[ec], P)
    v[!is.finite(v)] <- NA_real_
    agg <- rowsum(v, fl$grp[ec] + (gc - 1L) * nT, na.rm = FALSE)
    Sv <- rep(NA_real_, nT * G); Sv[as.integer(rownames(agg))] <- agg
    matrix(Sv, nT, G)
  }
  amax <- function(M) { Mf <- M; Mf[is.na(Mf)] <- -Inf
    jm <- max.col(Mf, ties.method = "first")
    jm[!is.finite(Mf[cbind(seq_len(nrow(M)), jm)])] <- 0L; jm }
  nll <- function(th) {
    if (any(!is.finite(th)) || th[2] > 3 || th[9] > 3) return(1e12)
    P <- unpack(th, H); if (any(!is.finite(P$gD))) return(1e12)
    M1 <- gridM(P, fl$start, GCOARSE); j1 <- amax(M1); if (any(j1 == 0L)) return(1e12)
    M <- gridM(P, fl$start + GCOARSE[j1], GFINE); jm <- amax(M); if (any(jm == 0L)) return(1e12)
    G <- ncol(M); tot <- 0
    for (t in seq_len(fl$nTg)) {
      j <- jm[t]; y2 <- M[t, j]
      if (j > 1L && j < G) { y1 <- M[t, j-1L]; y3 <- M[t, j+1L]
        if (is.finite(y1) && is.finite(y3)) { den <- y1 - 2*y2 + y3
          # vertex of the parabola through (-1,y1) (0,y2) (1,y3), in grid units.
        # UNGUARDED this explodes when the profile is flat (den -> 0-), which
        # silently produced POSITIVE log-likelihoods in script 33.
        if (den < 0) { corr <- -0.125 * (y1 - y3)^2 / den
          if (is.finite(corr) && corr <= 1) { tot <- tot + y2 + corr; next } } } }
      tot <- tot + y2
    }
    if (is.finite(tot) && tot <= 0) -tot else 1e12
  }
  ft <- stats::optim(start, nll, method = "BFGS", control = list(maxit = 200, reltol = 1e-11))
  ft <- stats::optim(ft$par, nll, method = "Nelder-Mead", control = list(maxit = 800, reltol = 1e-12))
  list(par = ft$par, logL = -ft$value, H = H)
}

# ---- forward simulation of the published value ------------------------------
# From the value now at age a0, step through the ages the FUTURE snapshots will
# land on, drawing move / sign / magnitude at each.  The magnitude scale uses the
# CURRENT simulated level -- the conditional variant flagged in the plan (2b).
sim_forward <- function(C0, a0, ages, P, n) {
  cur <- rep(as.numeric(C0), n)
  for (ag in ages) {
    pi_ <- stats::plogis(P$p0 + P$p1 * log1p(ag))
    th_ <- stats::plogis(P$t0 + P$t1 * log1p(ag))
    mv <- stats::runif(n) < pi_
    if (any(mv)) {
      m <- P$lkap + P$beta * log(pmax(cur[mv], 1))
      M <- pmax(round(exp(stats::rnorm(sum(mv), m, P$s))), 1)
      sgn <- ifelse(stats::runif(sum(mv)) < th_, 1, -1)
      cur[mv] <- pmax(cur[mv] + sgn * M, 0)
    }
  }
  cur
}
wis <- function(qs, y) {                      # qs at levels QL, pinball identity
  2 * mean(ifelse(y >= qs, QL * (y - qs), (1 - QL) * (qs - y)))
}

# =============================================================================
raw_all <- tbl.now::flusight |> filter(location_name == STATE, !is.na(observation)) |>
  mutate(d = floor(as.numeric(as_of - target_end_date) / 7)) |> arrange(as_of, target_end_date)
snaps <- sort(unique(raw_all$as_of))
origins <- utils::head(utils::tail(snaps, NORIG + HMAX), NORIG)
cat(sprintf("=== %s | %d snapshots | %d origins from %s to %s | H = %d ===\n\n",
            STATE, length(snaps), length(origins), min(origins), max(origins), HMAX))

rows_out <- list(); warm <- c(-3.5, 0.5, 2.2, -2.0, 1.3, 0.0, log(5), 0.00, log(1.5))
for (s0 in origins) {
  tr <- raw_all |> filter(as_of <= s0)
  fl <- build_fl(tr)
  ft <- fit_signmag(fl, warm); warm <- ft$par           # warm-start the next origin
  P <- unpack(ft$par, ft$H)

  # training ratio pool for the empirical baseline: (age now -> age target)
  pool <- tr |> group_by(target_end_date) |> arrange(d, .by_group = TRUE) |>
    mutate(prev = observation, prev_d = d) |> ungroup()

  cur <- raw_all |> filter(as_of == s0, d >= 0, d <= 15) |>
    select(target_end_date, a0 = d, C0 = observation) |> filter(C0 > 0)
  fut <- snaps[snaps > s0][seq_len(HMAX)]
  for (h in seq_along(fut)) {
    s_star <- fut[h]
    tgt <- raw_all |> filter(as_of == s_star) |> select(target_end_date, truth = observation)
    jj <- cur |> inner_join(tgt, by = "target_end_date") |>
      mutate(a_star = as.integer(floor(as.numeric(s_star - target_end_date) / 7))) |>
      filter(a_star <= 15)
    if (!nrow(jj)) next
    for (r in seq_len(nrow(jj))) {
      ages <- (jj$a0[r] + 1L):jj$a_star[r]
      if (jj$a_star[r] <= jj$a0[r]) next
      dr <- sim_forward(jj$C0[r], jj$a0[r], ages, P, NDRAW)
      q_sm <- as.numeric(stats::quantile(dr, QL, names = FALSE))
      # empirical baseline: ratios over the same age span, from training only
      rat <- pool |> filter(prev_d == jj$a0[r]) |> pull(observation)
      spans <- tr |> group_by(target_end_date) |> arrange(d, .by_group = TRUE) |>
        summarise(r = { i <- which(d == jj$a0[r]); j <- which(d == jj$a_star[r])
                        if (length(i) && length(j) && observation[i] > 0)
                          observation[j] / observation[i] else NA_real_ }, .groups = "drop") |>
        pull(r)
      spans <- spans[is.finite(spans)]
      q_em <- if (length(spans) >= 5) as.numeric(stats::quantile(jj$C0[r] * spans, QL, names = FALSE))
              else rep(jj$C0[r], length(QL))
      rows_out[[length(rows_out) + 1L]] <- data.frame(
        origin = s0, h = h, a0 = jj$a0[r], a_star = jj$a_star[r], truth = jj$truth[r],
        wis_signmag = wis(q_sm, jj$truth[r]), wis_emp = wis(q_em, jj$truth[r]),
        wis_persist = wis(rep(jj$C0[r], length(QL)), jj$truth[r]),
        cov90_sm = as.numeric(jj$truth[r] >= q_sm[2] & jj$truth[r] <= q_sm[22]),
        cov90_em = as.numeric(jj$truth[r] >= q_em[2] & jj$truth[r] <= q_em[22]))
    }
  }
  cat(sprintf("origin %s: logL %.1f, %d scored\n", as.character(s0), ft$logL, length(rows_out)))
}
R <- do.call(rbind, rows_out)
saveRDS(R, sprintf("devel/spa_diagnostics/backtest_%s.rds", gsub(" ", "", STATE)))

cat(sprintf("\n=== %d scored (cohort, origin, horizon) triples ===\n", nrow(R)))
cat("\n--- WIS by horizon (lower is better) ---\n")
print(as.data.frame(R |> group_by(h) |> summarise(n = n(),
  signmag = mean(wis_signmag), empirical = mean(wis_emp), persist = mean(wis_persist),
  skill_vs_emp = 1 - mean(wis_signmag)/mean(wis_emp),
  cov90_sm = mean(cov90_sm), cov90_em = mean(cov90_em)) |>
  mutate(across(where(is.numeric), ~round(.x, 3)))), row.names = FALSE)
cat("\n--- WIS by TARGET AGE (plan stage 2 gate) ---\n")
print(as.data.frame(R |> mutate(age = cut(a_star, c(-1, 1, 2, 4, 8, 15))) |>
  group_by(age) |> summarise(n = n(), signmag = mean(wis_signmag), empirical = mean(wis_emp),
  persist = mean(wis_persist), skill_vs_emp = 1 - mean(wis_signmag)/mean(wis_emp),
  cov90_sm = mean(cov90_sm), cov90_em = mean(cov90_em)) |>
  mutate(across(where(is.numeric), ~round(.x, 3)))), row.names = FALSE)
cat("\n--- overall ---\n")
cat(sprintf("signmag %.3f | empirical %.3f | persist %.3f | skill vs empirical %+.1f%%\n",
    mean(R$wis_signmag), mean(R$wis_emp), mean(R$wis_persist),
    100*(1 - mean(R$wis_signmag)/mean(R$wis_emp))))
cat(sprintf("90%% coverage: signmag %.3f | empirical %.3f  (nominal 0.90)\n",
    mean(R$cov90_sm), mean(R$cov90_em)))
