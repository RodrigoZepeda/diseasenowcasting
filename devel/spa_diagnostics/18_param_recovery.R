# =============================================================================
# 18 -- PARAMETER RECOVERY for p under the EXACT implemented interval model.
#
# The decisive question (HANDOFF thread B, response step 1/15):
#   Simulate cumulative trajectories from the model's own generative process,
#   using the real FluSight publication cadence and the interval observation
#   mechanism, then refit p (nuisance re-optimised).  Does p_true = 0.95 come
#   back as p_hat ~ 0.95?
#
#   If NO  -> intrinsic identification/implementation problem even when correct.
#   If YES but real FluSight still gives 0.4-0.6 -> real-data misspecification.
#
# The estimator (vskel / q_tab / neg_loglik) is COPIED VERBATIM from
# 17_true_interval_refit.R so recovery is measured against the same fit the
# diagnostics used, not a parallel re-implementation.
#
# Run with:  NOT_CRAN=true Rscript devel/spa_diagnostics/18_param_recovery.R
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})

set.seed(20260902)
MODE <- Sys.getenv("PR_MODE", "time1")   # "time1" = one fit + timing; "full" = MC; "none" = defs only (sourceable)

# ---- estimator, verbatim from script 17 -------------------------------------
vskel <- function(z, a, b) {
  out <- numeric(length(z))
  pure_add <- b <= 1e-9; pure_ret <- a <= 1e-9 & !pure_add
  mixed <- !pure_add & !pure_ret
  if (any(pure_add)) out[pure_add] <- ifelse(z[pure_add] < 0, -Inf,
    stats::dpois(pmax(z[pure_add], 0), a[pure_add] + 1e-10, log = TRUE))
  if (any(pure_ret)) out[pure_ret] <- ifelse(z[pure_ret] > 0, -Inf,
    stats::dpois(pmax(-z[pure_ret], 0), b[pure_ret] + 1e-10, log = TRUE))
  if (any(mixed)) {
    zz <- z[mixed]; aa <- a[mixed]; bb <- b[mixed]
    root <- sqrt(zz^2 + 4 * aa * bb)
    u <- ifelse(zz >= 0, (zz + root) / (2 * aa), 2 * bb / (root - zz))
    s <- log(u)
    out[mixed] <- aa * (u - 1) + bb * (1 / u - 1) - s * zz -
                  0.5 * log(2 * pi * (aa * u + bb / u))
  }
  out
}

q_tab <- function(gD, gC, p, cD) {
  GC <- cumsum(gC)
  Gf <- function(k) if (k < 0) 0 else GC[min(k, length(GC) - 1L) + 1L]
  Gb <- function(k) if (k < 0) 1 else 1 - GC[min(k, length(GC) - 1L) + 1L]
  qp <- qm <- matrix(0, cD + 2L, cD + 1L)
  for (a in -1:cD) { lo <- max(a + 1L, 0L); if (lo > cD) next
  for (b in lo:cD) {
    sp <- 0
    for (r in seq.int(lo, b)) if (r + 1L <= length(gD)) sp <- sp + gD[r + 1L] * (p + (1 - p) * Gb(b - r))
    sm <- 0
    if (a >= 0) for (r in 0:a) if (r + 1L <= length(gD)) sm <- sm + gD[r + 1L] * (Gf(b - r) - Gf(a - r))
    qp[a + 2L, b + 1L] <- sp; qm[a + 2L, b + 1L] <- (1 - p) * sm
  } }
  list(qp = qp, qm = qm)
}

# gD / gC discretisation, verbatim: week-binned lognormal pmfs.
mk_gD <- function(dmu, dsig, cD) {
  f <- diseasenowcasting:::.delay_distribution_functions(1L, dmu, 0.01 + dsig)
  acdf <- as.numeric(f$cdf(seq_len(cD + 1L))); c(acdf[1], diff(acdf))
}
mk_gC <- function(rmu, rsig, cD) {
  f <- diseasenowcasting:::.delay_distribution_functions(1L, rmu, 0.01 + rsig)
  c(0, diff(c(0, as.numeric(f$cdf(seq_len(cD))))))
}

# --- flatten an observation list into vectors; lambda_t profiled out by a
#     VECTORISED grid+parabola over all event times at once (one vskel call per
#     likelihood eval instead of ~1800 per-t optimize() calls). -----------------
flatten_obs <- function(obs) {
  keep <- which(!vapply(obs, is.null, logical(1)))
  a <- b <- z <- grp <- integer(0); start <- numeric(0)
  for (gi in seq_along(keep)) {
    o <- obs[[keep[gi]]]
    a <- c(a, o$a); b <- c(b, o$b); z <- c(z, o$z); grp <- c(grp, rep(gi, length(o$z)))
    start <- c(start, log(max(sum(pmax(o$z, 0)), 1)))
  }
  list(a = a, b = b, z = z, grp = grp, start = start, nTg = length(keep), nObs = length(z),
       idx = split(seq_along(z), grp))
}
GCOARSE <- seq(-13, 13, length.out = 27L)     # wide coarse locate (spacing 1.0)
GFINE   <- seq(-1.5, 1.5, length.out = 13L)   # fine refine (spacing 0.25)
# per-t loglik surface over a set of loglam CENTRES + offsets, argmax cell per t.
.grid_M <- function(fl, qp, qm, p_val, centre, off) {   # per-t loglik surface, NA=infeasible
  G <- length(off); nO <- fl$nObs; nT <- fl$nTg
  ec <- rep.int(seq_len(nO), G); gc <- rep(seq_len(G), each = nO)
  loglam <- centre[fl$grp[ec]] + off[gc]
  mu <- exp(loglam) / p_val
  v <- vskel(fl$z[ec], mu * qp[ec], mu * qm[ec])
  v[!is.finite(v)] <- NA_real_
  agg <- rowsum(v, fl$grp[ec] + (gc - 1L) * nT, na.rm = FALSE)  # cell NA if any obs infeasible
  Sv <- rep(NA_real_, nT * G); Sv[as.integer(rownames(agg))] <- agg
  matrix(Sv, nT, G)
}
.argmax_na <- function(M) {                        # vectorised argmax; 0 flags an all-NA row
  Mf <- M; Mf[is.na(Mf)] <- -Inf
  jm <- max.col(Mf, ties.method = "first")
  ok <- is.finite(Mf[cbind(seq_len(nrow(M)), jm)])
  jm[!ok] <- 0L; jm
}
neg_loglik <- function(theta, p_val, fl, cD) {
  dmu <- theta[1]; dsig <- exp(theta[2]); rmu <- theta[3]; rsig <- exp(theta[4])
  gD <- mk_gD(dmu, dsig, cD); gC <- mk_gC(rmu, rsig, cD)
  if (any(!is.finite(gD)) || any(!is.finite(gC))) return(1e12)
  Q <- q_tab(gD, gC, p_val, cD)
  qp <- Q$qp[cbind(fl$a + 2L, fl$b + 1L)]; qm <- Q$qm[cbind(fl$a + 2L, fl$b + 1L)]
  nT <- fl$nTg
  M1 <- .grid_M(fl, qp, qm, p_val, fl$start, GCOARSE)          # locate
  j1 <- .argmax_na(M1)
  if (any(j1 == 0L)) return(1e12)                              # some t infeasible everywhere -> reject
  centre2 <- fl$start + GCOARSE[j1]
  M <- .grid_M(fl, qp, qm, p_val, centre2, GFINE)              # refine
  jm <- .argmax_na(M); if (any(jm == 0L)) return(1e12)
  G <- ncol(M); total <- 0
  for (t in seq_len(nT)) {
    j <- jm[t]; y2 <- M[t, j]
    if (j > 1L && j < G) {
      y1 <- M[t, j-1L]; y3 <- M[t, j+1L]
      if (is.finite(y1) && is.finite(y3)) { den <- y1 - 2*y2 + y3
        if (den < 0) { corr <- -0.125*(y1-y3)^2/den   # >= 0; UNGUARDED this
          # explodes when the mu profile is flat (den -> 0-) or when the grid
          # values are wildly asymmetric.  A refinement over a grid of step
          # 0.125 in log mu cannot be worth more than a fraction of a nat.
          if (is.finite(corr) && corr <= 1) { total <- total + y2 + corr; next } } }
    }
    total <- total + y2
  }
  if (!is.finite(total) || total > 0) 1e12 else -total
}

# profile p on a grid; return p_hat (parabolic refine) and the profile.
P_GRID <- c(0.15, 0.30, 0.45, 0.60, 0.75, 0.85, 0.92, 0.97)
start_par <- c(0, log(0.5), log(4), log(0.5))
profile_p <- function(obs, cD, passes = 1L, start = start_par) {
  fl <- flatten_obs(obs)
  ll <- rep(-Inf, length(P_GRID)); pars <- vector("list", length(P_GRID))
  # warm-start continuation, sweeping high p -> low p (high p converges cleanly).
  ord <- order(P_GRID, decreasing = TRUE); prev <- start
  for (i in ord) {
    fit <- stats::optim(prev, neg_loglik, p_val = P_GRID[i], fl = fl, cD = cD,
                        method = "Nelder-Mead", control = list(maxit = 300, reltol = 1e-8))
    ll[i] <- -fit$value; pars[[i]] <- fit$par; prev <- fit$par
  }
  if (passes >= 2L) for (i in rev(ord)) {         # optional low -> high cleanup
    seed <- if (i > 1L && !is.null(pars[[i-1L]])) pars[[i-1L]] else start
    fit <- stats::optim(seed, neg_loglik, p_val = P_GRID[i], fl = fl, cD = cD,
                        method = "Nelder-Mead", control = list(maxit = 300, reltol = 1e-8))
    if (-fit$value > ll[i]) { ll[i] <- -fit$value; pars[[i]] <- fit$par }
  }
  imax <- which.max(ll); phat <- P_GRID[imax]; halfwidth <- NA_real_
  if (imax > 1 && imax < length(P_GRID)) {
    x <- P_GRID[(imax-1):(imax+1)]; y <- ll[(imax-1):(imax+1)]
    d <- (x[1]-x[2])*(x[1]-x[3])*(x[2]-x[3])
    A <- (x[3]*(y[2]-y[1]) + x[2]*(y[1]-y[3]) + x[1]*(y[3]-y[2])) / d
    B <- (x[3]^2*(y[1]-y[2]) + x[2]^2*(y[3]-y[1]) + x[1]^2*(y[2]-y[3])) / d
    if (A < 0) { phat <- min(max(-B/(2*A), 0.02), 0.995); halfwidth <- sqrt(1.92/(-A)) }
  }
  # 1.92-nat CI: local parabola half-width if available, else grid interpolation.
  if (is.finite(halfwidth)) { ci <- c(phat - halfwidth, phat + halfwidth) } else {
    thr <- max(ll) - 1.92; lo <- P_GRID[1]; hi <- P_GRID[length(P_GRID)]
    if (imax > 1) for (i in 1:(imax-1)) if (ll[i] < thr && ll[i+1] >= thr) {
      lo <- P_GRID[i] + (thr-ll[i])/(ll[i+1]-ll[i])*(P_GRID[i+1]-P_GRID[i]); break }
    if (imax < length(P_GRID)) for (i in length(P_GRID):(imax+1)) if (ll[i] < thr && ll[i-1] >= thr) {
      hi <- P_GRID[i-1] + (thr-ll[i-1])/(ll[i]-ll[i-1])*(P_GRID[i]-P_GRID[i-1]); break }
    ci <- c(lo, hi)
  }
  list(phat = phat, ll = ll, ci = pmin(pmax(ci, 0), 1))
}

# ---- real Texas cadence + mature counts -------------------------------------
START <- as.Date("2023-09-23"); STATE <- "Texas"
raw <- tbl.now::flusight |>
  filter(location_name == STATE, target_end_date >= START, as_of >= START) |>
  filter(as_of <= max(target_end_date))
tn <- tbl_now(raw, event_date = target_end_date, report_date = as_of,
              case_count = observation, data_type = "count-cumulative",
              verbose = FALSE) |> align_weeks(date_col = "report_date")
base <- prepare_from_tbl_now(tn, model(poisson_likelihood(), ar1_epidemic(),
          lognormal_delay(), validation = validation_process(p = 0.95)),
          now = tbl.now::get_now(tn))
e  <- base$data
cD <- min(as.integer(e$max_conf_delay) - 1L, 15L)
inc <- e$increment_array
asof <- sort(unique(tn[[tbl.now::get_report_date(tn)]]))
ev   <- sort(unique(tn[[tbl.now::get_event_date(tn)]]))
maxT <- e$max_time
# mature (fully-retained) expected count per event time = observed final cumulative.
lambda_t <- sapply(seq_len(maxT), function(t) {
  h <- min(as.integer(e$d_star[t, 1]), cD); if (h < 0L) return(NA_real_)
  sum(inc[t, seq_len(h + 1L), 1]) })
# observed-delay set per event time (which delays carry a real snapshot)
obs_delays <- lapply(seq_len(maxT), function(t) {
  h <- min(as.integer(e$d_star[t, 1]), cD); if (h < 0L) return(integer(0))
  (0:h)[(ev[1] + (t - 1 + 0:h) * 7) %in% asof] })

cat(sprintf("Texas: maxT=%d event times, cD=%d, %d with >=1 snapshot; lambda_t range [%.0f, %.0f]\n",
            maxT, cD, sum(lengths(obs_delays) > 0), min(lambda_t, na.rm=TRUE), max(lambda_t, na.rm=TRUE)))

# ---- individual-level generative simulator ----------------------------------
# For event time t: N_t ~ Poisson(mu_t = lambda_t / p).  Each report:
#   delay r ~ gD (weeks); true w.p. p; false get retraction lag c ~ gC (>=1).
#   present at delay d  iff  r <= d  and  (true  or  r + c > d).
# Observe cumulative at snapshot delays -> cadence-interval increments z.
sim_dataset <- function(p_true, dmu, dsig, rmu, rsig) {
  gD <- mk_gD(dmu, dsig, cD); gD <- pmax(gD, 0); gD <- gD / sum(gD)
  gC <- mk_gC(rmu, rsig, cD); gC <- pmax(gC, 0)
  gCsum <- sum(gC)                       # mass within horizon; rest = "retracts beyond cD"
  out <- vector("list", maxT)
  for (t in seq_len(maxT)) {
    od <- obs_delays[[t]]; if (!length(od)) { out[[t]] <- NULL; next }
    mu <- lambda_t[t] / p_true
    N <- rpois(1, mu); if (N == 0) { out[[t]] <- list(a = c(-1L, head(od,-1L)), b = od, z = rep(0, length(od))); next }
    r <- sample.int(cD + 1L, N, replace = TRUE, prob = gD) - 1L    # delay 0..cD
    is_true <- runif(N) < p_true
    # retraction lag for false reports: 1..cD from gC[2..], else Inf (beyond horizon)
    c_lag <- rep(Inf, N)
    nf <- sum(!is_true)
    if (nf > 0) {
      draw <- sample.int(cD + 1L, nf, replace = TRUE, prob = c(gC / max(gCsum, 1e-12), max(1 - gCsum, 0))[seq_len(cD + 1L)])
      # index cD+1 sentinel -> Inf; indices 1..cD -> lag = index (since gC[idx+1] is lag idx)
      lag <- draw; lag[draw > cD] <- Inf
      c_lag[!is_true] <- lag
    }
    retract_delay <- r + c_lag                    # delay at which the false report disappears
    # cumulative present count at each observed delay d
    Cd <- sapply(od, function(d) sum(r <= d & (is_true | retract_delay > d)))
    a <- c(-1L, head(od, -1L)); z <- Cd - c(0, head(Cd, -1L))
    out[[t]] <- list(a = a, b = od, z = z)
  }
  out
}

# true generative nuisance (correctly-specified lognormal families)
DMU_TRUE <- log(0.8); DSIG_TRUE <- 0.5          # reporting delay ~ 0.8 wk mean, fast
SCEN <- list(
  A = list(rmu = log(1.5),  rsig = 1.0),        # retraction fast: mostly within horizon
  B = list(rmu = log(6),    rsig = 4.0),        # moderate tail
  C = list(rmu = log(30),   rsig = 20.0)        # long tail: most retract beyond cD
)

# theoretical retained fraction at the observation horizon: r(a) = p + (1-p) Gbar(a)
retained_at_horizon <- function(p, rmu, rsig) {
  gC <- mk_gC(rmu, rsig, cD); Gbar <- 1 - sum(gC)   # mass beyond cD = P(retract after horizon)
  c(Gbar_h = Gbar, r_h = p + (1 - p) * Gbar)
}

if (MODE == "full") {
  R    <- as.integer(Sys.getenv("PR_R", "12"))
  LOG  <- "devel/spa_diagnostics/pr_progress.log"
  cat(sprintf("PARAMETER RECOVERY  R=%d  %s\n", R, format(Sys.time())), file = LOG)
  # cells: scenario B across p (bias curve); A and C at two p (tail confounding)
  cells <- rbind(
    data.frame(scen = "B", p = c(0.40, 0.60, 0.80, 0.95)),
    data.frame(scen = "A", p = c(0.60, 0.95)),
    data.frame(scen = "C", p = c(0.60, 0.95)))
  res <- list(); k <- 0L
  for (ci in seq_len(nrow(cells))) {
    sc <- cells$scen[ci]; p0 <- cells$p[ci]; S <- SCEN[[sc]]
    rh <- retained_at_horizon(p0, S$rmu, S$rsig)
    for (r in seq_len(R)) {
      d <- sim_dataset(p0, DMU_TRUE, DSIG_TRUE, S$rmu, S$rsig)
      pr <- profile_p(d, cD, passes = 1L)
      k <- k + 1L
      res[[k]] <- data.frame(scen = sc, p_true = p0, rep = r, phat = pr$phat,
                             ci_lo = pr$ci[1], ci_hi = pr$ci[2],
                             covered = p0 >= pr$ci[1] && p0 <= pr$ci[2],
                             Gbar_h = rh["Gbar_h"], r_h = rh["r_h"])
    }
    saveRDS(do.call(rbind, res), "devel/spa_diagnostics/param_recovery.rds")  # per-cell checkpoint
    sub <- do.call(rbind, res[(k - R + 1L):k])
    cat(sprintf("[%s] scen=%s p_true=%.2f  Gbar(h)=%.3f r(h)=%.3f  | mean_phat=%.3f bias=%+.3f rmse=%.3f sd=%.3f cover=%.2f\n",
                format(Sys.time(), "%H:%M:%S"), sc, p0, rh["Gbar_h"], rh["r_h"],
                mean(sub$phat), mean(sub$phat) - p0, sqrt(mean((sub$phat - p0)^2)),
                sd(sub$phat), mean(sub$covered)), file = LOG, append = TRUE)
  }
  all <- do.call(rbind, res)
  saveRDS(all, "devel/spa_diagnostics/param_recovery.rds")
  agg <- do.call(rbind, by(all, list(all$scen, all$p_true), function(s) data.frame(
    scen = s$scen[1], p_true = s$p_true[1], Gbar_h = s$Gbar_h[1], r_h = s$r_h[1],
    mean_phat = mean(s$phat), bias = mean(s$phat) - s$p_true[1],
    rmse = sqrt(mean((s$phat - s$p_true[1])^2)), sd = sd(s$phat),
    ci_width = mean(s$ci_hi - s$ci_lo), coverage = mean(s$covered), n = nrow(s))))
  agg <- agg[order(agg$scen, agg$p_true), ]
  cat("\n=== PARAMETER RECOVERY (interval model, real Texas cadence) ===\n")
  print(agg, row.names = FALSE, digits = 3)
  cat("\nScenario retraction tails: A short, B moderate, C long (Gbar(h)=P[retract beyond horizon]).\n")
  cat("Decisive: does p_true=0.95 -> mean_phat ~ 0.95?\n")
}

if (MODE == "time1") {
  cat("\n--- timing a single simulate+fit (scenario B, p_true=0.95) ---\n")
  d <- sim_dataset(0.95, DMU_TRUE, DSIG_TRUE, SCEN$B$rmu, SCEN$B$rsig)
  nz <- sum(sapply(d, function(o) if (is.null(o)) 0 else sum(o$z != 0)))
  tot_down <- sum(sapply(d, function(o) if (is.null(o)) 0 else sum(pmin(o$z, 0))))
  cat(sprintf("simulated: %d nonzero interval increments, total down-revision = %.0f\n", nz, tot_down))
  t0 <- Sys.time()
  pr <- profile_p(d, cD)
  dt <- as.numeric(Sys.time() - t0, units = "secs")
  cat(sprintf("fit took %.1f s\n", dt))
  names(pr$ll) <- P_GRID
  cat("profile logL (rel):\n"); print(round(pr$ll - max(pr$ll), 1))
  cat(sprintf("p_hat = %.3f   95%% CI [%.3f, %.3f]\n", pr$phat, pr$ci[1], pr$ci[2]))
  cat(sprintf("\nEstimated full-MC time: %d scen-p cells x R reps x %.1f s\n", 12L, dt))
}
