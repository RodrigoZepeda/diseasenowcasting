# =============================================================================
# 40 -- MULTI-STATE, MULTI-ORIGIN SWEEP OF THE FOUR-WAY COMPARISON.
#
# Everything in FINDINGS T/U rests on ONE location (Texas) and 578 scored
# triples.  This script asks whether the two conclusions survive outside it:
#
#   1. do the models FIT everywhere -- converge, reproduce the exact-zero
#      count, pass a PIT test, land on a sane S_R(15) -- or is Texas special?
#   2. does the ORDERING hold?  On Texas it is
#
#        script 38   mean inflated by 1/(1-P0), 8-37x    WIS 1.702  +22.4%  cov .941
#        script 36   free magnitude, kappa*level^beta    WIS 1.959  +10.6%  cov .943
#        script 39   mean EXACTLY mu_t q_C(d)            WIS 2.120   +3.3%  cov .813
#        script 37   mean EXACTLY mu_t q_C(d), Skellam   WIS 2.838  -29.4%
#
#      which is the uncomfortable result that the two models whose mean is the
#      identified tex quantity forecast WORSE than the two whose mean is not.
#      One location cannot tell an ordering from an accident.
#
# All four models are refit independently at every (location, origin) and scored
# on IDENTICAL (cohort, origin, horizon) triples -- the triple set is built once
# per origin and handed to all four.  Baselines are the empirical ratio lookup
# and persistence, as in scripts 33/36/37/38/39.
#
# The model code is NOT touched.  40_core3X.R are verbatim extracts of the
# function definitions of scripts 36-39; each is sourced into its OWN
# environment because the scripts reuse names (unpack, build_fl, lmag, prof,
# make_env, ...) for different objects.  Diverging from those files would make
# the sweep untraceable to the Texas numbers it is meant to test.
#
# Per-location checkpoint to ms/<location>.rds, and a location already on disk
# is skipped, so a kill costs one location and the sweep is resumable.  Every
# location is wrapped in try(): one failure must not take the worker down.
#
# One worker:  NOT_CRAN=true SLICE=1 NWORK=12 Rscript devel/spa_diagnostics/40_multistate.R
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})

E36 <- new.env(parent = globalenv()); E37 <- new.env(parent = globalenv())
E38 <- new.env(parent = globalenv()); E39 <- new.env(parent = globalenv())
sys.source("devel/spa_diagnostics/40_core36.R", envir = E36)
sys.source("devel/spa_diagnostics/40_core37.R", envir = E37)
sys.source("devel/spa_diagnostics/40_core38.R", envir = E38)
sys.source("devel/spa_diagnostics/40_core39.R", envir = E39)

NORIG <- as.integer(Sys.getenv("NORIG", "16"))
HMAX  <- 4L
NDRAW <- 2000L
QL    <- E38$QL
STARTD <- as.Date("2023-09-23")
OUT <- Sys.getenv("OUTDIR", "devel/spa_diagnostics/ms")
dir.create(OUT, showWarnings = FALSE)
SLICE <- as.integer(Sys.getenv("SLICE", "1"))
NWORK <- as.integer(Sys.getenv("NWORK", "1"))

# starts: exactly the ones each source script used
S36 <- list(
  c(-3.5, 0.5, 2.2, -2.0, 0.6, 1.3, 0.0, log(30), 0.00, log(0.17)),
  c(-3.5, 0.5, 2.2, -2.0, 0.6, 1.3, 0.0, log(3), 0.35, log(0.3)),
  c(-2.0, 0.5, 2.5, -2.2, 1.0, 1.5, 0.2, log(60), -0.1, log(0.1)))
# FINDINGS V.3: every published start puts h0 = qlogis(0.4..0.6), i.e. "half the
# reports are eventually withdrawn", while the data want S_R(15) ~ 0.99.  From
# there script 39 reaches a degenerate basin (S_R 0.002-0.15, size ~ 0.01) whose
# likelihood is 55-212 nats WORSE than the optimum a high-retention start finds.
# One such start is therefore added to every model that carries h_R -- 37, 38 and
# 39 alike, so no model gains a start the others do not.
HIRET <- stats::qlogis(0.02)
S37 <- list(c(-1, log(0.5), stats::qlogis(0.5), rep(0, 14)),
            c(-1, log(0.5), HIRET, rep(0, 14)))
S38 <- list(
  c(-1, log(0.5), stats::qlogis(0.5), rep(0, 14), 2.2, -2.0, 0.6, log(0.2)),
  c(-0.3, log(0.8), stats::qlogis(0.4), rep(0, 14), 2.0, -1.8, 0.6, log(0.1)),
  c(-2.0, log(0.4), stats::qlogis(0.6), rep(0, 14), 2.4, -2.2, 0.8, log(0.4)),
  c(-1, log(0.5), HIRET, rep(0, 14), 2.2, -2.0, 0.6, log(0.2)))
S39 <- list(
  c(-1, log(0.5), stats::qlogis(0.5), rep(0, 14), -1.0, 0.5, -0.3, log(0.2)),
  c(-0.3, log(0.8), stats::qlogis(0.4), rep(0, 14), -2.0, 0.8, -0.5, log(0.1)),
  c(-2.0, log(0.4), stats::qlogis(0.6), rep(0, 14),  0.0, 0.3, -0.2, log(0.5)),
  c(-1, log(0.5), HIRET, rep(0, 14), -1.0, 0.5, -0.3, log(0.2)))

wis <- E38$wis

# A diverged forward simulation shows up as NaN draws (script 36 can send its
# magnitude mean to Inf under warm-starting, which makes qnbinom NaN).  Turn
# that into a clean error so the origin can be dropped, rather than letting a
# NaN reach quantile() and take the whole location down.
qsafe <- function(v) {
  if (anyNA(v) || any(!is.finite(v))) stop("non-finite draws")
  as.numeric(stats::quantile(v, QL, names = FALSE))
}

# ---- a zero-truncated NB draw that cannot hang -------------------------------
# The published simulators draw the ZTNB by CDF inversion,
# qnbinom(runif(P0, 1), size, mu).  qnbinom searches upward, so its cost grows
# with mu: FINDINGS V.5 measured 119 s per 2000 draws at mu = 1e7, size = 0.5 --
# enough for one triple to stall a whole location.  Rejection sampling (draw NB,
# redraw the zeros) yields the SAME distribution at cost 1/(1-P0), and the two
# are cheap in complementary regimes: inversion is slow only when P0 is small,
# which is exactly when rejection accepts on the first try.  Switching on P0
# therefore removes the cliff without changing the distribution sampled.
rztnb <- function(n, size, mu) {
  if (n == 0L) return(numeric(0))
  mu <- rep_len(as.numeric(mu), n); size <- rep_len(as.numeric(size), n)
  p0 <- stats::dnbinom(0, size = size, mu = mu)
  out <- numeric(n)
  rej <- p0 < 0.5                       # expected <= 2 draws
  if (any(rej)) {
    todo <- which(rej)
    for (k in 1:100) {                  # 0.5^100 residual: never taken
      x <- stats::rnbinom(length(todo), size = size[todo], mu = mu[todo])
      ok <- x > 0
      out[todo[ok]] <- x[ok]
      todo <- todo[!ok]
      if (!length(todo)) break
    }
    if (length(todo)) out[todo] <- 1
  }
  if (any(!rej)) {                      # P0 >= 0.5: mass near 0, inversion cheap
    i <- which(!rej)
    out[i] <- stats::qnbinom(stats::runif(length(i), p0[i], 1),
                             size = size[i], mu = mu[i])
  }
  out
}

# ---- forward simulators, from each source script's PART B --------------------
# Identical to the originals except that the ZTNB draw goes through rztnb().
sim36 <- function(C0, state0, ages, P, n) {
  cur <- rep(as.numeric(C0), n); st <- rep(as.numeric(state0), n)
  for (ag in ages) {
    pi_ <- stats::plogis(P$p0 + P$p1 * log1p(ag) + P$gam * st)
    th_ <- stats::plogis(P$t0 + P$t1 * log1p(ag))
    mv <- stats::runif(n) < pi_
    if (any(mv)) {
      m <- exp(P$lkap + P$beta * log(pmax(cur[mv], 1)))
      M <- rztnb(sum(mv), P$size, m)
      cur[mv] <- pmax(cur[mv] + ifelse(stats::runif(sum(mv)) < th_, 1, -1) * M, 0)
    }
    st <- as.numeric(mv)
  }
  cur
}

# 37 draws the whole span in one Skellam step, as in 37_tex_model.R
sim37 <- function(C0, mu_t, a0, a_star, gD, hR, n) {
  Q <- E37$q_pairs(gD, hR, a0, a_star)
  pmax(C0 + stats::rpois(n, mu_t * Q$qp) - stats::rpois(n, mu_t * Q$qm), 0)
}
sim38 <- function(C0, st0, mu_t, ages, P, n) {
  cur <- rep(as.numeric(C0), n); st <- rep(as.numeric(st0), n)
  for (g in ages) {
    Q <- E38$q_pairs(P$gD, P$hR, g - 1L, g)
    a_ <- max(mu_t * Q$qp, 1e-10); o_ <- max(mu_t * Q$qm, 1e-10)
    th_ <- a_ / (a_ + o_)
    pi_ <- stats::plogis(E38$lin_pi(P, g, st))
    mM_ <- pmax((a_ + o_) / pmax(pi_, 1e-8), 1e-8)
    mv <- stats::runif(n) < pi_
    if (any(mv)) {
      M <- rztnb(sum(mv), P$size, mM_[mv])
      cur[mv] <- pmax(cur[mv] + ifelse(stats::runif(sum(mv)) < th_, 1, -1) * M, 0)
    }
    st <- as.numeric(mv)
  }
  cur
}
sim39 <- function(C0, st0, mu_t, ages, P, n) {
  cur <- rep(as.numeric(C0), n); st <- rep(as.numeric(st0), n)
  for (g in ages) {
    Q <- E39$q_pairs(P$gD, P$hR, g - 1L, g)
    a_ <- max(mu_t * Q$qp, 1e-10); o_ <- max(mu_t * Q$qm, 1e-10)
    th_ <- a_ / (a_ + o_)
    D <- E39$derive(P, g, st, a_ + o_)
    mv <- stats::runif(n) < D$pi
    if (any(mv)) {
      M <- rztnb(sum(mv), P$size, D$m[mv])
      cur[mv] <- pmax(cur[mv] + ifelse(stats::runif(sum(mv)) < th_, 1, -1) * M, 0)
    }
    st <- as.numeric(mv)
  }
  cur
}

# ---- in-sample gates, one row each for the two hurdle models ----------------
# 38 and 39 differ only in how (pi, m) are obtained, so one routine takes them
# both: `dispatch` returns per-cell pi, theta and the NB parent mean m.
gates_hurdle <- function(fl, ft, EE, dispatch, tag) {
  pr <- ft$env$prof(ft$par); P <- pr$P; mu <- pr$mu
  Qf <- EE$q_pairs(P$gD, P$hR, fl$a, fl$b)
  mu_i <- mu[fl$grp]
  al <- pmax(mu_i * Qf$qp, 1e-10); om <- pmax(mu_i * Qf$qm, 1e-10)
  th_i <- al / (al + om)
  D <- dispatch(P, fl$b, fl$pm, al + om)
  pi_i <- D$pi; m_i <- D$m
  k <- which(fl$post)
  # realised increment mean vs the tex quantity alpha - omega: this is the whole
  # point of 39 over 38, so measure it rather than assume it.
  EM <- m_i / (1 - stats::dnbinom(0, size = P$size, mu = m_i))
  mean_err <- abs(pi_i[k] * EM[k] * (2 * th_i[k] - 1) - (al[k] - om[k]))
  infl <- EM[k] / pmax(al[k] + om[k], 1e-12) * pi_i[k]   # 1 iff mean preserved
  cdf_lower <- function(i, z) {
    if (z > 0) return(1 - pi_i[i] * th_i[i] * EE$Smag_nb(z - 1, m_i[i], P$size))
    if (z == 0) return(pi_i[i] * (1 - th_i[i]))
    pi_i[i] * (1 - th_i[i]) * EE$Smag_nb(-z, m_i[i], P$size)
  }
  pmf1 <- function(i, z) {
    if (z == 0) return(1 - pi_i[i])
    if (z > 0) return(pi_i[i] * th_i[i] * exp(EE$lmag(z, m_i[i], P$size)))
    pi_i[i] * (1 - th_i[i]) * exp(EE$lmag(-z, m_i[i], P$size))
  }
  U <- vapply(k, function(i) cdf_lower(i, fl$z[i]) + stats::runif(1) * pmf1(i, fl$z[i]), 1.0)
  U <- U[is.finite(U)]
  ks <- suppressWarnings(stats::ks.test(U, "punif"))
  tail_ratio <- function(thr) {
    e <- sum(pi_i[k] * EE$Smag_nb(thr, m_i[k], P$size))
    sum(abs(fl$z[k]) > thr) / max(e, 1e-9)
  }
  data.frame(
    model = tag, logL = ft$logL, npost = length(k),
    zero_obs = mean(fl$z[k] == 0), zero_mod = mean(1 - pi_i[k]),
    up_obs = mean(fl$z[k][fl$z[k] != 0] > 0), up_mod = mean(th_i[k]),
    SR15 = 1 - sum(P$hR), gD0 = P$gD[1], gDmean = sum((seq_along(P$gD) - 1) * P$gD),
    size = P$size, pi_med = stats::median(pi_i[k]), pi_clamped = mean(pi_i[k] >= 1 - 1e-9),
    mean_err_max = max(mean_err), infl_med = stats::median(infl),
    infl_max = max(infl),
    ks_D = as.numeric(ks$statistic), ks_p = ks$p.value,
    pit_out95 = mean(U < 0.025 | U > 0.975),
    tail20 = tail_ratio(20), tail50 = tail_ratio(50), tail100 = tail_ratio(100),
    stringsAsFactors = FALSE)
}
disp38 <- function(P, age, pm, tot) {
  pi_ <- stats::plogis(E38$lin_pi(P, age, pm))
  list(pi = pi_, m = pmax(tot / pmax(pi_, 1e-8), 1e-8))
}
disp39 <- function(P, age, pm, tot) {
  D <- E39$derive(P, age, pm, tot); list(pi = D$pi, m = D$m)
}

# A warm start is a SPEED-UP, not a commitment.  FINDINGS V.4: the first sweep
# seeded each origin chain from a fit to the whole series and then chained
# warm-to-warm, so one bad fit became an absorbing state -- West Virginia lost 26
# of 32 origins, and re-seeding from a cold start recovered 32/32.  Here the warm
# fit is accepted only if it is usable; otherwise we fall back to the cold starts
# and keep the best logL.  "Usable" means the fit converged and its delay and
# retention masses are finite -- deliberately NOT a threshold on S_R(15), which
# would prejudge the retention question the sweep exists to answer (script 37
# legitimately fits S_R(15) ~ 0.47 on Texas).
fit_origin <- function(fitfun, fl, warm, colds, masses) {
  f <- if (is.null(warm)) NULL else try(fitfun(fl, warm), silent = TRUE)
  usable <- function(g) {
    if (is.null(g) || inherits(g, "try-error")) return(FALSE)
    # The objectives return 1e12 as a FAILURE SENTINEL, so a failed fit comes
    # back as logL = -1e12 -- which is_finite() happily accepts.  This is how 64
    # non-converged script-37 fits reached the simulator in the first sweep.
    if (!is.finite(g$logL) || g$logL < -1e11) return(FALSE)
    m <- try(masses(g), silent = TRUE)
    if (inherits(m, "try-error")) return(FALSE)
    # prof() returns NULL on failure, so masses() comes back NULL/empty -- and
    # all(is.finite(NULL)) is vacuously TRUE.  Length must be checked FIRST.
    length(m) > 0 && all(is.finite(m))
  }
  if (usable(f)) { f$cold <- FALSE; return(f) }
  best <- NULL
  for (st in colds) {
    g <- try(fitfun(fl, st), silent = TRUE)
    if (usable(g) && (is.null(best) || g$logL > best$logL)) best <- g
  }
  if (!is.null(best)) best$cold <- TRUE
  best
}
# what each model must produce finitely for the simulators to work
m36 <- function(g) { P <- E36$unpack(g$par, g$H)
  c(P$gD, P$size, exp(P$lkap + P$beta * log(1e5))) }   # also catches the qnbinom cliff
m37 <- function(g) { p <- g$env$prof(g$par)
  if (is.null(p)) return(numeric(0)); c(p$gD, p$hR, p$mu) }
m38 <- function(g) { p <- g$env$prof(g$par)
  if (is.null(p) || is.null(p$P)) return(numeric(0)); c(p$P$gD, p$P$hR, p$mu, p$P$size) }
m39 <- m38

best_of <- function(fitfun, fl, starts) {
  b <- NULL
  for (st in starts) {
    ftt <- try(fitfun(fl, st), silent = TRUE)
    if (!inherits(ftt, "try-error") && is.finite(ftt$logL) &&
        (is.null(b) || ftt$logL > b$logL)) b <- ftt
  }
  b
}

# ---- one location ------------------------------------------------------------
run_loc <- function(loc) {
  t0 <- Sys.time()
  raw_all <- tbl.now::flusight |>
    filter(location_name == loc, !is.na(observation)) |>
    mutate(d = floor(as.numeric(as_of - target_end_date) / 7)) |>
    arrange(as_of, target_end_date)
  fl <- E38$build_fl(raw_all |> filter(target_end_date >= STARTD))

  b36 <- best_of(E36$fit_one, fl, S36)
  b37 <- best_of(E37$fit_tex, fl, S37)
  b38 <- best_of(E38$fit_m,   fl, S38)
  b39 <- best_of(E39$fit_m,   fl, S39)
  if (is.null(b36) || is.null(b37) || is.null(b38) || is.null(b39))
    stop("in-sample fit failed")

  G <- rbind(gates_hurdle(fl, b38, E38, disp38, "38"),
             gates_hurdle(fl, b39, E39, disp39, "39"))
  G$location <- loc; G$nTg <- fl$nTg; G$nObs <- fl$nObs
  G$logL36 <- b36$logL; G$logL37 <- b37$logL

  # ---- rolling origins ------------------------------------------------------
  snaps <- sort(unique(raw_all$as_of))
  origins <- utils::head(utils::tail(snaps, NORIG + HMAX), NORIG)
  rows <- list(); ologs <- list()
  # Seed the chain from NOTHING, so origin 1 multi-starts on its OWN training
  # data.  The first sweep seeded from b*$par -- a fit to the whole series,
  # including snapshots after every origin -- which both leaked future data into
  # the origin-1 fit and started thin-data locations in a region they never left.
  w36 <- w37 <- w38 <- w39 <- NULL
  for (s0 in origins) {
    tr  <- raw_all |> filter(as_of <= s0)
    flb <- E38$build_fl(tr)
    f36 <- fit_origin(E36$fit_one, flb, w36, S36, m36)
    f37 <- fit_origin(E37$fit_tex, flb, w37, S37, m37)
    f38 <- fit_origin(E38$fit_m,   flb, w38, S38, m38)
    f39 <- fit_origin(E39$fit_m,   flb, w39, S39, m39)
    if (is.null(f36) || is.null(f37) || is.null(f38) || is.null(f39)) {
      ologs[[length(ologs) + 1L]] <- data.frame(location = loc, origin = s0,
        logL36 = NA_real_, logL37 = NA_real_, logL38 = NA_real_, logL39 = NA_real_,
        SR15_38 = NA_real_, SR15_39 = NA_real_, scored = 0L,
        skipped = "fit failed", cold36 = NA, cold37 = NA, cold38 = NA, cold39 = NA,
        stringsAsFactors = FALSE)
      next
    }
    w36 <- f36$par; w37 <- f37$par; w38 <- f38$par; w39 <- f39$par
    P36 <- E36$unpack(f36$par, f36$H)
    p37 <- f37$env$prof(f37$par)
    p38 <- f38$env$prof(f38$par); P38 <- p38$P
    p39 <- f39$env$prof(f39$par); P39 <- p39$P
    ev <- unique((tr |> filter(d <= 15, d >= 0))$target_end_date)

    # the triple set, built ONCE and handed to all four models
    cur <- raw_all |> filter(as_of == s0, d >= 0, d <= 15) |>
      select(target_end_date, a0 = d, C0 = observation) |> filter(C0 > 0)
    st0 <- raw_all |> filter(as_of <= s0, d >= 0, d <= 15) |>
      group_by(target_end_date) |> arrange(d, .by_group = TRUE) |>
      summarise(st = if (n() >= 3) as.numeric(tail(diff(observation), 1) != 0) else 0,
                .groups = "drop")
    cur <- cur |> left_join(st0, by = "target_end_date") |>
      mutate(st = ifelse(is.na(st), 0, st))

    # empirical ratio lookup, memoised per (a0, a_star): the source scripts
    # recomputed it once per triple, which is the same answer at 60x the cost.
    trw <- tr |> filter(d >= 0, d <= 15) |> select(target_end_date, d, observation)
    span_tab <- new.env(parent = emptyenv())
    get_spans <- function(i, j) {
      key <- paste0(i, "_", j)
      if (!is.null(span_tab[[key]])) return(span_tab[[key]])
      A <- trw |> filter(d == i) |> select(target_end_date, oi = observation)
      B <- trw |> filter(d == j) |> select(target_end_date, oj = observation)
      s <- A |> inner_join(B, by = "target_end_date") |> filter(oi > 0) |>
        mutate(rr = oj / oi) |> pull(rr)
      s <- s[is.finite(s)]
      assign(key, s, envir = span_tab); s
    }

    fut <- snaps[snaps > s0][seq_len(HMAX)]
    n_before <- length(rows)
    sc <- try({
    for (h in seq_along(fut)) {
      s_star <- fut[h]; if (is.na(s_star)) next
      tgt <- raw_all |> filter(as_of == s_star) |> select(target_end_date, truth = observation)
      jj <- cur |> inner_join(tgt, by = "target_end_date") |>
        mutate(a_star = as.integer(floor(as.numeric(s_star - target_end_date) / 7))) |>
        filter(a_star <= 15, a_star > a0)
      if (!nrow(jj)) next
      for (r in seq_len(nrow(jj))) {
        kk <- match(jj$target_end_date[r], ev); if (is.na(kk)) next
        ages <- (jj$a0[r] + 1L):jj$a_star[r]
        q36 <- qsafe(sim36(jj$C0[r], jj$st[r], ages, P36, NDRAW))
        q37 <- qsafe(sim37(jj$C0[r], p37$mu[kk], jj$a0[r], jj$a_star[r],
                           p37$gD, p37$hR, NDRAW))
        q38 <- qsafe(sim38(jj$C0[r], jj$st[r], p38$mu[kk], ages, P38, NDRAW))
        q39 <- qsafe(sim39(jj$C0[r], jj$st[r], p39$mu[kk], ages, P39, NDRAW))
        spans <- get_spans(jj$a0[r], jj$a_star[r])
        q_em <- if (length(spans) >= 5)
          as.numeric(stats::quantile(jj$C0[r] * spans, QL, names = FALSE))
        else rep(jj$C0[r], length(QL))
        y <- jj$truth[r]; qp <- rep(jj$C0[r], length(QL))
        rows[[length(rows) + 1L]] <- data.frame(
          location = loc, origin = s0, h = h, a0 = jj$a0[r], a_star = jj$a_star[r],
          C0 = jj$C0[r], truth = y,
          wis36 = wis(q36, y), wis37 = wis(q37, y), wis38 = wis(q38, y),
          wis39 = wis(q39, y), wis_emp = wis(q_em, y), wis_persist = wis(qp, y),
          cov90_36 = as.numeric(y >= q36[3] & y <= q36[21]),
          cov90_37 = as.numeric(y >= q37[3] & y <= q37[21]),
          cov90_38 = as.numeric(y >= q38[3] & y <= q38[21]),
          cov90_39 = as.numeric(y >= q39[3] & y <= q39[21]),
          cov90_emp = as.numeric(y >= q_em[3] & y <= q_em[21]),
          cov50_38 = as.numeric(y >= q38[8] & y <= q38[16]),
          cov50_39 = as.numeric(y >= q39[8] & y <= q39[16]),
          stringsAsFactors = FALSE)
      }
    }
    TRUE }, silent = TRUE)
    # Failure policy: if ANY model's simulation diverges at this origin, drop the
    # origin for ALL FOUR.  Scoring the survivors would leave the four WIS columns
    # resting on different triple sets, which is the confound the matched-triple
    # design exists to prevent.
    if (inherits(sc, "try-error")) {
      # DUMP=1 saves the exact state that failed, so a scoring divergence can be
      # reproduced offline instead of guessed at.
      if (nzchar(Sys.getenv("DUMP"))) {
        dd <- file.path(OUT, sprintf("DUMP_%s_%s.rds", gsub("[^A-Za-z0-9]","_",loc), as.character(s0)))
        saveRDS(list(loc = loc, origin = s0, err = as.character(sc), cur = cur, ev = ev,
                     P36 = P36, p37 = p37, P38 = P38, P39 = P39,
                     mu37 = p37$mu, mu38 = p38$mu, mu39 = p39$mu,
                     fut = fut, snaps = snaps, flb_b = flb$b, flb_nTg = flb$nTg), dd)
      }
      if (length(rows) > n_before) rows <- rows[seq_len(n_before)]
      ologs[[length(ologs) + 1L]] <- data.frame(location = loc, origin = s0,
        logL36 = f36$logL, logL37 = f37$logL, logL38 = f38$logL, logL39 = f39$logL,
        SR15_38 = 1 - sum(P38$hR), SR15_39 = 1 - sum(P39$hR), scored = 0L,
        skipped = gsub("\n", " ", as.character(sc)),
        cold36 = f36$cold, cold37 = f37$cold, cold38 = f38$cold, cold39 = f39$cold,
        stringsAsFactors = FALSE)
      next
    }
    ologs[[length(ologs) + 1L]] <- data.frame(location = loc, origin = s0,
      logL36 = f36$logL, logL37 = f37$logL, logL38 = f38$logL, logL39 = f39$logL,
      SR15_38 = 1 - sum(P38$hR), SR15_39 = 1 - sum(P39$hR),
      scored = length(rows) - n_before, skipped = NA_character_,
      cold36 = f36$cold, cold37 = f37$cold, cold38 = f38$cold, cold39 = f39$cold,
      stringsAsFactors = FALSE)
  }
  list(location = loc, gates = G, origins = do.call(rbind, ologs),
       R = if (length(rows)) do.call(rbind, rows) else NULL,
       par = list(p36 = b36$par, p37 = b37$par, p38 = b38$par, p39 = b39$par),
       mins = as.numeric(difftime(Sys.time(), t0, units = "mins")))
}

# ---- worker ------------------------------------------------------------------
locs <- sort(unique(tbl.now::flusight$location_name))
mine <- locs[seq(SLICE, length(locs), by = NWORK)]
cat(sprintf("worker %d/%d: %d locations | NORIG=%d\n", SLICE, NWORK, length(mine), NORIG))
for (loc in mine) {
  f <- file.path(OUT, paste0(gsub("[^A-Za-z0-9]", "_", loc), ".rds"))
  if (file.exists(f)) { cat(sprintf("skip %s\n", loc)); flush.console(); next }
  set.seed(20260903 + match(loc, locs))
  res <- try(run_loc(loc), silent = TRUE)
  if (inherits(res, "try-error")) {
    cat(sprintf("FAIL %s: %s\n", loc, gsub("\n", " ", as.character(res))))
    saveRDS(list(location = loc, error = as.character(res)), f)
  } else {
    saveRDS(res, f)
    R <- res$R
    sk <- function(v) if (is.null(R)) NA_real_ else 100 * (1 - mean(v) / mean(R$wis_emp))
    cat(sprintf("done %-22s %5.1f min | %4d triples | skill 38 %+6.1f%%  36 %+6.1f%%  39 %+6.1f%%  37 %+6.1f%%\n",
                loc, res$mins, if (is.null(R)) 0L else nrow(R),
                sk(R$wis38), sk(R$wis36), sk(R$wis39), sk(R$wis37)))
  }
  flush.console()
}
cat("worker done\n")
