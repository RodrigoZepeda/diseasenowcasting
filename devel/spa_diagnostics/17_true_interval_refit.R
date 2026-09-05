# =============================================================================
# TRUE profile in p under the interval likelihood: nuisance parameters
# re-optimised UNDER the interval objective, not inherited from the point fit.
# =============================================================================
# Nuisance treatment, identical for both objectives so the comparison is fair:
#   * lambda_t saturated -- one free value per event time, profiled out by 1-D
#     optimisation.  The interval likelihood for event time t depends only on
#     lambda_t given (p, g_D, g_C), so this is separable and exact, and it is MORE
#     flexible than the AR1 process, so it cannot handicap either objective.
#   * outer optimisation over (delay_mu, delay_sigma, retract_mu, retract_sigma).
#
# Observations:
#   point    -- (d-1, d] for every d in 0..h                  (what the package does)
#   interval -- (d_prev, d_next] over delays carrying a SNAPSHOT   (cadence-given)
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})

# Vectorised Skellam log-pmf: saddlepoint for the mixed case, exact Poisson when a
# stream is structurally absent.  Matches the production scalar to ~1e-5.
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

# Observation sets: list per event time of (a, b, z).
make_obs <- function(kind) {
  out <- vector("list", e$max_time)
  for (t in seq_len(e$max_time)) {
    h <- min(as.integer(e$d_star[t, 1]), cD)
    if (h < 0L) { out[[t]] <- NULL; next }
    cum <- cumsum(inc[t, seq_len(h + 1L), 1])
    if (kind == "point") {
      a <- (0:h) - 1L; b <- 0:h; z <- inc[t, seq_len(h + 1L), 1]
    } else {
      obs_d <- (0:h)[(ev[1] + (t - 1 + 0:h) * 7) %in% asof]
      if (!length(obs_d)) { out[[t]] <- NULL; next }
      a <- c(-1L, head(obs_d, -1L)); b <- obs_d
      z <- cum[obs_d + 1L] - c(0, cum[head(obs_d, -1L) + 1L])
    }
    out[[t]] <- list(a = a, b = b, z = z)
  }
  out
}
OBS <- list(point = make_obs("point"), interval = make_obs("interval"))

q_tab <- function(gD, gC, p) {
  GC <- cumsum(gC)
  Gf <- function(k) if (k < 0) 0 else GC[min(k, length(GC) - 1L) + 1L]
  Gb <- function(k) if (k < 0) 1 else 1 - GC[min(k, length(GC) - 1L) + 1L]
  qp <- qm <- matrix(0, cD + 2L, cD + 1L)      # rows a+2 (a from -1), cols b+1
  # NB: `lo:cD` counts DOWN when lo > cD, which silently produces an out-of-range
  # b.  Guard it rather than relying on the colon operator.
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

# Total log-likelihood with lambda_t profiled out, for one parameter vector.
neg_loglik <- function(theta, p_val, obs) {
  par <- theta; p <- p_val
  dmu <- par[1]; dsig <- 0.01 + exp(par[2]); rmu <- par[3]; rsig <- 0.01 + exp(par[4])
  dfns <- diseasenowcasting:::.delay_distribution_functions(1L, dmu, dsig)
  acdf <- as.numeric(dfns$cdf(seq_len(cD + 1L))); gD <- c(acdf[1], diff(acdf))
  rfns <- diseasenowcasting:::.delay_distribution_functions(1L, rmu, rsig)
  gC <- c(0, diff(c(0, as.numeric(rfns$cdf(seq_len(cD))))))
  if (any(!is.finite(gD)) || any(!is.finite(gC))) return(1e12)
  Q <- q_tab(gD, gC, p)
  total <- 0
  for (t in seq_along(obs)) {
    o <- obs[[t]]; if (is.null(o)) next
    qp <- Q$qp[cbind(o$a + 2L, o$b + 1L)]; qm <- Q$qm[cbind(o$a + 2L, o$b + 1L)]
    zz <- o$z
    nll_t <- function(loglam) {
      mu <- exp(loglam) / p
      -sum(vskel(zz, mu * qp, mu * qm))
    }
    start <- log(max(sum(pmax(zz, 0)), 1))
    fit <- stats::optimize(nll_t, interval = c(start - 6, start + 6), tol = 1e-4)
    total <- total + fit$objective
  }
  if (!is.finite(total)) 1e12 else total
}

P_GRID <- c(0.10, 0.20, 0.40, 0.60, 0.80, 0.90, 0.96, 0.99)
start_par <- c(0, log(0.5), 0, log(0.5))
rows <- list()
for (kind in c("point", "interval")) {
  for (p_fixed in P_GRID) {
    fit <- stats::optim(par = start_par, fn = neg_loglik, p_val = p_fixed,
                        obs = OBS[[kind]], method = "Nelder-Mead",
                        control = list(maxit = 600, reltol = 1e-9))
    dmu <- fit$par[1]; rmu <- fit$par[3]
    rsig <- 0.01 + exp(fit$par[4])
    rfns <- diseasenowcasting:::.delay_distribution_functions(1L, rmu, rsig)
    gC1 <- diff(c(0, as.numeric(rfns$cdf(1L))))[1]
    rows[[length(rows) + 1L]] <- data.frame(
      kind = kind, p = p_fixed, logL = -fit$value,
      gD_median = exp(dmu), gC_median = exp(rmu), gC1 = gC1, conv = fit$convergence)
    cat(sprintf("%-9s p=%.2f  logL=%11.1f  gD_med=%7.3f  gC_med=%7.3f  gC(1)=%.4f\n",
                kind, p_fixed, -fit$value, exp(dmu), exp(rmu), gC1))
  }
}
tab <- do.call(rbind, rows)
cat("\n=== TRUE profile in p (nuisance re-optimised under each objective) ===\n")
for (kind in c("point", "interval")) {
  sub <- tab[tab$kind == kind, ]; sub$rel <- sub$logL - max(sub$logL)
  cat(sprintf("\n-- %s --  optimum p = %.2f\n", kind, sub$p[which.max(sub$logL)]))
  print(sub[, c("p", "rel", "gD_median", "gC_median", "gC1")], row.names = FALSE, digits = 5)
}
saveRDS(tab, "devel/spa_diagnostics/true_interval_profile.rds")
