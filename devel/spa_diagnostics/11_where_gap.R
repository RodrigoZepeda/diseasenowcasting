# =============================================================================
# Section 9: WHERE do the 14,275 nats come from?
# =============================================================================
# A very large composite-likelihood gap can be a modest per-cell preference
# repeated thousands of times, or a few cells with catastrophic probability.  Those
# have completely different implications, so decompose it.
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})

START <- as.Date("2023-09-23"); STATE <- "Texas"
raw <- tbl.now::flusight |>
  filter(location_name == STATE, target_end_date >= START, as_of >= START) |>
  filter(as_of <= max(target_end_date))
tn <- tbl_now(raw, event_date = target_end_date, report_date = as_of,
              case_count = observation, data_type = "count-cumulative",
              verbose = FALSE) |> align_weeks(date_col = "report_date")

cells_of <- function(p_spec) {
  f <- suppressMessages(suppressWarnings(nowcast(
    tn, model(poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
              validation = validation_process(p = p_spec)),
    type = "one_stage", n_draws = 20, temporal_effects = "none", seed = 1)))
  fit <- f@fits[[1]]; e <- f@engine; pr <- f@priors
  cD <- min(as.integer(e$max_conf_delay) - 1L, 15L)
  p <- if (isTRUE(pr$confirm_p$is_constant == 1L)) pr$confirm_p$fixed
       else stats::plogis(as.numeric(fit$parList$logit_confirm_p))
  dfns <- diseasenowcasting:::.delay_distribution_functions(
    as.integer(e$delay_family), as.numeric(fit$delay_mu), as.numeric(fit$delay_sigma))
  acdf <- as.numeric(dfns$cdf(seq_len(cD + 1L))); gD <- c(acdf[1], diff(acdf))
  rmu <- as.numeric(fit$parList$retract_mu)
  rsd <- 0.01 + exp(as.numeric(fit$parList$log_retract_sd_exc))
  rfns <- diseasenowcasting:::.delay_distribution_functions(as.integer(pr$retract_family), rmu, rsd)
  gC <- c(0, diff(c(0, as.numeric(rfns$cdf(seq_len(cD))))))
  gW <- diseasenowcasting:::.convolve_delays(gD, gC)
  mu <- as.numeric(fit$lambda) / p; eta <- (1 - p) * mu
  out <- list()
  for (t in seq_len(e$max_time)) {
    h <- min(as.integer(e$d_star[t, 1]), cD); if (h < 0L) next
    for (d in 0:h) out[[length(out) + 1L]] <- c(
      z = e$increment_array[t, d + 1L, 1], d = d,
      a = mu[t] * gD[d + 1L], w = eta[t] * gW[d + 1L])
  }
  frame <- as.data.frame(do.call(rbind, out))
  frame$ll <- vapply(seq_len(nrow(frame)), function(i)
    diseasenowcasting:::.log_skellam_increment(frame$z[i], frame$a[i], frame$w[i],
                                               if (frame$d[i] == 0L) 0L else 1L), numeric(1))
  attr(frame, "p") <- p
  frame
}

low  <- cells_of(beta_prior(9.6, 0.4))
high <- cells_of(0.9572)

cat(sprintf("\nlow p = %.4f    high p = %.4f\n", attr(low, "p"), attr(high, "p")))
cat("\n=== observed cell composition ===\n")
cat(sprintf("  z = 0 : %4d cells (%.1f%%)\n", sum(low$z == 0), 100*mean(low$z == 0)))
cat(sprintf("  z > 0 : %4d cells (%.1f%%)   max z = %.0f\n",
            sum(low$z > 0), 100*mean(low$z > 0), max(low$z)))
cat(sprintf("  z < 0 : %4d cells (%.1f%%)   min z = %.0f\n",
            sum(low$z < 0), 100*mean(low$z < 0), min(low$z)))

cat("\n=== log-likelihood by cell type ===\n")
cat(sprintf("%-8s %6s %14s %14s %14s %12s\n",
            "type", "n", "logL low-p", "logL high-p", "low - high", "per cell"))
total <- 0
for (grp in list(list("z = 0", low$z == 0), list("z > 0", low$z > 0), list("z < 0", low$z < 0))) {
  idx <- grp[[2]]
  a <- sum(low$ll[idx]); b <- sum(high$ll[idx])
  total <- total + (a - b)
  cat(sprintf("%-8s %6d %14.1f %14.1f %14.1f %12.3f\n",
              grp[[1]], sum(idx), a, b, a - b, (a - b) / sum(idx)))
}
cat(sprintf("%-8s %6d %14.1f %14.1f %14.1f\n", "TOTAL", nrow(low),
            sum(low$ll), sum(high$ll), total))

cat("\n=== how well does each solution predict P(z = 0)? ===\n")
p0 <- function(fr) {
  mean(exp(vapply(seq_len(nrow(fr)), function(i)
    diseasenowcasting:::.log_skellam_increment(0, fr$a[i], fr$w[i],
                                               if (fr$d[i] == 0L) 0L else 1L), numeric(1))))
}
cat(sprintf("  observed share of exact zeros : %.3f\n", mean(low$z == 0)))
cat(sprintf("  mean model P(z=0), low-p      : %.3f\n", p0(low)))
cat(sprintf("  mean model P(z=0), high-p     : %.3f\n", p0(high)))

cat("\n=== the 10 cells contributing most to the gap ===\n")
gap <- low$ll - high$ll
ord <- order(-abs(gap))[1:10]
cat(sprintf("%8s %4s %10s %10s %10s %10s %12s\n","z","d","a low","w low","a high","w high","low - high"))
for (i in ord) cat(sprintf("%8.0f %4.0f %10.1f %10.1f %10.2f %10.2f %12.1f\n",
    low$z[i], low$d[i], low$a[i], low$w[i], high$a[i], high$w[i], gap[i]))
cat(sprintf("\n  top 10 cells account for %.1f%% of the total gap\n",
            100 * sum(gap[ord]) / sum(gap)))
