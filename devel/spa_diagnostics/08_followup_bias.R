# =============================================================================
# Step 6: how far above the asymptotic p does p_empirical sit? (their section 14)
# =============================================================================
# A report followed for `a` periods is still present with probability
# p + (1 - p) Gbar_C(a), so a finite-horizon retained fraction OVERSTATES p.  The
# diagnostic is the retained fraction as a function of MINIMUM follow-up age:
#
#   r(a) = sum_t C_t(a) / sum_t [ C_t(a) + (retractions observed by delay a) ]
#
# Gross appearances by delay a = what is still on the books at a, plus everything
# already withdrawn by then.  If g_C is proper -- which section 5 established it
# is -- r(a) must approach p from ABOVE and flatten.
# =============================================================================
suppressMessages({library(dplyr); library(tbl.now)
                  pkgload::load_all("/Users/rodzepeda/Documents/dcast3", quiet = TRUE)})

START <- as.Date("2023-09-23")
STATES <- c("Texas", "California", "New York", "Florida")

for (state in STATES) {
  raw <- tbl.now::flusight |>
    filter(location_name == state, target_end_date >= START, as_of >= START) |>
    filter(as_of <= max(target_end_date))
  tn <- tbl_now(raw, event_date = target_end_date, report_date = as_of,
                case_count = observation, data_type = "count-cumulative",
                verbose = FALSE) |> align_weeks(date_col = "report_date")
  mdl <- model(poisson_likelihood(), ar1_epidemic(), lognormal_delay(),
               validation = validation_process(p = 0.95))
  eng <- prepare_from_tbl_now(tn, mdl, now = tbl.now::get_now(tn))$data
  inc <- eng$increment_array; d_star <- eng$d_star
  conf_D <- dim(inc)[2] - 1L

  cat(sprintf("\n=== %s ===\n", state))
  cat(sprintf("%6s %10s %14s %14s %10s %8s\n",
              "age a", "n_events", "on books C(a)", "withdrawn<=a", "r(a)", "cells"))
  prev <- NA_real_
  for (a in 0:min(conf_D, 20L)) {
    ts <- which(pmin(as.integer(d_star[, 1]), conf_D) >= a)
    if (length(ts) < 5L) break
    on_books <- 0; withdrawn <- 0
    for (t in ts) {
      z <- inc[t, seq_len(a + 1L), 1]
      on_books  <- on_books + sum(z)
      withdrawn <- withdrawn + sum(pmax(-z, 0))
    }
    r <- on_books / (on_books + withdrawn)
    cat(sprintf("%6d %10d %14.0f %14.0f %10.5f %8s\n", a, length(ts), on_books, withdrawn, r,
                if (is.na(prev)) "" else sprintf("%+.5f", r - prev)))
    prev <- r
  }
}
