# Fast integration smoke matrix for the production count-cumulative objective.
#
# Run from the package root:
#   Rscript devel/smoke_count_cumulative_integration.R
#
# This is deliberately an AD/tape gate, not the long all-location optimizer
# sweep. It crosses every supported epidemic process, report/retraction delay
# family, and count-cumulative observation composite. A non-finite objective or
# gradient aborts the script and the CSV is written only after all cells pass.

devtools::load_all(quiet = TRUE)

make_fixture <- function() {
  start <- as.Date("2023-01-07")
  rows <- lapply(seq_len(14L), function(event_index) {
    event <- start + (event_index - 1L) * 7L
    final <- 20L + event_index
    levels <- round(final * c(0.4, 0.7, 0.9, 1))
    if (event_index %% 4L == 0L) levels[4L] <- levels[3L] - 1L
    data.frame(
      event = event,
      report = event + 0:3 * 7L,
      count = as.integer(levels)
    )
  })
  observations <- do.call(rbind, rows)
  tbl.now::tbl_now(
    observations,
    event_date = event,
    report_date = report,
    case_count = count,
    data_type = "count-cumulative",
    event_units = "weeks",
    report_units = "weeks",
    now = max(observations$report),
    verbose = FALSE
  )
}

epidemics <- list(
  AR = ar1_epidemic(),
  HSGP = hsgp_epidemic(num_basis = 8L),
  SIR = sir_epidemic(N_pop = 10000)
)
delays <- list(
  lognormal = lognormal_delay(),
  gamma = gamma_delay(),
  generalized_gamma = generalized_gamma_delay()
)
observations <- c("cumulative", "hurdle_ztnb", "hurdle_ztpoisson")
data <- make_fixture()
rows <- list()

for (epidemic_name in names(epidemics)) {
  for (delay_name in names(delays)) {
    for (observation in observations) {
      specification <- model(
        nb_likelihood(), epidemics[[epidemic_name]], delays[[delay_name]],
        count_cumulative = count_cumulative_process(
          observation = observation,
          retraction_delay = delays[[delay_name]],
          settlement = 6L
        )
      )
      engine <- diseasenowcasting:::prepare_from_tbl_now(
        data, specification, now = as.Date("2023-04-08")
      )$data
      priors <- default_priors(specification, engine)
      use_random <- identical(observation, "cumulative")
      started <- proc.time()[["elapsed"]]
      built <- diseasenowcasting:::build_joint_obj(
        engine, priors, use_random = use_random
      )
      objective <- built$obj$fn(built$obj$par)
      gradient <- built$obj$gr(built$obj$par)
      elapsed <- proc.time()[["elapsed"]] - started
      finite <- is.finite(objective) && all(is.finite(gradient))
      rows[[length(rows) + 1L]] <- data.frame(
        epidemic = epidemic_name,
        delay = delay_name,
        observation = observation,
        use_random = use_random,
        objective = objective,
        max_abs_gradient = max(abs(gradient)),
        finite = finite,
        elapsed_seconds = elapsed
      )
      if (!finite) {
        stop("Non-finite smoke cell: ", epidemic_name, "/", delay_name,
             "/", observation)
      }
    }
  }
}

result <- do.call(rbind, rows)
output <- file.path("devel", "count_cumulative_smoke_results.csv")
utils::write.csv(result, output, row.names = FALSE)
cat(sprintf(
  "PASS: %d/%d production objectives finite; max initial |gradient| = %.6g\n",
  sum(result$finite), nrow(result), max(result$max_abs_gradient)
))
cat("Wrote ", output, "\n", sep = "")
