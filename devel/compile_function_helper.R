set.seed(265824)
sims <- simulate_process_for_testing()

# Run a nowcast with very few iterations
predictions <- nowcast(sims, "onset_date", "report_date", cores = 4)

generated_qs <- rstan::stan_model("inst/stan/generated_quantities.stan")
generated_quantities <- rstan::gqs(generated_qs, data = predictions$stan_data, draws = as.matrix(predictions$model))
