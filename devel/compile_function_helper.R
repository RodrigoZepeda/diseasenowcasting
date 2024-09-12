library(rstan)
data(denguedat)
now <- as.Date("1990-10-01")

disease_data <- denguedat
disease_data <- preprocess_for_nowcast(disease_data, "onset_week", "report_week",
                                       now = now, units = "weeks")|>
  dplyr::mutate(.delay = .delay + 1)

# Nmatrix
N_cases <- disease_data |>
  dplyr::select(-onset_week, -report_week) |>
  dplyr::select(n, .tval, -.delay, tidyr::everything()) |>
  dplyr::mutate(strata = 1)

stan_data <- list(
  num_steps  = max(disease_data$.tval),
  num_delays = max(disease_data$.delay),
  num_strata = 1,
  n_rows     = nrow(N_cases),
  N_cases    = as.matrix(N_cases),

  #Trend specification
  mu_degree = 2,
  nu_degree = 1,
  mu_is_constant = FALSE,
  nu_is_constant = TRUE,

  #Priors and other specifications
  is_negative_binomial = T,
  prior_only = F,
  dispersion_prior_shape = 0.001,
  dispersion_prior_rate = 0.001,
  mu_shape_prior = 0.0,
  mu_rate_prior = 1.0,
  nu_shape_prior = 0.0,
  nu_rate_prior = 1.0,

  mean_mu_0_prior = log(mean(N_cases$n)),
  mean_nu_0_prior  = 0.0,
  sigma_mu_0_prior = 0.01,
  sigma_nu_0_prior = 0.01,


  #Prior distributions
  mu_prior = get_prior_code_stan("standard_normal"),
  nu_prior = get_prior_code_stan("standard_normal"),
  r_prior  = get_prior_code_stan("inverse_gamma")
)

model <- rstan::stan_model("inst/stan/nowcast_v2.stan")
out   <- rstan::sampling(model, data = stan_data, cores = 4, chains = 4,
                         control = list(adapt_delta = 0.95, max_treedepth = 12))

generated_qs <- rstan::stan_model("inst/stan/generated_quantities.stan")
generated_quantities <- rstan::gqs(generated_qs, data = stan_data, draws = as.matrix(out))
