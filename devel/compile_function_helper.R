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
  n_rows = nrow(N_cases),
  N_cases = as.matrix(N_cases),

  #Trend specification
  mu_degree = 2,
  nu_degree = 1,
  mu_is_constant = FALSE,
  nu_is_constant = TRUE,

  #Priors and other specifications
  is_negative_binomial = F,
  prior_only = F,
  dispersion_prior_shape = 0.001,
  dispersion_prior_rate = 0.001,
  beta_mean_prior = 0,
  beta_sd_prior = 1,
  alpha_mean_prior = 0,
  alpha_sd_prior = 31,
  alphat_shape_prior = 0.001,
  alphat_rate_prior = 0.001
)

model <- rstan::stan_model("inst/stan/nowcast.stan")
out   <- rstan::sampling(model, data = stan_data, cores = 4, init = 0)
