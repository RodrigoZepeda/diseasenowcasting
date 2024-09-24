library(rstan)

model <- rstan::stan_model("devel/prior_experiments.stan")
param_values <- list(beta = 2, sigma = 10)
prior_samples <- sampling(model,
                          data = list(N = 100, y = rnorm(100, 0, 1), prior_only = TRUE),
                          algorithm = "Fixed_param",
                          iter = 1000, chains = 1,
                          init = function(...) return(param_values),
                          seed = 1234)
prior_samples |> as_draws() |> summarise_draws()
