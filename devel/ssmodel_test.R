num_steps  <- 100
num_delays <- 20
num_strata <- 4
tsize      <- num_delays*num_strata
mu_p       <- 3
mu_q       <- 2
nu_p      <- 5
phi_mu    <- create_phi_AR(rep(1, mu_p), rstan::get_stream())
A         <- matrix(rnorm(25), ncol = 5)
AR(A, phi_mu, t = 5, p = 3, rstan::get_stream())



microbenchmark::microbenchmark({
ssmod <- state_space_model(num_steps     = num_steps,
                           num_delays    = num_delays,
                           num_strata    = num_strata,
                           phi_mu        = runif(mu_p, -1, 1),
                           theta_mu      = runif(mu_q, -1, 1),
                           phi_nu        = runif(nu_p, -1, 1),
                           mu_intercept  = rlnorm(tsize),
                           nu_intercept  = rlnorm(num_strata),
                           lambda_cycle  = 2*pi,
                           mu_p          = mu_p,
                           mu_q          = mu_q,
                           nu_p          = nu_p,
                           has_cycle     = FALSE,
                           mu_init       = rlnorm(tsize),
                           nu_init       = rlnorm(num_strata),
                           c_init        = rnorm(tsize),
                           ctilde_init   = rnorm(tsize),
                           sd_mu         = rexp(1),
                           sd_nu         = rexp(1),
                           sd_cycle      = as.matrix(rexp(1)),
                           sd_ctilde     = as.matrix(rexp(1)),
                           xi_mu         = matrix(rnorm((num_steps - 1)*tsize), nrow = tsize, ncol = num_steps - 1),
                           xi_nu         = matrix(rnorm(num_strata*(num_delays - 1)), nrow = num_strata, ncol = num_delays - 1),
                           xi_cycle      = matrix(rnorm((num_steps - 1)*tsize), nrow = tsize, ncol = num_steps - 1),
                           xi_ctilde     = matrix(rnorm((num_steps - 1)*tsize), nrow = tsize, ncol = num_steps - 1),
                           pstream__     = rstan::get_stream())
})

n_obs <- 10
case_idx <-lapply(
  1:n_obs, function(x){c(
    "t" = sample(1:num_steps, 1, FALSE),
    "d" = sample(1:num_delays, 1, FALSE),
    "s" = sample(1:num_strata, 1, FALSE)
  )}
)

observed_mean(n_rows = n_obs, num_delays, m = ssmod, case_idx, s_col = 3, d_col = 2, t_col = 1,
              pstream__ = rstan::get_stream())
