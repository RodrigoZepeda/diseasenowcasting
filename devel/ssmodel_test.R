num_steps  <- 100
num_delays <- 20
num_strata <- 4
tsize      <- num_delays*num_strata
mu_p       <- 3
mu_q       <- 2
mu_nu      <- 5

phi_nu_star <- create_phi_AR(runif(mu_nu, -1, 1), pstream__ = rstan::get_stream())
AR(matrix(c(rep(1, num_strata), rep(NA_real_, num_strata*(num_delays - 1))), nrow = num_strata, ncol = num_delays),
   phi = phi_nu_star, t = 1, pstream__ = rstan::get_stream())

ssmod <- state_space_model(num_steps     = num_steps,
                           num_delays    = num_delays,
                           num_strata    = num_strata,
                           phi_mu        = runif(mu_p, -1, 1),
                           theta_mu      = runif(mu_q, -1, 1),
                           phi_nu        = runif(mu_nu, -1, 1),
                           mu_intercept  = rlnorm(tsize),
                           nu_intercept  = rlnorm(num_strata),
                           mu_init       = rlnorm(tsize),
                           nu_init       = rlnorm(num_strata),
                           sd_mu         = rexp(tsize),
                           sd_nu         = rexp(num_strata),
                           sd_m          = rexp(tsize),
                           xi_mu         = matrix(rnorm(num_steps*tsize), nrow = tsize, ncol = num_steps),
                           xi_nu         = matrix(rnorm(num_strata*num_delays), nrow = num_strata, ncol = num_delays),
                           xi_m          = matrix(rnorm(num_steps*tsize), nrow = tsize, ncol = num_steps),
                           pstream__     = rstan::get_stream())

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
