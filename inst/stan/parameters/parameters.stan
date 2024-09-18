//Initial values for mu and nu
matrix[num_strata*num_delays, mu_0_size] mu_0_centered;
matrix[num_strata*num_delays, nu_0_size] nu_0_centered;

//Priors for the parameters 1 and 2 of mu and nu
real mu_0_mean;
real nu_0_mean;
real<lower=0> mu_0_sd;
real<lower=0> nu_0_sd;

real<lower=0> mu_sd;
real<lower=0> nu_sd;

//Normalized errors
array[num_steps - 1] matrix[num_strata*num_delays, xi_mu_size] xi_mu_centered;
array[num_steps - 1] matrix[num_strata*num_delays, xi_nu_size] xi_nu_centered;

//Precision parameter for negative binomial
vector<lower=0>[is_negative_binomial ? 1 : 0] r;
