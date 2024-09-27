//Initial values for mu and nu
matrix[num_strata*num_delays, mu_0_size] mu_0_centered;
matrix[num_strata*num_delays, nu_0_size] nu_0_centered;

//Centered values for the AR component
vector<lower=-1,upper=1>[p] phi_AR;   //To ensure stationary we consider |phi| <= 1
vector<lower=-1,upper=1>[q] theta_MA; //To ensure stationary we consider |theta| <= 1

//Priors for the parameters 1 and 2 of mu and nu
real mu_0_mean;
real nu_0_mean;
real<lower=0> mu_0_sd;
real<lower=0> nu_0_sd;

real<lower=0> xi_sd;
real<lower=0> xi_mu_sd;
real<lower=0> xi_nu_sd;

//Normalized errors
matrix[num_strata*num_delays, num_steps] xi_centered;
array[num_steps - 1] matrix[num_strata*num_delays, xi_mu_size] xi_mu_centered;
array[num_steps - 1] matrix[num_strata*num_delays, xi_nu_size] xi_nu_centered;

//Variances for cases
vector<lower=0>[has_variance ? 1 : 0] var_cases_0;
matrix[num_steps - 1, has_variance ? 1 : 0] error_var_cases;
vector<lower=0>[has_variance ? 1 : 0] r_sd;


