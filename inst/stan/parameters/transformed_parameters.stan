//Uncentered variables
matrix[num_strata*num_delays, mu_0_size] mu_0 =
  rep_matrix(mu_0_mean, num_strata*num_delays, mu_0_size) + mu_0_sd*mu_0_centered;
matrix[num_strata*num_delays, nu_0_size] nu_0 =
  rep_matrix(nu_0_mean, num_strata*num_delays, nu_0_size) + nu_0_sd*nu_0_centered;

array[num_steps - 1] matrix[num_strata*num_delays, xi_mu_size] xi_mu;
array[num_steps - 1] matrix[num_strata*num_delays, xi_nu_size] xi_nu;
for (t in 1:(num_steps - 1)){
  xi_mu[t] = mu_sd*xi_mu_centered[t];
  xi_nu[t] = nu_sd*xi_nu_centered[t];
}

//Get the state space process simulations
matrix[num_delays*num_strata, num_steps] lambda = state_space_process(
      num_steps, num_delays, num_strata, A_mu, A_nu, R_mu, R_nu,
      L_mu, L_nu, mu_0, xi_mu, nu_0, xi_nu, B_cnt, X_cnt);

//Create a vectorized version of the lambda
//The lambda function is organized by delays and then strata so
//
//   Strata      |    Delay    |    Lambda    |
//---------------------------------------------
//      1        |      1      |      1       |
//      2        |      1      |      2       |
//      1        |      2      |      3       |
//      2        |      2      |      4       |
//      1        |      3      |      5       |
//      2        |      3      |      6       |
//      1        |      4      |      7       |
//      2        |      4      |      8       |
//---------------------------------------------
//
vector[n_rows] lambda_mean;
for (n in 1:n_rows)
  lambda_mean[n] = lambda[num_strata*(N_cases[n,d_col] - 1) + N_cases[n,s_col], N_cases[n,t_col]];

//Hyper priors
// ------------------------------------------------------------------------------------------------


//Priors
// ------------------------------------------------------------------------------------------------
real lprior = 0;
lprior += std_normal_lpdf(to_vector(mu_0_centered));
lprior += std_normal_lpdf(to_vector(nu_0_centered));

for (t in 1:(num_steps - 1)){
  lprior += std_normal_lpdf(to_vector(xi_mu_centered[t]));
  lprior += std_normal_lpdf(to_vector(xi_nu_centered[t]));
}

//Hyperparameter priors
lprior += dist_lpdf(mu_0_mean | mu_0_mean_param_1, mu_0_mean_param_2, mu_0_mean_hyperprior);
lprior += dist_lpdf(mu_0_sd | mu_0_sd_param_1, mu_0_sd_param_2, mu_0_sd_hyperprior);
lprior += dist_lpdf(nu_0_mean | nu_0_mean_param_1, nu_0_mean_param_2, nu_0_mean_hyperprior);
lprior += dist_lpdf(nu_0_sd | nu_0_sd_param_1, nu_0_sd_param_2, nu_0_sd_hyperprior);
lprior += dist_lpdf(mu_sd | mu_sd_param_1, mu_sd_param_2, mu_sd_prior);
lprior += dist_lpdf(nu_sd | nu_sd_param_1, nu_sd_param_2, nu_sd_prior);

//Add prior to the negative binomial precision
if (is_negative_binomial)
  lprior += dist_lpdf(r| r_param_1, r_param_2, r_prior);
