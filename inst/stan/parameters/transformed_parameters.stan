//Uncenter the xi parameter
matrix[num_delays*num_strata, num_steps] xi = xi_sd*xi_centered;

//Get the state space process simulations
matrix[num_delays*num_strata, num_steps] lambda = state_space_process(
      num_steps, num_delays, num_strata, A_mu, A_nu, R_mu, R_nu, L_mu, L_nu, xi_mu_centered,
      xi_nu_centered, xi_mu_sd, xi_nu_sd, mu_0_centered, nu_0_centered, mu_0_sd, nu_0_sd,
      mu_0_mean, nu_0_mean, B_cnt, X_cnt, phi_AR, theta_MA, xi);



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

//Priors for mu coefficients
//------------------------------------------------------------------------------------------

//For the centered version of mu
lprior += std_normal_lpdf(to_vector(mu_0_centered));

//Hyperparameter priors
lprior += dist_lpdf(mu_0_mean | mu_0_mean_param_1, mu_0_mean_param_2, mu_0_mean_hyperprior, 0);
lprior += dist_lpdf(mu_0_sd   | mu_0_sd_param_1, mu_0_sd_param_2, mu_0_sd_hyperprior, 1);

//Errors related to mu
lprior += dist_lpdf(xi_mu_sd  | mu_sd_param_1, mu_sd_param_2, mu_sd_prior, 1);

//Priors for nu coefficients
//------------------------------------------------------------------------------------------
//For the centered version of mu
lprior += std_normal_lpdf(to_vector(nu_0_centered));

//Hyperparameter priors
lprior += dist_lpdf(nu_0_mean | nu_0_mean_param_1, nu_0_mean_param_2, nu_0_mean_hyperprior, 0);
lprior += dist_lpdf(nu_0_sd   | nu_0_sd_param_1, nu_0_sd_param_2, nu_0_sd_hyperprior, 1);

//Errors related to nu
lprior += dist_lpdf(xi_nu_sd  | nu_sd_param_1, nu_sd_param_2, nu_sd_prior, 1);

//Priors for nu and mu errors
//------------------------------------------------------------------------------------------
for (t in 1:(num_steps - 1)){
  lprior += std_normal_lpdf(to_vector(xi_nu_centered[t]));
  lprior += std_normal_lpdf(to_vector(xi_mu_centered[t]));
}

//ARMA components
lprior += dist_lpdf(phi_AR | phi_AR_param_1, phi_AR_param_2, phi_AR_prior, 0);
lprior += dist_lpdf(theta_MA | theta_MA_param_1, theta_MA_param_2, theta_MA_prior, 0);

//Errors
lprior += std_normal_lpdf(to_vector(xi_centered));
lprior += dist_lpdf(xi_sd | xi_sd_param_1, xi_sd_param_2, xi_sd_prior, 1);

