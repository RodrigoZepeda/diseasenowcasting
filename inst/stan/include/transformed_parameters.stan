//Uncenter
vector[tsize] mu_intercept      = mu_intercept_param_1 + mu_intercept_param_2*mu_intercept_centered;
vector[tsize] mu_init           = mu_0_param_1 + mu_0_param_2*mu_init_centered;
vector[num_strata] nu_intercept = nu_intercept_param_1 + nu_intercept_centered*nu_intercept_param_2;
vector[num_strata] nu_init      = nu_0_param_1 + nu_0_param_2*nu_init_centered;


//Matrix of the average (or log average) of the model
matrix[tsize, num_steps] m = state_space_model(num_steps, num_delays, num_strata, phi_mu,
  theta_mu, phi_nu, mu_intercept, nu_intercept, mu_p, mu_q, nu_p, mu_init, nu_init,
  sd_mu, sd_nu, xi_mu, xi_nu);

//Calculate the error between observed and cases
array[n_rows] real dist_val = get_val_for_model(n_rows, num_delays, m, cases, case_idx,
  s_col, d_col, t_col, sd_m, is_continuous);

//Prior calculation
#include include/lprior.stan
