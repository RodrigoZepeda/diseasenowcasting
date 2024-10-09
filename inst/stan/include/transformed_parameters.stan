/*Uncenter the parameters of each component-------------------------------------------------------*/
vector[tsize] mu_intercept      = mu_intercept_param_1 + mu_intercept_param_2*mu_intercept_centered;
vector[num_strata] nu_intercept = nu_intercept_param_1 + nu_intercept_centered*nu_intercept_param_2;
vector[tsize] mu_init           = mu_0_param_1 + mu_0_param_2*mu_init_centered;
vector[num_strata] nu_init      = nu_0_param_1 + nu_0_param_2*nu_init_centered;

/*Get the linearized version of the State Space Model---------------------------------------------*/
matrix[tsize, num_steps] m = state_space_model(num_steps, num_delays, num_strata, phi_mu,
  theta_mu, phi_nu, mu_intercept, nu_intercept, mu_p, mu_q, nu_p, mu_init, nu_init,
  sd_mu, sd_nu, xi_mu, xi_nu);

/*Transform the state space model using the corresponding link function---------------------------*/
matrix[tsize, num_steps] m_trans = transform_state_space_model(m, identity_link_x, log_link_x,
  softplus_link_x, dist_hyper_link_x, control_k_transform, control_c_transform);

/*Get the value that is actually going to be modelled---------------------------------------------*/
array[n_rows] real dist_val = get_val_for_model(n_rows, num_delays, m_trans, case_idx, s_col,
  d_col, t_col);

/*Include the priors------------------------------------------------------------------------------*/
#include include/lprior.stan
