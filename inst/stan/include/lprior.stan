real lprior = 0;

// Priors for components
// mu:
lprior += normal_lpdf(mu_intercept | mu_intercept_param_1, mu_intercept_param_2);
lprior += normal_lpdf(phi_mu | AR_mu_param_1, AR_mu_param_2);
lprior += normal_lpdf(theta_mu | MA_mu_param_1, MA_mu_param_2);
lprior += normal_lpdf(mu_init | mu_0_param_1, mu_0_param_2);

// nu:
lprior += normal_lpdf(nu_intercept | nu_intercept_param_1, nu_intercept_param_2);
lprior += normal_lpdf(phi_nu | AR_nu_param_1, AR_nu_param_2);
lprior += normal_lpdf(nu_init | nu_0_param_1, nu_0_param_2);

// Errors for each of the components
lprior += std_normal_lpdf(to_vector(xi_mu));
lprior += std_normal_lpdf(to_vector(xi_nu));

// Variances
lprior += normal_lpdf(sd_mu | sd_mu_param_1, sd_mu_param_2) - normal_lccdf(0 | sd_mu_param_1, sd_mu_param_2);
lprior += normal_lpdf(sd_nu | sd_nu_param_1, sd_nu_param_2) - normal_lccdf(0 | sd_nu_param_1, sd_nu_param_2);
lprior += normal_lpdf(sd_m  | sd_m_param_1, sd_m_param_2)   - normal_lccdf(0 | sd_m_param_1, sd_m_param_2);
//lprior += normal_lpdf(sd_obs| sd_obs_param_1, sd_obs_param_2) - normal_lccdf(0 | sd_obs_param_1, sd_obs_param_2);
