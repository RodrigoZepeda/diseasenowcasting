real lprior = 0;

// Priors for components
// mu:
lprior += std_normal_lpdf(mu_intercept_centered);
lprior += std_normal_lpdf(mu_init_centered);

// nu:
lprior += std_normal_lpdf(nu_intercept_centered);
lprior += std_normal_lpdf(nu_init_centered);

// Errors for each of the components
lprior += std_normal_lpdf(to_vector(xi_mu));
lprior += std_normal_lpdf(to_vector(xi_nu));

// Variances
lprior += normal_lpdf(sd_mu | sd_mu_param_1, sd_mu_param_2) - normal_lccdf(0 | sd_mu_param_1, sd_mu_param_2);
lprior += normal_lpdf(sd_nu | sd_nu_param_1, sd_nu_param_2) - normal_lccdf(0 | sd_nu_param_1, sd_nu_param_2);
lprior += normal_lpdf(sd_m  | sd_m_param_1, sd_m_param_2)   - normal_lccdf(0 | sd_m_param_1, sd_m_param_2);
//lprior += normal_lpdf(sd_obs| sd_obs_param_1, sd_obs_param_2) - normal_lccdf(0 | sd_obs_param_1, sd_obs_param_2);
