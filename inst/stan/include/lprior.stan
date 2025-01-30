real lprior = 0;

// Priors for components
// mu:
lprior += std_normal_lpdf(mu_intercept_centered);
lprior += std_normal_lpdf(mu_init_centered);

// nu:
lprior += std_normal_lpdf(nu_intercept_centered);
lprior += std_normal_lpdf(nu_init_centered);

// cycle:
lprior += std_normal_lpdf(c_init_centered);
lprior += std_normal_lpdf(ctilde_init_centered);

//epidemic temporal effects
lprior += std_normal_lpdf(beta_dow_epi_param);
lprior += std_normal_lpdf(beta_wkend_epi_param);
lprior += std_normal_lpdf(beta_dom_epi_param);
lprior += std_normal_lpdf(beta_month_epi_param);
lprior += std_normal_lpdf(beta_week_epi_param);
lprior += std_normal_lpdf(beta_holidays_epi_param);

// Errors for each of the components
lprior += std_normal_lpdf(to_vector(xi_mu));
lprior += std_normal_lpdf(to_vector(xi_nu));
lprior += std_normal_lpdf(to_vector(xi_cycle));
lprior += std_normal_lpdf(to_vector(xi_ctilde));

// Variances
lprior += normal_lpdf(sd_mu | sd_mu_param_1, sd_mu_param_2) - normal_lccdf(0 | sd_mu_param_1, sd_mu_param_2);
lprior += normal_lpdf(sd_nu | sd_nu_param_1, sd_nu_param_2) - normal_lccdf(0 | sd_nu_param_1, sd_nu_param_2);

if (!is_poisson)
  lprior += normal_lpdf(sd_m  | sd_m_param_1, sd_m_param_2)   - normal_lccdf(0 | sd_m_param_1, sd_m_param_2);

if (has_cycle){
  lprior += normal_lpdf(sd_cycle  | sd_c_param_1, sd_c_param_2) - normal_lccdf(0 | sd_c_param_1, sd_c_param_2);
  lprior += normal_lpdf(sd_ctilde | sd_ctilde_param_1, sd_ctilde_param_2) - normal_lccdf(0 | sd_ctilde_param_1, sd_ctilde_param_2);
}

//Variances for temporal coeficients
if (has_day_of_week_epi)
  lprior += normal_lpdf(sd_dow_epi  | sd_dow_epi_param_1, sd_dow_epi_param_2) - normal_lccdf(0 | sd_dow_epi_param_1, sd_dow_epi_param_2);

if (has_weekend_epi)
  lprior += normal_lpdf(sd_wkend_epi  | sd_wkend_epi_param_1, sd_wkend_epi_param_2) - normal_lccdf(0 | sd_wkend_epi_param_1, sd_wkend_epi_param_2);

if (has_day_of_month_epi)
  lprior += normal_lpdf(sd_dom_epi  | sd_dom_epi_param_1, sd_dom_epi_param_2) - normal_lccdf(0 | sd_dom_epi_param_1, sd_dom_epi_param_2);

if (has_month_of_year_epi)
  lprior += normal_lpdf(sd_month_epi  | sd_month_epi_param_1, sd_month_epi_param_2) - normal_lccdf(0 | sd_month_epi_param_1, sd_month_epi_param_2);

if (has_week_of_year_epi)
  lprior += normal_lpdf(sd_week_epi  | sd_week_epi_param_1, sd_week_epi_param_2) - normal_lccdf(0 | sd_week_epi_param_1, sd_week_epi_param_2);

if (has_holidays_epi)
  lprior += normal_lpdf(sd_holidays_epi | sd_holidays_epi_param_1, sd_holidays_epi_param_2) - normal_lccdf(0 | sd_holidays_epi_param_1, sd_holidays_epi_param_2);
