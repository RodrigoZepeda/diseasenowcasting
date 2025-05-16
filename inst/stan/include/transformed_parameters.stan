/*Uncenter the parameters of each component-------------------------------------------------------*/
vector[tsize] mu_intercept      = mu_intercept_param_1 + mu_intercept_param_2*mu_intercept_centered;
vector[num_strata] nu_intercept = nu_intercept_param_1 + nu_intercept_centered*nu_intercept_param_2;
vector[tsize] mu_init           = mu_0_param_1 + mu_0_param_2*mu_init_centered;
vector[num_strata] nu_init      = nu_0_param_1 + nu_0_param_2*nu_init_centered;

/*Uncenter the temporal parameters----------------------------------------------------------------*/

//EPIDEMIOLOGICAL:

//->Day of week
vector[has_day_of_week_epi ? 7: 0] beta_dow_epi = rep_vector(0.0, has_day_of_week_epi ? 7: 0);
if (has_day_of_week_epi)
  beta_dow_epi[1:6] = sd_dow_epi[1]*beta_dow_epi_param;

//->Weekend
vector[has_weekend_epi ? 1: 0] beta_wkend_epi;
if (has_weekend_epi)
  beta_wkend_epi = sd_wkend_epi[1]*beta_wkend_epi_param;

//->Day of month
vector[has_day_of_month_epi ? 31: 0] beta_dom_epi = rep_vector(0.0, has_day_of_month_epi ? 31: 0);
if (has_day_of_month_epi)
  beta_dom_epi[1:30] = sd_dom_epi[1]*beta_dom_epi_param;

//->Month of year
vector[has_month_of_year_epi ? 12: 0] beta_month_epi = rep_vector(0.0, has_month_of_year_epi ? 12: 0);
if (has_month_of_year_epi)
  beta_month_epi[1:11] = sd_month_epi[1]*beta_month_epi_param;

//->Week of year
vector[has_week_of_year_epi ? 53: 0] beta_week_epi = rep_vector(0.0, has_week_of_year_epi ? 53: 0);
if (has_week_of_year_epi){
  for (k in 2:52)
    beta_week_epi[k] = beta_week_epi[k-1] + sd_week_epi[1]*beta_week_epi_param[k-1]; //Prior for next week is this week

  //In some cases epiweek 53 exists thus we average the closest 2
  beta_week_epi[53] = (beta_week_epi_param[52] + beta_week_epi_param[1]) / 2.0;

}

//->Holidays
vector[has_holidays_epi ? 1: 0] beta_holidays_epi;
if (has_holidays_epi){
  //Add holiday effect
  beta_holidays_epi = sd_holidays_epi[1]*beta_holidays_epi_param;

  //If has weekend add weekened to holiday effect so that weekend is the prior for holiday
  if (has_weekend_epi)
    beta_holidays_epi += beta_wkend_epi;
}

//DELAY:
/*
//->Day of week
vector[has_day_of_week_dly ? 7: 0] beta_dow_dly = rep_vector(0.0, has_day_of_week_dly ? 7: 0);
if (has_day_of_week_dly)
  beta_dow_dly[1:6] = sd_dow_dly[1]*beta_dow_dly_param;

//->Weekend
vector[has_weekend_dly ? 1: 0] beta_wkend_dly;
if (has_weekend_dly)
  beta_wkend_dly = sd_wkend_dly[1]*beta_wkend_dly_param;

//->Day of month
vector[has_day_of_month_dly ? 31: 0] beta_dom_dly = rep_vector(0.0, has_day_of_month_dly ? 31: 0);
if (has_day_of_month_dly)
  beta_dom_dly[1:30] = sd_dom_dly[1]*beta_dom_dly_param;

//->Month of year
vector[has_month_of_year_dly ? 12: 0] beta_month_dly = rep_vector(0.0, has_month_of_year_dly ? 12: 0);
if (has_month_of_year_dly)
  beta_month_dly[1:11] = sd_month_dly[1]*beta_month_dly_param;

//->Week of year
vector[has_week_of_year_dly ? 53: 0] beta_week_dly = rep_vector(0.0, has_month_of_year_dly ? 53: 0);
if (has_week_of_year_dly){
  for (k in 2:52)
    beta_week_dly[k] = beta_week_dly[k-1] + sd_week_dly[1]*beta_week_dly_param[k-1]; //Prior for next week is this week

  //In some cases dlyweek 53 exists thus we average the closest 2
  beta_week_dly[53] = (beta_week_dly_param[52] + beta_week_dly_param[1]) / 2.0;

}

//->Holidays
vector[has_holidays_dly ? 1: 0] beta_holidays_dly;
if (has_holidays_dly){
  //Add holiday effect
  beta_holidays_dly = sd_holidays_dly[1]*beta_holidays_dly_param;

  //If has weekend add weekened to holiday effect so that weekend is the prior for holiday
  if (has_weekend_dly)
    beta_holidays_dly += beta_wkend_dly;
}
*/

/*Uncenter cycle components-----------------------------------------------------------------------*/
vector[has_cycle ? tsize: 0] c_init      = c_0_param_1 + c_0_param_2*c_init_centered;
vector[has_cycle ? tsize: 0] ctilde_init = ctilde_0_param_1 + ctilde_0_param_2*ctilde_init_centered;

/*Get the linearized version of the State Space Model---------------------------------------------*/
matrix[tsize, num_steps] m = state_space_model(num_steps, num_delays, num_strata, tsize, phi_mu,
  theta_mu, phi_nu, mu_intercept, nu_intercept, lambda_cycle,
  has_day_of_week_epi, has_weekend_epi, has_day_of_month_epi, has_month_of_year_epi,
  has_week_of_year_epi, has_holidays_epi,beta_dow_epi, beta_wkend_epi,
  beta_dom_epi, beta_month_epi, beta_week_epi, beta_holidays_epi,
  day_of_week_epi, weekend_epi, day_of_month_epi, month_of_year_epi, week_of_year_epi, holidays_epi,
  mu_p, mu_q, nu_p, has_cycle,
  mu_init, nu_init, c_init, ctilde_init, sd_mu, sd_nu, sd_cycle, sd_ctilde, xi_mu, xi_nu,
  xi_cycle, xi_ctilde);

/*Transform the state space model using the corresponding link function---------------------------*/
matrix[tsize, num_steps] m_trans = transform_state_space_model(m, identity_link_x, log_link_x,
  softplus_link_x, dist_hyper_link_x, control_k_transform, control_c_transform);

/*Get the value that is actually going to be modelled---------------------------------------------*/
array[n_rows] real dist_val = get_val_for_model(n_rows, num_delays, m_trans, case_idx, s_col,
  d_col, t_col);

/*Include the priors------------------------------------------------------------------------------*/
#include include/lprior.stan
