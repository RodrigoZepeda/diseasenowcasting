#include include/arma.stan
#include include/linear_algebra_utils.stan

matrix state_space_model(
    //Data characteristics
    data int num_steps, data int num_delays, data int num_strata, data int tsize,

    //Parameter vectors for coefficients
    vector phi_mu, vector theta_mu, vector phi_nu, vector mu_intercept, vector nu_intercept,
    vector lambda_cycle,

    //Indicators of epidemic time effects
    data int has_day_of_week_epi, data int has_weekend_epi, data int has_day_of_month_epi,
    data int has_month_of_year_epi, data int has_week_of_year_epi, data int has_holidays_epi,

    //Parameters for epidemic time effects
    vector beta_dow_epi, vector beta_wkend_epi, vector beta_dom_epi, vector beta_month_epi,
    vector beta_week_epi, vector beta_holidays_epi,

    //Epidemic time effects
    data array[] int day_of_week_epi, data array[] int weekend_epi,
    data array[] int day_of_month_epi, data array[] int month_of_year_epi,
    data array[] int week_of_year_epi, data array[] int holidays_epi,

    //Sizes
    data int mu_p, data int mu_q, data int nu_p, data int has_cycle,

    //Parameter vectors for initial values
    vector mu_init, vector nu_init, vector c_init, vector ctilde_init,

    //Variances:
    real sd_mu, real sd_nu, vector sd_cycle, vector sd_ctilde,

    //Errors
    matrix xi_mu, matrix xi_nu, matrix xi_cycle, matrix xi_ctilde
  ){
    /*
    * @title State space model
    *
    * @description Implements the following state space model:
    *
    * @details
    *
    * @param
    */

  /*Transform the errors multiplying by the variances*/
  matrix[tsize, num_steps - 1]  error_mu      = sd_mu*xi_mu;
  matrix[num_strata, num_delays - 1] error_nu = sd_nu*xi_nu;

  vector[mu_p + 1]   phi_mu_star = create_phi_AR(phi_mu);
  vector[mu_q + 1] theta_mu_star = create_theta_MA(theta_mu);
  vector[nu_p + 1]   phi_nu_star = create_phi_AR(phi_nu);

  /*Initial matrices for saving the data*/
  matrix[tsize, num_steps]        m = rep_matrix(0.0, tsize, num_steps);
  matrix[tsize, num_steps]       mu = rep_matrix(0.0, tsize, num_steps);       //Mean time effect
  matrix[num_strata, num_delays] nu = rep_matrix(0.0, num_strata, num_delays);

  /*Optional cycle components*/
  matrix[has_cycle ? tsize : 0, has_cycle ? num_steps - 1: 0]  error_cycle   = rep_matrix(0.0, has_cycle ? tsize : 0, has_cycle ? num_steps - 1: 0);
  matrix[has_cycle ? tsize : 0, has_cycle ? num_steps - 1: 0]  error_ctilde  = rep_matrix(0.0, has_cycle ? tsize : 0, has_cycle ? num_steps - 1: 0);
  matrix[has_cycle ? tsize : 0, has_cycle ? num_steps: 0] cycle_t  = rep_matrix(0.0, has_cycle ? tsize : 0, has_cycle ? num_steps: 0);
  matrix[has_cycle ? tsize : 0, has_cycle ? num_steps: 0] ctilde_t = rep_matrix(0.0, has_cycle ? tsize : 0, has_cycle ? num_steps: 0);

  if (has_cycle){

    //Assign the error terms
    error_cycle  = sd_cycle[1]*xi_cycle;
    error_ctilde = sd_ctilde[1]*xi_ctilde;

    /*Initial vectors for the time step*/
    cycle_t[,1]  = c_init;
    ctilde_t[,1] = ctilde_init;
  }

  /*Initial vectors for the time step*/
  mu[,1] = mu_init;
  nu[,1] = nu_init;

  //Loop through the delays to compute the delay effects
  if (num_delays > 1){
    for (d in 1:(num_delays - 1)){

      //Update the term
      nu[,d + 1] = nu_intercept + AR(nu, phi_nu_star, d + 1, nu_p) + error_nu[,d];

    }
  }

  //Loop through time
  if (num_steps > 1){
    for (t in 1:(num_steps - 1)){

      //Calculate the mean (resp. log-mean) by summing the time and delay effects
      m[,t] = mu[,t] + colwise_mat_2_vec(nu);

      //Update the value for mu
      mu[,t + 1] = mu_intercept + AR(mu, phi_mu_star, t + 1, mu_p) + MA(error_mu, theta_mu_star, t, mu_q) + error_mu[,t];

      //OPTIONAL EFFECTS---------------------------------------------------------------------------

      //Epidemic trend effects
      //Day of the week
      if (has_day_of_week_epi)
        m[,t] += rep_vector(beta_dow_epi[day_of_week_epi[t]], tsize);

      //Weekend
      if (has_weekend_epi)
        m[,t] += rep_vector(beta_wkend_epi[weekend_epi[t]], tsize);

      //Day of month
      if (has_day_of_month_epi)
        m[,t] += rep_vector(beta_dom_epi[day_of_month_epi[t]], tsize);

      //Month of year
      if (has_month_of_year_epi)
        m[,t] += rep_vector(beta_month_epi[month_of_year_epi[t]], tsize);

      //Week of year
      if (has_week_of_year_epi)
        m[,t] += rep_vector(beta_week_epi[week_of_year_epi[t]], tsize);

      //Week of year
      if (has_holidays_epi)
        m[,t] += rep_vector(beta_holidays_epi[holidays_epi[t]], tsize);

      //Cycle component
      if (has_cycle){

        //Add cycle to mean effect
        m[,t] += cycle_t[,t];

        //Update the cycle value
        cycle_t[,t + 1]  = cycle_t[,t].*cos(lambda_cycle) + ctilde_t[,t].*sin(lambda_cycle) + error_cycle[,t];
        ctilde_t[,t + 1] = -cycle_t[,t].*sin(lambda_cycle) + ctilde_t[,t].*cos(lambda_cycle) + error_ctilde[,t];

      }
    }
  }

  //Last element of for m
  m[,num_steps] = mu[,num_steps] + colwise_mat_2_vec(nu);

  //Optional cycle component
  if (has_cycle)
    m[,num_steps] += cycle_t[,num_steps];

  return m;
}
