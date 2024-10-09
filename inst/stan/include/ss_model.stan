#include include/arma.stan
#include include/linear_algebra_utils.stan

matrix state_space_model(
    //Data characteristics
    data int num_steps, data int num_delays, data int num_strata,

    //Parameter vectors for coefficients
    vector phi_mu, vector theta_mu, vector phi_nu, vector mu_intercept, vector nu_intercept,

    //Sizes
    data int mu_p, data int mu_q, data int nu_p,

    //Parameter vectors for initial values
    vector mu_init, vector nu_init,

    //Variances:
    real sd_mu, real sd_nu,

    //Errors
    matrix xi_mu, matrix xi_nu
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

  //Get the size of the errors
  int tsize = num_strata*num_delays;

  /*Transform the errors multiplying by the variances*/
  matrix[tsize, num_steps - 1]  error_mu = sd_mu*xi_mu;
  matrix[num_strata, num_delays - 1] error_nu = sd_nu*xi_nu;

  vector[mu_p + 1]   phi_mu_star   = create_phi_AR(phi_mu);
  vector[mu_q + 1] theta_mu_star   = create_theta_MA(theta_mu);
  vector[nu_p + 1]   phi_nu_star   = create_phi_AR(phi_nu);

  /*Initial matrices for saving the data*/
  matrix[tsize, num_steps]        m = rep_matrix(0.0, tsize, num_steps);
  matrix[tsize, num_steps]       mu = rep_matrix(0.0, tsize, num_steps);
  matrix[num_strata, num_delays] nu = rep_matrix(0.0, num_strata, num_delays);

  /*Initial vectors for the time step*/
  mu[,1] = mu_init;
  nu[,1] = nu_init;

  //Loop through the delays
  if (num_delays > 1){
    for (d in 1:(num_delays - 1)){
      //Update the term
      nu[,d + 1] = nu_intercept + AR(nu, phi_nu_star, d + 1, nu_p) + error_nu[,d]; //Check whether its d or d + 1 in AR
    }
  }

  //Loop through time
  if (num_steps > 1){
    for (t in 1:(num_steps - 1)){

      //Calculate the mean (resp. log-mean)
      m[,t] = mu[,t] + colwise_mat_2_vec(nu);

      //Update the value for mu
      mu[,t + 1] = mu_intercept + AR(mu, phi_mu_star, t + 1, mu_p) + MA(error_mu, theta_mu_star, t, mu_q) + error_mu[,t];

    }
  }

  //Last element of for m
  m[,num_steps]  = mu[,num_steps] + colwise_mat_2_vec(nu);

  return m;
}
