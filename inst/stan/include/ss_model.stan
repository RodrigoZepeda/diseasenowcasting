#include include/arma.stan
#include include/linear_algebra_utils.stan

matrix state_space_model(
    //Data characteristics
    int num_steps, int num_delays, int num_strata,

    //Parameter vectors for coefficients
    vector phi_mu, vector theta_mu, vector phi_nu, vector mu_intercept, vector nu_intercept,

    //Parameter vectors for initial values
     vector mu_init, vector nu_init,

    //Variances:
    vector sd_mu, vector sd_nu, vector sd_m,

    //Errors
    matrix xi_mu, matrix xi_nu, matrix xi_m
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
  matrix[tsize, num_steps]       error_mu;
  matrix[num_strata, num_delays] error_nu;
  matrix[tsize, num_steps]       error_m;

  vector[num_elements(phi_mu) + 1]   phi_mu_star   = create_phi_AR(phi_mu);
  vector[num_elements(theta_mu) + 1] theta_mu_star = create_theta_MA(theta_mu);
  vector[num_elements(phi_nu) + 1]   phi_nu_star   = create_phi_AR(phi_nu);

  /*Initial values for errors*/
  error_mu[,1] = sd_mu .* xi_mu[,1];
  error_nu[,1] = sd_nu .* xi_nu[,1];
  error_m[,1]  = sd_m  .* xi_m[,1];

  /*Initial matrices for saving the data*/
  matrix[tsize, num_steps]        m = rep_matrix(0.0, tsize, num_steps);
  matrix[tsize, num_steps]       mu = rep_matrix(0.0, tsize, num_steps);
  matrix[num_strata, num_delays] nu = rep_matrix(0.0, num_strata, num_delays);


  /*Initial vectors for the time step*/
  mu[,1] = mu_intercept + mu_init + error_mu[,1];
  nu[,1] = nu_intercept + nu_init + error_nu[,1];

  //Loop through the delays
  if (num_delays > 1){
    for (d in 2:num_delays){

      //Update the error
      error_nu[,d] = sd_nu .* xi_nu[,d];

      //Update the term
      nu[,d] = nu_intercept + AR(nu, phi_nu_star, d) + error_nu[,d]; //The error is here in AR
    }
  }

  //Loop through time
  if (num_steps > 1){
    for (t in 1:(num_steps - 1)){

      //Calculate the mean (resp. log-mean)
      m[,t] = mu[,t] + colwise_mat_2_vec(nu) + error_mu[,t];

      //Update error terms
      error_mu[,t + 1] = sd_mu .* xi_mu[,t + 1];
      error_m[,t + 1]  = sd_m  .* xi_m[,t + 1];

      //Update the value for mu
      mu[,t + 1] = mu_intercept + AR(mu, phi_mu_star, t) + MA(error_mu, theta_mu_star, t);

    }
  }

  //Last element of for m
  m[,num_steps]  = mu[,num_steps] + colwise_mat_2_vec(nu) + error_mu[,num_steps];

  return m;
}
