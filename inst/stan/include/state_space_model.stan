#include include/license.stan
#include include/arma.stan

matrix state_space_process(
  int num_steps,        //Number of time steps to run the model for
  int num_delays,       //Number of delays considered in the model
  int num_strata,       //Number of strata considered in the model
  matrix A_mu,          //Matrix for the latent epidemic process' trend
  matrix A_nu,          //Matrix for the latent delay process' trend
  matrix R_mu,          //Matrix for the errors of the epidemic process
  matrix R_nu,          //Matrix for the errors of the delay process
  vector L_mu,          //Matrix for the latent epidemic process -> to the mean of cases
  vector L_nu,          //Matrix for the latent delay process -> to the mean of cases
  array[] matrix xi_mu_centered, //Error parameters for the epidemic process (centered)
  array[] matrix xi_nu_centered, //Error parameters for the delay process (centered)
  vector xi_mu_sd,      //Vector of standard deviation for xi_mu
  vector xi_nu_sd,      //Vector of standard deviation for xi_nu
  matrix mu_0_centered, //Initial value of the latent epidemic process (centered)
  matrix nu_0_centered, //Initial value of the latent trend process (centered)
  real mu_0_sd,         //Scaling for mu_0 (for uncentering)
  real nu_0_sd,         //Scaling for nu_0 (for uncentering)
  real mu_0_mean,       //Centering parameter for mu_0
  real nu_0_mean,       //Centering parameter for nu_0
  vector B_cnt,         //Parameter of constant covariates
  matrix X_cnt,         //Matrix of constant covariates
  vector phi_AR,        //Autoregresive parameters for AR(p)
  vector theta_MA,      //Autoregresive parameters for MA(q)
  matrix xi             //Error in the L scale (for the MA(q) parameter)
   //vector B_t, array[] matrix X_t
   ){

    //Initialize the vectors
    matrix[num_delays*num_strata, num_steps] l = rep_matrix(0.0, num_delays*num_strata, num_steps);

    //Create the initial state by uncentering the value
    array[num_steps] matrix[num_delays*num_strata, num_elements(L_mu)] mu;
    mu[1] = rep_matrix(mu_0_mean, num_strata*num_delays, num_elements(L_mu)) + mu_0_sd*mu_0_centered;

    array[num_steps] matrix[num_delays*num_strata, num_elements(L_nu)] nu;
    nu[1] = rep_matrix(nu_0_mean, num_strata*num_delays, num_elements(L_nu)) + nu_0_sd*nu_0_centered;

    //Get the ARMA vectors
    vector[num_elements(phi_AR) + 1] phi     = create_phi_AR(phi_AR);
    vector[num_elements(theta_MA) + 1] theta = create_theta_MA(theta_MA);

    //Calculate the constant coefficient vector
    //vector[] constant_coef = X_cnt*B_cnt;

    //Loop through the rest of the vectors
    for (t in 1:(num_steps - 1)){
      l[,t]   = mu[t]*L_mu + nu[t]*L_nu + AR(l, phi, t) + MA(xi, theta, t);
      mu[t+1] = mu[t]*A_mu + xi_mu_sd[t]*xi_mu_centered[t]*R_mu;
      nu[t+1] = nu[t]*A_nu + xi_nu_sd[t]*xi_nu_centered[t]*R_nu;
    }

    //Last step of loop
    //l[num_steps,d,s] = L_mu*mu[num_steps,d,s] + L_nu*nu[num_steps,d,s] + constant_coef + X_t[num_steps,d,s]*B_t + epsilon[num_steps,d,s];
    l[,num_steps] = mu[num_steps]*L_mu + nu[num_steps]*L_nu +
      AR(l, phi, num_steps) + MA(xi, theta, num_steps);

    return l;

}
