#include include/license.stan
//Functions for running the state space model

array[] vector state_space_process(int num_steps, int lsize, matrix L_mu,
  matrix L_nu, vector B_cnt, matrix X_cnt, vector B_t, array[] matrix X_t,
  array[] vector mu, array[] vector nu, array[] vector epsilon){

    //Instantiate vector
    array[num_steps] vector[lsize] l;

    //Calculate the constant coefficient vector
    vector[cols(X_cnt)] constant_coef = X_cnt*B_cnt;

    //Loop through the rest of the vectors
    for (t in 1:num_steps)
      l[t] = L_mu*mu[t] + L_nu*nu[t] + constant_coef + X_t[t]*B_t + epsilon[t];

    return l;

}

array[] vector time_dependent_process(int num_steps, matrix A_mu, matrix R_mu, vector mu_0, array[] vector xi_mu){
  //Initialize the vector
  array[num_steps] vector[num_elements(mu_0)] mu;
  mu[1] = mu_0;

  //Loop through time
  for (t in 2:num_steps)
    mu[t] = A_mu*mu[t-1] + R_mu*xi_mu[t];

  return mu;
}

array[] vector time_delay_dependent_process(int num_steps, matrix A_nu, matrix R_nu, vector nu_0, array[] vector xi_nu){
  //Initialize the vector
  array[num_steps] vector[num_elements(nu_0)] nu;
  nu[1] = nu_0;

  for (t in 2:num_steps)
    nu[t] = A_nu*nu[t-1] + R_nu*xi_nu[t];

  return nu;
}
