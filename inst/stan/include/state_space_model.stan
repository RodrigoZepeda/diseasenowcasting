#include include/license.stan
//Functions for running the state space model

array[,,] vector state_space_process(int num_steps, int num_delays, int num_strata,
  matrix A_mu, matrix A_nu, matrix R_mu, matrix R_nu, row_vector L_mu, row_vector L_nu,
  array[,] vector mu_0, array[,,] vector xi_mu, array[,] vector nu_0, array[,,] vector xi_nu,
   vector B_cnt, matrix X_cnt,
   //vector B_t, array[] matrix X_t,
   array[,,] vector epsilon){

    //Initialize the vectors
    array[num_steps, num_delays, num_strata] vector[1] l;

    array[num_steps, num_delays, num_strata] vector[num_elements(mu_0[1,1])] mu;
    mu[1,:,:] = mu_0;

    array[num_steps, num_delays, num_strata] vector[num_elements(nu_0[1,1])] nu;
    nu[1,:,:] = nu_0;

    //Calculate the constant coefficient vector
    vector[cols(X_cnt)] constant_coef = X_cnt*B_cnt;

    //Loop through the rest of the vectors

    for (s in 1:num_strata){
      for (d in 1:num_delays){
        for (t in 1:(num_steps - 1)){
          //l[t,d,s] = L_mu*mu[t,d,s] + L_nu*nu[t,d,s] + constant_coef + X_t[t,d,s]*B_t + epsilon[t,d,s];
          l[t,d,s]    = L_mu*mu[t,d,s] + L_nu*nu[t,d,s] + constant_coef + epsilon[t,d,s];
          mu[t+1,d,s] = A_mu*mu[t,d,s] + R_mu*xi_mu[t+1,d,s];
          nu[t+1,d,s] = A_nu*nu[t,d,s] + R_nu*xi_nu[t+1,d,s];
        }

        //Last step of loop
        //l[num_steps,d,s] = L_mu*mu[num_steps,d,s] + L_nu*nu[num_steps,d,s] + constant_coef + X_t[num_steps,d,s]*B_t + epsilon[num_steps,d,s];
        l[num_steps,d,s] = L_mu*mu[num_steps,d,s] + L_nu*nu[num_steps,d,s] + constant_coef + epsilon[num_steps,d,s];
      }
    }


    return l;

}
