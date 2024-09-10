#include include/license.stan
//Functions for running the state space model
array[,] vector state_space_process_v2(int num_steps, int num_delays, int num_strata,
  matrix A_mu, matrix A_nu, matrix R_mu, matrix R_nu, vector L_mu, vector L_nu,
  array[] matrix mu_0, array[,] matrix xi_mu, array[] matrix nu_0, array[,] matrix xi_nu,
   vector B_cnt, matrix X_cnt,
   //vector B_t, array[] matrix X_t,
   array[,] vector epsilon){

    //Initialize the vectors
    array[num_steps, num_strata] vector[num_delays] l;

    array[num_steps, num_strata] matrix[num_delays, num_elements(L_mu)] mu;
    mu[1,:] = mu_0;

    array[num_steps, num_strata] matrix[num_delays, num_elements(L_nu)] nu;
    nu[1,:] = nu_0;


    //Calculate the constant coefficient vector
    //vector[] constant_coef = X_cnt*B_cnt;

    //Loop through the rest of the vectors
    for (s in 1:num_strata){
        for (t in 1:(num_steps - 1)){
          //l[t,d,s] = L_mu*mu[t,d,s] + L_nu*nu[t,d,s] + constant_coef + X_t[t,d,s]*B_t + epsilon[t,d,s];
          l[t,s]    = mu[t,s]*L_mu + nu[t,s]*L_nu + epsilon[t,s];
          mu[t+1,s] = mu[t,s]*A_mu' + xi_mu[t+1,s]*R_mu';
          nu[t+1,s] = nu[t,s]*A_nu' + xi_nu[t+1,s]*R_nu';
        }

        //Last step of loop
        //l[num_steps,d,s] = L_mu*mu[num_steps,d,s] + L_nu*nu[num_steps,d,s] + constant_coef + X_t[num_steps,d,s]*B_t + epsilon[num_steps,d,s];
        l[num_steps,s] = mu[num_steps,s]*L_mu + nu[num_steps,s]*L_nu + epsilon[num_steps,s];
    }

    return l;

}
