matrix state_space_process_v3(int num_steps, int num_delays, int num_strata,
  matrix A_mu, matrix A_nu, matrix R_mu, matrix R_nu, vector L_mu, vector L_nu,
  matrix mu_0, array[] matrix xi_mu, matrix nu_0, array[] matrix xi_nu,
   vector B_cnt, matrix X_cnt
   //vector B_t, array[] matrix X_t
   ){

    //Initialize the vectors
    matrix[num_delays*num_strata, num_steps] l;

    array[num_steps] matrix[num_delays*num_strata, num_elements(L_mu)] mu;
    mu[1] = mu_0;

    array[num_steps] matrix[num_delays*num_strata, num_elements(L_nu)] nu;
    nu[1] = nu_0;

    //Calculate the constant coefficient vector
    //vector[] constant_coef = X_cnt*B_cnt;

    //Loop through the rest of the vectors
    for (t in 1:(num_steps - 1)){
      //l[t,d,s] = L_mu*mu[t,d,s] + L_nu*nu[t,d,s] + constant_coef + X_t[t,d,s]*B_t + epsilon[t,d,s];
      l[,t]   = mu[t]*L_mu + nu[t]*L_nu;
      mu[t+1] = mu[t]*A_mu + xi_mu[t]*R_mu;
      nu[t+1] = nu[t]*A_nu + xi_nu[t]*R_nu;
    }

    //Last step of loop
    //l[num_steps,d,s] = L_mu*mu[num_steps,d,s] + L_nu*nu[num_steps,d,s] + constant_coef + X_t[num_steps,d,s]*B_t + epsilon[num_steps,d,s];
    l[,num_steps] = mu[num_steps]*L_mu + nu[num_steps]*L_nu;

    return l;

}
