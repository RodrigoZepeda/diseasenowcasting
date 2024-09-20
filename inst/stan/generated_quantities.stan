#include /include/license.stan

functions {
  #include include/linear_algebra_utils.stan
  #include include/trend.stan
  #include include/seasonal_discrete.stan
  #include include/state_space_model.stan
  #include include/priors.stan
}

data {
  #include data/data.stan
}

transformed data {
  #include data/transformed_data.stan
}

parameters {
  #include parameters/parameters.stan
}

transformed parameters {
  #include parameters/transformed_parameters.stan
}

generated quantities {
  array[num_steps, num_delays*num_strata] int N_mat_predict;

  //Prediction of overall cases at time t
  matrix[num_steps, num_strata] N_predict = rep_matrix(0, num_steps, num_strata);

  //Get a matrix of all of the predictions
  for (t in 1:num_steps){
      if (is_negative_binomial){
          N_mat_predict[t,:] =
          neg_binomial_2_log_rng(lambda[:, t]', rep_vector(r[1], num_delays*num_strata));
      } else {
          N_mat_predict[t,:] = poisson_log_rng(lambda[:, t]');
      }
  }

  //Substitute back those values we do know
  for (n in 1:n_rows){
    N_mat_predict[N_cases[n,t_col], num_strata*(N_cases[n,d_col] - 1) + N_cases[n,s_col]] = N_cases[n,n_col];
  }

  //Get the overall total (rowsums)
  for (t in 1:num_steps){
    for (s in 1:num_strata){
      for (d in 1:num_delays){
        N_predict[t,s] += N_mat_predict[t,num_strata*(d - 1) + s];
      }
    }
  }
}
