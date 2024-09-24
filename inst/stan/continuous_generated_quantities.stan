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
  #include data/data_continuous.stan
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

  //This flag is to flag to the user that we have capped the lambda
  int lambda_higher_than_maxval_flag = 0;

  //
  array[num_steps, num_delays*num_strata] real N_mat_predict = rep_array(0, num_steps, num_delays*num_strata);
  matrix[num_steps, num_strata] N_predict = rep_matrix(0, num_steps, num_strata);     //Prediction of overall cases at time t
  matrix[num_steps, num_strata] N_predict_raw = rep_matrix(0, num_steps, num_strata);
  matrix[num_delays*num_strata, num_steps] lambda_transformed;


  //Caps the lambda so that if it explodes it doesn't destroy the poisson and binomial
  for (t in 1:num_steps){
    for (j in 1:(num_delays*num_strata))
      if (lambda[j, t] >= max_log_tol_val){
        lambda_higher_than_maxval_flag = 1;
        lambda_transformed[j, t] = max_log_tol_val;
      } else {
        lambda_transformed[j, t] = lambda[j, t];
      }
  }

  //Get a matrix of all of the predictions through all times
  #include generated/continuous_gq.stan

  //Get the overall prediction from the model (rowsums)
  for (t in 1:num_steps){
    for (s in 1:num_strata){
      for (d in 1:num_delays){
        N_predict_raw[t,s] += N_mat_predict[t,num_strata*(d - 1) + s];
      }
    }
  }

  //Substitute back those values we do know
  for (n in 1:n_rows){
    N_mat_predict[N_cases[n,t_col], num_strata*(N_cases[n,d_col] - 1) + N_cases[n,s_col]] = Cases[n,n_col];
  }

  //Get the overall total (rowsums) once the model is considered
  for (t in 1:num_steps){
    for (s in 1:num_strata){
      for (d in 1:num_delays){
        N_predict[t,s] += N_mat_predict[t,num_strata*(d - 1) + s];
      }
    }
  }
}
