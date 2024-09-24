#include license/license.stan

functions {
  #include include/linear_algebra_utils.stan
  #include include/trend.stan
  #include include/seasonal_discrete.stan
  #include include/state_space_model.stan
  #include lpdfs_lpmfs/distributions.stan
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
      if (lambda[j, t] >= max_log_tol_val && is_discrete){
        lambda_higher_than_maxval_flag = 1;
        lambda_transformed[j, t] = max_log_tol_val;
      } else {
        lambda_transformed[j, t] = lambda[j, t];
      }
  }

  //Get a matrix of all of the predictions through all times
  for (t in 1:num_steps){
    if (is_discrete){
      N_mat_predict[t,:] = discrete_data_distribution_rng(lambda_transformed[:, t]', rep_vector(r[1] + precision_tol, num_delays*num_strata), data_distribution);
    } else {
      N_mat_predict[t,:] = continuous_data_distribution_rng(lambda_transformed[:, t]', rep_vector(r[1] + precision_tol, num_delays*num_strata), data_distribution);
    }
  }

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
    if (is_discrete){
      N_mat_predict[N_cases_pos[n,t_col], num_strata*(N_cases_pos[n,d_col] - 1) + N_cases_pos[n,s_col]] = N_cases_int[n,1];
    } else {
      N_mat_predict[N_cases_pos[n,t_col], num_strata*(N_cases_pos[n,d_col] - 1) + N_cases_pos[n,s_col]] = N_cases_ct[n,1];
    }
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
