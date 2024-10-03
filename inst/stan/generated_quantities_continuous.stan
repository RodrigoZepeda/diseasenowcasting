//Version 0.1

functions {
    #include include/ss_model.stan
    #include include/get_val_for_model.stan
}

data {
  #include include/data.stan
  array[n_rows] real cases;
}

transformed data {
  #include include/transformed_data.stan
}

parameters {
  #include include/parameters.stan
}

transformed parameters {
  #include include/transformed_parameters.stan
}

generated quantities {

  array[num_steps, tsize] real N_mat_predict  = rep_array(0, num_steps, tsize);
  matrix[num_steps, num_strata] N_predict_raw = rep_matrix(0, num_steps, num_strata);
  matrix[num_steps, num_strata] N_predict     = rep_matrix(0, num_steps, num_strata);

  //Initial predictions
  if (is_normal){
    for (t in 1:num_steps){
      N_mat_predict[t,:] = normal_rng(m[:,t]', rep_vector(1.0, tsize));
    }
  } else if (is_student){
    for (t in 1:num_steps){
      N_mat_predict[t,:] = student_t_rng(dof, m[:,t]', 1.0);
    }
  }

  //Save the predictions from the model (rowsums)
  for (t in 1:num_steps){
    for (s in 1:num_strata){
      for (d in 1:num_delays){
        N_predict_raw[t,s] += N_mat_predict[t,num_delays*(s - 1) + d];
      }
    }
  }

  //Substitute back those values we do know
  for (n in 1:n_rows)
    N_mat_predict[case_idx[n,t_col], num_delays*(case_idx[n,s_col] - 1) + case_idx[n,d_col]] = cases[n];


  //Get the overall total (rowsums) once the model is considered
  for (t in 1:num_steps){
    for (s in 1:num_strata){
      for (d in 1:num_delays){
        N_predict[t,s] += N_mat_predict[t,num_delays*(s - 1) + d]*sd_cases[s,d] + mu_cases[s,d];
      }
    }
  }
}
