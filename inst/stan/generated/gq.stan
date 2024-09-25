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
        N_predict[t,s] += N_mat_predict[t,num_strata*(d - 1) + s]*sd_cases[s,d] + mu_cases[s,d];
      }
    }
  }
