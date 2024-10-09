/*Continuous case*/
array[] real get_val_for_model(data int n_rows, data int num_delays, matrix m,
  array[,] int case_idx, data int s_col, data int d_col, data int t_col){
  /*
  @title Obtain the value that is going to be the main part of the model
  */
  array[n_rows] real dist_val;
  for (n in 1:n_rows)
    dist_val[n] = m[num_delays*(case_idx[n,s_col] - 1) + case_idx[n,d_col], case_idx[n,t_col]];

  return dist_val;
}
