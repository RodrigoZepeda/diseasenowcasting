/*Continuous case*/
array[] real get_val_for_model(data int n_rows, data int num_delays, matrix m,
  array[] real cases_real, array[] real cases_int,
  array[,] int case_idx, data int s_col, data int d_col, data int t_col, vector sd_m,
  data int is_continuous, data int is_discrete){
  /*
  @title Obtain the value that is going to be the main part of the model
  */
  array[n_rows] real dist_val;
  if (is_continuous){
    for (n in 1:n_rows)
      /*In the continuous case we are actually modelling the error term as normal/student*/
      dist_val[n] = (cases_real[n] - m[num_delays*(case_idx[n,s_col] - 1) + case_idx[n,d_col], case_idx[n,t_col]])/sd_m[1];
  } else if (is_discrete) {
    for (n in 1:n_rows)
      /*In the discrete case we are actually using the link*/
      dist_val[n] = m[num_delays*(case_idx[n,s_col] - 1) + case_idx[n,d_col], case_idx[n,t_col]];
  } else {
    reject("Invalid distribution @get_val_for_model. This is an internal error of the `diseasenowcasting` package. Please report to `https://github.com/RodrigoZepeda/diseasenowcasting/issues`");
  }

  return dist_val;
}
