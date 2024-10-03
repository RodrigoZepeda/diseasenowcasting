array[] real observed_mean(int n_rows, int num_delays, matrix m, array[,] int case_idx, int s_col,
  int d_col, int t_col){

    array[n_rows] real m_obs;
    for (n in 1:n_rows)
      m_obs[n] = m[num_delays*(case_idx[n,s_col] - 1) + case_idx[n,d_col], case_idx[n,t_col]];

    return m_obs;

}

