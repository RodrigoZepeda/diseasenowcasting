matrix count_cases(data array[,] int case_idx, data int num_strata, data int num_delays,
  data int n_rows, data int d_col, data int s_col){
  /*
  * @title Counts the number of times observed by strata and delay
  *
  * @param case_idx Index for the number of cases ndicating in column 1 the time,
  * column 2 the delay and column 3 the strata.
  * @param num_strata Number of strata considered in the cases
  * @param num_delays Number of delays considered in the cases
  * @param d_col Column number of case_idx where delays are stored
  * @param s_col Column number of case_idx where strata are stored
  *
  * @return An array with rows = strata and columns = delays. Each entry corresponds to the number of
  * times that strata and delay was observed.
  */
  matrix[num_strata, num_delays] n_count = rep_matrix(0, num_strata, num_delays);

  //Sum the elements to the
  for (n in 1:n_rows)
    n_count[case_idx[n,s_col],case_idx[n,d_col]] += 1;

  return n_count;

}

matrix mean_cases(data array[] real cases_real, data array[,] int case_idx,
  data int num_strata, data int num_delays, data int n_rows, data int d_col, data int s_col){
  /*
  * @title Calculate the average number of cases by strata and delay
  *
  * @param cases_real Number of cases per day with case_idx indicating in column 1 the time,
  * column 2 the delay and column 3 the strata.
  * @param case_idx Index for the number of cases in cases_real.
  * @param num_strata Number of strata considered in the cases
  * @param num_delays Number of delays considered in the cases
  * @param d_col Column number of case_idx where delays are stored
  * @param s_col Column number of case_idx where strata are stored
  * @param mode Either = 0 then the mean is calculated; if mode = 1 then the mean is 0 if only 1 case
  * was calculated
  *
  * @return A matrix with rows = strata and columns = delays.
  */
  matrix[num_strata, num_delays] mu = rep_matrix(0.0, num_strata, num_delays);

  //Calculate the total number of cases
  matrix[num_strata, num_delays] n_count = count_cases(case_idx, num_strata, num_delays, n_rows, d_col, s_col);

  //Sum to the elements in each entry. Leave the mean as 0 if nothing or 1was observed
  for (n in 1:n_rows){
    if (n_count[case_idx[n,s_col], case_idx[n,d_col]] > 0.0){
      mu[case_idx[n,s_col], case_idx[n,d_col]] += cases_real[n]/n_count[case_idx[n,s_col], case_idx[n,d_col]];
    }
  }
  return mu;
}

matrix sd_cases(data array[] real cases_real, data array[,] int case_idx,
    data int num_strata, data int num_delays, data int n_rows,  data int d_col, data int s_col){
  /*
  * @title Calculate the standard deviation of cases by strata and delay
  *
  * @param cases_real Number of cases per day with case_idx indicating in column 1 the time,
  * column 2 the delay and column 3 the strata.
  * @param case_idx Index for the number of cases in cases_real.
  * @param num_strata Number of strata considered in the cases
  * @param num_delays Number of delays considered in the cases
  * @param d_col Column number of case_idx where delays are stored
  * @param s_col Column number of case_idx where strata are stored
  *
  * @return A matrix with rows = strata and columns = delays. Each entry corresponds to the standard deviation.
  * if no cases were observed the sd is set to 1
  */
  matrix[num_strata, num_delays] sigma = rep_matrix(0.0, num_strata, num_delays);

  //Calculate the total number of cases
  matrix[num_strata, num_delays] n_count = count_cases(case_idx, num_strata, num_delays, n_rows, d_col, s_col);
  matrix[num_strata, num_delays] mu      = mean_cases(cases_real, case_idx, num_strata, num_delays, n_rows, d_col, s_col);

  //Sum to the elements in each entry. Leave the mean as 0 if nothing was observed
  for (n in 1:n_rows)
    sigma[case_idx[n,s_col], case_idx[n,d_col]] += pow(cases_real[n] - mu[case_idx[n,s_col], case_idx[n,d_col]], 2);

  for (d in 1:num_delays){
    for (s in 1:num_strata){
      if (n_count[s,d] > 0.0){
        sigma[s,d] = sqrt(sigma[s,d]/n_count[s,d]);
      } else {
        sigma[s,d] = 0.0;
      }
    }
  }

  return sigma;
}

//Normalize the number of cases
array[] real normalize_cases(data array[] real cases_real, data array[,] int case_idx,
    data int num_strata, data int num_delays, data int n_rows, data int d_col, data int s_col,
    matrix mu, matrix sigma){

    /*Get the normalized cases*/
    array[num_elements(cases_real)] real normalized;
    real mu_value;
    real sigma_value;

    for (n in 1:n_rows){
      //Get mu and sigma for the normalization
      mu_value    = mu[case_idx[n, s_col], case_idx[n, d_col]];
      sigma_value = sigma[case_idx[n, s_col], case_idx[n, d_col]];
      if (sigma_value > 0.0){
        normalized[n] = (cases_real[n] - mu_value)/sigma_value;
      } else {
        normalized[n] = cases_real[n];
      }
    }

    return normalized;

}

//Unnormalize the number of cases
array[] real inv_normalize_cases(data array[] real normalized, data array[,] int case_idx,
    data int num_strata, data int num_delays, data int n_rows, data int d_col, data int s_col,
    matrix mu, matrix sigma){

    /*Get the normalized cases*/
    array[num_elements(normalized)] real real_cases;
    real mu_value;
    real sigma_value;

    for (n in 1:n_rows){
      //Get mu and sigma for the normalization
      mu_value    = mu[case_idx[n, s_col], case_idx[n, d_col]];
      sigma_value = sigma[case_idx[n, s_col], case_idx[n, d_col]];
      if (sigma_value > 0.0){
        real_cases[n] = normalized[n]*sigma_value + mu_value;
      } else {
        real_cases[n] = normalized[n];
      }
    }

    return real_cases;

}

array[,] real inv_normalize_cases_2(array[,] real normalized_mat, data array[,] int case_idx,
    data int num_strata, data int num_delays, data int num_steps, int tsize, data int d_col,
    data int s_col, matrix mu, matrix sigma){

    /*Get the un-normalized matrix*/
    array[num_steps, tsize] real unnormalized_mat = rep_array(0, num_steps, tsize);

    for (s in 1:num_strata){
      for (d in 1:num_delays){
        /*Loop through all the time steps*/
        for (t in 1:num_steps){
          if (sigma[s,d] > 0.0){
            unnormalized_mat[t,num_delays*(s - 1) + d] = normalized_mat[t, num_delays*(s - 1) + d]*sigma[s,d] + mu[s,d];
          } else {
            unnormalized_mat[t,num_delays*(s - 1) + d] = normalized_mat[t, num_delays*(s - 1) + d];
          }
        }
      }
    }

    return unnormalized_mat;

}

