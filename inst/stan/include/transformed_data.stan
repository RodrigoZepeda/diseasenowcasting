/*Get the total size of the vectors*/
int tsize = num_delays*num_strata;

/*Get the columns of the case_idxs*/
int t_col = 1; //Time is stored in this column.
int d_col = 2; //Delay is stored in this column.
int s_col = 3; //Strata is stored in this column.

/*Get the distribution*/
int is_normal     = dist == 0 ? 1: 0;       //Normal distribution flag
int is_student    = dist == 1 ? 1: 0;       //Student-T distribution flag
int is_poisson    = dist == 2 ? 1: 0;       //Poisson distribution flag
int is_negbin     = dist == 3 ? 1: 0;       //Negative Binomial distribution flag
int is_continuous = is_normal + is_student; //Distribution type is continuous
int is_discrete   = is_poisson + is_negbin; //Distribution type is discrete

/*Get the link for the model mean*/
int identity_link_x   = link_x == 0 ? 1: 0; //Leave y = y
int log_link_x        = link_x == 1 ? 1: 0; //This is actually log1p(y) = log(y + 1) to ensure no zeroes
int softplus_link_x   = link_x == 2 ? 1: 0; //Softplus link from https://doi.org/10.3390/e25101372
int dist_hyper_link_x = link_x == 3 ? 1: 0; //Distorted hyperbolic link from https://doi.org/10.3390/e25101372

int identity_link_y   = link_y == 0 ? 1: 0; //Leave y = y
int log_link_y        = link_y == 1 ? 1: 0; //This is actually log1p(y) = log(y + 1) to ensure no zeroes
int softplus_link_y   = link_y == 2 ? 1: 0; //Softplus link from https://doi.org/10.3390/e25101372
int dist_hyper_link_y = link_y == 3 ? 1: 0; //Distorted hyperbolic link from https://doi.org/10.3390/e25101372

/*Apply any of the link functions to the case data*/
array[n_rows] real cases_real_trans; //Transformed real cases
array[n_rows] int  cases_int_trans;  //Transformed real cases

/*Transform the data according to the link in y*/
if (identity_link_y){
  cases_real_trans = cases_real;                                                                    //No transform needed with identity link
  cases_int_trans  = cases_int;                                                                     //No transform needed with identity link
} else if (log_link_y && is_continuous){
  cases_real_trans = log1p_transform(cases_real);                                                   //Apply log to the cases which is actually log1p
} else if (softplus_link_y && is_continuous){
  cases_real_trans = softplus1p_transform(cases_real, control_k_transform);                         //Apply the softplus transformation
} else if (dist_hyper_link_y && is_continuous){
  cases_real_trans = dhyperbolic1p_transform(cases_real, control_k_transform, control_c_transform); //Apply the distorted hyperbolic transformation
} else {
  reject("Cannot transform non-continuous data with other link than `identity`. Did you mean to set `identity_link_x`? This is an internal error of the `diseasenowcasting` package @transformed_data.stan. Report to `https://github.com/RodrigoZepeda/diseasenowcasting/issues`");
}

matrix[num_strata, num_delays] mu_data;
matrix[num_strata, num_delays] sigma_data;

/*Transform the data if centering was required*/
if (normalize_data && is_continuous){ //Doesn't make sense to normalize if data is discrete

  /*Obtain the mean and variance if needed*/
  mu_data    = mean_cases(cases_real_trans, case_idx, num_strata, num_delays, n_rows, d_col, s_col);
  sigma_data = sd_cases(cases_real_trans, case_idx, num_strata, num_delays, n_rows, d_col, s_col);

  /*Normalize the number of cases*/
  cases_real_trans = normalize_cases(cases_real_trans, case_idx, num_strata, num_delays, n_rows,
    d_col, s_col, mu_data, sigma_data);

} else if (normalize_data && !is_continuous) {
  reject("Cannot normalize non-continuous data. This is an internal error of the `diseasenowcasting` package @transformed_data.stan. Report to `https://github.com/RodrigoZepeda/diseasenowcasting/issues`");
}
