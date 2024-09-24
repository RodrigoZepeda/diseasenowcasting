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

//Simulate from model
for (t in 1:num_steps){
    if (is_negative_binomial){
        N_mat_predict[t,:] =
        neg_binomial_2_log_rng(lambda_transformed[:, t]', rep_vector(r[1] + precision_tol, num_delays*num_strata));
    } else {
        N_mat_predict[t,:] = poisson_log_rng(lambda_transformed[:, t]');
    }
}
