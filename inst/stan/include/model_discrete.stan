//Add observations
if (!prior_only){
  if (is_poisson){
    target += poisson_log_lpmf(cases | dist_val);
  } else if (is_negbin){
    target += neg_binomial_2_log_lpmf(cases | dist_val, rep_vector(1.0 / sd_m[1] , n_rows));
  } else {
    reject("Unknown discrete distribution");
  }
}
