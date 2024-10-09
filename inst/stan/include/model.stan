//Add observations
if (!prior_only){
  if (is_normal){
    target += normal_lpdf(cases_real_trans | dist_val, rep_vector(sd_m[1] , n_rows));
  } else if (is_student){
    target += student_t_lpdf(cases_real_trans | rep_vector(dof, n_rows), dist_val, rep_vector(1.0 / sd_m[1], n_rows));
  } else if (is_poisson){
    target += poisson_lpmf(cases_int | dist_val);
  } else if (is_negbin){
    target += neg_binomial_2_lpmf(cases_int | dist_val, rep_vector(1.0 / sd_m[1] , n_rows));
  } else {
    reject("Unknown distribution type @model. This is an internal error of the `diseasenowcasting` package. Please report to `https://github.com/RodrigoZepeda/diseasenowcasting/issues`");
  }
}
