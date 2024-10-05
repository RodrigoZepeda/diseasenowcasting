//Add observations
if (!prior_only){
  if (is_normal){
    target += std_normal_lpdf(dist_val);
  } else if (is_student){
    target += student_t_lpdf(dist_val | rep_vector(dof, n_rows), rep_vector(0.0, n_rows), rep_vector(1.0, n_rows));
  } else if (is_poisson){
    target += poisson_lpmf(cases_int | dist_val);
  } else if (is_negbin){
    target += neg_binomial_2_lpmf(cases_int | dist_val, rep_vector(1.0 / sd_m[1] , n_rows));
  } else {
    reject("Unknown distribution type @model. This is an internal error of the `diseasenowcasting` package. Please report to `https://github.com/RodrigoZepeda/diseasenowcasting/issues`");
  }
}
