//Add observations
if (!prior_only){
  if (is_normal){
    target += std_normal_lpdf(dist_val);
  } else if (is_student){
    target += student_t_lpdf(dist_val | rep_vector(dof, n_rows), rep_vector(0.0, n_rows), rep_vector(1.0, n_rows));
  } else {
    reject("Unknown continuous distribution");
  }
}
