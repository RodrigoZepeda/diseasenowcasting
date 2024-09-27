//Caps the lambda so that if it explodes it doesn't destroy the poisson and binomial
for (t in 1:num_steps){
  for (j in 1:(num_delays*num_strata)){
    lambda_transformed[j, t] = lambda[j,t];
  }
}

for (t in 1:num_steps){
    if (is_normal){
        N_mat_predict[t,:] =
        normal_rng(lambda_transformed[:, t]', rep_vector(r[t,1] + precision_tol, num_delays*num_strata));
    } else if (is_student) {
        N_mat_predict[t,:] = student_t_rng(
          rep_vector(3.0, num_delays*num_strata),
          lambda_transformed[:, t]',
          rep_vector(r[t,1] + precision_tol, num_delays*num_strata));
    } else {
      reject("Invalid distribution specified");
    }
}
