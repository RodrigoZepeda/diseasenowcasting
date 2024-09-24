//MODEL FOR DISCRETE MODELS
//Evaluate the model whether its negative binomial
if (is_negative_binomial){
  target += normal_lpdf(Cases[,n_col] | lambda_mean, rep_vector(r[1], num_elements(lambda_mean)));
} else {
  target += student_t_lpdf(Cases[,n_col] | 3.0, lambda_mean, rep_vector(r[1], num_elements(lambda_mean)));
}
