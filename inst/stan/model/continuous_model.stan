//MODEL FOR DISCRETE MODELS
//Evaluate the model whether its negative binomial
if (is_normal){
  target += normal_lpdf(Cases[,n_col] | lambda_mean, r_mean);
} else if (is_student){
  target += student_t_lpdf(Cases[,n_col] | 3.0, lambda_mean, r_mean);
} else {
  reject("Invalid model specified in model{}");
}
