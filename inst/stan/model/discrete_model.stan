//MODEL FOR DISCRETE MODELS
//Evaluate the model whether its negative binomial
if (is_negative_binomial){
  target += neg_binomial_2_log_lpmf(Cases[,n_col] | 48.56737*lambda_mean, r_mean);
} else if (is_poisson) {
  target += poisson_log_lpmf(Cases[,n_col] | 48.56737*lambda_mean);
} else {
  reject("Invalid model specified in model{}");
}
