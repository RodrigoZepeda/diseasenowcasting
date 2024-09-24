//MODEL FOR DISCRETE MODELS
//Evaluate the model whether its negative binomial
if (is_negative_binomial){
  target += neg_binomial_2_log_lpmf(Cases[,n_col] | lambda_mean, rep_vector(r[1], num_elements(lambda_mean)));
} else {
  target += poisson_log_lpmf(Cases[,n_col] | lambda_mean);
}
