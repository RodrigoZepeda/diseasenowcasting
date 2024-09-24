//Add prior to the negative binomial precision
if (is_negative_binomial)
  lprior += dist_lpdf(r | r_param_1, r_param_2, r_prior, 1);
