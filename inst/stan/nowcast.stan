// nowcast.stan
//
// Model that generates a prediction of current number of cases given historical trends
// and historical delays controlling by strata and covariates.
//
// Data:
// ------------------------------------------------------------------------------------------------

#include /include/license.stan

functions {
  #include include/linear_algebra_utils.stan
  #include include/trend.stan
  #include include/seasonal_discrete.stan
  #include include/state_space_model.stan
  #include include/priors.stan
}

data {
  #include data/data.stan
}

transformed data {
  #include data/transformed_data.stan
}

parameters {
  #include parameters/parameters.stan
}

transformed parameters {
  #include parameters/transformed_parameters.stan
}

model {
  //Don't calculate posterior if user only wants prior
  if (!prior_only){

    //Evaluate the model whether its negative binomial
    if (is_negative_binomial){
      target += neg_binomial_2_log_lpmf(N_cases[,n_col] | lambda_mean, rep_vector(r[1], num_elements(lambda_mean)));
    } else {
      target += poisson_log_lpmf(N_cases[,n_col] | lambda_mean);
    }

  }

  // Add the priors
  target += lprior;
}
