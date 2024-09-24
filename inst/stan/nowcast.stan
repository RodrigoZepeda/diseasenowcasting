// nowcast.stan
//
// Model that generates a prediction of current number of cases given historical trends
// and historical delays controlling by strata and covariates.
//
// Data:
// ------------------------------------------------------------------------------------------------

#include license/license.stan

functions {
  #include include/linear_algebra_utils.stan
  #include include/trend.stan
  #include include/seasonal_discrete.stan
  #include include/state_space_model.stan
  #include lpdfs_lpmfs/distributions.stan
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
  if (!prior_only){ //Don't calculate posterior if user only wants prior
    if(is_discrete){
      target += discrete_data_distribution_lpmf(N_cases_int[,1] | lambda_mean, rvec, data_distribution); //Evaluate the model
    } else {
      target += continuous_data_distribution_lpdf(N_cases_ct[,1] | lambda_mean, rvec, data_distribution); //Evaluate the model
    }
  }

  // Add the priors
  target += lprior;
}
