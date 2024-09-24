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
  #include data/data_discrete.stan
}

transformed data {
  #include data/transformed_data.stan
}

parameters {
  #include parameters/parameters.stan
  #include parameters/parameters_discrete.stan
}

transformed parameters {
  #include parameters/transformed_parameters.stan
  #include parameters/transformed_parameters_discrete.stan
}

model {
  if (!prior_only){ //Don't calculate posterior if user only wants prior
    #include model/discrete_model.stan
  }

  // Add the priors
  target += lprior;
}
