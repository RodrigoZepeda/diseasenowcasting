#include license/license.stan

functions {
  #include include/ss_model.stan
  #include include/get_val_for_model.stan
  #include include/transform_state_space_model.stan
  #include include/mean_sd_cases.stan
}

data {
  #include include/data.stan
}

transformed data {
  #include include/transformed_data.stan
}

parameters {
  #include include/parameters.stan
}

transformed parameters {
  #include include/transformed_parameters.stan
}

model {
  #include include/model.stan

  // Add the priors
  target += lprior;
}
