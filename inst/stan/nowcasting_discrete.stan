//Version 0.0

functions {
  #include include/ss_model.stan
  #include include/get_val_for_model.stan
}

data {
  #include include/data.stan
  array[n_rows] int cases;
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
  #include include/model_discrete.stan

  // Add the priors
  target += lprior;
}
