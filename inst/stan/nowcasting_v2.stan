//Version 0.0

functions {
    #include include/ss_model.stan
    #include include/observed_mean.stan
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
    //Add observations
    if (!prior_only)
      target += normal_lpdf(cases | m_obs, rep_vector(sd_obs, n_rows));

    // Add the priors
    target += lprior;
}
