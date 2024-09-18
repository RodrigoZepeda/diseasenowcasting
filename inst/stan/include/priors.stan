 /**
  * Function for deciding the priors for coefficients
  *
  * @param eta Linear predictor vector
  * @param link An integer indicating the link function
  * @return A vector, i.e. inverse-link(eta)
  *
  * @note If adding a distribution here also change `get_prior_code` function.
  */
  real dist_lpdf(vector x, real param_1, real param_2, int prior_spec) {
    #include include/continuous_prior_spec.stan
  }

  real dist_lpdf(real x, real param_1, real param_2, int prior_spec) {
    #include include/continuous_prior_spec.stan
  }
