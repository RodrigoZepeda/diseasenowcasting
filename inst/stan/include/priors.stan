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
    if (prior_spec == 1) return std_normal_lpdf(x);
    else if (prior_spec == 2) return normal_lpdf(x | param_1, param_2);  // Normal (general)
    else if (prior_spec == 3) return student_t_lpdf(x | param_1, 0.0, param_2);   // Student (general)
    else if (prior_spec == 4) return cauchy_lpdf(x | param_1, param_2);   // Cauchy
    else if (prior_spec == 4) return exponential_lpdf(x | param_1);   // Exponential
    else if (prior_spec == 5) return gamma_lpdf(x | param_1, param_2);   // Gamma
    else if (prior_spec == 6) return inv_gamma_lpdf(x | param_1, param_2);   // Inverse gamma
    else if (prior_spec == 7) return lognormal_lpdf(x | param_1, param_2);   // Lognormal
    else if (prior_spec == 8) return weibull_lpdf(x | param_1, param_2);   // Weibull
    else if (prior_spec == 9) return frechet_lpdf(x | param_1, param_2);   // Frechet
    else if (prior_spec == 10) return double_exponential_lpdf(x | param_1, param_2);   // Double exponential
    else if (prior_spec == 11) return logistic_lpdf(x | param_1, param_2);   // Double exponential
    else if (prior_spec == 12) return rayleigh_lpdf(x | param_1);   // Lognormal
    else if (prior_spec == 13) return loglogistic_lpdf(x | param_1, param_2);   // Lognormal
    else if (prior_spec == 14) return gumbel_lpdf(x | param_1, param_2);   // Lognormal
    else reject("invalid link");
    return 0.0; // never reached
  }
