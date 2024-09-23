if (prior_spec == 0)       return 0.0;                                             //Jeffrey's prior adds nothing to target
else if (prior_spec == 1)  return std_normal_lpdf(x);
else if (prior_spec == 2)  return normal_lpdf(x | param_1, param_2);               // Normal
else if (prior_spec == 3)  return student_t_lpdf(x | param_1, 0.0, param_2);       // Student
else if (prior_spec == 4)  return cauchy_lpdf(x | param_1, param_2);               // Cauchy
else if (prior_spec == 4)  return exponential_lpdf(x | param_1);                   // Exponential
else if (prior_spec == 5)  return gamma_lpdf(x | param_1, param_2);                // Gamma
else if (prior_spec == 6)  return inv_gamma_lpdf(x | param_1, param_2);            // Inverse gamma
else if (prior_spec == 7)  return lognormal_lpdf(x | param_1, param_2);            // Lognormal
else if (prior_spec == 8)  return weibull_lpdf(x | param_1, param_2);              // Weibull
else if (prior_spec == 9)  return frechet_lpdf(x | param_1, param_2);              // Frechet
else if (prior_spec == 10) return double_exponential_lpdf(x | param_1, param_2);   // Double exponential
else if (prior_spec == 11) return logistic_lpdf(x | param_1, param_2);             // Logistic
else if (prior_spec == 12) return rayleigh_lpdf(x | param_1);                      // Rayleigh
else if (prior_spec == 13) return loglogistic_lpdf(x | param_1, param_2);          // Loglogistic
else if (prior_spec == 14) return gumbel_lpdf(x | param_1, param_2);               // Gumbel
else reject("invalid link");
return 0.0; // never reached
