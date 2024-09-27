/*
  * Distribution lpdf
  *
  * Function that generates the lpdf to sum to the prior inside transformed parameters
  * according to the prior specification, `prior_spec` and considering whether the
  * parameter under consideration is positive or not (`is_positive`).
  *
  * @param x A vector containing `parameters` that is distributed as:
  * x ~ dist_lpdf(param_1, param_2)
  * @param param_1 The first parameter for the distribution of x
  * @param param_2 The second parameter for the distribution of x
  * @param prior_spec A number indicating what is the parameter. See the note below
  * @param is_positive Either 1 if the parameter x is positive or 0 if it is on the real line
  *
  * @note If the distribution doesn't have param_1 or param_2 (or both) you can write any number.
  * As an example for a standard normal distribution you can write:
  * dist_lpdf(x, 10, 10, 1, 0);
  * and the function will ignore the values for param_1 and param_2 as they are of no use for a
  * standard normal.
  * Additionally if the distribution only has one parameter, (e.g. an exponential) this will ignore
  * the entry in param_2. So to call an exponential(3) you would do:
  * dist_lpdf(x, 3, 10, 1, 5);
  * and the value of 10 for param_2 is ignored.
  *
  * @section Specified distributions
  * The following table specifies the distirbutions
  *
  *
  *
  *
  * @return An lpdf object to sum to lprior.
*/

//This is the main body of the dist_lpdf function:
if (is_positive){
  //Positive distributions should be lpdf - lccdf such that it integrates correctly
  if (prior_spec == 0)       return 0.0;                                                                                            // Jeffrey's prior adds nothing to target
  else if (prior_spec == 1)  return std_normal_lpdf(x);                                                                             // Normal
  else if (prior_spec == 2)  return normal_lpdf(x | param_1, param_2) - normal_lccdf(0 | param_1, param_2);                         // Normal
  else if (prior_spec == 3)  return student_t_lpdf(x | 3.0, param_1, param_2) - student_t_lccdf(0 | 3.0, param_1, param_2);         // Student
  else if (prior_spec == 4)  return cauchy_lpdf(x | param_1, param_2) - cauchy_lccdf(0 | param_1, param_2);                         // Cauchy
  else if (prior_spec == 5)  return exponential_lpdf(x | param_1);                                                                  // Exponential
  else if (prior_spec == 6)  return gamma_lpdf(x | param_1, param_2);                                                               // Gamma
  else if (prior_spec == 7)  return inv_gamma_lpdf(x | param_1, param_2);                                                           // Inverse gamma
  else if (prior_spec == 8)  return lognormal_lpdf(x | param_1, param_2);                                                           // Lognormal
  else if (prior_spec == 9)  return weibull_lpdf(x | param_1, param_2);                                                             // Weibull
  else if (prior_spec == 10) return frechet_lpdf(x | param_1, param_2);                                                             // Frechet
  else if (prior_spec == 11) return double_exponential_lpdf(x | param_1, param_2) - double_exponential_lccdf(0 | param_1, param_2); // Double exponential
  else if (prior_spec == 12) return logistic_lpdf(x | param_1, param_2) - logistic_lccdf(0 | param_1, param_2);                     // Logistic
  else if (prior_spec == 13) return rayleigh_lpdf(x | param_1);                                                                     // Rayleigh
  else if (prior_spec == 14) return loglogistic_lpdf(x | param_1, param_2);                                                         // Loglogistic
  else if (prior_spec == 15) return gumbel_lpdf(x | param_1, param_2) - gumbel_lccdf(0 | param_1, param_2);                         // Gumbel
  else if (prior_spec == 16) return uniform_lpdf(x | param_1, param_2) - uniform_lccdf(0 | param_1, param_2);                       // Uniform
  else reject("invalid link");
  return 0.0; // never reached
} else {
  //Real-valued distributions should be lpdf only
  if (prior_spec == 0)       return 0.0;                                             //Jeffrey's prior adds nothing to target
  else if (prior_spec == 1)  return std_normal_lpdf(x);                              // Standard normal
  else if (prior_spec == 2)  return normal_lpdf(x | param_1, param_2);               // Normal
  else if (prior_spec == 3)  return student_t_lpdf(x | 3.0, param_1, param_2);       // Student
  else if (prior_spec == 4)  return cauchy_lpdf(x | param_1, param_2);               // Cauchy
  else if (prior_spec == 5)  return exponential_lpdf(x | param_1);                   // Exponential
  else if (prior_spec == 6)  return gamma_lpdf(x | param_1, param_2);                // Gamma
  else if (prior_spec == 7)  return inv_gamma_lpdf(x | param_1, param_2);            // Inverse gamma
  else if (prior_spec == 8)  return lognormal_lpdf(x | param_1, param_2);            // Lognormal
  else if (prior_spec == 9)  return weibull_lpdf(x | param_1, param_2);              // Weibull
  else if (prior_spec == 10) return frechet_lpdf(x | param_1, param_2);              // Frechet
  else if (prior_spec == 11) return double_exponential_lpdf(x | param_1, param_2);   // Double exponential
  else if (prior_spec == 12) return logistic_lpdf(x | param_1, param_2);             // Logistic
  else if (prior_spec == 13) return rayleigh_lpdf(x | param_1);                      // Rayleigh
  else if (prior_spec == 14) return loglogistic_lpdf(x | param_1, param_2);          // Loglogistic
  else if (prior_spec == 15) return gumbel_lpdf(x | param_1, param_2);               // Gumbel
  else if (prior_spec == 16) return uniform_lpdf(x | param_1, param_2);              // Uniform
  else reject("invalid link");
}
