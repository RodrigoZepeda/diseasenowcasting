#include include/license.stan

/*
  FUNCTIONS TO SPECIFY PRIOR DISTRIBUTIONS----------------------------------------------------------

  The `dist_lpdf` function obtains the lpdf function specified by the user.
*/
real dist_lpdf(vector x, real param_1, real param_2, int prior_spec, int is_positive) {
  //See for function description:
  #include include/continuous_prior_spec.stan
}

real dist_lpdf(real x, real param_1, real param_2, int prior_spec, int is_positive) {
  //See for function description:
  #include include/continuous_prior_spec.stan
}
