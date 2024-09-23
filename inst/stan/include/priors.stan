#include include/license.stan

real dist_lpdf(vector x, real param_1, real param_2, int prior_spec) {
  #include include/continuous_prior_spec.stan
}

real dist_lpdf(real x, real param_1, real param_2, int prior_spec) {
  #include include/continuous_prior_spec.stan
}
