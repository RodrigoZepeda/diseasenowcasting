//Filefor checking all of the functions in stan
#include include/license.stan

functions {
  #include include/linear_algebra_utils.stan
  #include include/trend.stan
  #include include/seasonal_discrete.stan
  #include include/state_space_model.stan
}
