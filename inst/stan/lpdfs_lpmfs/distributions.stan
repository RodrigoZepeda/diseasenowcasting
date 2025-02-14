#include license/license.stan
//Distributions for the priors
real dist_lpdf(vector x, real param_1, real param_2, int prior_spec, int is_positive) {
  #include lpdfs_lpmfs/snippets/continuous_prior_spec.stan
}

real dist_lpdf(real x, real param_1, real param_2, int prior_spec, int is_positive) {
  #include lpdfs_lpmfs/snippets/continuous_prior_spec.stan
}

//Distributions for the data (cases)
real continuous_data_distribution_lpdf(real x, real param_1, real param_2, int prior_spec){
  #include lpdfs_lpmfs/snippets/continuous_data_prior_lpdf.stan
}

real continuous_data_distribution_lpdf(array[] real x, vector param_1, vector param_2, int prior_spec){
  #include lpdfs_lpmfs/snippets/continuous_data_prior_lpdf.stan
}

real continuous_data_distribution_lpdf(array[] real x, real param_1, real param_2, int prior_spec){
  #include lpdfs_lpmfs/snippets/continuous_data_prior_lpdf.stan
}

real discrete_data_distribution_lpmf(array[] int x, real param_1, real param_2, int prior_spec){
  #include lpdfs_lpmfs/snippets/discrete_data_prior_lpmf.stan
}

real discrete_data_distribution_lpmf(array[] int x, vector param_1, vector param_2, int prior_spec){
  #include lpdfs_lpmfs/snippets/discrete_data_prior_lpmf.stan
}

real discrete_data_distribution_lpmf(int x, real param_1, real param_2, int prior_spec){
  #include lpdfs_lpmfs/snippets/discrete_data_prior_lpmf.stan
}

real  continuous_data_distribution_rng(real param_1, real param_2, int prior_spec){
  #include lpdfs_lpmfs/snippets/continuous_data_prior_rng.stan
}


array[] real  continuous_data_distribution_rng(vector param_1, vector param_2, int prior_spec){
  #include lpdfs_lpmfs/snippets/continuous_data_prior_rng.stan
}

array[] real  continuous_data_distribution_rng(row_vector param_1, vector param_2, int prior_spec){
  #include lpdfs_lpmfs/snippets/continuous_data_prior_rng.stan
}

int discrete_data_distribution_rng(real param_1, real param_2, int prior_spec){
  #include lpdfs_lpmfs/snippets/discrete_data_prior_rng.stan
}

array[] int discrete_data_distribution_rng(vector param_1, vector param_2, int prior_spec){
  #include lpdfs_lpmfs/snippets/discrete_data_prior_rng.stan
}

array[] int discrete_data_distribution_rng(row_vector param_1, vector param_2, int prior_spec){
  #include lpdfs_lpmfs/snippets/discrete_data_prior_rng.stan
}


int check_has_r(int data_distribution){
  //Check whether it is Poisson
  if (data_distribution == 100 || data_distribution == 102){
    return 0;
  } else {
    return 1;
  }

}
