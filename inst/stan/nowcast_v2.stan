// nowcast.stan
//
// Model that generates a prediction of current number of cases given historical trends
// and historical delays controlling by strata and covariates.
//
// Data:
// ------------------------------------------------------------------------------------------------
// Data consists of a matrix Nmat of nobs × (3 + num_covariates)
// where
// num_covariates .- Number of covariates included in the model.
// nobs           .- The number of observations is bounded by (max_time × max_delays × num_strata)
//
// with:
// max_time       .- Length of the time frame under consideration.
// max_delays     .- Maximum number of delays to consider.
// num_strata     .- Number of variables to stratify by.
//
// Here is an example of what the Nmat should look like:
//
// | Number of Cases | Time | Delay | Covariate 1 | Covariate 2 | Covariate 3 |
// |-----------------|------|-------|-------------|-------------|-------------|
// | 5               | 1    | 1     | 0.1         | 0.3         | 0.5         |
// | 7               | 1    | 2     | 0.2         | 0.4         | 0.6         |
// | 4               | 2    | 1     | 0.1         | 0.3         | 0.5         |
// | 6               | 2    | 2     | 0.2         | 0.4         | 0.6         |
// | 8               | 3    | 1     | 0.1         | 0.3         | 0.5         |
// | 9               | 3    | 2     | 0.2         | 0.4         | 0.6         |
// | 10              | 1    | 1     | 0.2         | 0.5         | 0.7         |
// | 11              | 1    | 2     | 0.3         | 0.6         | 0.8         |
// | 8               | 2    | 1     | 0.2         | 0.5         | 0.7         |
// | 7               | 2    | 2     | 0.3         | 0.6         | 0.8         |
// | 12              | 3    | 1     | 0.2         | 0.5         | 0.7         |
// | 14              | 3    | 2     | 0.3         | 0.6         | 0.8         |
//
// Model:
// ------------------------------------------------------------------------------------------------
// The model infers the total number of cases by time t that will be observed with delay d
// for strata s (n_{t,d}^{s}) assuming:
//
// n_{t,d}^{s} ~ F(lambda_{t,d}^{s}, theta) for a discrete distribution F (either Poisson or NegBinomial)
//
// where:
// log(lambda_{t,d}^{s}) = alpha_t^{s} + beta_d^{s} + Beta^{s}*Covariates
//
// and the dynamic priors:
// alpha_t - alpha_{t-1} ~ Normal(0, sigma_alpha)
//
// and the priors:
// alpha_0 ~ Normal(alpha_mean_prior, alpha_sd_prior)
// beta_d  ~ Normal(mu, sigma_beta)
//
// Distribution options:
// ------------------------------------------------------------------------------------------------
// The following discrete distributions are implemented:
// 0. Poisson
// 1. Negative Binomial
//
// Additional notes:
// Should add zero-inflation for cases when people stratify too much

#include /include/license.stan

functions {
  #include include/linear_algebra_utils.stan
  #include include/trend.stan
  #include include/seasonal_discrete.stan
  #include include/state_space_model.stan
  #include include/priors.stan
}

data {

    //Data -----------------------------------------------------------------------------------------
    int<lower=1> num_steps;      //Number of time steps modelled
    int<lower=0> num_delays;     //Maximum number of unique delays considered
    int<lower=1> num_strata;     //Number of strata included in the model
    int<lower=1> n_rows;         //Number of rows in data Nmat

    //Data with time, delays, strata. Each entry is a case count -----------------------------------
    array[n_rows, 4] int N_cases; //Matrix with first entry = n, second = time, third = delay, fourth = strata

    //Trend options --------------------------------------------------------------------------------
    int<lower=0> mu_degree;              //Degree associated to the mu's trend
    int<lower=0,upper=1> mu_is_constant; //Whether the mu effect is constant
    int<lower=0> nu_degree;                 //Degree associated to the delay's trend
    int<lower=0,upper=1> nu_is_constant;    //Whether the delay effect is constant

    //Sampling options -----------------------------------------------------------------------------
    int<lower=0, upper=1> is_negative_binomial; //Either 0 = Poisson or 1 = NegativeBinomial
    int<lower=0, upper=1> prior_only;   //Set to 1 to sample only from the prior

    //Priors ---------------------------------------------------------------------------------------
    real<lower=0> dispersion_prior_shape;
    real<lower=0> dispersion_prior_rate;

    real<lower=0> mu_shape_prior;
    real<lower=0> mu_rate_prior;

    real<lower=0> nu_shape_prior;
    real<lower=0> nu_rate_prior;

    int<lower=1,upper=14> mu_prior;
    int<lower=1,upper=14> nu_prior;
    int<lower=1,upper=14> r_prior;

    //Prior values for the initial mu_0 and nu_0
    real mean_mu_0_prior;
    real mean_nu_0_prior;
    real<lower=0> sigma_mu_0_prior;
    real<lower=0> sigma_nu_0_prior;

}

transformed data {

  //FIXME: Change these to be non constant
  vector[1] B_cnt = rep_vector(0.0, 1);
  matrix[1,1] X_cnt = rep_matrix(0.0, 1, 1);

  //Trend related transformations-------------------------------------------------------------------
  //Create trend matrices for mu (mu) and delay (nu)

  //A
  int nrows_mu_trend_A = get_num_rows_A_trend(mu_degree);
  int ncols_mu_trend_A = get_num_cols_A_trend(mu_degree);
  matrix[nrows_mu_trend_A, ncols_mu_trend_A] A_mu = create_trend_matrix_block_A(mu_degree);

  int nrows_nu_trend_A = get_num_rows_A_trend(nu_degree);
  int ncols_nu_trend_A = get_num_cols_A_trend(nu_degree);
  matrix[nrows_nu_trend_A, ncols_nu_trend_A] A_nu = create_trend_matrix_block_A(nu_degree);

  //R
  int nrows_mu_trend_R = get_num_rows_R_trend(mu_degree);
  int ncols_mu_trend_R = get_num_cols_R_trend(mu_degree);
  matrix[nrows_mu_trend_R, ncols_mu_trend_R] R_mu = create_trend_matrix_block_R(mu_degree, mu_is_constant);

  int nrows_nu_trend_R = get_num_rows_R_trend(nu_degree);
  int ncols_nu_trend_R = get_num_cols_R_trend(nu_degree);
  matrix[nrows_nu_trend_R, ncols_nu_trend_R] R_nu = create_trend_matrix_block_R(nu_degree, nu_is_constant);

  //L
  int num_elements_mu_L = get_num_elements_L_trend(mu_degree);
  vector[num_elements_mu_L] L_mu = create_trend_vector_block_L(mu_degree);

  int num_elements_nu_L = get_num_elements_L_trend(nu_degree);
  vector[num_elements_nu_L] L_nu = create_trend_vector_block_L(nu_degree);

  //Get initial mu and nu sizes
  int mu_0_size = num_elements_mu_L;
  int nu_0_size = num_elements_nu_L;

  //Get initial vector sizes for the errors
  int xi_mu_size = nrows_mu_trend_R;
  int xi_nu_size = nrows_nu_trend_R;

  //Get the columns of ncases
  int n_col = 1;
  int t_col = 2;
  int d_col = 3;
  int s_col = 4;

}

parameters {
  //Initial values for mu and nu with centered Gaussian parametrizations
  matrix[num_strata*num_delays, mu_0_size] mu_0_centered;
  matrix[num_strata*num_delays, nu_0_size] nu_0_centered;

  //Normalized errors
  array[num_steps - 1] matrix[num_strata*num_delays, xi_mu_size] xi_mu;
  array[num_steps - 1] matrix[num_strata*num_delays, xi_nu_size] xi_nu;

  //Precision parameter for negative binomial
  vector<lower=0>[is_negative_binomial ? 1 : 0] r;
  vector<lower=0>[1] sigma_mu;
  vector<lower=0>[1] sigma_nu;

}

transformed parameters {

  //Values for mu and nu without centering
  matrix[num_strata*num_delays, mu_0_size] mu_0 = rep_matrix(mean_mu_0_prior, num_strata*num_delays, mu_0_size) + sigma_mu_0_prior*mu_0_centered;
  matrix[num_strata*num_delays, nu_0_size] nu_0 = rep_matrix(mean_nu_0_prior, num_strata*num_delays, nu_0_size) + sigma_nu_0_prior*nu_0_centered;

  //Get the state space process simulations
  matrix[num_delays*num_strata, num_steps] lambda = state_space_process_v3(
        num_steps, num_delays, num_strata, A_mu, A_nu, sigma_mu[1]*R_mu, sigma_nu[1]*R_nu,
        L_mu, L_nu, mu_0, xi_mu, nu_0, xi_nu, B_cnt, X_cnt);

  //Create a vectorized version of the lambda
  //The lambda function is organized by delays and then strata so
  //
  //   Strata      |    Delay    |    Lambda    |
  //---------------------------------------------
  //      1        |      1      |      1       |
  //      2        |      1      |      2       |
  //      1        |      2      |      3       |
  //      2        |      2      |      4       |
  //      1        |      3      |      5       |
  //      2        |      3      |      6       |
  //      1        |      4      |      7       |
  //      2        |      4      |      8       |
  //---------------------------------------------
  //
  vector[n_rows] lambda_mean;
  for (n in 1:n_rows)
    lambda_mean[n] = lambda[num_strata*(N_cases[n,d_col] - 1) + N_cases[n,s_col], N_cases[n,t_col]];

  //Priors
  // ------------------------------------------------------------------------------------------------
  real lprior = 0;
  lprior += std_normal_lpdf(to_vector(mu_0_centered));
  lprior += std_normal_lpdf(to_vector(nu_0_centered));

  for (t in 1:(num_steps - 1)){
    lprior += std_normal_lpdf(to_vector(xi_mu[t]));
    lprior += std_normal_lpdf(to_vector(xi_nu[t]));
  }

  //Priors for the mu and the nu
  lprior += dist_lpdf(sigma_mu| mu_shape_prior, mu_rate_prior, mu_prior); //Prior for sigma_mu
  lprior += dist_lpdf(sigma_nu| nu_shape_prior, nu_rate_prior, nu_prior); //Prior for sigma_nu

  //Add prior to the negative binomial precision
  if (is_negative_binomial)
    lprior += dist_lpdf(r| dispersion_prior_shape, dispersion_prior_rate, r_prior);

}

model {
  //Don't calculate posterior if user only wants prior
  if (!prior_only){

    //Evaluate the model whether its negative binomial
    if (is_negative_binomial){
      target += neg_binomial_2_log_lpmf(N_cases[,n_col] | lambda_mean, rep_vector(r[1], num_elements(lambda_mean)));
    } else {
      target += poisson_log_lpmf(N_cases[,n_col] | lambda_mean);
    }

  }

  // Add the priors
  target += lprior;

}

