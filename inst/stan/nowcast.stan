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
// Currentlty strata and covariates are not programmed.
// Should add zero-inflation for cases when people stratify too much

#include /include/license.stan
// #include /include/timetrends.stan

data {

    //Data
    // ------------------------------------------------------------------------------------------------
    int<lower=1> max_time;       //Maximum number of unique times modelled
    int<lower=0> max_delays;     //Maximum number of unique delays considered
    int<lower=0> num_strata;     //Number of strata included in the model
    int<lower=0> num_covariates; //Number of covariates included in the model
    int<lower=0> nobs;           //Number of rows in data Nmat
    array[nobs, 3 + num_covariates] int Nmat; //Matrix with first entry = n, second = time, third = delay

    //Options
    // ------------------------------------------------------------------------------------------------
    int<lower=0, upper=1> is_negative_binomial; //Either 0 = Poisson or 1 = NegativeBinomial
    int<lower=0, upper=1> prior_only;   //Set to 1 to sample only from the prior

    //Priors
    // ------------------------------------------------------------------------------------------------
    real<lower=0> dispersion_prior_shape;
    real<lower=0> dispersion_prior_rate;

    real alpha_mean_prior;
    real<lower=0> alpha_sd_prior;

    real<lower=0> alphat_shape_prior;
    real<lower=0> alphat_rate_prior;

    real beta_mean_prior;
    real<lower=0> beta_sd_prior;
}

transformed data {
  //real tol = 1.e-20;
  //Number of observations
  int ncovs = 3 + num_covariates;
}

parameters {
    vector<lower=0>[is_negative_binomial ? 1 : 0] r; //Precision parameter for negative binomial
    vector[max_time] alpha_centered;        //Parameter for random walk
    vector[max_delays + 1] beta_centered;   //Parameter for delays (delays start at 0)
    vector[num_covariates] coef_covariates; //Parameter for covariates beta
    real<lower=0> sigma_alpha_t;            //Noise parameter for alpha
}

transformed parameters {

  //Centered parameters
  // ------------------------------------------------------------------------------------------------
  vector[max_time] alpha; //Parameter for random walk
  alpha[1] = alpha_mean_prior + alpha_sd_prior*alpha_centered[1];
  for (t in 2:max_time)
    alpha[t] = alpha[t - 1] + sigma_alpha_t*alpha_centered[t];

  vector[max_delays + 1] beta; //Parameter for random walk
  beta = beta_mean_prior + beta_sd_prior*beta_centered;

  //Regression coefficients
  // ------------------------------------------------------------------------------------------------
  matrix[max_time, max_delays + 1] lambda;
  for (d in 1:(max_delays + 1))
    lambda[,d] = alpha + beta[d];

  //Priors
  // ------------------------------------------------------------------------------------------------
  real lprior = 0;
  lprior += std_normal_lpdf(beta_centered);
  lprior += std_normal_lpdf(alpha_centered);
  lprior += inv_gamma_lpdf(sigma_alpha_t | alphat_shape_prior, alphat_rate_prior); //Prior for sigma_t

  //Add prior to the negative binomial precision
  if (is_negative_binomial)
    lprior += gamma_lpdf(r | dispersion_prior_shape, dispersion_prior_rate);

}

model {

  //Don't calculate posterior if user only wants prior
  if (!prior_only){

    //Evaluate the model whether its negative binomial
    if (is_negative_binomial){ //If outside for = faster
      for (k in 1:nobs)
        target += neg_binomial_2_log_lpmf(Nmat[k, 1] | lambda[Nmat[k, 2], Nmat[k, 3] + 1], r);
    } else {
      for (k in 1:nobs)
        target += poisson_log_lpmf(Nmat[k, 1] | lambda[Nmat[k, 2], Nmat[k, 3]  + 1]);
    }
  }

  // Add the priors
  target += lprior;

}

generated quantities {

  //Predicted matrix of n's of all predicted delay / times
  array[max_time, max_delays + 1] int Nmat_predict; //Matrix with all predictions

  //Prediction of overall cases at time t
  array[max_time] int N_predict;

  //Get a matrix of all of the predictions
  for (t in 1:max_time){
      if (is_negative_binomial){
          Nmat_predict[t,:] = neg_binomial_2_log_rng(lambda[t,:], rep_vector(r[1], max_delays + 1));
      } else {
          Nmat_predict[t,:] = poisson_log_rng(lambda[t,:]);
      }
  }

  //Substitute back those values we do know
  for (k in 1:nobs){
    Nmat_predict[Nmat[k,2], Nmat[k,3] + 1] = Nmat[k,1];
  }

  //Get the overall total (rowsums)
  for (t in 1:max_time){
    N_predict[t] = sum(Nmat_predict[t,:]);
  }
}
