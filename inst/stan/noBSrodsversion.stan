data {
    int<lower=1> Today;
    int<lower=1> D;
    array[Today, D + 1] int n;
    real<lower=0> dispersion_prior_shape;
    real<lower=0> dispersion_prior_rate;
    real alpha1_mean_prior;
    real<lower=0> alpha1_prec_prior;
    real<lower=0> alphat_shape_prior;
    real<lower=0> alphat_rate_prior;
    vector<lower=0>[D + 1] beta_priors;
}

transformed data {
  real tol = 1.e-20;
  real tau_alpha1_prec_prior = 1.0 / sqrt(alpha1_prec_prior);
}

parameters {
    real<lower=0> r;
    vector[Today] alpha_centered;
    vector[D + 1] beta;
    real<lower=0> tau2_alpha1_prec_prior;
}

transformed parameters {
  //Obtain alpha from the centered parametrizations
  vector[Today] alpha;
  alpha[1] = alpha1_mean_prior + tau_alpha1_prec_prior*alpha_centered[1];

  real<lower=0> tau2_alpha = 1.0 / sqrt(tau2_alpha1_prec_prior);

  for (t in 2:Today)
    alpha[t] = alpha[t - 1] + tau2_alpha*alpha_centered[t];

  //Lambda is the probability parameter of the regression
  matrix[Today, D + 1] lambda;
  for (d in 1:(D + 1))
    lambda[1:Today, d] = alpha + beta[d];

}

model {
  // Priors
  r ~ gamma(dispersion_prior_shape, dispersion_prior_rate);
  tau2_alpha1_prec_prior ~ gamma(alphat_shape_prior, alphat_rate_prior);
  alpha_centered ~ std_normal();
  beta ~ std_normal();

  // Likelihood
  for (t in 1:Today){
    for (d in 1:(D + 1)){
      if (n[t, d] >= 0)
        n[t, d] ~ neg_binomial_2_log(lambda[t, d], r);
    }
  }

}

generated quantities {
  array[Today, D + 1] int<lower=0> n_pred;
  array[Today] int<lower=0> N_cases_predicted;

  // Predictions
  for (t in 1:Today){
    N_cases_predicted[t] = 0;
    for (d in 1:(D + 1)){
      n_pred[t, d] = neg_binomial_2_log_rng(lambda[t, d], r);
      if (n[t,d] >= 0){
        N_cases_predicted[t] += n[t, d];
      } else {
        N_cases_predicted[t] += n_pred[t, d];
      }
    }
  }
}
