data {
    int<lower=1> Today;
    int<lower=1> D;
    array[Today, D + 1] int<lower=0> n;
    real<lower=0> dispersion_prior_shape;
    real<lower=0> dispersion_prior_rate;
    real alpha1_mean_prior;
    real<lower=0> alpha1_prec_prior;
    real<lower=0> alphat_shape_prior;
    real<lower=0> alphat_rate_prior;
    vector<lower=0>[D + 1] beta_priors;
}

transformed data {
  real tau_alpha1_prec_prior = 1.0 / sqrt(alpha1_prec_prior);
}

parameters {
    real<lower=0> r;
    vector[Today] alpha_centered;
    simplex[D + 1] beta;
    real<lower=0> tau2_alpha;
}

transformed parameters {
  //Obtain alpha from the centered parametrizations
  vector[Today] alpha;
  alpha[1] = alpha1_mean_prior + tau_alpha1_prec_prior*alpha_centered[1];
  for (t in 2:Today)
    alpha[t] = alpha[t - 1] + tau2_alpha*alpha_centered[t];

  //Lambda is the probability parameter of the regression
  matrix[Today, D + 1] lambda;
  for (d in 1:(D + 1))
    lambda[1:Today, d] = alpha + log(beta[d]);

}

model {
  // Priors
  r ~ gamma(dispersion_prior_shape, dispersion_prior_rate);
  tau2_alpha ~ gamma(alphat_shape_prior, alphat_rate_prior);
  alpha_centered ~ std_normal();
  beta ~ dirichlet(beta_priors);

  // Likelihood
  for (t in 1:Today)
    n[t, 1:(D + 1)] ~ neg_binomial_2_log(lambda[t, 1:(D + 1)], r);

}
