data {
  int<lower=0> N;
  vector[N] y;
  int prior_only;
}

parameters {
  real beta;  // Example parameter
  real<lower=0> sigma;  // Example parameter with a constraint
}

transformed parameters {
  real lprior = 0;
  lprior += normal_lpdf(beta | 0.0, 1);
  lprior += cauchy_lpdf(sigma | 0.0, 1) - cauchy_lccdf(sigma | 0.0, 1);
}

model {
  if (!prior_only){
    target += normal_lpdf(y | beta, sigma);
  }
  target += lprior;
}

generated quantities {
  real y_pred = normal_rng(beta, sigma);  // Sample from the predictive distribution
}
