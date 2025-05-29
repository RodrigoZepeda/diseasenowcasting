functions {

  //Function that implements the probability of being upper bounded
  real log_G(matrix right_censored, real param_1, real param_2, real tol, int distribution){

    //Get the delta as the probability of being on interval [0, d_+]:
    //G = G(d_+)
    int Niter = rows(right_censored);

    vector[Niter] log_G;
    for (n in 1:Niter){

      //Add the log cummulative density function
      if (distribution == 1)
        log_G[n] = gamma_lcdf(right_censored[n,3] + 0.5 | param_1, param_2);
      else if (distribution == 2)
        log_G[n] = lognormal_lcdf(right_censored[n,3] + 0.5 | param_1, param_2);
      else if (distribution == 3)
        log_G[n] = weibull_lcdf(right_censored[n,3] + 0.5 | param_1, param_2);
      else
        reject("Error in `log_G` distribution was not correctly specified. This is an internal error of the `diseasenowcasting` package. Report to `https://github.com/RodrigoZepeda/diseasenowcasting/issues`");


      log_G[n] = max({log_G[n], log(tol)});
    }

    //Finally compute the dot product to get sum(n*log_G)
    return dot_product(log_G, right_censored[,2]);

  }

  //Function that implements the probability of being on an interval
  real log_delta_G(matrix interval_censored, real param_1, real param_2, real tol, int distribution){

    /*
    @title Log delta cummulative distribution function of delays

    @description Computes the log cdf difference for when distribution is continuous
    and delay is rounded to nearest integer.

    @param interval_censored A matrix of 3 columns: 1) is time, 2) is case-count and 3) are delays
    The `time_delay_interval_censored` matrix is as follows:
      t  |  n  |      delay  |
    --------------------------
      1  |  5  |      0      |
      1  |  4  |      1      |
      1  |  1  |      3      |
      2  |  10 |      0      |
      2  |  6  |      1      |
      3  |  4  |      2      |
    where t is the time, n is the observation-count, delay represents the integer measured
    delay so that the true delay is within the bounds [max{delay - 0.5, 0.0}, delay + 0.5]

    @param param_1 First parameter for distribution `distribution`
    @param param_2 Second parameter for distribution `distribution`
    @param tol Tolerance value for log (so that it doesn't overflow)
    @param distribution (1 = Gamma, 2 = Lognormal and 3 = Weibull)

    */
    //Get the delta as the probability of being on interval [d_-, d_+]:
    //G = G(d_+) - G(d_-)
    int Niter = rows(interval_censored);

    vector[Niter] log_delta_G;
    for (n in 1:Niter){
      //Add the log cummulative density function
      if (distribution == 1)
        log_delta_G[n] = log(gamma_cdf(interval_censored[n,3] + 0.5 | param_1, param_2) - gamma_cdf(max({interval_censored[n,3] - 0.5, 0.0}) | param_1, param_2));
      else if (distribution == 2)
        log_delta_G[n] = log(lognormal_cdf(interval_censored[n,3] + 0.5 | param_1, param_2) - lognormal_cdf(max({interval_censored[n,3] - 0.5, 0.0}) | param_1, param_2));
      else if (distribution == 3)
        log_delta_G[n] = log(weibull_cdf(interval_censored[n,3] + 0.5 | param_1, param_2) - weibull_cdf(max({interval_censored[n,3] - 0.5, 0.0}) | param_1, param_2));
      else
        reject("Error in `log_delta_G` distribution was not correctly specified. This is an internal error of the `diseasenowcasting` package. Report to `https://github.com/RodrigoZepeda/diseasenowcasting/issues`");
      log_delta_G[n] = max({log_delta_G[n], log(tol)});
    }

    //Finally compute the dot product to get sum(n*log_delta_G)
    return dot_product(log_delta_G, interval_censored[,2]);

  }
}

data {
  //TODO: Program the delay distribution
  int<lower=1,upper=3> delay_distribution; //Delay distribution 1 = Gamma, 2 = Lognormal, 3 = Weibull
  int<lower=0> N_interval; //Number of data that are interval censored
  int<lower=0> N_right;    //Number of data points that are right censored
  int<lower=0> N_new_observations_interval; //Number of data that are interval censored in new observations (for changepoint detection)
  //int<lower=0> N_new_observations_right; //Number of data that are right censored in new observations (for changepoint detection)

  matrix<lower=0>[N_interval, 3] time_delay_interval_censored; //Delay matrix for interval censored observations
  matrix<lower=0>[N_new_observations_interval,3] new_time_delay_interval_censored; //Delay matrix for interval censored observations
  /*
  The `time_delay_interval_censored` matrix is as follows:
    t  |  n  |      delay  |
  --------------------------
    1  |  5  |      0      |
    1  |  4  |      1      |
    1  |  1  |      3      |
    2  |  10 |      0      |
    2  |  6  |      1      |
    3  |  4  |      2      |
  where t is the time, n is the observation-count, delay represents the integer measured
  delay so that the true delay is within the bounds [max{delay - 0.5, 0.0}, delay + 0.5]
  */

  matrix<lower=0>[N_right,3] time_delay_right_censored; //Delay matrix for censored observations
  //matrix<lower=0>[N_new_observations_right,3] new_time_delay_right_censored; //Delay matrix for censored observations
  /*
  The `time_delay_interval_censored` matrix is as follows:
    t  |  n  | delay_high  |
  --------------------------
    1  |  5  |      10     |
    1  |  1  |      8      |
    2  |  3  |      6      |
    2  |  4  |      10     |
    3  |  1  |      15     |
  where t is the time, n is the observation-count,and delay_high represent the bound
  so that delay <= delay_high + 0.5.
  */

}

transformed data {
  real tol = 1.e-12;

  //Get tge maximum observed delay
  real max_observed_delay = max({max(time_delay_right_censored[,3]), max(time_delay_interval_censored[,3] + 0.5)});
  int max_delay = to_int(ceil(max_observed_delay));

  //Get total number of new observations
  //int N_new = N_new_observations_interval + N_new_observations_right;
}

parameters {

  real<lower=0> delay_param_1;
  real<lower=0> delay_param_2;

}


transformed parameters {

  //Prior
  real lprior = 0;
  lprior += exponential_lpdf(delay_param_1| 1.0);
  lprior += exponential_lpdf(delay_param_2| 1.0);

}

model {

  //Add to target the values
  target += log_G(time_delay_right_censored, delay_param_1, delay_param_2, tol, delay_distribution);
  target += log_delta_G(time_delay_interval_censored, delay_param_1, delay_param_2, tol, delay_distribution);
  target += lprior;
}

//https://stats.stackexchange.com/questions/490853/probability-that-the-sample-comes-from-a-certain-distribution
generated quantities {
  real true_simulated_delays = gamma_rng(delay_param_1, delay_param_2);
  real observed_simulated_delays = round(true_simulated_delays);

  //Get the probability for each positive interval until maximum delay
  vector[max_delay + 1] probability_of_delay;
  for (k in 0:max_delay){
    probability_of_delay[k + 1] = gamma_cdf(k + 0.5| delay_param_1, delay_param_2) - gamma_cdf(max({k - 0.5, 0.0})| delay_param_1, delay_param_2);
  }
  probability_of_delay[max_delay + 1] = 1 - gamma_cdf(max_delay + 0.5| delay_param_1, delay_param_2);


  //Get the probability of new observations under the current model
  vector[N_new_observations_interval + 1] theta_new_obs_interval; //Probability of specific observations on interval
  for (k in 1:N_new_observations_interval){
    theta_new_obs_interval[k] = gamma_cdf(new_time_delay_interval_censored[k,3] + 0.5 | delay_param_1, delay_param_2) - gamma_cdf(max({new_time_delay_interval_censored[k,3] - 0.5, 0.0}) | delay_param_1, delay_param_2);
    theta_new_obs_interval[k] = max({theta_new_obs_interval[k], 0.0});
  }
  theta_new_obs_interval[N_new_observations_interval + 1] = 1.0 -  sum(theta_new_obs_interval[1:N_new_observations_interval]); //Probability of everything else

  array[N_new_observations_interval + 1] int new_observations;
  for (k in 1:N_new_observations_interval)
    new_observations[k] = to_int(round(new_time_delay_interval_censored[k,2]));
  new_observations[N_new_observations_interval + 1] = 0;

  real multiobs = exp(multinomial_lpmf(new_observations | theta_new_obs_interval));

}

