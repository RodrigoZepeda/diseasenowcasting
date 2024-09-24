//Data -----------------------------------------------------------------------------------------
int<lower=1> num_steps;      //Number of time steps modelled
int<lower=0> num_delays;     //Maximum number of unique delays considered
int<lower=1> num_strata;     //Number of strata included in the model
int<lower=1> n_rows;         //Number of rows in data Nmat

//Data with time, delays, strata. Each entry is a case count -----------------------------------
array[n_rows, 4] int N_cases; //Matrix with first entry = n, second = time, third = delay, fourth = strata

//Trend options --------------------------------------------------------------------------------
int<lower=0> mu_degree;                 //Degree associated to the mu's trend
int<lower=0,upper=1> mu_is_constant;    //Whether the mu effect is constant
int<lower=0> nu_degree;                 //Degree associated to the delay's trend
int<lower=0,upper=1> nu_is_constant;    //Whether the delay effect is constant

//ARMA component options ---------------------------------------------
int<lower=0> p;
int<lower=0> q;
real phi_AR_param_1;
real phi_AR_param_2;
real theta_MA_param_1;
real theta_MA_param_2;
real xi_sd_param_1;
real xi_sd_param_2;

//Sampling options -----------------------------------------------------------------------------
int<lower=0, upper=1> is_negative_binomial; //Either 0 = Poisson or 1 = NegativeBinomial
int<lower=0, upper=1> prior_only;   //Set to 1 to sample only from the prior

//Priors ---------------------------------------------------------------------------------------
real<lower=0> r_param_1;
real<lower=0> r_param_2;

real<lower=0> mu_sd_param_1;
real<lower=0> mu_sd_param_2;

real<lower=0> nu_sd_param_1;
real<lower=0> nu_sd_param_2;

//Distribution specification for mu_0 and
int<lower=0,upper=16> mu_sd_prior;
int<lower=0,upper=16> nu_sd_prior;
int<lower=0,upper=16> r_prior;
int<lower=0,upper=16> phi_AR_prior;
int<lower=0,upper=16> theta_MA_prior;
int<lower=0,upper=16> xi_sd_prior;
int<lower=0,upper=16> mu_0_mean_hyperprior;
int<lower=0,upper=16> nu_0_mean_hyperprior;
int<lower=0,upper=16> mu_0_sd_hyperprior;
int<lower=0,upper=16> nu_0_sd_hyperprior;

//Hyperpriors
real mu_0_mean_param_1;
real mu_0_mean_param_2;

real nu_0_mean_param_1;
real nu_0_mean_param_2;

real mu_0_sd_param_1;
real mu_0_sd_param_2;

real nu_0_sd_param_1;
real nu_0_sd_param_2;

//Numerical tolerance
real<lower=0> max_log_tol_val; //Maximum value allowable for log(lambda)
real<lower=0> precision_tol; //Precision value for r in negative binomial

