//Data -----------------------------------------------------------------------------------------
int<lower=1> num_steps;      //Number of time steps modelled
int<lower=0> num_delays;     //Maximum number of unique delays considered
int<lower=1> num_strata;     //Number of strata included in the model
int<lower=1> n_rows;         //Number of rows in case data

//Case data -------------------------------------------------------------------------------------
/*
`N_cases` is a matrix with columns 1 = time, 2 = delay, 3 = strata. The matrix corresponds
one to one to the `Cases` matrix defined either in the data_discrete.stan or data_continuous.stan
files.
The correspondance is 1-to-1 following the row number as follows:

    Cases                                   N_cases
                             Time            Delay            Strata
    ------                 -------------------------------------------
     10                       1                1                 1
     11                       1                2                 1
     09                       1                3                 1
     21                       1                1                 2
     16                       1                2                 2
     15                       1                3                 2
     12                       2                1                 1
     08                       2                2                 1
     08                       2                3                 1
     25                       2                1                 2
     20                       2                2                 2
     17                       2                3                 2
     -----------------------------------------------------------------

*/
array[n_rows, 3] int N_cases;

//Trend options --------------------------------------------------------------------------------
int<lower=0> mu_degree;                 //Degree associated to the mu's trend
int<lower=0,upper=1> mu_is_constant;    //Whether the mu's effect is constant
int<lower=0> nu_degree;                 //Degree associated to the delay's trend
int<lower=0,upper=1> nu_is_constant;    //Whether the delay's effect (nu) is constant

//ARMA component options -----------------------------------------------------------------------
int<lower=0> p;                         //Lags considered for an autoregresive AR(p) model
int<lower=0> q;                         //Lags considered for a moving average MA(q) model
real phi_AR_param_1;                    //Prior for the
real phi_AR_param_2;
real theta_MA_param_1;
real theta_MA_param_2;
real xi_sd_param_1;
real xi_sd_param_2;

//Sampling options -----------------------------------------------------------------------------
int<lower=0, upper=1> prior_only;   //Set to 1 to sample only from the prior

//Case distribution options --------------------------------------------------------------------
int<lower=0,upper=3> distribution;  //Poisson = 0, NegBinomial = 1, Normal = 2, Student = 3

//Priors ---------------------------------------------------------------------------------------

real<lower=0> mu_sd_param_1;
real<lower=0> mu_sd_param_2;

real<lower=0> nu_sd_param_1;
real<lower=0> nu_sd_param_2;

//Distribution specification for mu_0 and
int<lower=0,upper=16> mu_sd_prior;
int<lower=0,upper=16> nu_sd_prior;
int<lower=0,upper=16> r_prior;
int<lower=0,upper=16> r_sd_prior;
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

//Variance priors
real<lower=0> r_param_1;
real<lower=0> r_param_2;
real<lower=0> r_param_sd_1;
real<lower=0> r_param_sd_2;

//Numerical tolerance
real<lower=0> max_log_tol_val; //Maximum value allowable for log(lambda)
real<lower=0> precision_tol;   //Precision value for r in negative binomial

//Generated quantities-------------
matrix[num_strata, num_delays] sd_cases; //SD of cases for normalization
matrix[num_strata, num_delays] mu_cases; //Mean of cases for normalization

