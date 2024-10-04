/*Data -----------------------------------------------------------------------------------------*/
int<lower=1> num_steps;      //Number of time steps modelled
int<lower=0> num_delays;     //Maximum number of unique delays considered
int<lower=1> num_strata;     //Number of strata included in the model
int<lower=1> n_rows;         //Number of rows in case data

/*Case data ------------------------------------------------------------------------------------*/
array[n_rows, 3] int case_idx;

/*
`case_idx` is a matrix with columns 1 = time, 2 = delay, 3 = strata. The matrix corresponds
one to one to the `cases` matrix defined either in the data_discrete.stan or data_continuous.stan
files.
The correspondance is 1-to-1 following the row number as follows:

    cases                                   case_idx
                             Time            Delay            Strata
    ------                 -------------------------------------------
     10                       1                0                 1
     11                       1                0                 2
     09                       1                1                 1
     21                       1                1                 2
     16                       1                2                 1
     15                       1                2                 2
     12                       2                0                 1
     08                       2                0                 2
     08                       2                1                 1
     25                       2                1                 2
     20                       2                2                 1
     17                       2                2                 2
     -----------------------------------------------------------------

     Note that the `case_idx` table (and correspondingly the cases) should be ordered
     by Time then Delay then Strata.
*/

/*ARMA component options -----------------------------------------------------------------------*/
int<lower=0> mu_p;   //Lags considered for an autoregresive AR(p) model
int<lower=0> mu_q;   //Lags considered for a moving average MA(q) model
int<lower=0> nu_p;   //Amount of delays considered for the model for eta

/*Priors for ARMA components -------------------------------------------------------------------*/
/*mu:*/
real mu_intercept_param_1;
real<lower=0> mu_intercept_param_2;
real mu_0_param_1;
real<lower=0> mu_0_param_2;

/*nu:*/
real nu_intercept_param_1;
real<lower=0> nu_intercept_param_2;
real nu_0_param_1;
real<lower=0> nu_0_param_2;

/*Priors for variances -------------------------------------------------------------------------*/
real sd_mu_param_1;
real<lower=0> sd_mu_param_2;
real sd_nu_param_1;
real<lower=0> sd_nu_param_2;
real sd_m_param_1;
real<lower=0> sd_m_param_2;

/*Model options---------------------------------------------------------------------------------*/
int<lower=0,upper=1> prior_only;
int<lower=0,upper=3> dist;
int<lower=0,upper=3> link; //How to link the average to the data 0 = identity; 1 = log; 2 =
real<lower=0> dof; //Student-T degrees of freedom

/*Generated quantities--------------------------------------------------------------------------*/
matrix[num_strata, num_delays] sd_cases; //SD of cases for normalization
matrix[num_strata, num_delays] mu_cases; //Mean of cases for normalization
