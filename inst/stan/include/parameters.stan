/*Parameters for each of the components---------------------------------------------------------*/

//mu:
vector[tsize] mu_intercept;
vector<lower=-1,upper=1>[mu_p] phi_mu;    //Autoregresive component for epidemic
vector<lower=-1,upper=1>[mu_q] theta_mu;  //Moving average component for epidemic

//nu:
vector[num_strata] nu_intercept;            //Delay dependent constant component
vector<lower=-1,upper=1>[max_q_nu] phi_nu;  //Autoregresive component for delays

/*Initial values for each of the components-----------------------------------------------------*/
vector[tsize] mu_init;
vector[num_strata] nu_init;

//Errors for each of the components---------------------------------------------------------------
matrix[tsize, num_steps - 1]  xi_mu;
matrix[num_strata, num_delays - 1] xi_nu;

/*Variances-------------------------------------------------------------------------------------*/
real<lower=0> sd_mu;
real<lower=0> sd_nu;
real<lower=0> sd_m;
