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
matrix[tsize, num_steps]  xi_mu;
matrix[num_strata, num_delays] xi_nu;
matrix[tsize, num_steps]  xi_m;

/*Variances-------------------------------------------------------------------------------------*/
vector<lower=0>[tsize] sd_mu;
vector<lower=0>[num_strata] sd_nu;
vector<lower=0>[tsize] sd_m;
real<lower=0> sd_obs;
