/*Parameters for each of the components-----------------------------------------------------------*/

//mu:
vector[tsize] mu_intercept_centered;      //Centered value for epidemic-driven intercept
vector<lower=-1,upper=1>[mu_p] phi_mu;    //Autoregresive component for epidemic
vector<lower=-1,upper=1>[mu_q] theta_mu;  //Moving average component for epidemic

//nu:
vector[num_strata] nu_intercept_centered; //Delay dependent constant component
vector<lower=-1,upper=1>[nu_p] phi_nu;    //Autoregresive component for delays

/*Initial values for each of the components-------------------------------------------------------*/
vector[tsize] mu_init_centered;          //Centered initial value of epidemic-driven process
vector[num_strata] nu_init_centered;     //Centered initial value of delay-driven process

/*Errors for each of the components---------------------------------------------------------------*/
matrix[tsize, num_steps - 1]  xi_mu;      //Noise associated with epidemic-driven process
matrix[num_strata, num_delays - 1] xi_nu; //Noise associated with delay-driven process

/*Variances---------------------------------------------------------------------------------------*/
real<lower=0> sd_mu;                     //Standard deviation associated with xi_mu
real<lower=0> sd_nu;                     //Standard deviation associated with xi_nu
vector<lower=0>[is_poisson ? 0: 1] sd_m; //Standard deviation of data if model is not Poisson
