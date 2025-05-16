/*Parameters for each of the components-----------------------------------------------------------*/

//mu:
vector[tsize] mu_intercept_centered;      //Centered value for epidemic-driven intercept
vector<lower=-1,upper=1>[mu_p] phi_mu;    //Autoregresive component for epidemic
vector<lower=-1,upper=1>[mu_q] theta_mu;  //Moving average component for epidemic

//nu:
vector[num_strata] nu_intercept_centered; //Delay dependent constant component
vector<lower=-1,upper=1>[nu_p] phi_nu;    //Autoregresive component for delays

//cycle:
vector[has_cycle ? tsize: 0] lambda_cycle; //Cycle component

/*Parameters for temporal covariates with no dynamic priors---------------------------------------*/
vector[has_day_of_week_epi ? 6: 0] beta_dow_epi_param;
vector[has_weekend_epi ? 1: 0] beta_wkend_epi_param;
vector[has_day_of_month_epi ? 30: 0] beta_dom_epi_param;
vector[has_month_of_year_epi ? 11: 0] beta_month_epi_param;
vector[has_week_of_year_epi ? 52: 0] beta_week_epi_param;
vector[has_holidays_epi ? 1: 0] beta_holidays_epi_param;

//vector[has_day_of_week_dly ? 6: 0] beta_dow_dly_param;
//vector[has_weekend_dly ? 1: 0] beta_wkend_dly_param;
//vector[has_day_of_month_dly ? 30: 0] beta_dom_dly_param;
//vector[has_month_of_year_dly ? 11: 0] beta_month_dly_param;
//vector[has_week_of_year_dly ? 51: 0] beta_week_dly_param;
//vector[has_holidays_dly ? 1: 0] beta_holidays_dly_param;


/*Initial values for each of the components-------------------------------------------------------*/
vector[tsize] mu_init_centered;                     //Centered initial value of epidemic-driven process
vector[num_strata] nu_init_centered;                //Centered initial value of delay-driven process
vector[has_cycle ? tsize: 0] c_init_centered;       //Centered initial value of cycle (c)
vector[has_cycle ? tsize: 0] ctilde_init_centered;  //Centered initial value of cycle (c tilde; i.e. latent component)

/*Errors for each of the components---------------------------------------------------------------*/
matrix[tsize, num_steps - 1]  xi_mu;      //Noise associated with epidemic-driven process
matrix[num_strata, num_delays - 1] xi_nu; //Noise associated with delay-driven process
matrix[has_cycle ? tsize: 0, num_steps - 1]  xi_cycle;   //Noise associated with cycle (c)
matrix[has_cycle ? tsize: 0, num_steps - 1]  xi_ctilde;  //Noise associated with cycle (c tilde; i.e. latent)

/*Variances---------------------------------------------------------------------------------------*/
real<lower=0> sd_mu;                         //Standard deviation associated with xi_mu
real<lower=0> sd_nu;                         //Standard deviation associated with xi_nu

//Cycle variances
vector<lower=0>[has_cycle ? 1: 0] sd_cycle;  //Standard deviation of cycle component
vector<lower=0>[has_cycle ? 1: 0] sd_ctilde; //Standard deviation of cycle latent component
vector<lower=0>[is_poisson ? 0: 1] sd_m;     //Standard deviation of data if model is not Poisson

//Temporal variances
vector<lower=0>[has_day_of_week_epi ? 1: 0] sd_dow_epi;
vector<lower=0>[has_weekend_epi ? 1: 0] sd_wkend_epi;
vector<lower=0>[has_day_of_month_epi ? 1: 0] sd_dom_epi;
vector<lower=0>[has_month_of_year_epi ? 1: 0] sd_month_epi;
vector<lower=0>[has_week_of_year_epi ? 1: 0] sd_week_epi;
vector<lower=0>[has_holidays_epi ? 1: 0] sd_holidays_epi;

//vector<lower=0>[has_day_of_week_dly ? 1: 0] sd_dow_dly;
//vector<lower=0>[has_weekend_dly ? 1: 0] sd_wkend_dly;
//vector<lower=0>[has_day_of_month_dly ? 1: 0] sd_dom_dly;
//vector<lower=0>[has_month_of_year_dly ? 1: 0] sd_month_dly;
//vector<lower=0>[has_week_of_year_dly ? 1: 0] sd_week_dly;
//vector<lower=0>[has_holidays_dly ? 1: 0] sd_holidays_dly;

