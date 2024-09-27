//FIXME: Change these to be non constant
vector[1] B_cnt = rep_vector(0.0, 1);
matrix[1,1] X_cnt = rep_matrix(0.0, 1, 1);

//Trend related transformations-------------------------------------------------------------------
//Create trend matrices for mu (mu) and delay (nu)

//A
int nrows_mu_trend_A = get_num_rows_A_trend(mu_degree);
int ncols_mu_trend_A = get_num_cols_A_trend(mu_degree);
matrix[nrows_mu_trend_A, ncols_mu_trend_A] A_mu = trend_matrix_A(mu_degree);

int nrows_nu_trend_A = get_num_rows_A_trend(nu_degree);
int ncols_nu_trend_A = get_num_cols_A_trend(nu_degree);
matrix[nrows_nu_trend_A, ncols_nu_trend_A] A_nu = trend_matrix_A(nu_degree);

//R
int nrows_mu_trend_R = get_num_rows_R_trend(mu_degree);
int ncols_mu_trend_R = get_num_cols_R_trend(mu_degree);
matrix[nrows_mu_trend_R, ncols_mu_trend_R] R_mu = trend_matrix_R(mu_degree, mu_is_constant);

int nrows_nu_trend_R = get_num_rows_R_trend(nu_degree);
int ncols_nu_trend_R = get_num_cols_R_trend(nu_degree);
matrix[nrows_nu_trend_R, ncols_nu_trend_R] R_nu = trend_matrix_R(nu_degree, nu_is_constant);

//L
int num_elements_mu_L = get_num_elements_L_trend(mu_degree);
vector[num_elements_mu_L] L_mu = trend_vector_L(mu_degree);

int num_elements_nu_L = get_num_elements_L_trend(nu_degree);
vector[num_elements_nu_L] L_nu = trend_vector_L(nu_degree);

//Get initial mu and nu sizes
int mu_0_size = num_elements_mu_L;
int nu_0_size = num_elements_nu_L;

//Get initial vector sizes for the errors
int xi_mu_size = nrows_mu_trend_R;
int xi_nu_size = nrows_nu_trend_R;

//Get the columns of ncases
int n_col = 1;
int t_col = 1;
int d_col = 2;
int s_col = 3;

//Get the distribution coefficients
int<lower=0,upper=1> is_negative_binomial = 0;
int<lower=0,upper=1> is_normal = 0;
int<lower=0,upper=1> is_student = 0;
int<lower=0,upper=1> is_poisson = 0;

if (distribution == 0){
  is_poisson = 1;
} else if (distribution == 1){
  is_negative_binomial = 1;
} else if (distribution == 2){
  is_normal = 1;
} else if (distribution == 3){
  is_student = 1;
} else {
  reject("Incorrect distribution specified");
}

//Get whether the distribution is discrete
int<lower=0,upper=1> is_discrete = is_poisson + is_negative_binomial;

//Get whether the distribution has variance
int<lower=0,upper=1> has_variance = is_normal + is_negative_binomial + is_student;
