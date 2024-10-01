/*Get the maximum value for q*/
int max_q_nu = min_int(nu_p, num_delays);

/*Get the total size of the vectors*/
int tsize = num_delays*num_strata;

/*Check whether theta_mu exists*/
int has_theta_mu = 0;

/*Get the columns of the case_idxs*/
int t_col = 1; //Time is stored in this column.
int d_col = 2; //Delay is stored in this column.
int s_col = 3; //Strata is stored in this column.
