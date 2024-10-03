/*Get the total size of the vectors*/
int tsize = num_delays*num_strata;

/*Get the columns of the case_idxs*/
int t_col = 1; //Time is stored in this column.
int d_col = 2; //Delay is stored in this column.
int s_col = 3; //Strata is stored in this column.

/*Get the distribution*/
int is_normal     = dist == 0 ? 1: 0;
int is_student    = dist == 1 ? 1: 0;
int is_poisson    = dist == 2 ? 1: 0;
int is_negbin     = dist == 3 ? 1: 0;
int is_continuous = is_normal + is_student;
int is_discrete   = is_poisson + is_negbin;
