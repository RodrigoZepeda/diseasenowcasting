#include include/license.stan
/*
  TREND FUNCTIONS:
  --------------------------------------------------------------------------------------------------
  In a state space model that models the variable X we have
  X[t]  = a[t] L;
  a[t] = a[t-1] A + xi[t] R
  where a[t] is a latent variable, A and R are matrices and L is a vector.

  In the model X[t] is a vector which implies that a[t] is also a vector.

  CODE:
  --------------------------------------------------------------------------------------------------
  In this code:
  * trend_matrix_A: Constructs the A matrix specified above
  * trend_matrix_R: Constructs the R matrix specified above
  * trend_vector_L: Constructs the L vector specified above
  * get_num_rows_A_trend: Get the number of rows in the trend_matrix_A
  * get_num_cols_A_trend: Get the number of columns in the trend_matrix_A
  * get_num_rows_R_trend: Get the number of rows in the trend_matrix_R
  * get_num_cols_R_trend: Get the number of columns in the trend_matrix_R
  * get_num_elements_L_trend: Get the length of the trend_vector L
  * get_num_elements_alpha_trend: Get the length of the trend_vector alpha
  --------------------------------------------------------------------------------------------------
*/

//Functions to create the trend matrices------------------------------------------------------------
matrix trend_matrix_A(int degree) {
  /*
  * @title Trend matrix A for degree d
  *
  * @description Creates the degree x degree  trend matrix with entries given by
  * A[1,j] = (-1)^{j+1} choose(degree,j+1)
  * A[i,j-1] = 1 for i > 2
  * A[i,j] = 0 otherwise
  *
  * @return A matrix A' for the trend of a state-space model
  */

  //Create the matrix
  int ncols = get_num_cols_A_trend(degree);
  int nrows = get_num_rows_A_trend(degree);

  matrix[nrows, ncols] A = rep_matrix(0, nrows, ncols);

  // Fill the first row using binomial coefficients with alternating signs
  if (degree > 0){
    for (j in 1:ncols) {
      A[1, j] = (-1)^(j+1) * choose(degree, j);
    }

    // Fill the lower triangular part with the identity matrix shifted by 1 row
    for (i in 2:nrows) {
      A[i, i-1] = 1.0;
    }
  }

  return A';
}

vector trend_vector_L(int degree){
  /*
  * @title Trend vector L for degree d
  *
  * @description The trend vector L is a `degree` length vector of zeroes except for the
  * first entry which is 1.
  *
  * @return The trend vector L
  */
  int nsize = get_num_elements_L_trend(degree);

  vector[nsize] L = rep_vector(0.0, nsize);
  if (degree > 0){
    L[1] = 1.0;
  }

  return L;
}

matrix trend_matrix_R(int degree, int is_constant){
  /*
  * @title Trend matrix R for degree d
  *
  * @description The trend matrix R is a degree x degree matrix with 0's except in the
  * first entry where it can be 1 if it is constant and 0 if not.
  *
  * @return A matrix A' for the trend of a state-space model
  */

  //Create the matrix
  int ncols = get_num_cols_R_trend(degree);
  int nrows = get_num_rows_R_trend(degree);

  matrix[nrows,ncols] R = rep_matrix(0.0, nrows, ncols);

  if (degree > 0){
    R[1,1] = 1.0 - is_constant;
  }

  return R';
}

//Functions to obtain the sizes of the vectors and matrices associated with the trend---------------
int get_num_elements_alpha_trend(int degree){
  /*
  * @title Number of elements of trend vector alpha
  *
  * @description Gets the number of elements for the parameter vector alpha
  *
  * @return The number of elements in alpha
  */
  if (degree < 0)
    reject("Invalid negative `degree` specified");

  return degree;
}

int get_num_cols_A_trend(int degree){
  /*
  * @title Number of columns of trend matrix A
  *
  * @description Gets the number of columns for the trend matrix A
  *
  * @return The number of columns in A
  */
  if (degree < 0)
    reject("Invalid negative `degree` specified");

  return degree;
}

int get_num_rows_A_trend(int degree){
  /*
  * @title Number of rows of trend matrix A
  *
  * @description Gets the number of rows for the trend matrix A
  *
  * @return The number of rows in A
  */
  if (degree < 0)
    reject("Invalid negative `degree` specified");

  return degree;
}

int get_num_elements_L_trend(int degree){
  /*
  * @title Number of elements of trend vector L
  *
  * @description Gets the number of elements for the vector L
  *
  * @return The number of elements in L
  */
  if (degree < 0)
    reject("Invalid negative `degree` specified");

  return degree;
}

int get_num_cols_R_trend(int degree){
  /*
  * @title Number of columns of trend matrix R
  *
  * @description Gets the number of columns for the trend matrix R
  *
  * @return The number of columns in R
  */
  if (degree < 0)
    reject("Invalid negative `degree` specified");

  return degree;
}

int get_num_rows_R_trend(int degree){
  /*
  * @title Number of rows of trend matrix R
  *
  * @description Gets the number of rows for the trend matrix R
  *
  * @return The number of rows in R
  */
  if (degree < 0)
    reject("Invalid negative `degree` specified");

  return degree;
}
