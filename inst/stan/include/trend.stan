#include include/license.stan
//Functions for creating the trend matrices for the process

matrix create_trend_matrix_block_A(int degree) {
  // Create the trend matrix A for a trend with degree `degree`
  //
  // Creates the degree x degree  trend matrix with entries given by
  // A[1,j] = (-1)^{j+1} choose(degree,j+1)
  // A[i,j-1] = 1 for i > 2
  // A[i,j] = 0 otherwise

  //Create the matrix
  int ncols = get_num_cols_A_trend(degree);
  int nrows = get_num_rows_A_trend(degree);

  matrix[nrows, ncols] A = rep_matrix(0, nrows, ncols);

  // Fill the first row using binomial coefficients with alternating signs
  for (j in 1:ncols) {
    A[1, j] = (-1)^(j+1) * choose(degree, j);
  }

  // Fill the lower triangular part with the identity matrix shifted by 1 row
  for (i in 2:nrows) {
    A[i, i-1] = 1.0;
  }

  return A';
}

vector create_trend_vector_block_L(int degree){
  // Create the trend vector L
  //
  // The trend vector L is a degree length vector of zeroes except for the
  // first entry which is 1
  int nsize = get_num_elements_L_trend(degree);

  vector[nsize] L = rep_vector(0.0, nsize);
  L[1] = 1.0;

  return L;
}

matrix create_trend_matrix_block_R(int degree, int is_constant){
  // Create the matrix R
  //
  // The trend matrix R is a degree x degree matrix with 0's except in the
  // first entry where it can be 1 if it is constant and 0 if not.

  //Create the matrix
  int ncols = get_num_cols_R_trend(degree);
  int nrows = get_num_rows_R_trend(degree);

  matrix[nrows,ncols] R = rep_matrix(0.0, nrows, ncols);
  R[1,1] = 1.0 - is_constant;

  return R';
}

//Returns the number of elements in vector alpha for a trend with degree degree
//with d delays
int get_num_elements_alpha_trend(int degree){
  return degree;
}

int get_num_cols_A_trend(int degree){
  return degree;
}

int get_num_rows_A_trend(int degree){
  return degree;
}

int get_num_elements_L_trend(int degree){
  return degree;
}

int get_num_cols_R_trend(int degree){
  return degree;
}

int get_num_rows_R_trend(int degree){
  return degree;
}
