//Functions for creating the trend matrices for the process

//BLOCK MATRICES-------
//The trend matrix is constructed by blocks for each delay d. First we construct each
//of the blocks.
matrix create_trend_matrix_block_A(int k) {
  // Create the trend matrix A
  //
  // Creates the kxk trend matrix with entries given by
  // A[1,j] = (-1)^{j+1} choose(k,j+1)
  // A[i,j-1] = 1 for i > 2
  // A[i,j] = 0 otherwise

  //Create the matrix
  matrix[k, k] A = rep_matrix(0, k, k);

  // Fill the first row using binomial coefficients with alternating signs
  for (j in 1:k) {
    A[1, j] = (-1)^(j+1) * choose(k, j);
  }

  // Fill the lower triangular part with the identity matrix shifted by 1 row
  for (i in 2:k) {
    for (j in 1:i) {
      if (j == i - 1) {
        A[i, j] = 1;
      } else {
        A[i, j] = 0;
      }
    }
  }

  return A;
}

vector create_trend_vector_block_L(int k){
  // Create the trend vector L
  //
  // The trend vector L is a k length vector of zeroes except for the first entry which is 1

  vector[k] L = rep_vector(0.0, k);
  L[1] = 1.0;

  return L;
}

matrix create_trend_matrix_block_R(int k, int is_constant){
  // Create the matrix R
  //
  // The trend matrix R is a kxk matrix with 0's except in the first entry
  // where it can be 1 if it is constant and 0 if not.

  matrix[k,k] R = rep_matrix(0.0, k, k);
  R[1,1] = 1.0 - is_constant;

  return R;
}

vector create_initial_trend_vector_block_alpha(vector alpha){
  return alpha;
}

//COMPLETE MATRICES-------
//The trend matrix is constructed by blocks for each delay d. Here we bind all the blocks.
matrix create_trend_matrix_A(int k, int d){
  matrix[k, k] A = create_trend_matrix_block_A(k);
  return rep_diagonal_mat(A, d);
}

matrix create_trend_matrix_R(int k, int is_constant, int d){
  matrix[k, k] R = create_trend_matrix_block_R(k, is_constant);
  return rep_diagonal_mat(R, d);
}

vector create_trend_vector_L(int k, int d){
  vector[k] L = create_trend_vector_block_L(k);
  return rep_vec(L, d);
}

vector create_trend_vector_alpha(vector alpha){
  return alpha;
}

//Returns the number of elements in vector alpha for a trend of k
//with d delays
int get_num_elements_alpha_trend(int k, int d){
  return k*d;
}
