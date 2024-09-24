#include license/license.stan

//BLOCK MATRICES-------
//The seasonal_discrete matrix is constructed by blocks for each delay d. First we construct each
//of the blocks.
matrix create_seasonal_discrete_matrix_block_A(int num_seasons, int season_duration) {
  //Create a matrix of the appropriate size
  int k = num_seasons*season_duration + 1;
  matrix[k, k] A = rep_matrix(0.0, k, k);

  // Fill the first row with -1 every season_duration
  for (j in 1:(k - season_duration - 1)){
    if (j % season_duration == 0){
      A[1,j] = -1.0;
    }
  }

  // Fill with 1 the last element of the first column
  A[1,k] = 1.0;

  // Fill the lower triangular part with the identity matrix shifted by 1 row
  for (i in 2:(k-1)) {
    A[i, i-1] = 1.0;
  }

  //Fill the last element with a 1
  A[k,k] = 1.0;

  return A;
}

vector create_seasonal_discrete_vector_block_L(int num_seasons, int season_duration){
  //Get the vector size
  int k = num_seasons*season_duration + 1;

  //Create the vector with first entry = 1
  vector[k] L = rep_vector(0.0, k);
  L[1] = 1.0;

  return L;

}

matrix create_seasonal_discrete_matrix_block_R(int num_seasons, int season_duration, real time){
  // Create the matrix R
  //
  // The seasonal_discrete matrix R is a k x k matrix with 0's except in the
  // last entry where it can be 1 if the time is right

  //Get the matrix size
  int k = num_seasons*season_duration + 1;

  matrix[k,k] R = rep_matrix(0.0, k, k);
  real reminder = ceil(time / season_duration) - floor(time / season_duration);

  R[k,k] = 1.0 - reminder;

  return R;
}

vector create_initial_seasonal_discrete_vector_block_alpha(vector season_params, int season_duration, real error_term){
  //Get the vector size
  int k = num_elements(season_params)*season_duration + 1;

  //Fill the vector by repeating each element season_duration times
  vector[k] alpha;
  alpha[1:(k-1)] = rep_vec_piecewise(season_params, season_duration);

  //The last term of alpha is the error term
  alpha[k] = error_term;

  return alpha;

}

