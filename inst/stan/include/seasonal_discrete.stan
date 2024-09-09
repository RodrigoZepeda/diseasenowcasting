#include include/license.stan

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

//COMPLETE MATRICES-------
//The seasonal_discrete matrix is constructed by blocks for each delay d.
//Here we bind all the blocks.
matrix create_seasonal_discrete_matrix_A(int num_seasons, int season_duration, int num_delays){
  int k = num_seasons*season_duration + 1;
  matrix[k, k] A = create_seasonal_discrete_matrix_block_A(num_seasons, season_duration);
  return rep_diagonal_mat(A, num_delays);
}

matrix create_seasonal_discrete_matrix_R(int num_seasons, int season_duration, real time, int num_delays){
  int k = num_seasons*season_duration + 1;
  matrix[k, k] R = create_seasonal_discrete_matrix_block_R(num_seasons, season_duration, time);
  return rep_diagonal_mat(R, num_delays);
}

vector create_seasonal_discrete_vector_L(int num_seasons, int season_duration, int num_delays){
  int k = num_seasons*season_duration + 1;
  vector[k] L = create_seasonal_discrete_vector_block_L(num_seasons, season_duration);
  return rep_vec(L, num_delays);
}

vector create_seasonal_discrete_vector_alpha(vector alpha_params){
  return alpha_params;
}

//Returns the number of elements in vector alpha for a seasonal_discrete with degree degree
//with d delays
int get_num_elements_alpha_seasonal_discrete(int num_seasons, int season_duration, int num_delays){
  return (season_duration*num_seasons)*num_delays;
}

