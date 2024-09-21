matrix create_block_diagonal(matrix A, matrix B){
  /*
  Create a block diagonal matrix from matrix A and matrix B

  @param A A matrix for the first block of the diagonal
  @param B A matrix for the second block of the diagonal

  @return The block-diagonal matrix C given by diag(A,B)
  */

  matrix[rows(A) + rows(B), cols(A) + cols(B)] C = rep_matrix(0, rows(A) + rows(B), cols(A) + cols(B));

  //Fill with A values
  C[1:rows(A),1:cols(A)] = A;

  //Fill with B values
  C[(rows(A) + 1):(rows(A) + rows(B)),(cols(A) + 1):(cols(A) + cols(B))] = B;

  return C;

}

matrix rep_diagonal_mat(matrix A, int k){
  /*
  Create a block diagonal matrix of A repeated k times

  @param A A matrix to be repeated as diagonal blocks
  @param k the number of times to repeat the matrix A

  @return The block-diagonal matrix C given by diag(A,A,A,A,A,...,A) with A repeated k times
  */
  matrix[rows(A)*k, cols(A)*k] C = rep_matrix(0, rows(A)*k, cols(A)*k);

  //Fill with A values
  for (l in 1:k){
    C[((l-1)*rows(A) + 1):(l*rows(A)), ((l-1)*cols(A) + 1):(l*cols(A))] = A;
  }

  return C;
}

vector rep_vec(vector A, int k){
  /*
  Create a vector pasting A with itself k times

  @param A A vector to be repeated as in R's `rep`
  @param k the number of times to repeat the vector A

  @return The block-diagonal matrix C given by diag(A,A,A,A,A,...,A) with A repeated k times
  */
  vector[num_elements(A)*k] C;

  //Fill with A values
  for (l in 1:k){
    C[((l-1)*num_elements(A) + 1):(l*num_elements(A))] = A;
  }

  return C;
}

vector rep_vec_piecewise(vector A, int k){
  /*
  Creates a vector repeating each element of A k-times

  @details Given A = (a1, a2, a3, ..., an) returns the vector of k*n length
  specified as:

  (a1, a1, a1, ..., a1, a2, a2, a2, ..., a2, a3, a3, ..., a3, ..., an, an, ..., an)

  where each element of A is repeated k times.

  @param A A vector of length n
  @param k the number of times to repeat the vector A

  @return A k*n vector with each element repeatd k times;
  (a1, a1, a1, ..., a1, a2, a2, a2, ..., a2, a3, a3, ..., a3, ..., an, an, ..., an)
  */

  int n = num_elements(A);
  vector[n*k] C;

  //Fill with A values
  for (j in 1:n){
    C[((j - 1)*k + 1):(k*j)] = rep_vector(A[j], k);
  }

  return C;
}

int max_int(int a, int b){
  /*
  Return the maximum between two integers as an integer

  @param a An integer
  @param b An integer

  @return The maximum between a and b
  */
  if (a > b){
    return a;
  } else {
    return b;
  }
}

int min_int(int a, int b){
  /*
  Return the minimum between two integers as an integer

  @param a An integer
  @param b An integer

  @return The minimum between a and b
  */
  if (a > b){
    return b;
  } else {
    return a;
  }
}

//TODO:
// > Create the MA option for the y's
vector AR(matrix y, vector phi, int t){
  /*
  Calculate the autoregresive AR(p) component of a vector y at time t

  The AR(p) component is given by 0*y_t + phi_p*y_{t-1} + phi_{p-1}*y_{t-2} + ... + phi_1*y_{t-p}
  which in matrix form is coded as:

  y[,(t + 1 - min_int(t, p)):(t + 1)]*phi[((p + 1) - min_int(t, p)):(p + 1)]

  where the first part is (y_{t-p}, y_{t-(p-1)}, ..., y_{t-1}, y_t)
  and the second part is (phi_p, phi_{p-1}, ..., phi_1, 0)'
  so that (y_{t-p}, y_{t-(p-1)}, ..., y_{t-1}, y_t)*(phi_p, phi_{p-1}, ..., phi_1, 0)'
  yields 0*y_t + phi_p*y_{t-1} + phi_{p-1}*y_{t-2} + ... + phi_1*y_{t-p}.

  The `min_int(t,p)` part is set so that it works even for t < p

  @param y Matrix where each group/strata is a row and each column is a moment in time (t).
  @param phi (p+1)-dimensional vector where the first p entries are the AR(p) components and the
              last entry is 0
  @param t Integer moment in time (t > 0) to calculate the AR component

  @return The sum 0*y_t + phi_p*y_{t-1} + phi_{p-1}*y_{t-2} + ... + phi_1*y_{t-p}
  */
  return y[,(t + 1 - min_int(t, num_elements(phi) - 1)):(t + 1)]*phi[(num_elements(phi) - min_int(t, num_elements(phi) - 1)):num_elements(phi)];
}
