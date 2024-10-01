#include include/license.stan

matrix create_block_diagonal(matrix A, matrix B){
  /*
  * Create a block diagonal matrix from matrix A and matrix B
  *
  * @param A A matrix for the first block of the diagonal
  * @param B A matrix for the second block of the diagonal
  *
  * @return The block-diagonal matrix C given by diag(A,B)
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
  * Create a block diagonal matrix of A repeated k times
  *
  * @param A A matrix to be repeated as diagonal blocks
  * @param k the number of times to repeat the matrix A
  *
  * @return The block-diagonal matrix C given by diag(A,A,A,A,A,...,A) with A repeated k times
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
  * Create a vector pasting A with itself k times
  *
  * @param A A vector to be repeated as in R's `rep`
  * @param k the number of times to repeat the vector A
  *
  * @return The block-diagonal matrix C given by diag(A,A,A,A,A,...,A) with A repeated k times
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
  * Creates a vector repeating each element of A k-times
  *
  * @details Given A = (a1, a2, a3, ..., an) returns the vector of k*n length
  * specified as:
  *
  * (a1, a1, a1, ..., a1, a2, a2, a2, ..., a2, a3, a3, ..., a3, ..., an, an, ..., an)
  *
  * where each element of A is repeated k times.
  *
  * @param A A vector of length n
  * @param k the number of times to repeat the vector A
  *
  * @return A k*n vector with each element repeatd k times;
  * (a1, a1, a1, ..., a1, a2, a2, a2, ..., a2, a3, a3, ..., a3, ..., an, an, ..., an)
  */

  int n = num_elements(A);
  vector[n*k] C;

  //Fill with A values
  for (j in 1:n){
    C[((j - 1)*k + 1):(k*j)] = rep_vector(A[j], k);
  }

  return C;
}

vector append_val_2_vec(vector A, real val){
  /*
  * Append a value to a vector (at the end)
  *
  * @details Given vector A = (a_1, a_2, ..., a_n) the function generates a vector of size n + 1
  * given by: (a_1, a_2, ..., a_n, val)
  *
  * @param A The vector to which val will be appended
  * @param val The value to append at the end of A
  *
  * @return The vector B given by B = (a_1, a_2, ..., a_n, val)
  */
  vector [num_elements(A) + 1] B;

  //Append vector A
  if (num_elements(A) > 0)
    B[1:num_elements(A)] = A;

  //Add val at the end
  B[num_elements(B)] = val;

  return B;
}


vector append_vec_2_val(real val, vector A){
  /*
  * Append a value to a vector (at the beginning)
  *
  * @details Given vector A = (a_1, a_2, ..., a_n) the function generates a vector of size n + 1
  * given by: (val, a_1, a_2, ..., a_n)
  *
  * @param val The value to append at the beginning of A
  * @param A The vector to which val will be appended
  *
  * @return The vector B given by B = (val, a_1, a_2, ..., a_n)
  */
  vector [num_elements(A) + 1] B;

  //Append vector A
  if (num_elements(A) > 0)
    B[2:num_elements(B)] = A;

  //Add val at the beginning
  B[1] = val;

  return B;
}


vector rowwise_mat_2_vec(matrix A){
  /*
  * Create a vector from a matrix (rowwise)
  *
  * @details Given matrix A creates a vector by concatenating the rows
  *
  * @param A The matrix to convert to vector
  *
  * @return The vector B given by;
  * B = (a_{1,1}, a_{1,2}, ..., a_{1,n}, a_{2,1}, a_{2,2}, .... a_{2,n}, ..., a_{m,1}, ..., a_{m,n})
  */
  vector[num_elements(A)] B;
  for (d in 1:rows(A))
    B[(cols(A)*(d - 1) + 1):(cols(A)*d)] = A[d,:]';

  return B;

}

vector colwise_mat_2_vec(matrix A){
  /*
  * Create a vector from a matrix (rowwise)
  *
  * @details Given matrix A creates a vector by concatenating the rows
  *
  * @param A The matrix to convert to vector
  *
  * @return The vector B given by;
  * B = (a_{1,1}, a_{2,1}, ..., a_{m,1}, a_{1,2}, a_{2,2}, .... a_{m,2}, ..., a_{1,n}, ..., a_{m,n})
  */
  return rowwise_mat_2_vec(A');

}
