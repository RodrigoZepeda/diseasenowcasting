matrix create_block_diagonal(matrix A, matrix B){
  //Creates a block diagonal matrix from matrix A and matrix B
  //
  //Creates the matrix C given by diag(A,B)

  matrix[rows(A) + rows(B), cols(A) + cols(B)] C = rep_matrix(0, rows(A) + rows(B), cols(A) + cols(B));

  //Fill with A values
  C[1:rows(A),1:cols(A)] = A;

  //Fill with B values
  C[(rows(A) + 1):rows(C),(cols(A) + 1):cols(C)] = B;

  return C;

}

matrix rep_diagonal_mat(matrix A, int k){
  //Creates a block diagonal of matrix A repeated k times
  //
  //Creates the matrix C given by diag(A,A,A,A,A,...,A) with A repeated k times

  matrix[rows(A)*k, cols(A)*k] C = rep_matrix(0, rows(A)*k, cols(A)*k);

  //Fill with A values
  for (l in 1:k){
    C[((l-1)*rows(A) + 1):(l*rows(A)), ((l-1)*cols(A) + 1):(l*cols(A))] = A;
  }

  return C;
}

vector rep_vec(vector A, int k){
  //Creates a vector pasting A with itself k times
  //
  //Creates the vector C given by c(A,A,A,A,A,...,A) with A repeated k times

  vector[num_elements(A)*k] C;

  //Fill with A values
  for (l in 1:k){
    C[((l-1)*num_elements(A) + 1):(l*num_elements(A))] = A;
  }

  return C;
}
