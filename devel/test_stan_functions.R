#Quick way to check the stan functions

create_trend_vector_L(1,1, rstan::get_stream())
create_trend_matrix_R(2, FALSE, 5, rstan::get_stream())
create_trend_matrix_A(1,1, rstan::get_stream())
rep_diagonal_mat(matrix(c(1,2,3,4), nrow = 2),3, rstan::get_stream())
rep_vec(c(1,2,3,4),3, rstan::get_stream())
create_trend_matrix_A(1,1, rstan::get_stream())


