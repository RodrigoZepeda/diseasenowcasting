#Quick way to check the stan functions
funs  <- rstan::expose_stan_functions("inst/stan/include/trends.stan")

create_trend_vector_L(1,1)
create_trend_matrix_R(2, FALSE, 5)
create_trend_matrix_A(1,1)
rep_diagonal_mat(matrix(c(1,2,3,4), nrow = 2),3)
rep_diagonal_vec(c(1,2,3,4),3)
create_trend_matrix_A(1,1)


