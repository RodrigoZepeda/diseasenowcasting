# #File for testing all of the stan functions
# #The functions are created automatically from the stan file
# #after calling rstantools::rstan_config() and then
# #devtools::document()
# #Matrix for the examples
# Amat <- matrix(c(1,2,3,4), nrow = 2, byrow = TRUE)
# lvec <- c(1,2,3,4)
#
# test_that("Checking `create_block_diagonal`", {
#   #Checks the function to generate block diagonal matrices
#   mat  <- create_block_diagonal(Amat,
#                                 matrix(c(5,6,7,8), nrow = 2, byrow = T),
#                                 pstream__ = rstan::get_stream())
#   mat2 <- matrix(c(1,3,0,0,2,4,0,0,0,0,5,7,0,0,6,8), nrow = 4)
#   expect_equal(mat, mat2)
#
#   #Checks the function to generate block diagonal matrices when
#   #the matrices are of minimal size
#   mat  <- create_block_diagonal(matrix(1, nrow = 1),
#                                 matrix(1, nrow = 1),
#                                 pstream__ = rstan::get_stream())
#   mat2 <- diag(2)
#   expect_equal(mat, mat2)
# })
#
# test_that("Checking `rep_diagonal_mat`", {
#   #Checks the function to generate block diagonal matrices by repeating matrix
#   # A k times
#   mat  <- rep_diagonal_mat(Amat, 3, pstream__ = rstan::get_stream())
#   mat2 <- matrix(0, nrow = nrow(Amat)*3, ncol = ncol(Amat)*3)
#   mat2[1:2,1:2] <- Amat
#   mat2[3:4,3:4] <- Amat
#   mat2[5:6,5:6] <- Amat
#   expect_equal(mat, mat2)
#
#   #Checks that when 1 repetition is established then it only creates the
#   #same matrix
#   mat  <- rep_diagonal_mat(Amat, 1, pstream__ = rstan::get_stream())
#   expect_equal(mat, Amat)
# })
#
# test_that("Checking `rep_vec`", {
#   #Checks the function to generate repeated vectors by repeating vector
#   # lvec k times
#   mvec  <- rep_vec(lvec, 10, pstream__ = rstan::get_stream())
#   mvec2 <- rep(lvec, 10)
#   expect_equal(mvec, mvec2)
#
#   #Checks that when 1 repetition is established then it only creates the
#   #same vector
#   mvec  <- rep_vec(lvec, 1, pstream__ = rstan::get_stream())
#   expect_equal(lvec, mvec)
# })
#
#
# test_that("Checking `rep_vec_piecewise`", {
#   #Checks the function to generate repeated vectors by repeating vector
#   # lvec k times
#   mvec  <- rep_vec_piecewise(lvec, 10, pstream__ = rstan::get_stream())
#   mvec2 <- c()
#   for (k in 1:length(lvec)){
#     mvec2 <- c(mvec2, rep(lvec[k], 10))
#   }
#   expect_equal(mvec, mvec2)
#
#   #Checks that when 1 repetition is established then it only creates the
#   #same vector
#   mvec  <- rep_vec_piecewise(lvec, 1, pstream__ = rstan::get_stream())
#   expect_equal(lvec, mvec)
# })
#
# test_that("Checking `max_int` and `min_int`",{
#   expect_equal(max_int(10, 12, pstream__ = rstan::get_stream()), 12)
#   expect_equal(max_int(14, 12, pstream__ = rstan::get_stream()), 14)
#   expect_equal(min_int(10, 12, pstream__ = rstan::get_stream()), 10)
#   expect_equal(min_int(14, 12, pstream__ = rstan::get_stream()), 12)
# })
#
# test_that("Checking `colwise_mat_2_vec` and `rowwise_mat_2_vec`",{
#
#   vals <- rnorm(50)
#
#   expect_equal(
#     rowwise_mat_2_vec(matrix(vals, ncol = 10, nrow = 5, byrow = TRUE), pstream__ = rstan::get_stream()),
#     vals
#   )
#
#   expect_equal(
#     colwise_mat_2_vec(matrix(vals, ncol = 10, nrow = 5, byrow = FALSE), pstream__ = rstan::get_stream()),
#     vals
#   )
#
#   #Transposed of A should yield identical results
#   Amat <- matrix(vals, ncol = 10, nrow = 5, byrow = TRUE)
#   expect_equal(
#     rowwise_mat_2_vec(Amat, pstream__ = rstan::get_stream()),
#     colwise_mat_2_vec(t(Amat), pstream__ = rstan::get_stream())
#   )
#
# })
#
