#File for testing all of the stan functions
stanfuns <- stan_function_tester()

test_that("Checking `create_block_diagonal`", {

  #Checks the function to generate block diagonal matrices
  mat  <- create_block_diagonal(matrix(c(1,2,3,4), nrow = 2),
                                matrix(c(5,6,7,8), nrow = 2),
                                pstream__ = rstan::get_stream())
  mat2 <- matrix(c(1,2,0,0,3,4,0,0,0,0,5,6,0,0,7,8), nrow = 4)
  expect_equal(mat, mat2)

})
