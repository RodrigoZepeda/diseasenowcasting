#File for testing all of the stan functions
#The functions are created automatically from the stan file
#after calling rstantools::rstan_config() and then
#devtools::document()
#Matrix for the examples
Amat <- matrix(c(1,2,3,4), nrow = 2, byrow = TRUE)
lvec <- c(1,2,3,4)

test_that("Checking `create_block_diagonal`", {
  #Checks the function to generate block diagonal matrices
  mat  <- create_block_diagonal(Amat,
                                matrix(c(5,6,7,8), nrow = 2, byrow = T),
                                pstream__ = rstan::get_stream())
  mat2 <- matrix(c(1,3,0,0,2,4,0,0,0,0,5,7,0,0,6,8), nrow = 4)
  expect_equal(mat, mat2)

  #Checks the function to generate block diagonal matrices when
  #the matrices are of minimal size
  mat  <- create_block_diagonal(matrix(1, nrow = 1),
                                matrix(1, nrow = 1),
                                pstream__ = rstan::get_stream())
  mat2 <- diag(2)
  expect_equal(mat, mat2)
})

test_that("Checking `rep_diagonal_mat`", {
  #Checks the function to generate block diagonal matrices by repeating matrix
  # A k times
  mat  <- rep_diagonal_mat(Amat, 3, pstream__ = rstan::get_stream())
  mat2 <- matrix(0, nrow = nrow(Amat)*3, ncol = ncol(Amat)*3)
  mat2[1:2,1:2] <- Amat
  mat2[3:4,3:4] <- Amat
  mat2[5:6,5:6] <- Amat
  expect_equal(mat, mat2)

  #Checks that when 1 repetition is established then it only creates the
  #same matrix
  mat  <- rep_diagonal_mat(Amat, 1, pstream__ = rstan::get_stream())
  expect_equal(mat, Amat)
})

test_that("Checking `rep_vec`", {
  #Checks the function to generate repeated vectors by repeating vector
  # lvec k times
  mvec  <- rep_vec(lvec, 10, pstream__ = rstan::get_stream())
  mvec2 <- rep(lvec, 10)
  expect_equal(mvec, mvec2)

  #Checks that when 1 repetition is established then it only creates the
  #same vector
  mvec  <- rep_vec(lvec, 1, pstream__ = rstan::get_stream())
  expect_equal(lvec, mvec)
})


test_that("Checking `rep_vec_piecewise`", {
  #Checks the function to generate repeated vectors by repeating vector
  # lvec k times
  mvec  <- rep_vec_piecewise(lvec, 10, pstream__ = rstan::get_stream())
  mvec2 <- c()
  for (k in 1:length(lvec)){
    mvec2 <- c(mvec2, rep(lvec[k], 10))
  }
  expect_equal(mvec, mvec2)

  #Checks that when 1 repetition is established then it only creates the
  #same vector
  mvec  <- rep_vec_piecewise(lvec, 1, pstream__ = rstan::get_stream())
  expect_equal(lvec, mvec)
})

test_that("Checking `max_int` and `min_int`",{
  expect_equal(max_int(10, 12, pstream__ = rstan::get_stream()), 12)
  expect_equal(max_int(14, 12, pstream__ = rstan::get_stream()), 14)
  expect_equal(min_int(10, 12, pstream__ = rstan::get_stream()), 10)
  expect_equal(min_int(14, 12, pstream__ = rstan::get_stream()), 12)
})

test_that("Checking `AR`", {

  ymat <- matrix(rnorm(25), ncol = 5, nrow = 5)
  phi  <- c(rnorm(3),0)

  #For t = 0
  expect_error(
    AR(ymat, phi = phi, t = 0, pstream__ = rstan::get_stream()),
  )

  #For t = 1
  expect_equal(
    AR(ymat, phi = phi, t = 1, pstream__ = rstan::get_stream()),
    ymat[,1] * phi[length(phi)]
  )

  #For t = 2
  expect_equal(
    AR(ymat, phi = phi, t = 2, pstream__ = rstan::get_stream()),
    as.vector(ymat[,1:2] %*% phi[3:length(phi)])
  )

  #For t = 3
  expect_equal(
    AR(ymat, phi = phi, t = 3, pstream__ = rstan::get_stream()),
    as.vector(ymat[,1:3] %*% phi[2:length(phi)])
  )

  #For t = 4
  expect_equal(
    AR(ymat, phi = phi, t = 4, pstream__ = rstan::get_stream()),
    as.vector(ymat[,1:4] %*% phi[1:length(phi)])
  )

  #For t = 5
  expect_equal(
    AR(ymat, phi = phi, t = 5, pstream__ = rstan::get_stream()),
    as.vector(ymat[,2:5] %*% phi[1:length(phi)])
  )

  #For t = 6
  expect_error(
    AR(ymat, phi = phi, t = 6, pstream__ = rstan::get_stream()),
  )

})

test_that("Checking `MA`", {

  xi     <- matrix(rnorm(25), ncol = 5, nrow = 5)
  theta  <- c(rnorm(3),1)

  #For t = 0
  expect_error(
    MA(xi, theta = theta, t = 0, pstream__ = rstan::get_stream())
  )

  #For t = 1
  expect_equal(
    MA(xi, theta = theta, t = 1, pstream__ = rstan::get_stream()),
    xi[,1] * theta[length(theta)]
  )

  #For t = 2
  expect_equal(
    MA(xi, theta = theta, t = 2, pstream__ = rstan::get_stream()),
    as.vector(xi[,1:2] %*% theta[3:length(theta)])
  )

  #For t = 3
  expect_equal(
    MA(xi, theta = theta, t = 3, pstream__ = rstan::get_stream()),
    as.vector(xi[,1:3] %*% theta[2:length(theta)])
  )

  #For t = 4
  expect_equal(
    MA(xi, theta = theta, t = 4, pstream__ = rstan::get_stream()),
    as.vector(xi[,1:4] %*% theta[1:length(theta)])
  )

  #For t = 5
  expect_equal(
    MA(xi, theta = theta, t = 5, pstream__ = rstan::get_stream()),
    as.vector(xi[,2:5] %*% theta[1:length(theta)])
  )

  #For t = 6
  expect_error(
    MA(xi, theta = theta, t = 6, pstream__ = rstan::get_stream())
  )


})
