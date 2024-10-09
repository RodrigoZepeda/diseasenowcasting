# test_that("Checking `AR`", {
#
#   ntimes  <- 10
#   nstrata <- 7
#   pval    <- 3
#   ymat <- matrix(rnorm(ntimes*nstrata), ncol = ntimes, nrow = nstrata)
#   phi  <- c(rnorm(pval),0)
#
#   #Check that it works with empty vector phi
#   expect_equal(
#     AR(ymat, phi = c(0), t = 2, pstream__ = rstan::get_stream()),
#     rep(0.0, nstrata)
#   )
#
#   #For t = 0
#   expect_error(
#     AR(ymat, phi = phi, t = 0, pstream__ = rstan::get_stream())
#   )
#
#   #For all other t
#   for (tval in 1:ntimes){
#     expect_equal(
#       as.matrix(AR(ymat, phi = phi, t = tval, pstream__ = rstan::get_stream())),
#       as.matrix(ymat[,(tval + 1 - min(tval, pval + 1)):tval]) %*% as.vector(phi[((pval + 2) - min(tval, pval + 1)):(pval + 1)])
#     )
#   }
#
#   #For last t
#   expect_error(
#     AR(ymat, phi = phi, t = ntimes + 1, pstream__ = rstan::get_stream())
#   )
#
# })
#
# test_that("Checking `MA`", {
#
#   ntimes  <- 10
#   nstrata <- 7
#   qval    <- 3
#   xi     <- matrix(rnorm(ntimes*nstrata), ncol = ntimes, nrow = nstrata)
#   theta  <- c(rnorm(qval),1)
#
#   #Check that it works with empty vector phi
#   expect_equal(
#     MA(xi, theta = c(1), t = 1, pstream__ = rstan::get_stream()),
#     xi[,1]
#   )
#
#   #For t = 0
#   expect_error(
#     MA(xi, theta = theta, t = 0, pstream__ = rstan::get_stream())
#   )
#
#   #For all other t
#   for (tval in 1:ntimes){
#     expect_equal(
#       MA(xi, theta = theta, t = tval, pstream__ = rstan::get_stream()) |> as.matrix(),
#       as.matrix(xi[,(tval + 1 - min(tval, qval + 1)):tval]) %*% as.vector(theta[((qval + 2) - min(tval, qval + 1)):(qval + 1)])
#     )
#   }
#
#   #For last t
#   expect_error(
#     MA(xi, theta = theta, t = ntimes + 1, pstream__ = rstan::get_stream())
#   )
#
# })
