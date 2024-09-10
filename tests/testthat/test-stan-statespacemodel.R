num_steps  <- 10
num_delays <- 6
num_strata <- 4
degree     <- 2
A_mu       <- create_trend_matrix_block_A(degree, rstan::get_stream())
A_nu       <- create_trend_matrix_block_A(2, rstan::get_stream())
L_mu       <- create_trend_vector_block_L(degree, rstan::get_stream())
L_nu       <- create_trend_vector_block_L(2, rstan::get_stream())
R_mu       <- create_trend_matrix_block_R(degree, FALSE, rstan::get_stream())
R_nu       <- create_trend_matrix_block_R(2, TRUE, rstan::get_stream())

mu_0       <- array(runif(num_delays*num_strata*nrow(A_mu)), dim = c(num_delays, num_strata, nrow(A_mu)))
nu_0       <- array(runif(num_delays*num_strata*nrow(A_nu)), dim = c(num_delays, num_strata, nrow(A_nu)))

xi_mu   <- array(rnorm(num_delays*num_strata*num_steps*ncol(R_mu)), dim = c(num_steps, num_delays, num_strata, ncol(R_mu)))
xi_nu   <- array(rnorm(num_delays*num_strata*num_steps*ncol(R_nu)), dim = c(num_steps, num_delays, num_strata, ncol(R_mu)))
epsilon <- array(rnorm(num_delays*num_strata*num_steps), dim = c(num_steps, num_delays, num_strata))

B_cnt <- matrix(0, nrow = 1, ncol = 1)
X_cnt <- matrix(0, nrow = 1, ncol = 1)

test_that("The `state_space_process` works", {
  #Check the state space model runs
  expect_no_condition(
    state_space_process(num_steps = num_steps,
                        num_delays = num_delays,
                        num_strata = num_strata,
                        A_mu = A_mu,
                        A_nu = A_nu,
                        R_mu = R_mu,
                        R_nu = R_nu,
                        L_mu = L_mu,
                        L_nu = L_nu,
                        mu_0  = array_to_list(mu_0),
                        xi_mu = array_to_list(xi_mu),
                        nu_0  = array_to_list(nu_0),
                        xi_nu = array_to_list(xi_nu),
                        B_cnt = B_cnt,
                        X_cnt = X_cnt,
                        epsilon = array_to_list(epsilon, FALSE),
                        pstream__ = rstan::get_stream())
  )


  #Compute the process in stan and compare with R
  ss_process <- state_space_process(num_steps = num_steps,
                      num_delays = num_delays,
                      num_strata = num_strata,
                      A_mu = A_mu,
                      A_nu = A_nu,
                      R_mu = R_mu,
                      R_nu = R_nu,
                      L_mu = L_mu,
                      L_nu = L_nu,
                      mu_0  = array_to_list(mu_0),
                      xi_mu = array_to_list(xi_mu),
                      nu_0  = array_to_list(nu_0),
                      xi_nu = array_to_list(xi_nu),
                      B_cnt = B_cnt,
                      X_cnt = X_cnt,
                      epsilon = array_to_list(epsilon, FALSE),
                      pstream__ = rstan::get_stream())

  #Create initial mu and nu
  mu <- array(NA_real_, dim = c(num_steps, num_delays, num_strata, nrow(A_mu)))
  nu <- array(NA_real_, dim = c(num_steps, num_delays, num_strata, nrow(A_nu)))
  lv <- array(NA_real_, dim = c(num_steps, num_delays, num_strata, 1))

  #Check the state space model returns the appropriate result
  mu[1,,,] <- mu_0
  nu[1,,,] <- nu_0

  constant_coef <- X_cnt*B_cnt

  for (s in 1:num_strata){
    for (d in 1:num_delays){
      for (t in 1:num_steps){
          lv[t,d,s,]   = t(L_mu)%*%mu[t,d,s,] + t(L_nu)%*%nu[t,d,s,] + constant_coef + epsilon[t,d,s]
          if (t < num_steps){
            mu[t+1,d,s,] = A_mu%*%mu[t,d,s,] + R_mu%*%xi_mu[t+1,d,s,]
            nu[t+1,d,s,] = A_nu%*%nu[t,d,s,] + R_nu%*%xi_nu[t+1,d,s,]
          }
      }
    }
  }

  #Convert to same object of lists
  lv <- array_to_list(lv)

  #Expect equal
  expect_equal(ss_process, lv)


})
