num_steps  <- 2
num_delays <- 3
num_strata <- 4
degree     <- 2
A_mu       <- create_trend_matrix_block_A(degree, rstan::get_stream())
A_nu       <- create_trend_matrix_block_A(2, rstan::get_stream())
L_mu       <- create_trend_vector_block_L(degree, rstan::get_stream())
L_nu       <- create_trend_vector_block_L(2, rstan::get_stream())
R_mu       <- create_trend_matrix_block_R(degree, FALSE, rstan::get_stream())
R_nu       <- create_trend_matrix_block_R(2, TRUE, rstan::get_stream())

mu_0       <- array(runif(num_delays*num_strata*nrow(A_mu)), dim = c(num_strata, num_delays, nrow(A_mu)))
nu_0       <- array(runif(num_delays*num_strata*nrow(A_nu)), dim = c(num_strata, num_delays, nrow(A_nu)))

xi_mu   <- array(rnorm(num_delays*num_strata*(num_steps - 1)*ncol(R_mu)), dim = c(num_steps - 1, num_strata, num_delays, ncol(R_mu)))
xi_nu   <- array(rnorm(num_delays*num_strata*(num_steps - 1)*ncol(R_nu)), dim = c(num_steps - 1, num_strata, num_delays, ncol(R_nu)))

B_cnt <- matrix(0, nrow = 1, ncol = 1)
X_cnt <- matrix(0, nrow = 1, ncol = 1)

xi_mu_2   <- array(NA_real_, dim = c(num_steps - 1, num_strata*num_delays, ncol(R_mu)))
xi_nu_2   <- array(NA_real_, dim = c(num_steps - 1, num_strata*num_delays, ncol(R_nu)))

for (t in 1:(num_steps - 1)){
  for (s in 1:num_strata){
    for (d in 1:num_delays){
      xi_mu_2[t,num_strata*(d - 1) + s,] <- xi_mu[t,s,d,]
      xi_nu_2[t,num_strata*(d - 1) + s,] <- xi_nu[t,s,d,]
    }
  }
}

test_that("The `state_space_process_v3` works", {
  #Compute the process in stan and compare with R
  ss_process <- state_space_process_v3(
     num_steps  = num_steps,
     num_delays = num_delays,
     num_strata = num_strata,
     A_mu = A_mu,
     A_nu = A_nu,
     R_mu = R_mu,
     R_nu = R_nu,
     L_mu = L_mu,
     L_nu = L_nu,
     mu_0  = matrix(mu_0, nrow = num_strata*num_delays, ncol = nrow(A_mu)),
     xi_mu = array_to_list(xi_mu_2, "matrix"),
     nu_0  = matrix(nu_0, nrow = num_strata*num_delays, ncol = nrow(A_nu)),
     xi_nu = array_to_list(xi_nu_2, "matrix"),
     B_cnt = B_cnt,
     X_cnt = X_cnt,
     pstream__ = rstan::get_stream())

  #Create initial mu and nu
  mu <- array(NA_real_, dim = c(num_steps, num_strata, num_delays, nrow(A_mu)))
  nu <- array(NA_real_, dim = c(num_steps, num_strata, num_delays, nrow(A_nu)))
  lv <- array(NA_real_, dim = c(num_steps, num_strata, num_delays, 1))


  #Check the state space model returns the appropriate result
  for (s in 1:num_strata){
    for (d in 1:num_delays){
      mu[1,s,d,] <- mu_0[s,d,]
      nu[1,s,d,] <- nu_0[s,d,]
    }
  }

  constant_coef <- X_cnt*B_cnt

  for (s in 1:num_strata){
    for (t in 1:num_steps){
      for (d in 1:num_delays){
          lv[t,s,d,]   <- L_mu%*%mu[t,s,d,] + L_nu%*%nu[t,s,d,]
          if (t < num_steps){
            mu[t+1,s,d,] <- t(A_mu)%*%mu[t,s,d,] + t(R_mu)%*%xi_mu[t,s,d,]
            nu[t+1,s,d,] <- t(A_nu)%*%nu[t,s,d,] + t(R_nu)%*%xi_nu[t,s,d,]
          }
      }
    }
  }

  lv_2 <- matrix(NA_real_, nrow = num_strata*num_delays, ncol = num_steps)
  for (t in 1:num_steps){
    for (s in 1:num_strata){
      for (d in 1:num_delays){
        lv_2[num_strata*(d - 1) + s, t] <- lv[t,s,d,1]
      }
    }
  }

  #Expect equal
  expect_equal(ss_process, lv_2)

})
