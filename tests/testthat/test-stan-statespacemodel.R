nsteps = 10
Amu   = matrix(c(1,1,1,0), ncol = 2)
Rmu   = matrix(0, ncol = 2, nrow = 2)
mu_0  = c(0.2, 0.4)
xi_mu = lapply(1:nsteps, function(x) rnorm(2))

Anu   = matrix(c(1,1,1,0), ncol = 2)
Rnu   = matrix(0, ncol = 2, nrow = 2)
nu_0  = c(0.2, 0.4)
xi_nu = lapply(1:nsteps, function(x) rnorm(2))

test_that("The `time_dependent_process` works", {
    #Run the stan model
    stan_model = time_dependent_process(nsteps, Amu, Rmu, mu_0, xi_mu, pstream__ = rstan::get_stream())

    #Run the process manually
    mu = list(mu_0)
    for (n in 2:nsteps){
      mu_t = Amu%*%mu[[n-1]] + Rmu%*%xi_mu[[n-1]]
      mu   = append(mu, list(as.vector(mu_t)))
    }
    expect_equal(stan_model, mu)

    #One run should be mu_0
    stan_model_1 = time_dependent_process(1, Amu, Rmu, mu_0, xi_mu, pstream__ = rstan::get_stream())
    expect_equal(stan_model_1, list(mu_0))

})

test_that("The `time_delay_dependent_process` works", {
  #Run the stan model
  stan_model = time_delay_dependent_process(nsteps, Anu, Rnu, nu_0, xi_nu, pstream__ = rstan::get_stream())

  #Run the process manually
  nu = list(nu_0)
  for (n in 2:nsteps){
    nu_t = Anu%*%nu[[n-1]] + Rnu%*%xi_nu[[n-1]]
    nu   = append(nu, list(as.vector(nu_t)))
  }
  expect_equal(stan_model, nu)

  #One run should be mu_0
  stan_model_1 = time_delay_dependent_process(1, Anu, Rnu, nu_0, xi_nu, pstream__ = rstan::get_stream())
  expect_equal(stan_model_1, list(nu_0))

})
