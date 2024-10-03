  //Matrix of the average (or log average) of the model
  matrix[tsize, num_steps] m = state_space_model(num_steps, num_delays, num_strata, phi_mu,
    theta_mu, phi_nu, mu_intercept, nu_intercept, mu_p, mu_q, nu_p, mu_init, nu_init,
    sd_mu, sd_nu, xi_mu, xi_nu);


  array[n_rows] real merror;
  for (n in 1:n_rows)
    merror[n] = (cases[n] - m[num_delays*(case_idx[n,s_col] - 1) + case_idx[n,d_col], case_idx[n,t_col]])/sd_m;

  //Vector of observed cases
  //array[n_rows] real m_obs = observed_mean(n_rows, num_delays, m, case_idx, s_col, d_col, t_col);

  //Prior calculation
  #include include/lprior.stan
