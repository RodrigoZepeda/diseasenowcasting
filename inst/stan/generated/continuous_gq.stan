for (t in 1:num_steps){
    if (is_negative_binomial){
        N_mat_predict[t,:] =
        normal_rng(lambda[:, t]', rep_vector(r[1] + precision_tol, num_delays*num_strata));
    } else {
        N_mat_predict[t,:] = student_t_rng(
          rep_vector(3.0, num_delays*num_strata),
          lambda[:, t]',
          rep_vector(r[1] + precision_tol, num_delays*num_strata));
    }
}
