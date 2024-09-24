for (t in 1:num_steps){
    if (is_negative_binomial){
        N_mat_predict[t,:] =
        normal_rng(lambda[:, t]', rep_vector(r[1] + precision_tol, num_delays*num_strata));
    } else {
        N_mat_predict[t,:] = student_t_rng(3.0, lambda[:, t]', rep_vector(r[1] + precision_tol, num_delays*num_strata));
    }
}
