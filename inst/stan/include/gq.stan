array[num_steps, tsize] int N_mat_predict_int   = rep_array(0, num_steps, tsize);
array[num_steps, tsize] real N_mat_predict_real = rep_array(0, num_steps, tsize);
matrix[num_steps, tsize] N_mat_predict          = rep_matrix(0, num_steps, tsize);
matrix[num_steps, num_strata] N_predict_raw     = rep_matrix(0, num_steps, num_strata);
matrix[num_steps, num_strata] N_predict         = rep_matrix(0, num_steps, num_strata);

/*Initial predictions on the untransformed scale (for y)*/
if (is_poisson){
  for (t in 1:num_steps)
    N_mat_predict_int[t,:] = poisson_rng(m_trans[:,t]');
} else if (is_negbin){
  for (t in 1:num_steps)
    N_mat_predict_int[t,:] = neg_binomial_2_rng(m_trans[:,t]', rep_row_vector(1.0 / sd_m[1], tsize));
} else if (is_normal){
  for (t in 1:num_steps)
    N_mat_predict_real[t,:] = normal_rng(m_trans[:,t]', rep_vector(sd_m[1.0], tsize));
} else if (is_student){
  for (t in 1:num_steps)
    N_mat_predict_real[t,:] = student_t_rng(dof, m_trans[:,t]', sd_m[1.0]);
} else {
  reject("Unknown distribution @gq.stan. This is an internal error of the `diseasenowcasting` package. Report to `https://github.com/RodrigoZepeda/diseasenowcasting/issues`");
}

/*REMOVING NORMALIZATION*/
if (normalize_data && is_continuous){
  N_mat_predict_real = inv_normalize_cases_2(N_mat_predict_real, case_idx, num_strata,
    num_delays, num_steps, tsize, d_col, s_col, mu_data, sigma_data);
} else if (normalize_data && !is_continuous){
  reject("Non-continuous data cannot be normalized. Don't know how to proceed @gq.stan. This is an internal error of the `diseasenowcasting` package. Report to `https://github.com/RodrigoZepeda/diseasenowcasting/issues`");
}


/*SCALING: Scale back y's predictions on the untransformed scale (for y)--------------------------*/
for (t in 1:num_steps){
  for (s in 1:tsize){
    if (identity_link_y && is_discrete){
      N_mat_predict[t,s] = N_mat_predict_int[t,s];
    } else if (identity_link_y && is_continuous){
      N_mat_predict[t,s] = N_mat_predict_real[t,s];
    } else if (log_link_y && is_continuous){
      N_mat_predict[t,s] = inv_log1p_transform(N_mat_predict_real[t,s]);
    } else if (softplus_link_y && is_continuous){
      N_mat_predict[t,s] = inv_softplus1p_transform(N_mat_predict_real[t,s], control_k_transform);
    } else if (dist_hyper_link_y && is_continuous){
      N_mat_predict[t,s] = inv_dhyperbolic1p_transform(N_mat_predict_real[t,s], control_k_transform, control_c_transform);
    } else {
      reject("Invalid link specified for transform @gq.stan. This is an internal error of the `diseasenowcasting` package. Report to `https://github.com/RodrigoZepeda/diseasenowcasting/issues`");
    }
  }
}

/*Save the predictions from the model-------------------------------------------------------------*/
for (t in 1:num_steps){
  for (s in 1:num_strata){
    for (d in 1:num_delays){
      N_predict_raw[t,s] += N_mat_predict[t, num_delays*(s - 1) + d];
    }
  }
}

/*Substitute back those values we have already observed-------------------------------------------*/
for (n in 1:n_rows){
  if (is_discrete){
    N_mat_predict[case_idx[n,t_col], num_delays*(case_idx[n,s_col] - 1) + case_idx[n,d_col]] = cases_int[n];
  } else if (is_continuous) {
    N_mat_predict[case_idx[n,t_col], num_delays*(case_idx[n,s_col] - 1) + case_idx[n,d_col]] = cases_real[n];
  } else {
    reject("Unknown distribution to substitue to N_mat @gq.stan. This is an internal error of the `diseasenowcasting` package. Report to `https://github.com/RodrigoZepeda/diseasenowcasting/issues`");
  }
}

/*Get the overall total (rowsums) once the model and known values are considered------------------*/
for (t in 1:num_steps){
  for (s in 1:num_strata){
    for (d in 1:num_delays){
      N_predict[t,s] += N_mat_predict[t,num_delays*(s - 1) + d];
    }
  }
}
