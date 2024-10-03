#include include/license.stan
int max_int(int a, int b){
  /*
  * @title Maximum integer
  *
  * @description Return the maximum between two integers as an integer
  *
  * @param a An integer
  * @param b An integer
  *
  * @return The maximum between a and b
  */
  if (a > b){
    return a;
  } else {
    return b;
  }
  reject("Error in `max_int` function.");
  return 1;
}

int min_int(int a, int b){
  /*
  * @title Minimum integer
  *
  * @description Return the minima between two integers as an integer
  *
  * @param a An integer
  * @param b An integer
  *
  * @return The minimum between a and b
  */
  if (a > b){
    return b;
  } else {
    return a;
  }
  reject("Error in `min_int` function.");
  return 1;
}

vector AR(matrix y, vector phi, int t, int p){
  /*
  * @title Calculate the autoregresive AR(p) component of a vector y at time t
  *
  * @description The AR(p) component is given by:
  *
  * 0*y_t + phi_p*y_{t-1} + phi_{p-1}*y_{t-2} + ... + phi_1*y_{t-p}
  *
  * which in matrix form is coded as:
  *
  * y[,(t + 1 - min_int(t, p)):(t + 1)]*phi[((p + 1) - min_int(t, p)):(p + 1)]
  *
  * where the first part is (y_{t-p}, y_{t-(p-1)}, ..., y_{t-1}, y_t)
  * and the second part is (phi_p, phi_{p-1}, ..., phi_1, 0)'
  * so that (y_{t-p}, y_{t-(p-1)}, ..., y_{t-1}, y_t)*(phi_p, phi_{p-1}, ..., phi_1, 0)'
  * yields 0*y_t + phi_p*y_{t-1} + phi_{p-1}*y_{t-2} + ... + phi_1*y_{t-p}.
  *
  * @note The `min_int(t,p)` part is set so that it works even for t < p
  *
  * @param y Matrix where each group/strata is a row and each column is a moment in time (t).
  * @param phi (p+1)-dimensional vector where the first p entries are the AR(p) components and the
  *             last entry is 0
  * @param t Integer moment in time (t > 0) to calculate the AR component
  *
  * @return The sum 0*y_t + phi_p*y_{t-1} + phi_{p-1}*y_{t-2} + ... + phi_1*y_{t-p}
  */
  return y[,(t + 1 - min_int(t, p + 1)):t]*phi[(p + 2 - min_int(t, p + 1)):(p + 1)];
}

vector MA(matrix xi, vector theta, int t, int q){
  /*
  * Calculate the moving average MA(q) component given errors xi at time t
  *
  * @details The MA(q) component is given by:
  *
  * €_t + ø_{q}*€_{t-1} + ø_{q-1}*€_{t-2} + ... + ø_{1}*€_{t-q}
  *
  * which in matrix form is coded as:
  *
  * €[,(t + 1 - min_int(t, q)):(t + 1)]*theta[((q + 1) - min_int(t, q)):(q + 1)]
  *
  * where the first part is (€_{t-q}, €_{t-(q-1)}, ..., €_{t-1}, €_t)
  *
  * and the second part is (ø_1, ø_2, ..., ø_q, 1)'
  *
  * so that
  *
  * (€_{t-q}, €_{t-(q-1)}, ..., €_{t-1}, €_t)*(ø_1, ø_{2}, ..., ø_q, 1)'
  *
  * yields €_t + ø_{q}*€_{t-1} + ø_{q-1}*€_{t-2} + ... + ø_{1}*€_{t-q}
  *
  * @note The `min_int(t,q)` part is set so that it works even for t < q
  *
  * @param € Error matrix where each group/strata is a row and each column is a moment in time (t).
  * @param theta (q+1)-dimensional vector where the first q entries are the MA(q) components and the
  *             last entry is 1
  * @param t Integer moment in time (t > 0) to calculate the MA component
  *
  * @return The sum €_t + ø_{q}*€_{t-1} + ø_{q-1}*€_{t-2} + ... + ø_{1}*€_{t-q}
  */
  return AR(xi, theta, t, q);
}


vector create_phi_AR(vector phi){
  /*
  * Create the phi vector for AR(p) coeficients
  *
  * @description The AR(p) component is given by:
  *
  * 0*y_t + phi_p*y_{t-1} + phi_{p-1}*y_{t-2} + ... + phi_1*y_{t-p}
  *
  *  and this the phi_AR vector is given by:
  *
  *  (phi_1, phi_2, ..., phi_p, 0)
  *
  * @param phi A vector with entries (phi_1, phi_2, ..., phi_p)
  * @return The vector (phi_1, phi_2, ..., phi_p, 0)
  */
  return append_val_2_vec(phi, 0.0);
}

vector create_theta_MA(vector theta){
  /*
  * Create the theta vector for MA(p) coeficients
  *
  * @description The MA(q) component is given by:
  *
  * €_t + ø_{q}*€_{t-1} + ø_{q-1}*€_{t-2} + ... + ø_{1}*€_{t-q}
  *
  * and this the phi_MA vector is given by:
  * (ø_1, ø_2, ..., ø_q, 1)
  *
  * @param theta A vector with entries (ø_1, ø_2, ..., ø_q)
  * @return The vector (ø_1, ø_2, ..., ø_q, 1)
  */
  return append_val_2_vec(theta, 1.0);
}
