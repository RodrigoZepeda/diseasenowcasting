//Transform with log1p the number of cases
real log_transform(real x){
  return log(x);
}

real inv_log_transform(real y){
  return exp(y);
}

//Transform with log1p the number of cases
array[] real log_transform(data array[] real x){

    /*Get the normalized cases*/
    int n_rows = num_elements(x);
    array[n_rows] real y;

    for (n in 1:n_rows)
      y[n] = log_transform(x[n]);

    return y;

}

//Transform with log1p the number of cases
array[] real inv_log_transform(data array[] real y){

    /*Get the normalized cases*/
    int n_rows = num_elements(y);
    array[n_rows] real x;

    for (n in 1:n_rows)
      x[n] = max({inv_log_transform(y[n]), 0.0});

    return x;

}

//Transform with log1p the number of cases
real log1p_transform(real x){
  return log_transform(x + 1);
}

real inv_log1p_transform(real y){
  return inv_log_transform(y) - 1.0;
}

//Transform with log1p the number of cases
array[] real log1p_transform(data array[] real x){

    /*Get the normalized cases*/
    int n_rows = num_elements(x);
    array[n_rows] real y;

    for (n in 1:n_rows)
      y[n] = log1p_transform(x[n]);

    return y;

}

//Transform with log1p the number of cases
array[] real inv_log1p_transform(data array[] real y){

    /*Get the normalized cases*/
    int n_rows = num_elements(y);
    array[n_rows] real x;

    for (n in 1:n_rows)
      x[n] = max({inv_log1p_transform(y[n]), 0.0});

    return x;

}

//Get the classical softplus transform
real softplus_transform(real x, real k){
  return k*log(exp(x/k) - 1.0);
}

real inv_softplus_transform(real y, real k){
  return k*log(1.0 + exp(y/k));
}

//Transform with log1p the number of cases
array[] real softplus_transform(data array[] real x, data real k){

    /*Get the normalized cases*/
    int n_rows = num_elements(x);
    array[n_rows] real y;

    for (n in 1:n_rows)
      y[n] = softplus_transform(x[n], k);

    return y;

}

//Transform with log1p the number of cases
array[] real inv_softplus_transform(data array[] real y, data real k){

    /*Get the normalized cases*/
    int n_rows = num_elements(y);
    array[n_rows] real x;

    for (n in 1:n_rows)
      x[n] = inv_softplus_transform(y[n], k);

    return x;

}

//Get the classical softplus transform
real softplus1p_transform(real x, real k){
  return softplus_transform(x + 1, k);
}

real inv_softplus1p_transform(real y, real k){
  return inv_softplus_transform(y, k) - 1.0;
}

//Transform with log1p the number of cases
array[] real softplus1p_transform(data array[] real x, data real k){

    /*Get the normalized cases*/
    int n_rows = num_elements(x);
    array[n_rows] real y;

    for (n in 1:n_rows)
      y[n] = softplus1p_transform(x[n], k);

    return y;

}

//Transform with log1p the number of cases
array[] real inv_softplus1p_transform(data array[] real y, data real k){

    /*Get the normalized cases*/
    int n_rows = num_elements(y);
    array[n_rows] real x;

    for (n in 1:n_rows)
      x[n] = max({inv_softplus1p_transform(y[n], k), 0.0});

    return x;

}

/*
@title Distorted hyperbolic transform

@description These functions implement the distorted hyperbolic transform given by:
c*x/2 + sqrt( (c*x)^2/4 + k) for c,k > 0
which has inverse given by:
x = y^2 - k/(c*y)
as calculated with Wolfram Alpha
*/

real dhyperbolic_transform(real x, real k, real c){
  return (pow(x,2) - k)/(c*x);
}

real inv_dhyperbolic_transform(real y, real k, real c){
  return c*y/2.0 + sqrt(pow(c*y/2.0, 2) + k);
}

//Transform with log1p the number of cases
array[] real dhyperbolic_transform(data array[] real x, data real k, data real c){

    /*Get the normalized cases*/
    int n_rows = num_elements(x);
    array[n_rows] real y;

    for (n in 1:n_rows)
      y[n] = dhyperbolic_transform(x[n], k, c);

    return y;

}

//Transform with log1p the number of cases
array[] real inv_dhyperbolic_transform(data array[] real y, data real k, data real c){

    /*Get the normalized cases*/
    int n_rows = num_elements(y);
    array[n_rows] real x;

    for (n in 1:n_rows)
      x[n] = inv_dhyperbolic_transform(y[n], k, c);

    return x;

}

real dhyperbolic1p_transform(real x, real k, real c){
  return dhyperbolic_transform(x + 1, k, c);
}

real inv_dhyperbolic1p_transform(real y, real k, real c){
  return inv_dhyperbolic_transform(y, k, c) - 1.0;
}

//Transform with log1p the number of cases
array[] real dhyperbolic1p_transform(data array[] real x, data real k, data real c){

    /*Get the normalized cases*/
    int n_rows = num_elements(x);
    array[n_rows] real y;

    for (n in 1:n_rows)
      y[n] = dhyperbolic1p_transform(x[n], k, c);

    return y;

}

//Transform with log1p the number of cases
array[] real inv_dhyperbolic1p_transform(data array[] real y, data real k, data real c){

    /*Get the normalized cases*/
    int n_rows = num_elements(y);
    array[n_rows] real x;

    for (n in 1:n_rows)
      x[n] = max({inv_dhyperbolic1p_transform(y[n], k, c), 0.0});

    return x;

}

//Transform the m matrix either into exp / identity or other functions
matrix transform_state_space_model(matrix m, int identity_link, int log_link, int softplus_link,
  int dist_hyper_link, real k, real c){

  matrix[rows(m), cols(m)] m_trans;
  if (identity_link){
    return m;
  } else if (log_link){
    for (i in 1:rows(m)){
      for (j in 1:cols(m)){
        m_trans[i,j] = inv_log_transform(m[i,j]);
      }
    }
    return m_trans;
  } else if (softplus_link){
    for (i in 1:rows(m)){
      for (j in 1:cols(m)){
        m_trans[i,j] = inv_softplus_transform(m[i,j], k);
      }
    }
    return m_trans;
  } else if (dist_hyper_link){
    for (i in 1:rows(m)){
      for (j in 1:cols(m)){
        m_trans[i,j] = inv_dhyperbolic_transform(m[i,j], k, c);
      }
    }
    return m_trans;
  } else {
    reject("Invalid link @transform_state_space_model. This is an internal error of the `diseasenowcasting` package. Report to `https://github.com/RodrigoZepeda/diseasenowcasting/issues`");
  }
}
