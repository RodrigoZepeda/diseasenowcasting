//Version 0.0

functions {
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
}

data {
  //Data -----------------------------------------------------------------------------------------
  int<lower=1> num_steps;      //Number of time steps modelled
  int<lower=0> num_delays;     //Maximum number of unique delays considered
  int<lower=1> num_strata;     //Number of strata included in the model
  int<lower=1> n_rows;         //Number of rows in case data

  //Case data -------------------------------------------------------------------------------------
  array[n_rows, 3] int case_idx;
  array[n_rows, 1] real cases;
  /*
  `case_idx` is a matrix with columns 1 = time, 2 = delay, 3 = strata. The matrix corresponds
  one to one to the `cases` matrix defined either in the data_discrete.stan or data_continuous.stan
  files.
  The correspondance is 1-to-1 following the row number as follows:

      cases                                   case_idx
                               Time            Delay            Strata
      ------                 -------------------------------------------
       10                       1                1                 1
       11                       1                2                 1
       09                       1                3                 1
       21                       1                1                 2
       16                       1                2                 2
       15                       1                3                 2
       12                       2                1                 1
       08                       2                2                 1
       08                       2                3                 1
       25                       2                1                 2
       20                       2                2                 2
       17                       2                3                 2
       -----------------------------------------------------------------
  */

  //ARMA component options -----------------------------------------------------------------------
  int<lower=0> mu_p;   //Lags considered for an autoregresive AR(p) model
  int<lower=0> mu_q;   //Lags considered for a moving average MA(q) model
  int<lower=0> eta_d;  //Amount of delays considered for the model


}
