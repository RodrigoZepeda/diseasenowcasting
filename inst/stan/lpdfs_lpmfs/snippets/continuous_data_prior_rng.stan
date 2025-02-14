/*
  * Data prior rng
  *
  * Function that generates a random number (rng) of the data according to the prior specification,
  * `prior_spec`.
  *
  * @param param_1 The first parameter for the distribution
  * @param param_2 The second parameter for the distribution
  * @param prior_spec A number the distribution. See the note below
  *
  * @note If the distribution doesn't have param_2 you can write any number.
  * As an example for an exponential(3) you would do:
  * data_lpf(x, 3, 10, 5);
  * and the value of 10 for param_2 is ignored.
  *
  * @return An lpdf (respectively lpmf) object to sum to target
*/
//This is the main body of the dist_lpdf function:
if (prior_spec == 1)  return normal_rng(param_1, param_2);
else if (prior_spec == 2)  return student_t_rng(3.0, param_1, param_2);
else reject("invalid link");
