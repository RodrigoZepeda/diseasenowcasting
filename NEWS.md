# diseasenowcasting 0.3.0

* New likelihood function accounts for delay-truncated and batched observations. This new likelihood
does not require zeroes making the model more light-weight. 
* Changes the `preprocess_for_nowcast()` function so that the dataset given to the model does not 
include zeroes (previous function extended the observations for all zeroes) and also returns `.dstar` the
maximum possible observed delay. 
* Reduced the number of distributions to Poisson and NegativeBinomial only. 

# diseasenowcasting 0.2.6

* Fixed the time covariates from preprocess_dates.

# diseasenowcasting 0.1.0

* Initial creation.
