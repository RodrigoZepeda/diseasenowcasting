# Draw random samples from a prior (or fall back to [`base::sample()`](https://rdrr.io/r/base/sample.html))

A generic that draws from a `prior_class` with the appropriate random
number generator. For **any other object** (numeric, character, factor,
`Date`, list, ...) it dispatches to
[`base::sample()`](https://rdrr.io/r/base/sample.html), so `sample()`
keeps its usual base behaviour outside the package.

## Usage

``` r
sample(object, size, ...)

## S7 method for class <any>
sample(object, size, ...)

## S7 method for class <diseasenowcasting::prior_class>
sample(object, size, ...)
```

## Arguments

- object:

  A `prior_class`, or any object accepted by
  [`base::sample()`](https://rdrr.io/r/base/sample.html).

- size:

  Number of draws.

- ...:

  Passed through to the underlying sampler.

## Value

For a prior, a numeric vector of length `size`; otherwise whatever
[`base::sample()`](https://rdrr.io/r/base/sample.html) returns.

## Examples

``` r
sample(normal_prior(log(7), 0.5), 10)
#>  [1] 2.232158 2.765740 1.837899 2.358293 1.934652 1.836037 1.191866 2.013519
#>  [9] 2.383025 1.077693
sample(gamma_prior(2, 0.1), 10)
#>  [1] 20.668476  8.490311  8.253859 26.999449 23.900658  2.719121 27.365881
#>  [8] 24.380057 11.581564 23.867802
sample(as.Date("2020-01-01") + 0:9, 3)   # falls back to base::sample()
#> [1] "2020-01-03" "2020-01-04" "2020-01-07"
```
