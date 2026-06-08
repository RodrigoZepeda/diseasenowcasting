# Draw random samples from a prior (or fall back to [`base::sample()`](https://rdrr.io/r/base/sample.html))

A generic that draws from a `prior_class` with the appropriate random
number generator. For **any other object** (numeric, character, factor,
`Date`, list, ...) it dispatches to
[`base::sample()`](https://rdrr.io/r/base/sample.html), so `sample()`
keeps its usual base behaviour outside the package.

## Usage

``` r
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
#>  [1] 1.2458884 2.0735687 0.7272783 1.9431245 2.2566865 2.5201160 1.0350013
#>  [8] 1.8222475 1.8238103 1.8045574
sample(gamma_prior(2, 0.1), 10)
#>  [1]  8.985053  6.346810  1.674857 21.627396  7.190608 14.362678 41.630195
#>  [8]  6.945420 19.774561  3.277254
sample(as.Date("2020-01-01") + 0:9, 3)   # falls back to base::sample()
#> [1] "2020-01-05" "2020-01-02" "2020-01-01"
```
