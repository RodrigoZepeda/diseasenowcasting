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
#>  [1] 2.322268 2.354589 1.867373 1.886908 1.606422 2.207643 2.057884 1.845427
#>  [9] 3.174986 2.157822
sample(gamma_prior(2, 0.1), 10)
#>  [1] 38.660561  6.194159 32.130265 23.606798 15.165826 22.241371 25.510277
#>  [8] 23.322631 25.561888 13.360454
sample(as.Date("2020-01-01") + 0:9, 3)   # falls back to base::sample()
#> [1] "2020-01-08" "2020-01-06" "2020-01-02"
```
