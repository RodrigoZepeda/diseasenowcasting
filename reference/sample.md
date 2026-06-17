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
#>  [1] 2.834974 1.508284 1.285096 1.251464 2.342570 2.266902 1.765483 2.411622
#>  [9] 2.132079 1.273937
sample(gamma_prior(2, 0.1), 10)
#>  [1] 18.332930  9.852027  9.440716 42.166949 61.715371 42.632386 38.849332
#>  [8] 21.278147  6.658082 32.736891
sample(as.Date("2020-01-01") + 0:9, 3)   # falls back to base::sample()
#> [1] "2020-01-06" "2020-01-03" "2020-01-02"
```
