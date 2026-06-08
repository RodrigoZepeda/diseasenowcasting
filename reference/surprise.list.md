# Surprise score on a raw fit() result

Surprise score on a raw fit() result

## Usage

``` r
# S3 method for class 'list'
surprise(
  object,
  new_data,
  type = c("both", "count", "delay"),
  level = 0.99,
  n_draws = 500L,
  seed = sample.int(.Machine$integer.max, 1)
)
```

## Arguments

- object:

  A list returned by
  [`fit()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/fit.md).

- new_data:

  See
  [`surprise()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/surprise.md).

- type:

  See
  [`surprise()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/surprise.md).

- level:

  See
  [`surprise()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/surprise.md).

- n_draws:

  See
  [`surprise()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/surprise.md).

- seed:

  See
  [`surprise()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/surprise.md).
