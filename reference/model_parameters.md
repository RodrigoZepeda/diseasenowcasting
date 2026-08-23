# Parameter estimates from a fitted nowcast

Returns every estimated parameter as a long data frame with credible
intervals derived from the Laplace approximation posterior precision
matrix. This is the table
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) returned
before version 2.1.0.

## Usage

``` r
model_parameters(x, conf.level = 0.95, ...)
```

## Arguments

- x:

  A `nowcast` object from
  [`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md).

- conf.level:

  Credible level for the interval (default 0.95).

- ...:

  Unused.

## Value

A `data.frame` with columns `term`, `estimate`, `std.error`, `conf.low`,
`conf.high`, `type`.

## See also

[`tidy()`](https://generics.r-lib.org/reference/tidy.html) for the
per-date nowcast table.
