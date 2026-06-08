# Tidy parameter estimates from a fitted nowcast

Returns all estimated parameters as a long data frame with credible
intervals derived from the Laplace approximation posterior precision
matrix.

## Usage

``` r
tidy(x, conf.level = 0.95, ...)
```

## Arguments

- x:

  A `nowcast_class` object.

- conf.level:

  Credible level for the interval (default 0.95).

- ...:

  Unused.

## Value

A `data.frame` with columns `term`, `estimate`, `std.error`, `conf.low`,
`conf.high`, `type`.
