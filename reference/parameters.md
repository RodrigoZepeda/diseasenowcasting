# Parameter estimates from a fitted nowcast

Returns all estimated parameters as a long data frame with credible
intervals derived from the Laplace approximation posterior precision
matrix.

## Usage

``` r
parameters(x, conf.level = 0.95, ...)
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

## Details

[`tidy()`](https://generics.r-lib.org/reference/tidy.html) on a nowcast
gives you the **nowcast** – the predicted counts, via `tbl.now`'s method
for the broom generic. This function gives you the **parameters**. They
are different questions, and this package used to answer the second one
under the first one's name.

## Examples

``` r
if (requireNamespace("tbl.now", quietly = TRUE)) {
  # nc <- nowcast(data, model())
  # parameters(nc)
}
#> NULL
```
