# Surprising (extreme) values flagged during the last `update()`

Returns a tidy `data.frame` with one row per flagged surprise –
currently the reporting delays that arrived later than the model expects
(`surprise = "delay"`, `direction = "long"`) – together with their tail
probability and the `level` used. Returns `NULL` when nothing was
flagged (or surprise was not computed).

## Usage

``` r
extreme_values(nc)
```

## Arguments

- nc:

  A `nowcast_class` object returned by
  [`update()`](https://rdrr.io/r/stats/update.html).

## Value

A `data.frame` of flagged surprises, or `NULL`.
