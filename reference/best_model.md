# The winning [`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md) object from a nowcast

Returns the fitted nowcast's
[`model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/model.md)
specification. For an
[`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md)
result this is the **selected** model, so you can reuse it elsewhere,
e.g. `nowcast(other_data, model = best_model(nc))` or pass it to
[`backtest()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/backtest.md).

## Usage

``` r
best_model(nc)
```

## Arguments

- nc:

  A `nowcast_class` (from
  [`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
  or
  [`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md)).

## Value

A `model_class` object.

## See also

[`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md),
[`best_model_name()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/best_model_name.md)
