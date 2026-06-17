# Name of the model chosen by `auto_nowcast()`

Name of the model chosen by
[`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md)

## Usage

``` r
best_model_name(nc)
```

## Arguments

- nc:

  A `nowcast_class` returned by
  [`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md).

## Value

The winning model's label, a string of the form
`"epidemic/likelihood/delay"` (e.g. `"HSGP/nb/Dirichlet"`).

## See also

[`auto_nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/auto_nowcast.md),
[`best_model()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/best_model.md),
[`comparison_scores()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/comparison_scores.md)
