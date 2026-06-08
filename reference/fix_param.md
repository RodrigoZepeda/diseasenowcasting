# Hard-fix a parameter in a prior bundle (treat as data, drop from estimation)

Hard-fix a parameter in a prior bundle (treat as data, drop from
estimation)

## Usage

``` r
fix_param(priors, key, value)
```

## Arguments

- priors:

  A prior bundle from
  [`default_priors()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/default_priors.md).

- key:

  Parameter key to fix.

- value:

  Fixed numeric value.

## Value

The modified prior bundle.
