# Quantile-table summary of a pooled nowcast draws matrix

Matches diseasenowcast2's `.summarise_nowcast_matrix()` exactly: one row
per event (`.event_num` 0-indexed, ascending) with columns
`mean, median, sd, mad, q2.5, q5, q10, q25, q50, q75, q90, q95, q97.5`.

## Usage

``` r
summarise_nowcast_matrix(draws_matrix)
```

## Arguments

- draws_matrix:

  Pooled draws matrix `[n_draws x max_time]`.

## Value

A data.frame in the scoring-pipeline format.
