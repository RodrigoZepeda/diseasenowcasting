# Compute surprise scores for new observations

Given a fitted nowcast and new data that arrived *after* the fit,
compute how surprising the new data is under the fitted posterior.

Two types of surprise are supported (select via the `type` argument):

- `"count"`: How surprising is a new case count at a given event time

- `"delay"`: How surprising is a report with a very long delay

- `"both"`: Compute both (requires appropriate `new_data` format).

## Usage

``` r
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

  A `nowcast_class` object (or
  [`fit()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/fit.md)
  result).

- new_data:

  For `type = "count"`: a data.frame with columns `event_index`
  (0-indexed) and `count` (observed new total at that time). For
  `type = "delay"`: a data.frame with columns `delay` (numeric delay
  values) and optionally `weight` (counts with that delay, default 1).
  For `type = "both"`: supply both sets of columns.

- type:

  Which surprise type(s) to compute. One of `"count"`, `"delay"`,
  `"both"` (default `"both"`).

- level:

  Credible level used to flag surprises (default `0.99`). A count is
  flagged when it falls in the outer `(1 - level)/2` tail of the
  posterior predictive (too high or too low); a delay is flagged when
  `P(D >= d)` or `P(D <= d)` is below `1 - level` (surprisingly long or
  short).

- n_draws:

  Number of posterior draws (default 500).

- seed:

  Optional RNG seed.

## Value

A list with:

- `$count_surprise` (data.frame): one row per event time with
  `event_index`, `observed`, `posterior_mean`, `lpd`, `ppp_right`,
  `ppp_left`, `relative_surprise`.

- `$delay_surprise` (data.frame): one row per delay value with `delay`,
  `tail_prob` (= 1 - G_D(delay)), `lpd`, `relative_surprise`.
