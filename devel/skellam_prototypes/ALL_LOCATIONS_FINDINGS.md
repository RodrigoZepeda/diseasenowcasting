# All-location stability sweep

> The numerical table below describes the original fixed-MAP sweep. The
> cumulative models were subsequently moved to RTMB Laplace random effects;
> see `OPTIMIZATION_FINDINGS.md` for the improved 1,060-fit rerun.

## Configuration

- Locations: all 53 FluSight series (50 states, District of Columbia, Puerto
  Rico, and US).
- Origins: `2025-01-02`, `2025-02-15`, `2025-03-29`, `2025-04-26`, and
  `2025-05-31`.
- Clocks: calendar and compressed publication-step clocks.
- Settlement horizon: `H = 26` model periods.
- Epidemic process: AR.
- Report and retraction delay family: lognormal.
- Models: cumulative Poisson composite, cumulative NB composite, and signed
  hurdle--ZTNB update composite.
- Reconstruction draws: 250 per target, conditional on fitted parameters.
- Recent targets: five per origin and location, giving 1,325 scored targets per
  clock and reconstruction.

The run took roughly 37 minutes after its one-time all-location data
preparation. It is checkpointed after every location/origin/clock job.

## Did it break?

No execution or domain failure occurred:

- 530/530 location/origin/clock jobs completed;
- 1,590/1,590 model fits returned finite objectives, parameters, epidemic
  means, and `q_C` values;
- 1,590/1,590 leakage assertions passed;
- all requested reconstructions returned finite draws;
- zero captured errors, including for Puerto Rico and US.

This establishes broad software stability for AR plus lognormal delays. The
separate smoke suite tapes and differentiates AR, HSGP, and SIR crossed with
lognormal, gamma, and generalized-gamma delays.

## Numerical quality

| clock | model | fits | optimizer code 0 | max gradient < 0.1 | median gradient | maximum gradient |
|---|---|---:|---:|---:|---:|---:|
| calendar | cumulative NB | 265 | 265 | 218 | 0.036 | 0.750 |
| calendar | cumulative Poisson | 265 | 264 | 148 | 0.087 | 23.2 |
| calendar | hurdle--ZTNB | 265 | 263 | 261 | 0.00056 | 0.213 |
| compressed | cumulative NB | 265 | 265 | 179 | 0.070 | 8.87 |
| compressed | cumulative Poisson | 265 | 264 | 166 | 0.074 | 38.3 |
| compressed | hurdle--ZTNB | 265 | 260 | 265 | 0.00087 | 0.0999 |

The hurdle model is substantially more stable than either cumulative-level
composite. Across its 530 fits, all were finite, 523 had optimizer code zero,
and 526 had maximum gradient below 0.1. The seven nonzero optimizer codes all
had gradients below 0.1. The only four gradient warnings were calendar fits for
Delaware (`0.213`, `0.161`) and US (`0.105`, `0.160`). Stricter refits returned
the same solutions. Inspection localized each residual primarily to one AR
innovation rather than a delay, hurdle, or ZTNB parameter.

Thus the hurdle likelihood does not show a support/domain breakdown, but the
four calendar fits should retain a numerical-warning flag. The cumulative
composites need more optimizer work before being treated as dependable,
especially because a finite return is not the same as a stationary solution.

## Aggregate predictive check

Absolute scores mix state and national count scales, so they are descriptive.

| clock | reconstruction | WIS | MAE | 90% coverage |
|---|---|---:|---:|---:|
| calendar | empirical multiplier | 38.2 | 38.2 | 0.393 |
| calendar | cumulative Poisson, anchored | 32.2 | 35.2 | 0.608 |
| calendar | cumulative NB, direct | 198.0 | 140.0 | 0.891 |
| calendar | hurdle--ZTNB, anchored | **28.0** | **36.7** | **0.750** |
| compressed | empirical multiplier | 38.3 | 38.3 | 0.408 |
| compressed | cumulative Poisson, anchored | **26.8** | **30.2** | 0.630 |
| compressed | cumulative NB, direct | 32.3 | 38.9 | 0.832 |
| compressed | hurdle--ZTNB, anchored | 28.4 | 37.6 | **0.689** |

At the location level, hurdle--ZTNB beats the empirical multiplier in mean WIS
for 49/53 calendar-clock locations and 42/53 compressed-clock locations. The
median location-level WIS improvement is 24.7% and 15.5%, respectively.

Intervals still condition on fitted parameters. Coverage should therefore not
be interpreted as calibrated Bayesian or frequentist coverage; composite-score
and parameter uncertainty remain to be added.

## Output

- `all_locations_ar_lognormal_H26.rds`: complete diagnostics and scores.
- `all_locations_ar_lognormal_H26_diagnostics.csv`: one row per fit.
- `all_locations_ar_lognormal_H26_failures.csv`: empty; no failures.
- `checkpoint_ar_lognormal_H26.rds`: resumable job-level checkpoint.
- `strict_hurdle_refits.csv`: the 11 hurdle warning cases under the stricter
  optimizer budget.
