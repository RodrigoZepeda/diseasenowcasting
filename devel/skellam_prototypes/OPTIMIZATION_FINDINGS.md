# Cumulative-model optimization

## Problem found

The first all-location sweep treated every AR innovation as an ordinary outer
MAP parameter. Depending on the origin and clock, this left roughly 60--90
epidemic coefficients plus delay and observation parameters in one `nlminb`
problem. The fits usually returned finite values and optimizer code zero, but
many were not close enough to a stationary point.

Across the 1,060 cumulative Poisson/NB fits:

- only 711 had maximum gradient below `0.1`;
- median maximum gradient was `0.067`;
- the worst maximum gradient was `38.3`.

The residual gradients were usually epidemic-path innovations rather than
report-delay, retraction, or count-dispersion parameters.

## Changes

1. `build_identifiability_model(..., use_random = TRUE)` now sends the latent
   AR innovations, HSGP coefficients, or SIR transmission innovations to
   RTMB's random-effect block.
2. Cumulative prototypes use this Laplace formulation by default. The hurdle
   model retains its faster MAP formulation because its original gradients were
   already stable and the hurdle Laplace objective is considerably slower.
3. Conservative outer-parameter bounds prevent exploratory optimizer steps
   from overflowing delay scales, epidemic transforms, or count dispersion.
4. Fits begin with bounded `nlminb`. If a Laplace fit still has maximum gradient
   above `0.05`, a guarded bounded L-BFGS-B polish is attempted. It is accepted
   only when the objective is no worse within numerical tolerance and the
   gradient improves.
5. The stability runner now uses `proc.time()` for monotonic ETAs. A host-clock
   discontinuity during testing showed why `Sys.time()` was unsuitable.

## Full rerun

The optimized sweep repeated all 53 locations, five origins, and both clocks
for cumulative Poisson and cumulative NB: 530 jobs and 1,060 fits.

| clock | model | fits | optimizer code 0 | gradient < 0.1 | median gradient | maximum gradient |
|---|---|---:|---:|---:|---:|---:|
| calendar | cumulative NB | 265 | 265 | 265 | 0.000394 | 0.00615 |
| calendar | cumulative Poisson | 265 | 265 | 264 | 0.000958 | 0.171 |
| compressed | cumulative NB | 265 | 265 | 265 | 0.000228 | 0.0183 |
| compressed | cumulative Poisson | 265 | 265 | 265 | 0.000257 | 0.0703 |

Thus Laplace improved the gradient gate from 711/1,060 to 1,059/1,060, reduced
the median gradient by roughly two orders of magnitude, and reduced the maximum
from `38.3` to `0.171`. There were no failed or non-finite final fits.

The sole remaining sweep warning was US/calendar/cumulative-Poisson at
`2025-03-29`. The new L-BFGS-B stage was added after that sweep and reduced its
gradient from `0.171` to `0.0158`, so the final optimizer configuration passes
the `0.1` gate on all previously tested cases.

Terminal-retention estimates were not materially displaced merely by changing
the optimizer: median absolute change in `q_C(H)` was `0.00010`, and its 95th
percentile was `0.0182`. Predictive scores were also similar, which indicates
that the change fixes numerical stationarity rather than manufacturing a new
substantive solution.

## Remaining statistical issue

Optimization is no longer the main limitation of the cumulative prototype.
Some cumulative fits still estimate implausibly low terminal retention, and
the cumulative models remain sensitive to calendar versus compressed clocks.
That is a likelihood/identifiability and composite-weighting issue, not an
optimizer failure. A finite, stationary Laplace fit should therefore not be
mistaken for validation of the cumulative marginal independence approximation.
