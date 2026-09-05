# Prototype findings

These are early diagnostic results, not a model-selection conclusion. They use
Texas, an AR epidemic path, lognormal report/retraction delays, and 500
conditional predictive draws. Final 2024/2025 snapshots are retrospective
truth.

## Leakage audit

For `now = 2025-01-02`, both clock arms stop at the release dated
`2024-12-28`. Every fitted row satisfies both `event_date <= now` and
`report_date <= now`. The truth table is joined only after fitting and is never
used by the empirical baseline.

## One-year terminal horizon (`H = 52` model periods)

| clock | reconstruction | mean WIS | median MAE | 90% coverage |
|---|---:|---:|---:|---:|
| calendar | empirical multiplier | 26.6 | 26.6 | 0.0 |
| calendar | cumulative Poisson, anchored | 51.0 | 81.7 | 1.0 |
| calendar | cumulative Poisson, direct | 64.3 | 80.4 | 0.2 |
| calendar | cumulative NB, direct | 62.0 | 89.6 | 1.0 |
| calendar | hurdle--ZTNB, anchored | 26.8 | 9.4 | 1.0 |
| compressed | empirical multiplier | 26.4 | 26.4 | 0.0 |
| compressed | cumulative Poisson, anchored | 163.0 | 205.0 | 0.0 |
| compressed | cumulative Poisson, direct | 154.0 | 172.0 | 0.0 |
| compressed | cumulative NB, direct | 22.8 | 26.2 | 1.0 |
| compressed | hurdle--ZTNB, anchored | 31.5 | 6.2 | 1.0 |

The hurdle model gives much better point reconstruction than the empirical
baseline in this tiny slice, but its intervals are broad. WIS is therefore
similar to or somewhat worse than the baseline at the one-year horizon. The
baseline has no interval coverage here because the available age-specific
multipliers are nearly degenerate.

The cumulative Poisson fit is fragile: the calendar fit ended with maximum
gradient 1.54 and the inferred terminal retention varied sharply by clock. The
cumulative NB fit was more stable on the compressed clock, but remained weak on
the calendar clock. This is evidence that repeated dependent cumulative levels,
zero-filled off-season cells, and tail assumptions can strongly influence a
naive composite fit. It is not evidence that the terminal retained fraction is
identified.

## Six-month terminal horizon (`H = 26` model periods)

A three-origin check at `2025-01-02`, `2025-03-29`, and `2025-05-31` scores 15
recent cohorts per clock:

| clock | reconstruction | mean WIS | median MAE | 90% coverage |
|---|---:|---:|---:|---:|
| calendar | empirical multiplier | 15.9 | 15.9 | 0.13 |
| calendar | cumulative Poisson, anchored | 24.8 | 29.0 | 0.27 |
| calendar | cumulative Poisson, direct | 35.3 | 43.2 | 0.73 |
| calendar | cumulative NB, direct | 44.9 | 49.5 | 1.00 |
| calendar | hurdle--ZTNB, anchored | **9.79** | **15.0** | 0.80 |
| compressed | empirical multiplier | 15.8 | 15.8 | 0.13 |
| compressed | cumulative Poisson, anchored | 93.0 | 129.0 | 0.67 |
| compressed | cumulative Poisson, direct | 92.6 | 107.3 | 0.20 |
| compressed | cumulative NB, direct | 30.2 | 47.7 | 1.00 |
| compressed | hurdle--ZTNB, anchored | **13.2** | **9.7** | 1.00 |

All six hurdle fits converged; maximum gradients were below 0.021. This small
check favors the anchored hurdle reconstruction over the empirical multiplier,
especially in point error, while the cumulative-level fits remain sensitive to
the clock and observation family.

`H` means calendar weeks in the calendar arm and active publication steps in
the compressed arm. Those are deliberately different scientific clocks; the
choice of whether a settlement assumption should instead be imposed in actual
calendar time for both arms remains a design decision.

## Mechanical checks

`smoke_identifiability.R` passes for all nine epidemic-process by delay-family
combinations for both primary likelihoods. It verifies:

- as-of trimming on original dates;
- finite RTMB objective and gradient;
- `sum_{e<=d}(alpha_e-omega_e) = q_C(d)`;
- numerical inversion from a requested ZTNB mean to the parent NB mean; and
- Monte Carlo preservation of `E[Delta] = alpha-omega`.

The next meaningful evaluation is a multi-origin, multi-state backtest with
parameter uncertainty and cohort-clustered calibration. The current predictive
draws condition on point estimates.
