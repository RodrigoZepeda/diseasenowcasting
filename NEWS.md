# diseasenowcasting (development version)

## Breaking: the default negative-binomial overdispersion prior is now on the right scale

`nb_likelihood()`'s `phi` is the dispersion, the reciprocal of the NB size:
counts with mean `mu` have variance `mu + phi * mu^2`. The objective, the
predictive draws and `coef()` have always used it that way. The default prior
`lognormal_prior(log(20), 0.5)` came from diseasenowcast2 (Stan), where the same
numbers were a prior on the size. Applied to `1 / size`, it centred the size on
0.05, which is extreme overdispersion.

The default is now `lognormal_prior(log(0.1), 1.5)`: a median size of 10, with a
95% prior interval for the size of about 0.5 to 190.

* **Where it matters.** On long series the data dominate this prior. Fitted to
  30 days of simulated counts with size 20, the old default estimated size 1.7.
  On the benchmark's 50 dates per disease, fitting only the last 40 weeks or
  days cut the mean WIS of the nine models by 12% (COVID-19), 18% (dengue) and
  22% (mpox). The old default's 90% intervals covered 97-100% of truths there;
  the new one's cover 79-89%.
* **Full histories.** The benchmark tables were re-run with the new default.
  WIS changes by -1.5% on dengue, -14% on mpox and +1.7% on COVID-19. The 90%
  coverage of COVID-19, already below nominal, falls from 0.82 to 0.78: the old
  prior's wide intervals had partly hidden it.
* **Custom priors.** A `phi` prior you pass is used unchanged, and it has always
  been a prior on the dispersion. Some earlier documentation described `phi`
  as a precision: the `nb_likelihood()` example, the "Understanding priors"
  vignette, and the prior-sensitivity labels. If you followed that advice and
  chose a *larger* `phi` for wider intervals, you got narrower ones, and vice
  versa. The documentation now states the parameterisation.
* To keep the old behaviour, pass `nb_likelihood(phi = lognormal_prior(log(20), 0.5))`.

## Bug fixes

* The anchored-prior fallback of the two-stage fit replaced the model's `phi`
  prior with `lognormal_prior(log(20), 0.5)`: it read `priors$phi_nb_prior`,
  which nothing set. It now keeps the caller's prior.
* The NB dispersion starts the optimiser at the median of a log-normal `phi`
  prior instead of always at `phi = 20`.
* An empty `phi` slot (`nb_likelihood(phi = numeric(0))`) now falls back to the
  `nb_likelihood()` default instead of `exponential_prior(1)`.
* `nowcast_twostage()` had its own `phi = lognormal_prior(log(20), 0.5)`
  argument that overrode the model's `phi` prior. It now defaults to `NULL`,
  which uses the likelihood's prior.


## Hurdle count-cumulative models can condition on the first published level

`cumulative_process(initial_report = "offset")` gives each event a
Gamma(`initial_size`, `initial_size`) effect `Xi_t` shared by all its updates,
as the NB-Skellam model in the diseasenowcastingML experiments does. `C_t(0)`
is Poisson with mean `Xi_t mu_t q_C(0)`, so its delay-0 term is
NB(`mu_t q_C(0)`, `initial_size`) instead of a hurdle update. The later hurdle
updates use `mu_t E[Xi_t | C_t(0)]`: small `initial_size` makes
`C_t(0) / q_C(0)` an offset, large `initial_size` recovers `mu_t`. Nowcasts
draw `Xi_t` from its posterior given `C_t(0)`; forecasts draw it from its
prior. `E[C_t(H)] = mu_t q_C(H)` holds exactly. `initial_size` is fixed at 100
by default: estimated, it fell to about one on short series, with the effect
absorbing the epidemic and the AR(1) trend flat. The default remains
`initial_report = "hurdle"`.

On FluSight California (ZTNB, AR(1), 10 origins from 2023-11-18 to 2024-03-23,
horizons -1 to 2), mean WIS fell from 114.7 (`"hurdle"`) to 77.0, and 90%
coverage rose from 0.83 to 0.93. The largest gains are in forecasts: the
hurdle model's delay-0 movement probability put about a quarter of its
forecast mass at zero, which the NB delay-0 term does not.

## Count-cumulative events older than the first report date

`complete_zeroes()` filled the ages before a stream's first report date with
zero. An event published for the first time at age 3 (FluSight's first
three weeks from September 2023) then looked like three zero levels followed by
a jump. Those cells are now unobserved, and hurdle likelihoods only use a
signed update when both of its levels are observed. The level model changes
too: with the zeros, it learned a long delay tail that partly matched late
batch revisions in these data. Without them its nowcasts follow the reporting
observed at each origin (on the FluSight backtest above, its h = -1 WIS went
from 6.4 to 24.5).

## Hurdle count-cumulative fits keep `lambda` on the level scale

The signed hurdle likelihoods preserve `E[C_t(H)] = lambda_t q_C(H)`, but their
fitted `lambda` could still collapse to one flat value at the season average.
On FluSight California (weekly, `now = 2024-01-27`) the `hurdle_ztnb` fit put
every week at `lambda = 321` while published levels peaked at 1,750, and AR(1)
innovation SD sat at its floor. Nowcasts hid this because they anchor on the
latest published level; simulations of new events from zero did not.

Three separate causes, each now fixed:

1. **The log-mean ceiling bound every count-cumulative fit.** The softplus cap
   keeps `B x / (B + x)` of the latent mean, and `B` was the largest published
   count. A count-cumulative `lambda` lives on that same scale, so the cap
   halved the peak and made a flat trajectory the optimiser's basin: data
   simulated from the ZTNB hurdle observation law collapsed in 8 of 8 fits.
   The 2.5.0 widening of the ceiling by `log(100)` for every fit removes this.
2. **One ZTNB size served two different quantities.** Delay-0 magnitudes (the
   initial report) are nearly Poisson around their mean, while later revisions
   are heavy-tailed relative to theirs. The shared size was pulled to about 0.6,
   which left the initial reports too little information to move `lambda_t`,
   so the AR(1) prior flattened it. On FluSight the flat trajectory was then
   the genuine maximum. `cumulative_process()` gains `revision_magnitude_size`
   (delays `1:H`); `magnitude_size` now applies to delay 0 only. Nowcast and
   `forecast()` draws use the delay-0 size for a first report and the revision
   size after it.
3. **Every cold rung started inside the flat basin.** Hurdle AR(1) fits now add
   one rung that starts on the log of each event's latest published level; MAP
   selection keeps it only when its objective is lower. On FluSight it is
   (423.6 vs 439.2).

Remaining limitation: the movement probability at delay 0 is
`plogis(movement_intercept)` and is shared with the revision regression. On
FluSight it fits about 0.73 although every week publishes an initial report.
The magnitude mean is `total / movement_probability`, so fitted
`lambda q_C(H)` sits about 25% below the published levels there. Anchored
nowcasts are unaffected; anything simulating new events from zero inherits a
27% chance of no initial report.

# 2.5.0

## `forecast()`: carrying a nowcast past `now`

`forecast(fit, h = 1)` extends a fitted nowcast `h` event times past `now`
without refitting. After `now` nothing has been reported, so those event times
contribute nothing to the likelihood and the posterior of the epidemic there is
the fitted process's own transition from its posterior at `now`. The forecast
therefore reuses the nowcast's Laplace draws, runs each latent recursion `h`
more steps with fresh standard-normal innovations, and applies the same
observation model. The nowcast and forecast are one joint draw.

* Every recursive epidemic process forecasts: `ar1_epidemic()`,
  `arima_epidemic()`, `ets_epidemic()`, `sts_epidemic()`, the random walks,
  `theta_epidemic()` and `sir_epidemic()`. `hsgp_epidemic()` extrapolates up
  to the edge of its domain and refuses a longer horizon; a `custom_epidemic()`
  cannot be extended.
* With a report-level `revision_process()`, `category` selects the
  `"overall"`, `"confirmed"`, `"retracted"` or `"pending"` reports by their
  eventual status. Gross reports are one negative-binomial cloud and the
  genuine share is binomial in `p`, so the categories add up draw by draw.
* Count-cumulative fits forecast the settled `C_t(H)` by running the fitted
  signed updates from an empty level. The forecast is only as good as the scale
  of the fitted latent incidence: the level composite
  (`observation = "cumulative"`) ties it to the published levels, whereas the
  hurdle observations can fit it well below them.
* Temporal effects are recomputed for the new dates; other event covariates
  are supplied through `new_data`.
* The result is a `tbl_nowcast` with a `.horizon` column (`0` at `now`,
  negative for the nowcast, `1..h` for the forecast), so `autoplot()`, `tidy()`
  and scoring work unchanged. `generics` moves to `Imports` for the generic
  (it was already installed as a `dplyr` dependency).
* `.joint_reconstruct()` now builds the epidemic log-mean through
  `.reconstruct_log_mean()`, which takes the horizon; the fitted event times are
  unchanged.
* New vignette: `vignette("Forecasting")`.

## Model components construct their S7 parent explicitly

The development version of S7 requires `new_object()` to receive an instance of
the parent class. Likelihood, delay and epidemic constructors now build that
instance instead of passing a bare `S7_object()`, which works with both the
CRAN and the development S7.

## A joint fit takes the Newton step its own adequacy check has priced

`.joint_fit_diagnostic()` judges a fit on `quadratic_gap = 0.5 r' H_FF^-1 r`,
which is exactly the objective decrease a Newton step on the free subspace
would buy. Where that gap was the only thing failing a fit, and the Hessian it
was computed from is positive definite, the optimizer has not failed and the
curvature is not suspect -- it has stopped just short of a step it can compute.
`fit()` now takes that step instead of reporting the shortfall.

It matters on long series. At 985 event-times `sts_epidemic("semilocal")`
carries 2T latent innovations, and the quasi-Newton leaves roughly a thousand
of them about 0.05 from the mode against curvature of order 30. No single
coordinate is badly off -- the sum is, at 0.019 against a tolerance of 0.01, on
an objective of 63,405. A five-vector L-BFGS-B cannot find the direction that
fixes a thousand coordinates at once; one Newton step does. On dengue at
`2008-11-10` this turns a `warning` into a `pass`, with the gap going from
0.019 to 9.9e-05 and no Laplace regularization, and `ets_epidemic()` gains its
sixth adequate init rung (gap 3.2e-04 to 1.7e-05).

**The tempting version of this is wrong and makes things worse.** A Newton step
is a local model, and on a problem with a condition number of 1e6 to 1e7 a full
step can lower the objective and still land where the Hessian is indefinite. A
line search that accepts on objective alone takes that trade: across the three
STS-with-slope cells at 985 event-times it fixed one and broke two, costing the
Laplace precision diagonal ridges of 4.7 and 478.5 -- and the precision matrix
is what the posterior draws are sampled from. Three things prevent it:

* the refinement discards any step whose landing point it did not certify with
  a successful Cholesky, so it never returns a point worse in that sense than
  the one it stepped from;
* the refined point is a CANDIDATE, kept only where a re-run diagnostic is
  positive definite and either adequate or strictly lower-gap;
* `last.par.best` is restored on every decline. RTMB records it whenever
  `fn()` sees a lower objective, and `.nowcast_draws()` samples the Laplace
  posterior at that vector -- so a line search that evaluates a
  better-but-rejected candidate leaves the DRAWS on a point the fit discarded.
  This is invisible in every optimizer statistic: the fit came back
  bit-identical to its pre-refinement self and still took a ridge of 4.711.

**It is not a blanket cure for long-series STS.** Where no step can be
certified the refinement declines and the fit is returned exactly as before,
and on the one-stage STS-with-slope cells at 985 event-times that is four
cases out of five: lognormal (gap 0.0165), gengamma (0.0273), `dengue_strata`
(0.0553) and `local_linear` (0.1094) all step onto indefinite curvature on
every one of their six init rungs. Those fits still need
`type = "two_stage"`, `trend = "local_level"`, or `hsgp_epidemic()`. A fit that already passed pays nothing
and is unchanged to the last digit -- the gate declines with
`already_adequate` before computing anything.

`fit()` carries the outcome as `$refinement` alongside `$polish`, and
`attempt_diagnostics` gains `newton_steps`, `newton_objective_change` and
`newton_reason`.


## `fit_check()` reports when an ARMA is only weakly identified

An `arima_epidemic(p, d, q)` with `p >= 1` and `q >= 1` can sit on a flat ridge
that trades an AR coefficient against an MA one. The likelihood barely moves
along it, so the fit converges and the Hessian stays positive definite -- the
flatness is a 2x2 block, not a global near-singularity, which is why
`hessian_positive_definite` never caught it. What it costs is the predictive
interval. At 985 event-times an ARIMA(1,1,1) returned 90% bands of 175x to 691x
the settled count against about 7x for the matching ARIMA(2,1,0), while
reporting a converged fit.

`fit_check()` now carries `arma_ridge_correlation` -- the largest absolute
correlation between an AR and an MA coordinate in the Laplace covariance -- and
`arma_ridge`, and both `nowcast()` and `fit_check()` warn above 0.6. Like the
`log_mean` cap this stays out of `optimizer_adequate` and only sets
`fit_status == "warning"`: the optimizer has arrived, at a mode that happens to
sit on a ridge, and the two failures want different remedies.

The threshold is calibrated over 65 fits against the ratio of the ARIMA(p,d,q)
band to the ARIMA(p+q,d,0) band on the same cell. The two populations separate
cleanly -- median correlation 0.198 where the band ratio is at most 5x, 0.834
where it exceeds it -- and every cut in `[0.5, 0.7]` flags the same 24 fits with
no false alarms.

**It detects near-collinearity only.** The six blow-ups it does not flag are all
`q = 2`, with correlations of 0.16-0.26 and objectives 8.6 to 37.0 nll units
better than their reference: that is overfitting, a different failure. A silent
check is not a guarantee that the interval is sound, and the warning says so.

The documentation for `arima_epidemic()` has been corrected accordingly. It
previously said an ARMA(1,1) "sits close to a common factor, where `ar` and `ma`
nearly cancel". The fitted `ar + ma` is in fact nowhere near zero; it is the
curvature, not the point estimate, that shows the cancellation.

## The `log_mean` cap warning is suppressed for count-cumulative fits

On a count-cumulative stream the horizon-0 nowcast is built by the cohort kernels
from the observed cumulative rather than from `lambda`, so a saturated cap has no
predictive consequence there. Lifting the bound from about 12 to 20 on flusight
moved the median by 0.5% and -0.2% and the objective by noise, while `lambda`
peaked in the interior of the series, nowhere near the event-time being scored.
Left reportable the warning fired on 52-65% of those fits and would have taught
callers to ignore something that matters a great deal on the count-incidence
path.

`fit_check()` gains `log_mean_cap_reportable`, which is `FALSE` for such fits and
is the column to act on. `log_mean_cap_bound` still records the fact, and the
warning is unchanged on count-incidence streams.

## The cap on the latent incidence no longer truncates the nowcast

The objective caps the latent `log_mean` with a softplus so a bad optimiser step
cannot overflow `exp()`. The ceiling was
`min(max(6, log1p(casemax)), 16)`, where `casemax` is the largest count
**reported so far**. But the latent incidence exceeds the reported count by
`1/Gstar`, the reciprocal of the reporting fraction -- which is precisely the
quantity a nowcast exists to estimate. On a stream that is growing while only a
percent or two has arrived, the ceiling sat *below* the answer and the fit
pinned to it.

On the `covid_us` case series the ceiling was below the settled truth at six of
seven as-of dates in March-April 2020 (ceiling over truth 0.14 to 0.54) and the
median nowcast came out at 8.7% of the settled count. Because the cap is applied
downstream of every epidemic process, all nine were biased identically -- which
is why the effect looked like a property of the data rather than of the engine.

The bound is now `min(max(6, log1p(casemax)) + log(100), 16)`. The `log(100)`
admits a hundredfold reporting inflation, which covers what early-2020
`covid_us` needs; `exp(16)` (about 8.9 million) remains the hard overflow stop,
which was always the guard's actual job. `prepare_data()` and, through `...`,
`nowcast()` accept `mu_log_upper_bound = ` to set it explicitly.

The failure was silent, in the same way the `iter.max = 500` one below was, so
it is now audible. `fit_check()` reports `log_mean_upper_bound`,
`max_log_mean`, `log_mean_headroom` and `log_mean_cap_bound`, and `nowcast()`
warns whenever the fitted `log_mean` comes within three log units of the
ceiling. Three is where the distortion stops being negligible: the softplus
keeps exactly `plogis(bound - log_mean)` of `lambda`, which is 95.3% three units
down, 88.1% two units down and 50% at the bound, and once it saturates the
gradient vanishes, so the trend is unidentified above the ceiling and `lambda`
goes flat.

The check reads the **uncapped** `log_mean`. A cap-bound fit is reported as
`fit_status == "warning"` while `optimizer_adequate` stays `TRUE`: the optimiser
has converged, to a ceiling, and the two failures want different remedies.

## `reporting_fraction()`: the multiplier a nowcast is applying

Every nowcast is at bottom one number per event-time -- the fraction of that
cohort that has arrived by the as-of date. The engine has always computed it as
`Gstar`, but nothing exposed it. `reporting_fraction()` now returns it per
(event-time, stratum) together with `inflation = 1 / reporting_fraction`, which
is exactly the multiplier applied to the observed count, and with the range over
retained fits -- under `type = "two_stage"` that spread is the delay uncertainty
the cascade propagates.

It is worth reading because it is where most nowcast level errors live and it
does not appear in the predictive summary. On `covid_colombia` at
`now = 2020-08-01`, every configuration tried -- three epidemic processes, one-
and two-stage, and a susceptible-pool sweep spanning four orders of magnitude --
put the horizon-0 inflation at 132-208, while the settled truth for that cohort
was 9,171 against 309 reported, an inflation of 29.7. No epidemic process can be
right when it is handed a reporting fraction five times too small.

**It deliberately does not warn.** A very large inflation on a young cohort is
correct, not suspicious: early-2020 `covid_us` genuinely needs 37-83x. There is
no threshold that is universally wrong, so the number is reported and the
judgement is left to the caller.

## `hsgp_epidemic()` warns when the basis outnumbers the series

The basis count sets the shortest wavelength the trend can resolve, and the
place an over-flexible basis bends is the right-hand edge of the series, where
reporting is least complete and the likelihood constrains it least. On mpox at
`now = 2022-08-09` — 33 event-times, settled truth 64 — `num_basis = 20` gives a
median of 1,540 with a 90% band of [180, 13,373], where the automatic count of
12 gives 186 with [19, 1,913] and covers.

`prepare_data()` now warns when an explicitly supplied `num_basis` exceeds half
the event-times. An automatic count never warns: the ladder's floor of 12 is
itself more than half of a 20-step series, so checking it would fire on the
package's own default path -- eleven times across this package's own test suite,
none of them a choice anyone made. Short series are already handled better by
`auto_nowcast()`, which keeps HSGP out of its candidate grid below
`min_hsgp = 30`. What is actionable is a number the caller supplied, typically
one carried over from a longer series: the `num_basis = 20L` recommended for
COVID-length daily data is exactly the value that breaks a 33-day one. The
warning is skipped for `delay_only` Stage-1 fits, which build no epidemic
process.

This was previously invisible because the `log_mean` ceiling clipped the runaway
into something plausible-looking.

## Classical time-series epidemic processes

The epidemic-process menu gains five constructors beyond HSGP, AR(1) and SIR:
`arima_epidemic(p, d, q)`, `sts_epidemic(trend = "semilocal")`,
`ets_epidemic(trend, damped)`, `random_walk_epidemic()` / `naive_epidemic()`
and `theta_epidemic()`. Each supplies the latent trend in
`log_mean[t, s] = mu_intercept[s] + (X gamma)[t, s] + trend[s](t)`, the same slot
HSGP and AR(1) fill, so `tbl_now` covariates and temporal effects apply to them
unchanged and every trend coefficient is estimated per stratum.

Seasonality stays on the covariate path rather than in the state vector: there
is no SARIMA `(P, D, Q)_s` and no Holt-Winters seasonal block.
`temporal_effects(day_of_week = TRUE)` contributes reference-coded weekday
dummies and `temporal_effects(seasons = )` contributes Fourier pairs, which cost
a handful of coefficients where a seasonal state would cost `s` latent states
per stratum -- the difference between a fit that converges on these series and
one that does not.

ARIMA is parameterised by the partial autocorrelations of its AR and MA
polynomials and mapped to coefficients through Levinson-Durbin, so stationarity
and invertibility hold by construction and the optimiser cannot step into a
region where the recursion explodes. A drift is included by default once
`d >= 1` and is refused at `d = 0`, where the ARMA mean and `mu_intercept` are
the same quantity.

Exponential smoothing is written with `sigma` as the level innovation SD and
`beta` as Hyndman's `beta* = beta / alpha`, because the classical
`(alpha, beta, sigma)` triple is identified only up to a common rescaling once
the innovation is latent rather than observed. For the same reason
`ets_epidemic(trend = "none")` is documented as being the random walk, and
`theta_epidemic()` as the random walk with drift: under a count likelihood the
exponential smoothing of the classical methods is what the Kalman filter for a
local level model already performs.

## The optimiser's iteration budget scales with the problem

`fit()` hard-coded `iter.max = 500` regardless of how many parameters the tape
had. The joint fit optimises one latent innovation per event-time, so that count
is a property of the data: 28 for an HSGP on any series, 1,623 for a structural
time series on a 1,095-week one. Five hundred iterations is generous for the
first and nowhere near enough for the second.

The failure was silent. `nlminb` returns code 1, the fit is kept, and the only
trace is a `fit_check()` warning about a non-positive-definite Hessian -- never
an error, so a caller who did not inspect the diagnostics got a fit that had
stopped short of a mode. On the package's 1,095-week dengue series this affected
six of the nine epidemic processes: `ets_epidemic()` passed `fit_check()` on 15%
of fits and `sts_epidemic()` on 39%. At 500 iterations ETS stopped with a maximum
gradient of 6.6 and `theta_epidemic()` with 107.

`fit(control = NULL)`, the new default, sizes the budget from the tape:
`iter.max = max(500, min(50000, 25 * n_parameters))`. All nine processes now pass
`fit_check()` on dengue with a positive-definite Hessian, ETS and Theta reaching
maximum gradients of 0.32 and 0.036, and ETS finding a better mode than it
previously stopped at (objective 53061.5 against 53049.0). The L-BFGS-B polish
step scales the same way. A supplied `control` list is still used verbatim.

Raising a cap costs nothing when it does not bind, which is what makes this
safe: short series converge in the same number of steps and return a
bit-identical objective. Long series pay for the iterations they were previously
skipping -- a structural trend on 1,095 weeks now takes around five minutes
rather than ninety seconds, and converges.

This also revises the note added with the time-series processes above: they do
not have an inherent scaling limit, they had an optimiser budget that did not
scale. They remain the expensive choice on a long series, where `hsgp_epidemic()`
keeps a fixed-size basis.

## Fixed parameters are honoured instead of discarded

A number in a parameter slot has always meant "hold this here" on the delay
side. For the epidemic-process and likelihood parameters it meant nothing at
all: `build_joint_obj()` read the prior entry's `$dist` and never its
`$is_constant` or `$fixed`, so a supplied number was replaced by a
standard-normal prior and the parameter was estimated anyway. Nine slots were
affected -- `nb_likelihood(mu =, phi =)`, `hsgp_epidemic(alpha =, ell =)`,
`ar1_epidemic(phi =, sigma =)` and `sir_epidemic(R0 =, gamma =, N_eff =)` --
including two that the roxygen examples advertised as working.

They are now held the way the delay parameters are: the parameter is seeded at
the unconstrained value that maps to the supplied one, mapped out of the
optimisation, and its prior and Jacobian terms are skipped. The same applies to
every parameter of the new time-series processes, which until now refused a
fixed value outright because there was no machinery to honour one. Stratified
fits take either a single shared value or one per stratum.

`coef()` reports a held parameter; `parameters()` does not, because it reports
estimates with credible intervals and a held parameter has none.

A held value outside its domain is now refused where it can be seen. Domains
that are a property of the parameter -- an autocorrelation in (-1, 1), a
probability in (0, 1), a scale above zero -- are checked by the constructor.
`sigma` is bounded by the engine's `ar_sigma_max`, which the constructor cannot
know, so it is checked during the fit and rethrown past the initialisation
ladder: no retry rescues a value outside its domain, and burying it under
"failed to converge for all init attempts" hid the one message that said what
to change.

Fixing `mu` under `strata_pooling = "hierarchical"` is an error. The pooling is
a model for the intercept, so pinning the intercept leaves it nothing to do.

Fits that pin nothing are unaffected: the joint objective and every
reconstruction are unchanged to the last digit.

## Classical time-series epidemic processes (continued)

`arima_epidemic()` defaults to order `(2, 1, 0)`. An MA term remains available
but is not the default: on a latent trend an ARMA(1, 1) sits close to a common
factor, where the AR and MA polynomials nearly cancel and neither is identified.
Across a 2,376-fit backtest on nine dataset variants, `(1, 1, 1)` was the worst
of the nine processes compared, with intervals 2.4 times the settled count and
15 of the 21 over-wide fits recorded in the whole grid; `(2, 1, 0)` converged
more often, scored better and produced two.

These trends carry one latent innovation per event-time, so the Laplace
Hessian grows with the series and they do not scale the way HSGP does. On the
1,095-week dengue series `fit_check()` pass rates fall to 15% (ETS), 39% (STS)
and 71% (random walk), against 96-100% for all of them on the other seven
datasets; `ar1_epidemic()` shows the same pattern more mildly at 86%, and
`hsgp_epidemic()` is unaffected at 100%. Past roughly 500 event-times, HSGP
remains the right choice. This is documented under "Long series" in
`?timeseries_epidemic`.

Unlike the older constructors, these refuse a fixed numeric in a parameter slot.
`arima_epidemic(sigma = 0.1)` is an error rather than a value that is silently
estimated anyway.

The trend recursions and their constraint maps have a single implementation that
serves both the RTMB tape and the plain-R reconstruction `predict()` runs on, so
the two cannot disagree about the model that was fitted.

# 2.4.1

## Reporting-delay hazard: correct tails, pinned designs

The discrete-hazard regressions now read their baseline hazard off the delay
law's **log-survival** instead of differencing its CDF. Differencing lost every
bin past the point where `F` saturates to 1 in double precision -- delay 16 for a
LogNormal with mean 3 and SD 0.6 -- and replaced the true tail hazards with a
floor, so a report at delay 40 cost 42 log-units less than it should. The effect
was worst exactly where reporting is fast and an occasional report is very late.
Zero coefficients now reproduce the stationary likelihood over the whole
parameter space rather than only where the CDF had not saturated.

Design matrices for the delay and revision roles are built once and pinned as a
schema on the fitted object. Reference levels come from a factor's declared
`levels()`, so a level that has not appeared yet keeps its slot instead of
renumbering every other coefficient at the next as-of date; a character column
warns that its reference level is not pinned. Constant, duplicate and exactly
collinear columns are dropped before fitting, by name, instead of surfacing as a
singular Hessian. `update()` replays the fitted schema; `backtest()` deliberately
rebuilds it per date, since replaying a later one would leak.

`surprise()` scores a delay against the law its own cohort faces when the fit
carries a reporting regression -- pass an `event_index` column, which `update()`
now does automatically. Without one it warns rather than silently comparing
against the untilted baseline.

The two-stage cascade now carries a reporting regression, instead of silently
falling back to a single joint fit. Stage 1 fits the delay *and* its hazard
coefficients as one censored regression on the recent window, with the calendar
and cohort designs sliced to that window; the whole parameter vector is drawn
from the Stage-1 joint Laplace; and Stage 2 hard-fixes all of it, so the delay
observations are read exactly once across the two stages.

The stationary imputation is deliberately untouched: it keeps its independent
normals on `(delay_mu, delay_sigma)` and the tuned `floor_mu` / `floor_sig_frac`
spreads, because the convergence behaviour on real data was tuned against them.
Only a model with `P_delay > 0` takes the joint draw, where the floors survive as
a minimum marginal spread applied without disturbing the correlation structure.
Drawing on the unconstrained scale also retires the `pmax(0.05, .)` truncation
that the natural-scale sigma draws needed.

Tape construction for regression models is several times faster than in the first
cut, but still grows roughly quadratically in the number of event times.

## Covariates can target event, reporting-delay, or revision processes

`as_event_covariates()`, `as_delay_covariates()`, and
`as_revision_covariates()` now attach process-role S3 classes to vectors (or
selected columns of a data frame / `tbl_now`). Untagged `tbl.now` covariates
remain event covariates. Report- and revision-date temporal effects and tagged
covariates are fitted as discrete-hazard regressions; they no longer enter the
epidemic mean. Coefficients are reported as `event_beta`, `delay_beta`, and
`revision_beta`, with hazard odds-ratio interpretations for the latter two.
Categorical temporal effects use reference-level contrasts, and tagged factor
strata remain compatible with `tbl.now` grid-completion joins.

**This changes existing daily fits.** Day-of-week previously entered the epidemic
mean as a single column of weekday numbers, which imposed an artificial linear
trend across the week; it is now six reference-level dummies. Since
`temporal_effects = "auto"` is the default, nowcasts, `P`, the shape of `gamma`
and saved warm starts all change for daily data. Earlier fits are not comparable
with this release.

The same rule now applies to user covariates in every role, which it previously
did not: an **unordered** factor becomes reference-level dummies, an **ordered**
factor keeps a single ordinal score, and a numeric column is used as it stands.
A factor event covariate used to be flattened to its integer codes, which
asserted that its levels were equally spaced and in alphabetical order. Dummies
are built by comparison rather than through `model.matrix()`, so the encoding no
longer depends on the session's `options("contrasts")`. An event covariate with
no value at some event time is still filled with zero, but now says so: for a
factor that means the reference level, which the data did not state.

This release removes three things `diseasenowcasting` was duplicating from
`tbl.now`. All three were invisible in normal use and none change results.

## `covid_colombia` moved to tbl.now

The `covid_colombia` dataset is gone from this package; it now lives in
`tbl.now`, alongside the other example datasets (`denguedat`, `mpoxdat`,
`flusight`, ...). The two copies were byte-identical, and shipping the same
35,501-row data frame from two packages that are always attached together only
made `?covid_colombia` and `data(covid_colombia)` ambiguous.

Nothing changes for users: `diseasenowcasting` depends on `tbl.now`, so
`library(diseasenowcasting)` still puts `covid_colombia` on the search path.
Code that qualified the name as `diseasenowcasting::covid_colombia` must now
say `tbl.now::covid_colombia`.

`LazyData` was dropped from `DESCRIPTION` along with the now-empty `data/`
directory.

## `?revision_delay` pointed at the wrong package

`diseasenowcasting` and `tbl.now` both documented a help topic named
`revision_delay`, meaning different things: the revision-lag *distributions*
here (`lognormal_revision()`, `dirichlet_revision()`, ...) and the
confirmed-vs-retracted *diagnostic* there (`diagnose_revision_delay()`,
`plot_revision_delay()`). With both packages attached -- which is always, since
one depends on the other -- `?revision_delay` prompted for a disambiguation and
then resolved to `tbl.now`, so a user who had just called `lognormal_revision()`
was shown the wrong page.

This package's topic is now `revision_distributions`; `?revision_delay`
unambiguously means `tbl.now`'s. No function was renamed, and
`?lognormal_revision` and its siblings still land on the right page. Only a
literal `?revision_delay` or a `[revision_delay]` doc link needs updating.

## `dn_palette()` no longer keeps its own copy of the colours

All eight `dn_palette()` colours were the `tbl.now::tbl_now_palette()` defaults
hard-coded a second time under different role names (`reported` for `epidemic`,
`accent` for `reporting`, and so on). Nowcast plots are routinely drawn beside
`tbl_now` plots in one document, so the copy would have stopped matching the
moment `tbl.now` retuned a colour -- silently, with no error to notice.

`dn_palette()` now reads `tbl.now::tbl_now_palette()` and renames the roles.
The returned values, names, order and `n` behaviour are unchanged, so plots
render identically. `theme_diseasenowcasting()` and the `autoplot()` bar
colours, which had their own hard-coded copies of the same two hexes, now go
through `dn_palette()` as well; the `color` argument still accepts any colour.

# 2.4.0

## Documentation: vignettes split into CRAN vignettes and website articles

Building the vignettes took 13.4 minutes, almost all of it re-fitting models.
Five of them are now pkgdown-only **articles** under `vignettes/articles/`
(`Benchmark`, `Handling_Outlier_Delays_with_Censoring`, `LLM_Usage`,
`Mathematics`, `Revision_processes`); they remain on the package website but no
longer ship in the tarball or run during `R CMD check`. Links to them from the
remaining vignettes now point at the website.

The four vignettes that still ship (`introduction`, `Understanding_Priors`,
`Nowcasting_at_the_start_of_an_Epidemic`, `Custom_delays_and_processes`) run the
same code on smaller inputs: `auto_nowcast()` in `introduction` selects over the
dengue data up to 1992 with three backtest dates instead of ten dates on data up
to 1994, its mpox backtest uses three dates instead of five, and
`Nowcasting_at_the_start_of_an_Epidemic` fits four observation windows instead of
seven.

The `Benchmark` article now shows the four tables per disease that its text
promises. Dengue previously collapsed the NobBS, epinowcast and baselinenowcast
views into a single de-duplicated table, the `diseasenowcasting`-only view was
never rendered for any disease, and the dengue heading said Colombia instead of
Puerto Rico.


## Breaking: fitted nowcasts now use the common `tbl.now` result grammar

`nowcast()` and `auto_nowcast()` now return a diseasenowcasting subclass of
`tbl.now::tbl_nowcast`. Predictive draws and requested quantiles are materialised
in the public result, while the complete native model fit is retained in `@fit`.
Native operations such as `predict()`, `coef()`, `parameters()`, `update()`,
`surprise()`, and model diagnostics unwrap that fit automatically.

The same object now works directly with the common `tbl.now` workflow:
`tidy()`, `as_tibble()`, `autoplot()`, `score_nowcast()`, forecast conversion,
weighting, and ensembling need no explicit adapter. `nowcast()` gains
`quantile_levels` so direct calls and `tbl.now::run_nowcast()` preserve the same
requested quantile grid. Event axes retain their original numeric, daily, or
weekly representation, and declared strata are reconstructed from the
authoritative input data rather than from lossy display labels where possible.

The result's standard `tbl_nowcast` properties are the source of truth for the
data, event axis, strata, and analysis date. Namespaced metadata is reserved for
diseasenowcasting prediction semantics and fit diagnostics. Model type, fitting
rung, revision mode, and automatic-selection evidence remain available on the
diseasenowcasting subclass and its retained native fit.

## Breaking: backtesting and predictive scoring delegate to `tbl.now`

`backtest()` now translates one or more native `model()` specifications into
labelled `tbl.now::engine_diseasenowcasting()` specifications and returns
`tbl.now::nowcast_backtest()` directly. This replaces the package-local
backtest class and makes canonical tidying, plotting, forecast conversion,
scoring, weighting, and ensembling available immediately.

The revised interface supports canonical `horizon`, `keep_draws`, `on_error`,
`verbose`, `truth_axis`, `truth_type`, and `quantile_levels` controls. Explicit
model-list names become method labels; inferred labels describe the model
components, and duplicate labels are rejected. Default truth semantics match
the fitted estimand: confirmed cases for confirmation/both revision modes,
still-standing cases for retraction-only mode, and reported totals otherwise.
Automatic backtest dates for count-cumulative models respect their settlement
horizon.

The exported package-local `score()` has been removed. Predictive evaluation
belongs to `tbl.now::score_nowcast()` and `scoringutils`; the new `fit_check()`
reports only RTMB optimizer and Laplace diagnostics.

## Optimizer and Laplace diagnostics

Joint fits now use one structured adequacy predicate throughout low-level
fitting, the two-stage collector, final warnings, prediction metadata, and
`fit_check()`. It requires a finite objective and derivatives, a successful
optimizer path, the box-constrained KKT conditions, positive-definite curvature
on the locally free subspace, finite reconstructed incidence, and a
curvature-scaled estimate of remaining objective improvement
`0.5 * r' H^(-1) r <= 0.01`. The raw maximum gradient remains available for
debugging but is no longer treated as a parameterization-independent
convergence criterion.

L-BFGS-B refinement now optimizes the centered objective
`Q(theta) - Q(theta_start)`. Its stopping and acceptance rules therefore do not
depend on an inferentially irrelevant additive constant in the negative log
posterior. A successful base `nlminb` solve is not invalidated by a later
line-search termination after an accepted improvement, and a successful polish
can rehabilitate the optimizer path. Both solver codes, centered objective
change, acceptance tolerance, and before/after gradients are retained for
auditability.

Cold initialization attempts are first classified by the common adequacy
predicate; the adequate candidate with the lowest negative log posterior is
then selected. If none is adequate, the lowest-objective candidate is retained
only as an explicitly degraded fit or internal initializer. This replaces
selection by the smallest unscaled gradient.

Internal warm, Stage-1, and discarded imputation fits no longer emit end-user
warnings. In two-stage fitting, only adequate Stage-2 fits contribute draws,
and at most one aggregate warning describes exclusions or a degraded retained
fit. `fit()` gains `warn` for controlling warnings from direct low-level joint
fits. The legacy matrix interface `nowcast_twostage()` now uses the same fitting
cascade and adequacy policy as `nowcast(type = "two_stage")`.

Laplace sampling records whether the original precision admitted a Cholesky
factorization and whether a diagonal ridge, non-finite repair, or eigenvalue
floor was required. Strictly complementary active box coordinates are held
fixed and sampling uses the certified free-coordinate precision. Any altered
precision is visible in result metadata and causes `fit_check()` to report a
warning rather than an unqualified pass.

Cross-platform numerical handling is now deterministic. Non-finite sparse
precision matrices are rejected before CHOLMOD factorization and sent directly
to the documented repair path, avoiding non-finite predictive draws on some
Linux builds. When an RTMB Laplace-marginal objective does not expose an
analytic Hessian, the adequacy check computes observed curvature by centered
finite differences of its analytic gradient; the source is recorded in the
fit diagnostic.

Two-stage results now expose an auditable `fit_diagnostics` record containing
the requested and resolved fitting type; requested, attempted, retained, and
excluded imputation counts; exclusion reasons; warm-fit use and status;
Stage-1 Hessian/Laplace status; fallback information; retained-fit diagnostics;
and Laplace-sampling regularization. The same record is available at
`@fit_diagnostics` and `@metadata$diseasenowcasting$fit_diagnostics`.

## Automatic model selection

`auto_nowcast()` keeps native model-grid construction and full-data refit
fallbacks but now evaluates its canonical backtest through `scoringutils`.
Selection defaults to pairwise relative skill for the requested metric, which
avoids rewarding a model merely because it succeeded on an easier subset of
targets. Set `relative_score = FALSE` to use the raw mean score.

Effectively tied scores are deterministic. The default
`tie_break = "epidemic_priority"` prefers HSGP, then AR(1), SIR, and custom
epidemic processes; `tie_break = "fastest"` instead prefers the smallest median
successful retrospective fit time. The unused rule and original candidate-grid
order provide secondary tie-breaks. Failed retrospective cells are skipped,
and a failed full-data winner falls through to the next-ranked candidate.

Backtest cell durations, full-data refit attempts, and total selection time are
recorded and exposed by the new `selection_timings()` function.
`comparison_scores()`, `best_score()`, and `selection_metric()` now report the
canonical scoringutils-based selection evidence.

## Updating, persistence, dependencies, and documentation

`update()` preserves the common public result while warm-starting the retained
native model through the same one- or two-stage collector. `save_nowcast()` now
accepts the public common result, and `load_nowcast()` restores that result
grammar together with its event axis, strata, quantile levels, fit diagnostics,
and automatic-selection evidence. The stored native mode and precision still
support prediction without rebuilding the RTMB tape.

The minimum `tbl.now` version is now 0.35.3. `future` is now suggested rather
than imported because parallel execution is owned by the canonical backtest
workflow; `future.apply` is no longer imported, and `generics` is suggested for
interoperability tests and workflows.

New `?diseasenowcasting_workflows` documentation explains the boundary between
native modelling/diagnostics and common cross-engine result operations. The
README, introductory material, model-selection documentation, and
outlier-censoring vignette have been updated for the common result, canonical
backtest, scoringutils relative-skill, and fit-diagnostic workflows.

# 2.3.0

## Breaking: tbl.now revision vocabulary is now the package vocabulary

The report-level post-report component is now called a revision process
everywhere. The exported API is `revision_process()`, `model(revision = )`,
`revision_delay =`, and the `*_revision()` delay aliases. Older spellings are
removed rather than deprecated.

`diseasenowcasting` now depends on `tbl.now (>= 0.35.0)` and reads the current
tbl.now revision metadata directly: `revision_date`, `revision_type`, and
`is_censored_revision`.

## Breaking: `model_parameters()` is now `parameters()`, and `tidy()` belongs to tbl.now

Version 2.1.0 moved the per-parameter table to `model_parameters()` and defined
a `tidy()` here that returned the nowcast. Both halves of that are now revised.

* `model_parameters()` is renamed **`parameters()`**. It is the same table
  (`term`, `estimate`, `std.error`, `conf.low`, `conf.high`, `type`) and the
  same `conf.level` argument; only the name changed. The old name is removed
  rather than deprecated.
* Standard errors are fixed. The interval was computed with `base::diag()` on
  the sparse solve of the precision matrix, which errors out and left
  `std.error` `NA` for **every** parameter. It now uses `Matrix::diag()`, and
  falls back to marginal SDs estimated from Laplace draws when the precision
  matrix is too large or too ill-conditioned to invert.
* **`tidy()` is no longer defined in this package.** `tbl.now (>= 0.35.0)`
  exports the shared generic and registers the `diseasenowcasting` method
  itself, so `tidy()` on a fit still returns the cross-package nowcast table
  described under 2.1.0 -- it is simply no longer our code, and we no longer
  register a method on a generic we do not own.

## Breaking: cumulative model configuration is now `cumulative_process()`

The count-cumulative model component is now configured with
`cumulative_process()` and passed as `model(cumulative = )`. Automatic
count-cumulative model selection remains inside `nowcast()`: when a
`tbl_now` has `data_type = "count-cumulative"` and no explicit cumulative
component, `nowcast()` selects the default signed hurdle--ZTNB cumulative model.

Count-cumulative data consume signed changes in the cumulative trajectory only.
Any `revision_date` metadata on such objects is treated as data provenance, not
as an individual report-level revision likelihood.

## Default model selection

`nowcast(type = "auto")` continues to select a fitting strategy automatically,
but the user-facing default remains `type = "two_stage"`. All automatic
selection for the diseasenowcasting backend lives in
`diseasenowcasting::nowcast()`; `tbl.now::run_nowcast()` passes the `tbl_now`
and engine arguments through without injecting model components.

# 2.2.0

## Breaking: the revision process is now the revision process

The optional component describing what happens to a report *after* it is filed --
a laboratory result comes back, and the report is either confirmed or retracted --
is called a **revision** process throughout, matching `tbl.now` 0.28.0. The old
spellings are gone, not deprecated.

| was | is |
|---|---|
| `revision_process()`, `resolution_process()` | `revision_process()` |
| `model(revision = )` | `model(revision = )` |
| `retract_delay = `, `resolution_delay = ` | `revision_delay = ` |
| `lognormal_retraction()` / `_confirmation()` / `_resolution()` | `lognormal_revision()` |
| `gamma_*`, `generalized_gamma_*`, `dirichlet_*` (three spellings each) | one `*_revision()` each |

Twelve lag constructors become four. The **outcome values are unchanged**: a case
is still `"confirmed"`, `"retracted"` or `"pending"`. Revision is what the
process does; confirmed is one of the things it can conclude.

## Breaking: the revision process is detected, not requested

`nowcast()` no longer takes `retraction_date`, `confirmation_date`,
`retraction_censored` or `confirmation_censored`. It reads the process off the
data instead, in the two places it can be:

* the `tbl_now` carries `revision_date` / `revision_type` (see
  `tbl.now::add_revision_date()`).

```r
# before
nowcast(data, model(), retraction_date = "retracted")

# now
data <- tbl.now::add_revision_date(data, retracted, revision_type = outcome)
nowcast(data, model())
```

Revision censoring is also read from the `tbl_now` object's
`is_censored_revision` attribute. There is no parallel column-name argument in
`nowcast()`; event, report, revision, outcome, and censoring metadata all have
one source of truth.

**The mode is inferred from the data.** `unique(revision_type)` over the
**full** data -- not the as-of view -- decides between `confirmation_only`,
`retraction_only` and `both`, so the mode is a stable property of the data source
and cannot flip between backtest dates. Assert it with
`revision_process(mode = )` when you want the check rather than the inference:
an assertion the data cannot support is an error, whereas inference with no
evidence falls back to the ordinary count model.

A revision **date with an `NA` `revision_type`** is now an error. The report
has resolved but its sign is unknown, so it cannot enter either lag law.

`backtest()` needs nothing extra either: the truth is built from the cases that
settle positive (confirmed, or never retracted), with pending cases kept --
"not resolved yet" is not "not a case".

The `nowcast_class` stores the resolved `@revision_mode` (`"none"`,
`"confirmation_only"`, `"retraction_only"`, or `"both"`). Date and censoring
column metadata remain on its `tbl_now` data.

## Documentation

`vignette("Retractions_and_confirmations")` is now
**`vignette("Revision_processes")`**, rewritten around the detected process
rather than the old column arguments. The old name is gone, like the old API.

## Breaking: `tidy()` is now `parameters()`

`tidy()` on a nowcast gives you the **nowcast** -- the predicted counts, through
`tbl.now`'s method for the broom generic. The parameter table is
`parameters()`. This package used to own the generic and answer the second
question under the first one's name; `tbl.now`'s `.onLoad()` takes it over now
that the method is gone.

```r
coef(nc)          # unchanged
parameters(nc)    # was tidy(nc)
tidy(nc)          # now tbl.now's: the nowcast itself
```

## count-cumulative data

The old fixed-`p` Skellam/SkNB implementation is replaced by a dedicated
`cumulative_process()` component. A cumulative stream identifies the
unconditional finite-age kernel `h_R`, not a biological truth probability and a
conditional revision-delay law separately. An old cumulative specification
using `revision_process()` is translated once to the collapsed kernel with a
targeted deprecation warning; the old likelihood is not used.

Three observation composites are selectable:

* `"cumulative"`: Poisson or negative-binomial cumulative-level marginals;
* `"hurdle_ztnb"`: signed updates with a structural zero and a
  zero-truncated-NB magnitude indexed by its own mean; and
* `"hurdle_ztpoisson"`: the same hurdle/sign construction with a
  zero-truncated-Poisson magnitude indexed by `(alpha + omega) / pi` and no
  magnitude dispersion.

The settlement horizon `H` is configurable and defaults to 26 model steps.
Predictions target `C_t(H)`, finite-horizon database retention, and are anchored
to the level observed at the analysis origin. Completed zeroes inside the as-of
triangle are scored; future cells are masked. The products over levels or signed
updates are composite likelihoods, and the current pseudo-posterior intervals do
not include sandwich/Godambe calibration.

## Fixed: `prior_only = TRUE` returned all-`NA` draws for cumulative data (#129)

The prior sampler now supplies the collapsed-kernel and model-specific hurdle
parameters. `hurdle_ztpoisson` deliberately supplies no magnitude dispersion.
A sampler that cannot reconstruct any draw aborts with the captured first error
instead of returning a correctly shaped but entirely missing result.

# 2.1.0

## `tidy()` now returns the nowcast, and uses the shared `generics` generic

**Breaking change.** `tidy()` on a fitted nowcast used to return one row per
estimated *parameter*. It now returns the **nowcast** — one row per event date
per stratum — matching the contract every other nowcasting engine returns, so
downstream code (plotting, scoring, cross-engine comparison via `tbl.now`) does
not have to special-case this package.

* `tidy()` is now **re-exported from `generics`** instead of being a generic
  defined here. The old package-local generic masked `generics::tidy` after
  `library(diseasenowcasting)`, which made every method other packages register
  on the shared generic invisible — including `tbl.now`'s.
* `tidy()` works on both a fitted `nowcast()` and on `predict(fit)`. On a fit it
  draws the posterior predictive first (pass `n_draws` / `seed` through `...`).
  It returns a tibble sorted by `stratum` then `event_date`, with columns
  `event_date`, `stratum` (`"all"` when unstratified), `estimate` (posterior
  **median**), `conf.low`, `conf.high`, `level` (the width the interval actually
  has) and `engine`. Event dates are reported on the model's own grid — never
  re-gridded.
* A `probs` argument appends one exact quantile column per probability, named
  `q5`, `q50`, `q95` (`probs * 100`, so `0.025` gives `q2.5`).
* Stratified fits get one block of rows per stratum, read from the per-stratum
  draws rather than the pooled ones.
* **New `model_parameters()`** returns the old `tidy()` table (`term`,
  `estimate`, `std.error`, `conf.low`, `conf.high`, `type`). Calling `tidy()` on
  a fit warns once per session and names `model_parameters()`.
* `tidy.default` is gone: registering a default method on the shared generic
  would have changed `tidy()`'s behaviour for every other package in the
  session. `model_parameters()` keeps a default method that errors clearly.

# 2.0.0

## Resolution processes: confirmation as well as retraction

A report is rarely a case outright — it is provisional, and **resolved exactly
once**: a test comes back, and it is either positive (the case is *confirmed*) or
negative (the case is *retracted*). Nothing is confirmed and then later retracted.
What differs between surveillance systems is only which resolutions get a date
column, and `diseasenowcasting` now handles all three possibilities under one
likelihood:

| | **retraction only** | **confirmation only** | **both** |
|---|---|---|---|
| dates recorded | the negatives | the positives | both signs |
| a missing date means | not retracted **yet** | not confirmed **yet** | not resolved **yet** |
| target | reports never retracted | reports eventually confirmed | reports resolving positive |
| lag support | `{1, 2, ...}` | `{0, 1, ...}` | `{0, 1, ...}` |
| argument | `retraction_date =` | `confirmation_date =` | both |

The lag support differs only because a *retraction* in the same period as its
report describes a case never visible in any data vintage, whereas a test coming
back the day it was ordered is ordinary.

* `nowcast(confirmation_date = , confirmation_censored = )` mirrors the retraction
  pair, with the same censoring, stratified-`p` and `g_C`-family support.
* The sign of the evidence flips. Under retraction the longer a report stands
  unretracted the more likely it is genuine (`rho(j)` rises); under confirmation
  the longer it sits unconfirmed the more likely it never will be (`rho(j)`
  falls). Confirmed rows enter the nowcast with certainty; retracted rows with
  probability zero.
* New lag constructors `lognormal_confirmation()`, `gamma_confirmation()`,
  `generalized_gamma_confirmation()`, `dirichlet_confirmation()`.
* **`count-incidence` data are supported for both modes**: one row per distinct
  `(event, report, resolution)` with a case count, `NA` marking the unresolved.
  Every statistic is a weighted tally, so the aggregated form and the linelist
  give bit-identical engines and log-likelihoods.
* **`count-cumulative` data are not row-level revision data.** As of 2.2.0
  they use `cumulative_process()` and the collapsed finite-age retraction
  kernel described above.
* **Both outcome values in one revision column** fit the full process: a report
  is resolved exactly once and the resolution is either positive (confirmed) or negative (retracted),
  as when a test comes back. Seeing the sign is strictly more informative than
  inferring it from the censoring: `p` collapses to a plain binomial on the
  resolved rows (`N+ / (N+ + N-)`, no censoring correction), an unresolved row
  contributes only `1 - G_C(j)`, free of `p`, and it enters the nowcast with
  probability `p` **flat in its age** — with a shared lag law the age says nothing
  about which way a pending test will go.
* The first prototype uses one revision-delay law. In a single-outcome mode it
  is the lag of that outcome; with both outcomes recorded it is shared by positive
  and negative resolutions. Separate competing-risk lag laws are outside this
  prototype.
* Not implemented, and named as such in the vignette: the **two published
  streams** count-cumulative model (a source publishing the reported *and*
  confirmed cumulatives, which does identify `p` and `g_K` apart).
* New vignette: `vignette("Retractions_and_confirmations")`, written for
  practitioners; the derivation stays in `vignette("Mathematics")` section 8.

## Verified for the negative binomial

The resolution blocks were derived by Poisson colouring, which raises the fair
question of whether they hold under `nb_likelihood()`. They do, and it is now
tested rather than asserted: conditional on the shared gamma frailty the
trajectory-type counts are independent Poisson, so the split of the rows across
types is multinomial and **free of the frailty size `r`** — checked by Monte Carlo
across `r` from 0.5 to 1e6. The count block is then the ordinary negative-binomial
`S_k`, and no quadrature is needed. `p` comes out identical under Poisson and NB
(it lives entirely in the frailty-free block), which is also tested.

### The negative-binomial predictive is wider than nominal — diagnosed, fix opt-in

On data simulated with a genuine per-origin gamma frailty, NB predictive
intervals cover far more than they should at recent horizons (50% intervals
covering ~0.77, 95% covering ~1.00, against Poisson's ~0.49 / ~0.94). It
reproduces identically on a plain nowcast with no retractions, so it is
package-wide and predates this work.

**Cause, now confirmed rather than suspected.** `predict()` draws the count still
to come from the *prior* frailty `Gamma(r, r)`. That gives the correct *marginal*
spread — which is why it survives casual checks — but a predictive must be
conditional on what has already been seen, and the `k_t` reports already in hand
pin that origin's frailty down. Simulated exactly, with true parameters and
conditioning on `k`: the true future SD is 11.6, the prior draw gives 32.6, and
the conjugate posterior `Gamma(r + k_t, r + E[observed])` gives 11.6.

**The fix is implemented but off by default**, behind
`options(diseasenowcasting.conditional_frailty = TRUE)`. Switching it on fixes the
calibration on data simulated from the model (50% coverage 0.77 → 0.51, intervals
~40% narrower). But on the real dengue / mpox / COVID benchmark it made mean WIS
*worse* on every disease (COVID 11.1 → 160.4) at essentially unchanged coverage:
the epidemic mean is always somewhat misspecified on real data, and the prior
draw's extra width was quietly absorbing that. Sharper intervals around a
slightly-off centre score worse. The default therefore reproduces the published
benchmarks exactly, and the option is there for anyone who wants the theoretically
correct predictive. Resolving the underlying misfit is separate work.

## `tidy()` reports uncertainty again

`tidy()` returned `NA` for every standard error on any joint AR(1) / HSGP fit —
`diag()` was being called on a sparse `Matrix`, which errors, and the surrounding
`tryCatch` turned the whole column into `NA`s. Fixed, with a Laplace-sampling
fallback for precision matrices too large or ill-conditioned to invert. `tidy()`
also now reports `confirm_p` (or `confirm_p[<stratum>]`) on the **natural scale**,
with the exact transformed credible interval rather than a logit-scale one.

## Linelist data with retractions

A linelist can now carry a **retraction date** — the date a reported case was
withdrawn from the register (reclassified, corrected, a duplicate) — and
`diseasenowcasting` will nowcast the **settled** count: cases that are reported
and never retracted.

* `nowcast(retraction_date = "<column>")` switches the observation model on. A
  missing retraction date is read as *not retracted yet*, not as *genuine*: the
  retraction lag of a standing row is right-censored at the age of its report, so
  the retraction block is a Berkson–Gage **mixture-cure** likelihood with cure
  fraction `p`. Retractions dated after `now` are masked out automatically.
* The row-level retraction structure uses `p` (the probability a report is
  genuine) and a revision lag. Left alone, `nowcast()` attaches a sensible
  default. A linelist identifies `p` directly from resolved and standing rows,
  so it gets a **weak** data-informed Beta prior. Count-cumulative data no longer
  use this decomposition; see the 2.2.0 migration entry.
* New retraction-delay constructors — `lognormal_retraction()`,
  `gamma_retraction()`, `generalized_gamma_retraction()`,
  `dirichlet_retraction()` — so a model reads as what it is. They are aliases of
  the `*_delay()` constructors; the difference is the support, `g_C` living on
  `{1, 2, ...}`. **Prefer `dirichlet_retraction()` when counts are large**: the
  correction `rho(j)` is applied to every standing case, so a *shape* error in
  `g_C` biases the nowcast by more than its Monte-Carlo noise. On a COVID series
  of ~8000 cases/day a lognormal `g_C` fitted to a `1 + Poisson(2)` lag left a
  0.9% bias and lost nominal coverage; the Dirichlet recovered `rho` to four
  decimals.
* **Per-stratum `p`** via `revision_process(stratified_p = TRUE)`: each
  stratum gets its own confirmation probability under a shared prior, while `g_C`
  stays shared. Use it when strata plausibly differ in data quality and each has
  enough retractions; with sparse strata the shared `p` is safer.
* **Partially observed rows are supported**, in every combination. A censored
  report date (`tbl.now`'s `is_censored`) and/or a censored retraction date (a new
  `retraction_censored =` column of flags, marking rows whose retraction date is
  an upper bound) contribute the log of a sum over the appearance delays they are
  compatible with. Note that an exact retraction *also bounds* a censored report —
  a case cannot be withdrawn before it is filed — so the two constraints combine.
  Each kernel collapses to the exact-row term when its interval is a single point.
* The posterior predictive thins the standing rows one by one: a row whose report
  is `j` periods old survives into the nowcast with probability
  `rho(j) = p / (p + (1 - p) * (1 - G_C(j)))`. Already-retracted rows contribute
  nothing.
* Negative-binomial over-dispersion needs **no quadrature** here — the gamma
  frailty cancels out of the row-level split, so the count block is the ordinary
  NB and the retraction block is closed form.
* With no retraction column, an all-`NA` one, or no retraction observed by `now`,
  the fit is bit-for-bit the ordinary count model.
* `backtest()` builds its truth from the cases that were never retracted when
  `retraction_date` is passed through — scoring against every reported row would
  make an unbiased model look biased low by the retraction rate.
* Cases retracted in the *same period* as their report are dropped: they were
  never visible in any data vintage, and `g_C` lives on `{1, 2, ...}`.
* See the new section 8 of the *Mathematical Foundations* vignette for the
  derivation, and `devel/benchmark_retraction.R` for the dengue / mpox / covid
  benchmark against ignoring or naively dropping the retracted rows.

## Count-cumulative data: counts that can revise *up and down*

Version 2.0.0 introduced the initial count-cumulative experiment. Its original
fixed-`p` Skellam/SkNB formulation is superseded by the 2.2.0 migration entry
above. Current code uses `cumulative_process()`, targets finite-horizon
retention, and does not estimate or report cumulative `p`.

# 1.3.2

* Lowered the two-stage delay-imputation spread floors `floor_mu` and
  `floor_sig_frac` to `0.08` (from `0.15` / `0.25`) in `nowcast()` and
  `nowcast_twostage()`. The previous defaults over-dispersed low-count daily
  data; `0.08` is the value the package is backtested at.
* Made `auto_nowcast()` more robust:
  - Candidate epidemic processes are now gated by a lower bound only -- a process
    is compared whenever the series is long enough to support it (and is no
    longer dropped for being *too* long), so the comparison always spans every
    process the data can support (e.g. `{SIR, AR(1), HSGP}` for long series).
  - Candidates are scored on the common set of as-of dates where they all
    produced a forecast, so a model can no longer "win" on a lucky subset.
  - The selection backtest is now wrapped: if it cannot run (e.g. the series is
    too short for any complete-truth date) `auto_nowcast()` refits the grid
    directly instead of erroring.
  - The winner is refit on the full data with a fallback: if it fails to
    converge, `auto_nowcast()` falls through to the next-best candidate, so it
    now converges whenever any candidate would.
  - New `K_select` argument: the selection backtest fits the whole grid over many
    dates, so it now uses a smaller delay-imputation count (default 10) than the
    final fit's `K` (default 25).  This removes a runtime cliff on long series
    (selection cost scales with `K_select`) while leaving the winning model fit at
    full `K`.
* `backtest()` gains a `recent` argument: when `dates` is `NULL`, choose the most
  recent complete-truth as-of dates instead of spreading them across the history.
  
# 1.3.0

* Added `save_nowcast()` / `load_nowcast()` to persist a fitted `nowcast()` (or
  `auto_nowcast()`) to a single `.rds`. The RTMB autodiff tape cannot be
  serialized, so the bundle stores the `model()` spec, the input `tbl_now`, and
  each fit's parameters plus its Laplace mode and precision. A loaded nowcast
  works with `predict()` / `coef()` / `tidy()` / `autoplot()` straight away (any
  `n_draws`, no RTMB needed -- even for custom delays/epidemics), and can be
  re-fit from its `model` + bundled data. `load_nowcast(file, rebuild = TRUE)`
  also re-tapes the objective (no re-optimization) when a live tape is needed.
* Removed the old `censor_delays_above()` spelling. The function now lives in
  `tbl.now` as `censor_reporting_delays_above()`.
  It remains available with the same signature because `tbl.now` is a dependency
  (attached whenever `diseasenowcasting` is).
* Improved documentation and badges with `lifecycle`. 
* Updated dependency on tbl.now to latest version (0.7.8)
* Removed `rlang` dependency
* Added `RTMBode` as a remote repository and to suggests. 
* The advanced ODE example in the *Custom Delays and Epidemic Processes* vignette
  now integrates the SIR system with the `RTMBode` solver instead of a
  hand-written RK4 scheme.

# 1.2.0

## Automatic model selection

* Added `auto_nowcast()`: give it a `tbl_now` and it builds a grid of candidate
  models (epidemic process x delay family) sized to how much data you have,
  backtests them over several dates, scores them, and refits the winner on the
  full data. The returned object is an ordinary `nowcast()` result with the
  ranked scoreboard in its `comparison` slot. You can
  - pass priors to the candidates (e.g. `sir = sir_epidemic(R0 = ...)`);
  - compare likelihoods (`likelihood = list(nb_likelihood(), poisson_likelihood())`);
  - add your own `custom_delay()`/`custom_epidemic()` models via `models = ...`;
  - select on `metric = "wis"` (default), `"ape"`, `"mse"`, or a calibration
    criterion: `"coverage_50"`, `"coverage_90"`, or `"coverage"` (both intervals'
    miss from nominal, summed).
* Accessors for an `auto_nowcast()` result: `best_model_name()` (the winning
  label), `best_model()` (the winning `model()` object, to reuse elsewhere),
  `comparison_scores()` (the ranked scoreboard), `best_score()` (the winner's
  row), and `selection_metric()` (the criterion used). Printing an
  `auto_nowcast()` result now also shows the top of the scoreboard.
* `nowcast()` / `backtest()` gain a `type = "auto"` option: the Dirichlet delay is
  fit one-stage and every other delay two-stage (the better choice for each in
  our experiments).
  
## Miscelaneous

* Updated `roxygen2` to 8.0.0
* Added the S7 `@` to `NAMESPACE`. 
* Improved test coverage. 
  
# 1.1.0  

## Custom (user-defined) components

* `diseasenowcasting` is now a **model-agnostic framework**: every nowcast is
  built from a likelihood, an epidemic process, and a reporting-delay
  distribution, and each can be a built-in *or* one you write yourself.
* Added **custom reporting-delay distributions** via `custom_delay()` (supply any
  RTMB-traceable CDF as `cdf`, with optional `log_cdf` / `log_survival`) with a
  `validate_custom_delay()` checker.
* Added **custom epidemic processes** via `custom_epidemic()` (supply any
  RTMB-traceable `intensity_fn(theta)` returning `log_mean[max_time x n_strata]`)
  with a `validate_custom_epidemic()` checker. Random walks, ODE/SIR models,
  regression surfaces, etc. all work.
* `custom_delay()` / `custom_epidemic()` no longer take an `n_params` argument —
  the number of parameters is inferred from `priors`, `param_names`, or `inits`.
* Added `infer_max_time()` to read off the number of event-times a model spans,
  for sizing a custom epidemic whose intensity function loops over time.
* New vignette *Custom Delays and Epidemic Processes* with worked examples on the
  `denguedat` and `mpoxdat` datasets, including an advanced RK4-integrated SIR ODE.

## Other changes

* `nowcast_diagnostic()` gains a `previous_times` argument (default 30) to limit
  the incidence and nowcast panels to the most recent event-times.
* Custom components require `library(RTMB)` to be attached; a clear error is
  raised otherwise.
* Removed the experimental `ppc()` posterior-predictive-check function.
* Internal modernisation (no change in results): non-standard evaluation now uses
  the rlang `.data` pronoun, backtesting uses `future.apply`.

#  1.0.0

* Changed the structure of the package from Stan to RTMB
* Added the gaussian process (HGSP) and SIR models
* Added the lognormal and generalized gamma delay
* Added surprise factors for extreme values
* Deprecated previous package
