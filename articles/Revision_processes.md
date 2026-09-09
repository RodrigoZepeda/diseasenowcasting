# Revision processes: when a report is not yet a case

``` r

library(diseasenowcasting)
library(tbl.now)
library(dplyr)
```

## The problem

The nowcasts in
[`vignette("introduction")`](https://rodrigozepeda.github.io/diseasenowcasting/articles/introduction.md)
assume a report *is* a case. Real surveillance rarely works that way. A
report arrives provisionally and is later **resolved**: a laboratory
result comes back, a clinician reclassifies, a duplicate is spotted.
Either

- the result is **positive** and the case is **confirmed**, or
- the result is **negative** and the case is **retracted** — struck from
  the register.

If you nowcast the raw report counts you are nowcasting the wrong
quantity: you will over-count by however many of today’s reports are
about to be withdrawn. This vignette shows how to nowcast the number
that actually matters — the cases that will still be there once the dust
settles.

### The one idea to take away

A missing resolution date does **not** mean “this one is fine”. It means
**not resolved yet**.

That distinction is the whole model. A case reported this morning with
no retraction date is not evidence of anything: there has been no time
for a retraction. A case reported eight weeks ago with no retraction
date is strong evidence that it is genuine. `diseasenowcasting` weighs
each report by how long it has had to be contradicted, which is why the
answer is not simply “multiply by the proportion that get retracted”.

## You record it once; the package works out the rest

Different registers record different things, and you do not need to
change your data to fit the package. What you do need is to say, **on
the `tbl_now`**, which column holds the revision date and which holds
the outcome:

``` r

tbl_now(df, event_date = onset, report_date = reported,
        revision_date = result, revision_type = outcome,
        is_censored_revision = result_is_upper_bound)
```

`revision_type` takes `"confirmed"`, `"retracted"` or `"pending"`. From
those values
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
works out for itself which of three situations you are in:

| Your register records | Inferred mode | The nowcast targets |
|----|----|----|
| only the **retractions** | `retraction_only` | cases reported and never retracted |
| only the **confirmations** | `confirmation_only` | cases that will eventually be confirmed |
| **both** outcomes | `both` | cases whose result comes back positive |

There is no argument to pass and none to get wrong. Recording both
outcomes is the most informative, because the model sees the *outcome*
of each resolved report rather than having to infer it from how long the
report has stood.

The mode is read from the **whole** dataset, not from the as-of view, so
it is a stable property of your data source and does not change as you
move `now` back through a backtest.

`is_censored_revision` is optional. When supplied, it names a logical
column whose `TRUE` rows record an upper bound on the revision date
rather than an exact date. Like all three date columns and
`revision_type`, this is attached to the `tbl_now`;
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
does not accept a duplicate column-name argument.

**A revision date with no outcome is an error.** The report has
resolved, but its sign is unknown, so it cannot enter either lag law.
Leave the date `NA` and mark the row `"pending"` instead.

## A worked example: retractions

We simulate a linelist in which 15% of reports are eventually retracted,
a few days after they were filed. In practice this column comes from
your register; here we build one so the truth is known.

``` r

set.seed(20260726)
origin <- as.Date("2024-01-01")
n_days <- 90

daily_mean <- 40 * exp(0.8 * sin(2 * pi * seq_len(n_days) / 60))
linelist <- lapply(seq_len(n_days), function(day) {
  n_reports <- rpois(1, daily_mean[day] / 0.85)
  if (n_reports == 0) return(NULL)
  reporting_delay <- 1 + rpois(n_reports, 3)
  genuine         <- runif(n_reports) < 0.85          # 85% are real
  retraction_lag  <- 1 + rpois(n_reports, 2)
  data.frame(
    onset     = origin + day - 1,
    reported  = origin + day - 1 + reporting_delay,
    retracted = as.Date(ifelse(genuine, NA,
                  as.numeric(origin + day - 1 + reporting_delay + retraction_lag)),
                  origin = "1970-01-01"))
}) |> bind_rows()

now <- origin + n_days - 1
glimpse(linelist)
#> Rows: 5,703
#> Columns: 3
#> $ onset     <date> 2024-01-01, 2024-01-01, 2024-01-01, 2024-01-01, 2024-01-01,…
#> $ reported  <date> 2024-01-04, 2024-01-05, 2024-01-07, 2024-01-06, 2024-01-05,…
#> $ retracted <date> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 202…
```

Fold the outcome into the two columns a `tbl_now` understands. Here only
retractions are recorded, so every other row is `"pending"` — *not*
`"confirmed"`, because a report that has simply not been withdrawn has
not been confirmed either:

``` r

linelist <- linelist |>
  mutate(revision_date = retracted,
         revision_type = if_else(is.na(retracted), "pending", "retracted"))

dat <- tbl_now(linelist, event_date = onset, report_date = reported,
               revision_date = revision_date, revision_type = revision_type,
               data_type = "linelist", verbose = FALSE)
```

Then just nowcast. There is nothing to tell it:

``` r

nc <- nowcast(dat, now = now)
nc
```

    #> ── diseasenowcasting ─────────────────────────────────── as of 2024-03-30 ──
    #> Model: NegBin / HSGP / LogNormal
    #> Resolution: modelling retractions; nowcasting cases reported and never retracted.
    #> P(not retracted) = 0.851 — see `parameters()` for its interval.

[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
says in words what it is modelling and reports the probability a report
survives. For its uncertainty, use
[`parameters()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/parameters.md)
— **not** [`tidy()`](https://generics.r-lib.org/reference/tidy.html),
which on a nowcast gives you the predicted counts:

``` r

parameters(nc) |> filter(type == "resolution")
```

    #>                 term estimate std.error conf.low conf.high       type
    #>  prob_not_retracted    0.8512        NA   0.8399    0.8619 resolution

The true value was 0.85.

### What you get that a naive analysis does not

Two tempting shortcuts, and why both fail:

- **Nowcast every report.** You target the gross report count, not the
  settled one, and over-shoot by the retraction rate.
- **Delete the reports already known to be retracted.** Now you
  under-shoot at recent event times, because the retractions that will
  cancel *today’s* reports have not arrived yet. This is the more
  seductive error: the intervals look reassuringly tight, and they are
  tight around the wrong number.

`devel/benchmark_retraction.R` runs all three on dengue, mpox and COVID
data with retractions injected at a known rate. The resolution model is
the only one whose intervals cover at close to their nominal rate;
deleting the known retractions covers about 2% of its own 95% intervals
on COVID.

## Confirmations instead

If your register records confirmations rather than retractions, nothing
changes in the call at all — only the values in `revision_type`:

``` r

confirmed_dat <- linelist |>
  mutate(revision_date = confirmed,
         revision_type = if_else(is.na(confirmed), "pending", "confirmed")) |>
  tbl_now(event_date = onset, report_date = reported,
          revision_date = revision_date, revision_type = revision_type,
          data_type = "linelist", verbose = FALSE)

nowcast(confirmed_dat, now = now)
```

The evidence runs the other way, which is worth internalising. Under
**retraction**, a report that has stood a long time is *more* likely
genuine. Under **confirmation**, a report that has sat a long time
unconfirmed is *less* likely ever to be confirmed. The package handles
the flip; you only need to know which column you have.

One practical difference: a confirmation may land on the **same day**
the case is reported (a same-day test result), and those rows are kept.
A retraction on the same day describes a case that was never visible in
any data extract, so those rows are dropped, with a message.

## Recording both outcomes

If you know, for every resolved report, *which way* it went, supply both
columns:

``` r

set.seed(11)
both <- lapply(seq_len(n_days), function(day) {
  n_reports <- rpois(1, daily_mean[day] / 0.7)
  if (n_reports == 0) return(NULL)
  reporting_delay <- 1 + rpois(n_reports, 3)
  positive        <- runif(n_reports) < 0.7
  result_lag      <- rpois(n_reports, 2)               # may be same-day
  result_date     <- origin + day - 1 + reporting_delay + result_lag
  data.frame(
    onset     = origin + day - 1,
    reported  = origin + day - 1 + reporting_delay,
    confirmed = as.Date(ifelse(positive,  as.numeric(result_date), NA), origin = "1970-01-01"),
    retracted = as.Date(ifelse(!positive, as.numeric(result_date), NA), origin = "1970-01-01"))
}) |> bind_rows()

both_dat <- both |>
  mutate(revision_date = coalesce(confirmed, retracted),
         revision_type = case_when(!is.na(confirmed) ~ "confirmed",
                                     !is.na(retracted) ~ "retracted",
                                     TRUE              ~ "pending")) |>
  tbl_now(event_date = onset, report_date = reported,
          revision_date = revision_date, revision_type = revision_type,
          data_type = "linelist", verbose = FALSE)
```

``` r

nowcast(both_dat, now = now)
```

This is the most informative case, and the model simplifies: the
probability a report is real becomes the plain proportion of resolved
reports that came back positive, with no censoring correction needed. A
pending report contributes only the fact that its result has not
arrived.

A report resolves **once**, and the `tbl_now` representation enforces
that by construction: one date, one outcome. There is no way to say
“confirmed and retracted” any more.

## One revision-delay law in the first prototype

[`revision_process()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/revision_process.md)
takes one `revision_delay`. In `mode = "confirmation_only"` it is the
report-to-confirmation lag; in `mode = "retraction_only"` it is the
report-to-retraction lag. When both outcomes are recorded, the same law
is used for both signs. Thus a pending report’s age tells us that its
result has not arrived, but—conditional on the shared-law
assumption—does not favour confirmation over retraction.

This restriction is deliberate for the first prototype. Separate
positive and negative lag laws would form a competing-risks extension
and would require enough resolved observations of both signs to identify
two distributions. That extension is not part of the current public API.

## Configuring the model

The defaults are chosen to work without tuning, but everything is
adjustable via
[`revision_process()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/revision_process.md):

``` r

model(
  nb_likelihood(), hsgp_epidemic(), lognormal_delay(),
  revision = revision_process(
    revision_delay = dirichlet_revision(bins = 10),  # shape of the lag
    p                = beta_prior(20, 3),                # your own prior on p
    stratified_p     = TRUE                              # one p per stratum
  )
)
```

## One-stage and two-stage inference

Both inference strategies support a revision process:

``` r

joint <- nowcast(dat, specification, type = "one_stage")
staged <- nowcast(dat, specification, type = "two_stage", K = 25)
```

The one-stage fit estimates the event-to-report delay, epidemic process,
revision delay, and `p` in one objective. The two-stage fit retains the
package’s original stepwise boundary: Stage 1 estimates and imputes only
the event-to-report delay; each Stage-2 fit conditions on one such
imputation and jointly estimates the epidemic process, revision delay,
and `p`. Pooling posterior draws over the `K` Stage-2 fits propagates
both sources of uncertainty. There is deliberately no separate plug-in
estimate of the revision process.

- **`revision_delay`** — how long results take to come back. Any delay
  family works. Prefer
  [`dirichlet_revision()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/revision_delay.md)
  when counts are large: the correction is applied to every pending
  report, so a wrong *shape* biases the total by more than sampling
  noise.
- **`p`** — the probability a report resolves positive. Left alone it
  gets a weak prior centred on what the data show among reports with
  enough follow-up. The naive proportion under-states retraction,
  because recent reports have not had time to be withdrawn.
- **`stratified_p = TRUE`** — a separate probability per stratum, for
  when data quality genuinely differs between laboratories or regions.
  With sparse strata the shared value is safer.
- **`mode`** — leave it `"auto"` and the outcomes decide. Set it
  explicitly when you want the *check* rather than the inference:
  asserting a mode your data cannot support is an error, and asserting
  one when nothing has resolved yet fits `p` from its prior instead of
  reducing to the ordinary count model.

## Data other than linelists

Everything above works unchanged if your data are **count-incidence**:
one row per distinct `(event date, report date, resolution date)` with a
case count, `NA` marking the unresolved. The aggregated and row-per-case
forms give identical results.

**count-cumulative** streams use a different observation process.
Repeated net levels do not reveal which individual reports remain
pending, so they cannot identify `p` separately from a conditional
revision-delay distribution. Do not attach
[`revision_process()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/revision_process.md)
to them. Configure the unconditional finite-age retraction kernel with
[`cumulative_process()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/cumulative_process.md)
instead:

``` r

model(
  nb_likelihood(), ar1_epidemic(), lognormal_delay(),
  cumulative = cumulative_process(
    observation = "hurdle_ztnb", settlement = 26L
  )
)
```

The component reports the finite-horizon retraction mass, h_R, S_R, q_C,
and terminal retention. It never labels one minus the retraction mass as
an identified truth probability. `hurdle_ztpoisson` is also available
when the non-zero update magnitude should be zero-truncated Poisson with
no dispersion parameter. See §9 of the [Mathematics
article](https://rodrigozepeda.github.io/diseasenowcasting/articles/Mathematics.md).

## Backtesting

[`backtest()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/backtest.md)
needs nothing extra, and — importantly — builds its **eventual truth**
from the cases that settle positive. Pending rows remain in each
historical fitting vintage, but do not enter the final confirmation-only
truth. A laboratory confirmation dated after a historical analysis date
is masked back to pending for that fit. Scoring against every reported
row would evaluate a different estimand and make a calibrated
confirmation model look biased low:

``` r

backtest(dat, n_dates = 10) |> score()
```

## Where the mathematics lives

Section 8 of the [Mathematics
article](https://rodrigozepeda.github.io/diseasenowcasting/articles/Mathematics.md)
derives all of this: the trajectory-type decomposition, why the
resolution block is a mixture-cure likelihood, why the negative-binomial
case needs no numerical integration, and how the predictive thins each
pending report by its own survival probability.
