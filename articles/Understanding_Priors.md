# Understanding Priors in diseasenowcasting

A **prior** is a probability distribution that encodes your beliefs
about a parameter before seeing the data. When the data is informative
(many cases, long reporting history), the prior has little impact and
the data overwhelms it. When the data is sparse or noisy (early
epidemic, small populations), the prior can substantially shape the
estimates.

### Quick reference

Here we show what to change depending on your situation:

| Priority | Setting(s) | What it Controls | When to Adjust |
|----|----|----|----|
| 1 | `phi` | Interval width (i.e. coverage) | Start here if coverage is off. |
| 2 | Delay’s `mu`, `sigma` and `Q` if applicable | How much the most-recent past is inflated. | Change here if nowcasting for previous dates (backcasting) has too wide or too short intervals. |
| 3 | `ell` (HSGP), `sigma` (AR1) | Trend flexibility near turning points | Adjust when the trend is too rigid or too reactive around inflection points. |
| 4 | `R0`, `gamma`, `N_eff` (SIR only) | Mechanistic epidemic shape | Keep these loose unless you have strong epidemiological knowledge. |
| 5 | `N_pop` (SIR only) | Population at risk | Set to the actual population at risk. |
| 6 | `num_basis` (HSGP) | Trend smoothness/complexity | Increase if the trend looks too smooth; decrease if it looks too noisy. |
| 7 | `covariate_prior` | Strength of covariate effects | Mainly matters when additional covariates are included and have strong effects. |

### What the rest of the vignette shows:

This vignette teaches the impact of the prior. It does it by showing how
two priors (a *tight* distribution vs a *loose* diffuse one) result in
different values. When data are abundant the prior barely matters; when
they are sparse or censored (exactly the situation a nowcast faces at
the most recent times) the setting shapes the answer.

Each figure has the same three panels, because a parameter can move
three different things:

     1. Reporting delay            2. Smoothed epidemic               3. Nowcast
     (fitted delay distribution)   (latent unobserved incidence)   (predicted nowcast)

In the nowcast panel, solid grey points are the counts **reported so
far** and hollow points are the **eventually-observed truth**.

For this simulation we use the `denguedat` dataset (weekly dengue cases
from Puerto Rico). However the qualitative impact of the priors can be
translated to any other disease.

## 1. Likelihood prior: NB precision (`phi`)

The negative-binomial precision `phi` controls how much count
variability is *not* explained by the trend. It is the single biggest
driver of **interval width**. As a number, small precision `phi` is very
overdispersed (wide intervals); large `phi` is very precise.

![](Understanding_Priors_files/figure-html/fig-phi-1.png)

**What to watch:** `phi`’s main job is the **width** of the epidemic
process (both nowcast and smoothed epidemic); it barely moves the delay
(panel 1). If your 90% intervals routinely miss the truth, use a smaller
`phi` (more overdispersion); if they are needlessly wide, use a larger
one.

## 2. Reporting-delay parameters

Delay parameters act first on **panel 1** (the fitted delay
distribution) and then propagate to the nowcast (**panel 3**), which
inflates the most-recent counts. Intuitively, if a delay has a heavier
tail, the nowcast will have a wider interval in the past as cases have
more time to arrive.

### 2.1 Log-Normal delay

#### 2.1.1 Location (`mu`)

`mu` is the (log) typical delay in the delay units (i.e. if a delay
usually takes 2 weeks then \mu \approx log(2) if units are weeks or \mu
\approx log(2 \* 7) if units are days ). Small = reports arrive fast;
large = a long mean delay.

![](Understanding_Priors_files/figure-html/fig-ln-mu-1.png)

**What to watch:** panel 1 slides left/right with `mu`. If the fitted
delay is too short the nowcast under-inflates recent weeks (panel 3 sits
too low); too long and it over-inflates.

#### 2.1.2 Scale (`sigma`)

`sigma` (\> 0) is the spread of the log-normal (i.e. how concentrated
the delays are). Small = a sharp spike at the typical delay; large = a
long, heavy tail.

![](Understanding_Priors_files/figure-html/fig-ln-sigma-1.png)

**What to watch:** `sigma` changes the *shape* of panel 1 without moving
its centre. A heavier tail means more cases are still expected to
arrive, so the nowcast inflates the recent past more.

### 2.2 Generalized-Gamma delay

#### 2.2.1 Location (`mu`)

The Generalized-Gamma adds a tail-shape parameter `Q` to a log-location
`mu` and scale `sigma`. `mu` behaves like the Log-Normal location.

![](Understanding_Priors_files/figure-html/fig-gg-mu-1.png)

**What to watch:** as for the Log-Normal, `mu` slides the whole delay
distribution (panel 1) to shorter or longer delays.

#### 2.2.2 Scale (`sigma`)

`sigma` (\> 0) controls the spread around the location.

![](Understanding_Priors_files/figure-html/fig-gg-sigma-1.png)

**What to watch:** small `sigma` concentrates the delay; large `sigma`
disperses it, widening the right tail the nowcast must account for.

#### 2.2.3 Tail shape (`Q`)

`Q` interpolates the tail. Small (negative) = a light tail; large
(positive) = a heavy tail.

![](Understanding_Priors_files/figure-html/fig-gg-q-1.png)

**What to watch:** `Q` reshapes the *tail* of panel 1 while leaving the
bulk roughly fixed. A heavier tail makes more late reports possible.

### 2.3 Dirichlet (non-parametric) delay

#### 2.3.1 Concentration (`alpha`)

The Dirichlet delay learns a free histogram. Its concentration `alpha`
controls how that histogram is regularised: small (\< 1) lets it spike
on only a few delays, large pushes it towards assigning positive
probability to more values.

![](Understanding_Priors_files/figure-html/fig-dirichlet-1.png)

**What to watch:** panel 1 shows the effect most directly – a small
`alpha` lets the delay pmf spike on the observed delays, a large `alpha`
flattens it letting more delays be possible.

## 3. Epidemic-process parameters

The epidemic process is the smooth latent trend the nowcast
extrapolates. The package offers three: the flexible **HSGP**, the
simpler **AR(1)**, and the mechanistic **SIR**.

### 3.1 HSGP

#### 3.1.1 Amplitude (`alpha`)

`alpha` as the GP amplitude dictates how far the latent trend may swing.
Small means a near-flat trend; large lets it reach high.

![](Understanding_Priors_files/figure-html/fig-gp-alpha-1.png)

**What to watch:** the amplitude controls the height of the latent trend
(panel 2) and the width of the nowcast (panel 3); the delay (panel 1) is
untouched. (As a *number*, fixing the amplitude is subtle because the
basis coefficients adapt – the prior row makes the effect clearer.)

#### 3.1.2 Length-scale (`ell`)

`ell` is the GP length-scale – the horizontal “wavelength” of the trend.
Small = wiggly and quick to bend; large = long and smooth.

![](Understanding_Priors_files/figure-html/fig-gp-ell-1.png)

**What to watch:** a short length-scale lets the trend bend sharply
through the peak (panel 2) at the cost of wiggle; a long one forces a
smoother curve that can lag turning points.

#### 3.1.4 HSGP: number of basis functions (`num_basis`)

`num_basis` is the **resolution** of the GP. It is inherently a number,
so it only has a numbers row. The default is automatic (~1.5\sqrt{T})
with T being the maximum time planned to be observed.

![](Understanding_Priors_files/figure-html/fig-num-basis-1.png)

**What to watch:** few basis functions force an over-smooth trend that
can lag the peak (panel 2); many let the model track rapid change but
risk over-fitting noise near the end. The delay (panel 1) is unaffected.

### 3.2 AR(1)

#### 3.2.1 Autocorrelation (`phi`)

`phi` in (-1, 1) is the persistence of the autoregressive trend. Near 0
it forgets the past immediately; near 1 it behaves like a random walk.

![](Understanding_Priors_files/figure-html/fig-ar-phi-1.png)

**What to watch:** high `phi` makes recent moves persist into the
nowcast (panel 2-3); low `phi` mean-reverts quickly.

#### 3.2.2 Innovation SD (`sigma`)

`sigma` (\> 0) is the size of the random step the trend takes each week.
Small = smooth and stable; large = jumpy.

![](Understanding_Priors_files/figure-html/fig-ar-sigma-1.png)

**What to watch:** small `sigma` keeps the trend smooth with narrow
intervals; large `sigma` lets it jump, widening the nowcast.

### 3.3 SIR

#### 3.3.1 Basic reproduction number (`R0`)

`R0` (\> 0) sets how fast a mechanistic SIR epidemic grows. Below ~1 it
fizzles; large values grow explosively.

![](Understanding_Priors_files/figure-html/fig-r0-1.png)

**What to watch:** `R0` acts through the *shape* of the epidemic curve
(panel 2) – how steeply it climbs. A confident, mis-specified `R0` can
pull the nowcast away from the data.

#### 3.3.2 Recovery rate (`gamma`)

`gamma` in (0, 1) is the rate people leave the infectious pool
(1/`gamma` is the mean infectious period). Small = slow recovery, long
epidemic; large = fast.

![](Understanding_Priors_files/figure-html/fig-gamma-1.png)

**What to watch:** `gamma` sets the epidemic’s *duration* and the speed
of its decline (panel 2), so it is shown a few weeks past the peak.

#### 3.3.3 Susceptible fraction (`N_eff`)

`N_eff` in (0, 1) is the fraction of the population actually at risk.
Small = tiny pool, the epidemic saturates fast and low; large = almost
everyone.

![](Understanding_Priors_files/figure-html/fig-neff-1.png)

**What to watch:** `N_eff` (together with `N_pop`) sets the size of the
susceptible pool and hence the peak height (panel 2).

#### 3.3.4 Total population (`N_pop`)

`N_pop` is the assumed total population – a fixed scalar, shown as
numbers only.

![](Understanding_Priors_files/figure-html/fig-npop-1.png)

**What to watch:** a small `N_pop` saturates the susceptible pool
quickly (short, sharp peak); a large one lets the epidemic grow slowly
over more time.

## 4. Covariate priors (`covariate_prior`)

When temporal effects (seasonality, day-of-week) are attached to a
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md)
call, each effect column becomes a covariate.
`model(covariate_prior = ...)` sets a shared prior on every covariate
coefficient. The fits below use weekly dengue data with 52-period
seasonality attached.

![](Understanding_Priors_files/figure-html/fig-cov-1.png)

**What to watch:** a tight prior (near-zero coefficients) forces the
HSGP to explain all variation alone, smoothing out the seasonal pattern;
a loose prior lets the seasonal covariate pull the curve up or down
strongly.

## 5. Practical guidance

Set any parameter the same way – pass a **number** to fix it, or a
`*_prior()` object to give it a prior – then call
[`nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.md):

``` r

# Fix values you know (numbers):
mdl <- model(nb_likelihood(), hsgp_epidemic(num_basis = 5),
             lognormal_delay(mu = log(2)))

# Or give priors (tight = confident, loose = diffuse):
mdl <- model(nb_likelihood(phi = lognormal_prior(log(5), 0.5)),
             hsgp_epidemic(alpha = half_normal_prior(0, 5)),
             lognormal_delay(mu = normal_prior(log(7), 0.1)))

# Inspect what the defaults resolve to:
default_priors(model(nb_likelihood(), hsgp_epidemic(), lognormal_delay()))
```

See also the summary at the beginning of this vignette for specific
steps.
