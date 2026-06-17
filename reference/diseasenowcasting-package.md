# diseasenowcasting: Nowcast Time Series of Epidemiological Cases

Implements Bayesian nowcasting models for incident disease cases using a
censored point-process approach. The reporting delay is modelled
directly as a stochastic process, jointly with the epidemic dynamics,
via a censored likelihood. Inference uses RTMB (CppAD autodiff + Laplace
approximation). Supports multiple epidemic models (HSGP, AR1, SIR),
multiple delay families (LogNormal, GenGamma, Dirichlet), stratified
data, extreme-delay detection, and backtesting.

## See also

Useful links:

- <https://rodrigozepeda.github.io/diseasenowcasting/>

- <https://github.com/RodrigoZepeda/diseasenowcasting>

- Report bugs at
  <https://github.com/RodrigoZepeda/diseasenowcasting/issues>

## Author

**Maintainer**: Rodrigo Zepeda-Tello <rzepeda17@gmail.com>
([ORCID](https://orcid.org/0000-0003-4471-5270))

Authors:

- Rodrigo Zepeda-Tello <rzepeda17@gmail.com>
  ([ORCID](https://orcid.org/0000-0003-4471-5270))

- Rami Yaari ([ORCID](https://orcid.org/0000-0002-8808-8937))

- Matteo Perini ([ORCID](https://orcid.org/0000-0002-9465-6216))

Other contributors:

- Tal Robin ([ORCID](https://orcid.org/0000-0002-3128-2499))
  \[contributor\]

- Teresa Yamana ([ORCID](https://orcid.org/0000-0001-8349-3151))
  \[contributor\]

- Jeffrey Shaman \[contributor\]

- Columbia University in the City of New York \[copyright holder,
  funder\]
