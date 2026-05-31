#' @keywords internal
"_PACKAGE"

## usethis namespace: start
# AD-aware density/CDF functions MUST come from RTMB (not stats) so that, inside
# the objective closure defined in this package's namespace, they dispatch on
# advector arguments. stats provides only the (numeric) optimisers, summaries,
# and RNG samplers.
#' @importFrom RTMB pnorm dnorm dgamma pgamma dbeta dexp dweibull dchisq dnbinom plogis
#' @importFrom stats nlminb optim median sd quantile approx
#'   rnorm rcauchy rt runif rgamma rweibull rlnorm rchisq rexp rlogis rbeta
#' @importFrom utils head tail
## usethis namespace: end
NULL

# Null-coalescing helper available on all supported R versions.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
