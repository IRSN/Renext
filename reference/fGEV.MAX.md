# Fit a GEV distribution from block maxima or r largest order statistics using an aggregated Renewal POT process

Fit a GEV distribution from block maxima or \\r\\ largest order
statistics using an aggregated Renewal POT process.

## Usage

``` r
fGEV.MAX(MAX.data, MAX.effDuration,
         MAX = NULL,
         control = list(maxit = 300, fnscale = -1),
         scaleData = TRUE,
         cov = TRUE,
         info.observed = TRUE,
         trace = 0)
```

## Arguments

- MAX.data:

  A list of block maxima or \\r\\ largest statistics as in
  [`Renouv`](https://irsn.github.io/Renext/reference/Renouv.md).

- MAX.effDuration:

  A vector of durations as in
  [`Renouv`](https://irsn.github.io/Renext/reference/Renouv.md). *The
  durations must be identical* in order to have a common GEV
  distribution for the maxima.

- MAX:

  A compact representation of the needed data as a list. This is
  typically created by using the (non exported) `Renext:::makeMAXdata`
  function.

- control:

  List to control the optimisation in
  [`optim`](https://rdrr.io/r/stats/optim.html).

- scaleData:

  Logical. If `TRUE`, the data in `MAX.data` are scaled before being
  used in the likelihood. The scaling operation is carried on the
  excesses (observations minus threshold).

- cov:

  Logical. If `TRUE` the standard deviation and the covariance matrix of
  the estimates are computed and returned as `sd` and `cov` elements of
  the list. However if the estimated shape parameter is \\\< - 0.5\\ the
  two elements are filled with `NA` because the regularity conditions
  can not be thought of as valid.

- info.observed:

  Logical. If `TRUE` the covariance is computed from the *observed*
  information matrix. If `FALSE`, the *expected* information matrix is
  used instead. This is only possible for block maxima data, i.e. when
  all the blocks contain only one observation. The computation relies on
  the formula given by Prescott and Walden. Note that the default value
  differs from that of the functions
  [`fGPD`](https://irsn.github.io/Renext/reference/fGPD.md),
  [`flomax`](https://irsn.github.io/Renext/reference/flomax.md) and
  [`fmaxlo`](https://irsn.github.io/Renext/reference/fmaxlo.md), for
  historical reasons.

- trace:

  Integer level of verbosity during execution. With the value `0`,
  nothing is printed.

## Details

The data are assumed to provide maxima or \\r\\ largest statistics
arising from an aggregated renewal POT model with unknown event rate
\\\lambda\\ and unknown two-parameter Generalised Pareto Distribution
for the excesses. A threshold \\u\\ is fixed below the given data and
the three unknown parameters `lambda`, `scale` and `shape` of the POT
model are found by maximising the likelihood. Then a vector of the three
parameters for the GEV distribution is computed by transformation. The
covariance matrix and standard deviations are computed as well using the
jacobian matrix of the transformation.

The maximisation is for the log-likelihood with the rate `lambda`
concentrated out, so it is a two-parameter optimisation.

## Value

A list

- estimate:

  Named vector of the estimated parameters for the GEV distribution of
  maxima.

- opt:

  Result of the optimisation.

- loglik:

  Identical to `opt$value`. This is the maximised log-likelihood for the
  renewal POT model.

- sd:

  Standard deviation of the estimates (approximation based on the ML
  theory).

- cov:

  Covariance matrix of the estimates (approximation based on the ML
  theory).

## References

The *Renext Computing Details* document.

Prescott P. and Walden A.T. (1980) Maximum Likelihood Estimation of the
Parameters of the Generalized Extreme-Value Distribution. *Biometrika*
**67**(3), 723-724.

## Author

Yves Deville

## Caution

Whatever be the data, the log-likelihood is infinite (hence maximal) for
any vector of GEV parameters with shape \\\< -1\\ and postive scale.
Hence the log-likelihood should be maximised with a constraint on the
shape, while an *unconstrained* optimisation is used here. In practice,
for numerical reasons, the estimate usually remains inside the
`shape > -1` region. *An estimation leading to shape \\\< -1\\ must be
considered as meaningless*. An estimation with shape \\\< -0.5\\ should
be considered with care.

## Note

This function could get more arguments in the future.

## See also

[`Renouv`](https://irsn.github.io/Renext/reference/Renouv.md).

## Examples

``` r
##====================================================================
## block maxima: simulated data and comparison with  the 'fgev'
## function from the 'evd' package
##====================================================================
set.seed(1234)
u <- 10
nBlocks <- 30
nSim <- 100   ## number of samples 
Par <- array(NA, dim = c(nSim, 3, 2),
             dimnames = list(NULL, c("loc", "scale", "shape"), c("MAX", "evd")))
LL <- array(NA, dim = c(nSim, 2),
            dimnames = list(NULL, c("MAX", "evd")))

for (i in 1:nSim) {
  rd <- rRendata(threshold = u,
                 effDuration = 1,
                 lambda = 12,
                 MAX.effDuration = rep(1, nBlocks),
                 MAX.r = rep(1, nBlocks),
                 distname.y = "exp", par.y = c(rate = 1 / 100))

  MAX <- Renext:::makeMAXdata(rd)
  fit.MAX <- fGEV.MAX(MAX = MAX)
  fit.evd <- fgev(x = unlist(MAX$data))
  Par[i, , "MAX"] <- fit.MAX$estimate
  Par[i, , "evd"] <- fit.evd$estimate
  LL[i, "MAX"] <- fit.MAX$loglik
  LL[i, "evd"] <- logLik(fit.evd)
}
#> Warning: estimated 'shape' is < -0.5. ML inference results not suitable

##====================================================================
## r largest: use 'ismev::rlarg.fit' on the venice data set.
## NB 'venice' is taken from the 'evd' package here.
##====================================================================
if (FALSE) { # \dontrun{ 
require(ismev);
fit1 <- ismev::rlarg.fit(venice)

## transform data: each row is a block
MAX.data <- as.list(as.data.frame(t(venice)))
## remove the NA imposed by the rectangular matrix format
MAX.data <- lapply(MAX.data, function(x) x[!is.na(x)])
MAX.effDuration <- rep(1, length(MAX.data))

fit2 <- fGEV.MAX(MAX.data = MAX.data,
                 MAX.effDuration = MAX.effDuration)

## estimates
est <- cbind(ismev = fit1$mle, RenextLab = fit2$estimate)
print(est)
# covariance
covs <- array(dim = c(2, 3, 3),
              dimnames = list(c("ismev", "RenextLab"),
                colnames(fit2$cov), colnames(fit2$cov)))
                
covs["ismev", , ] <- fit1$cov
covs["RenextLab", , ] <- fit2$cov
print(covs)
} # }
```
