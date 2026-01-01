# Initial estimation of GPD parameters for an aggregated renewal model

Initial estimation for an aggregated renewal model with GPD marks.

## Usage

``` r
parIni.MAX(MAX, threshold, distname.y = "exp")
parIni.OTS(OTS, threshold, distname.y = "exp")
```

## Arguments

- MAX:

  A list describing partial observations of `MAX` type. These are block
  maxima or block \\r\\-largest statistics. The list must contain
  elements named `block`, `effDuration`, `r`, and `data` (a by-block
  list of \\r\\-largest statistics).

- OTS:

  A list describing partial observations of `OTS` type. These are
  observations above the block thresholds, each being not smaller than
  `threshold`. This list contains `block`, `effDuration`, `threshold`,
  `r` and `data` (a by-block list of observations).

- threshold:

  The threshold of the POT-renewal model. This is the location parameter
  of the marks from which Largest Order Statistics are observed.

- distname.y:

  The name of the distribution. For now this can be `"exp"` or
  `"exponential"` for exponential excesses implying Gumbel block maxima,
  or `"gpd"` for GPD excesses implying GEV block maxima. The
  initialisation is the same in all cases, but the result is formatted
  according to the target distribution.

## Details

The functions estimate the Poisson rate `lambda` along with the shape
parameter say `sigma` for exponential excesses. If the target
distribution is GPD, then the initial shape parameter is taken to be 0.

In the "MAX" case, the estimates are obtained by regressing the maxima
or \\r\\-Largest Order Statistics within the blocks on the log-duration
of the blocks. The response for a block is the minimum of the \\r\\
available Largest Order Statistics as found within the block, and \\r\\
will generally vary across block. When some blocks contain \\r \> 1\\
largest order statistics, the corresponding spacings are used to compute
a spacings-based estimation of \\\sigma\\. This estimator is independent
of the regression estimator for \\\sigma\\ and the two estimators can be
combined in a weighted mean.

In the "OTS" case, the estimate of `lambda` is obtained by a Poisson
regression using the log durations of the blocks as an offset. The
estimate of `sigma` is simply the mean of all the available excesses,
which by assumption share the same exponential distribution.

## Value

A vector containing the estimate of the Poisson rate \\\lambda\\, and
the estimates of the parameters for the target distribution of the
excesses. For exponential excesses, these are simply a `rate` parameter.
For `GPD` excesses, these are the `scale` and `shape` parameters, the
second taken as zero.

## References

See the document *Renext Computing Details*.

## Author

Yves Deville

## Note

In the MAX case, the estimation is possible only when the number of
blocks is greater than \\1\\, since otherwise no information about
\\\lambda\\ can be gained from the data; recall that the time at which
the events occurred within a block is not known or used.

## See also

The [`spacings`](https://irsn.github.io/Renext/reference/spacings.md)
methods for the spacings used in the estimation.

## Examples

``` r
set.seed(1234)
## initialisation for 'MAX' data
u <- 10
nBlocks <- 30
nSim <- 100
ParMAX <- matrix(NA, nrow = nSim, ncol = 2)
colnames(ParMAX) <- c("lambda", "sigma")

for (i in 1:nSim) {
  rd <- rRendata(threshold = u,
                 effDuration = 1,
                 lambda = 12,
                 MAX.effDuration = c(60, rexp(nBlocks)),
                 MAX.r = c(5, 2 + rpois(nBlocks, lambda = 1)),
                 distname.y = "exp", par.y = c(rate = 1 / 100))

  MAX <- Renext:::makeMAXdata(rd)
  pari <- parIni.MAX(MAX = MAX, threshold = u)
  ParMAX[i, ] <- pari   
}
## the same for OTS data
u <- 10
nBlocks <- 10
nSim <- 100
ParOTS <- matrix(NA, nrow = nSim, ncol = 2)
colnames(ParOTS) <- c("lambda", "sigma")
rds <- list()

for (i in 1:nSim) {
  rd <- rRendata(threshold = u,
                 effDuration = 1,
                 lambda = 12,
                 OTS.effDuration = rexp(nBlocks, rate = 1 / 10),
                 OTS.threshold = u + rexp(nBlocks, rate = 1 / 10),
                 distname.y = "exp", par.y = c(rate = 1 / 100))
  rds[[i]] <- rd
  OTS <-  Renext:::makeOTSdata(rd)
  pari <- parIni.OTS(OTS = OTS, threshold = u)
  ParOTS[i, ] <- pari   
}
```
