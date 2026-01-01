# Fit a two-parameters Generalised Pareto Distribution from a sample

Fit a two-parameters Generalised Pareto Distribution from a sample.

## Usage

``` r
fGPD(x,
     info.observed = TRUE,
     shapeMin = -0.8,
     dCV = 1e-04,
     cov = TRUE,
     trace = 0)
```

## Arguments

- x:

  The sample data vector.

- info.observed:

  Logical. The observed information matrix is used when `TRUE` and the
  expected information matrix is used when `FALSE`.

- shapeMin:

  Lower bound on the shape parameter. This must be `>= -1.0` since
  otherwise the ML estimate is obtained with the `scale` parameter equal
  to `max(x)`.

- dCV:

  Half-length of a small interval centered on 1.0. When the Coefficient
  of Variation (CV) falls in this interval, an exponential distribution
  is fitted, see **Details**.

- cov:

  Logical. If `FALSE`, a minimal estimation is performed with no
  covariance matrix or derivative returned. This can be useful when a
  large number of ML estimations are required, e.g. to sample from a
  likelihood ratio.

- trace:

  Integer level of verbosity. The value `0` prints nothing.

## Details

This function mainly relies on the
[`flomax`](https://irsn.github.io/Renext/reference/flomax.md) and
[`fmaxlo`](https://irsn.github.io/Renext/reference/fmaxlo.md) functions.
When `CV` is larger than `1.0 + dCV`, a Lomax distribution is fitted by
Maximum Likelihood using a concentrated log-likelihood. When instead
`CV` is smaller than `1.0 - dCV`, a maxlo distribution is fitted.
Finally, when `CV -1.0` has absolute value `<= dCV`, an exponential
distribution is fitted. In all cases, the result is translated into a
parameter vector for the GPD.

Note that when `CV` is close to `1.0`, fitting a Lomax or a maxlo
distribution can lead to problems because the estimated values of the
shape and scale parameter are large, and because the concentrated
log-likelihood is a flat function of the scale parameter.

## Value

A list

- estimate:

  Vector containing the estimated values of the unknown parameters.

- CV:

  The coefficient of variation of `x` computed using `length(x)` as
  denominator in the variance estimation.

- logLik, dlogLik:

  The maximised value of the log-likelihood and the vector of its first
  order derivatives, which should be close to zero.

- sd, cov:

  Vector of approximated standard deviations and covariance matrix for
  the estimated parameters. These are based on the inversion of expected
  information matrix.

- sd, cov:

  Vector of standard deviations and covariance matrix for the estimates
  if the `cov` formal is `TRUE`.

- cvg:

  Logical. Was convergence reached? This logical flag is set to `TRUE`
  and remains for compatibility reasons.

## References

See the *Renext Computing Details* document.

## Author

Yves Deville

## Note

It may happen that the estimated shape parameter is `< -0.5`, in which
case the expected information matrix can not be computed, nor does the
covariance matrix and standard deviations. In this case, the `cov` and
`sd` objects contain `NA` values. This problem can arise also when the
shape parameter is greater than but close to the value `-0.5`. Even when
`info.observed` is `TRUE`, the information matrix, covariance and
standard deviations are set to `NA`.

When the true (unknown) value is is `< -0.5`, the regularity conditions
required in the ML approximated inference do not hold.

The default value of `info.observed` was set to `TRUE` from version
`3.0-1` because standard deviations obtained with this choice are
usually better.

## See also

The [`fmaxlo`](https://irsn.github.io/Renext/reference/fmaxlo.md) and
[`flomax`](https://irsn.github.io/Renext/reference/flomax.md) functions.

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(123456)
n <- 500
ns <- 1000
xi <- runif(ns, min = -0.5, max = 0.5)
X <- matrix(nrow = n, ncol = ns)

for (i in 1:length(xi)) {
  Xi <- rgpd(n, scale = 1, shape = xi[i])
  X[ , i] <- Xi
  res1 <- fGPD(Xi)
  res2 <- try(fpot(Xi, threshold = 0.0))
  if (inherits(res2, "try-error")) {
    cat(res2, "\n")
    break
  }
  logLik1 <- res1$loglik; logLik2 <- logLik(res2)
  if (abs(logLik1 - logLik2) > 0.001) {
    cat(sprintf("i = %d, xi = %7.4f\n", i, xi[i]))
    mat <- rbind(c(res1$estimate[1:2], logLik = logLik1),
                 c(res2$estimate[1:2], logLik = logLik2))
    rownames(mat) <- c("fGPD", "fpot")
    print(mat)
  }
}
} # }
```
