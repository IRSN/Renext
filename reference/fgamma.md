# ML estimation of the Gamma distribution

Fast Maximum Likelihood estimation of the Gamma distribution.

## Usage

``` r
fgamma(x, check.loglik = FALSE)
```

## Arguments

- x:

  Sample vector to be fitted. Should contain only positive non-NA
  values.

- check.loglik:

  If `TRUE`, the log-likelihood is recomputed using `dgamma` function
  with `log =` `TRUE`. The result is returned as a list element.

## Details

The likelihood is concentrated with respect to the scale parameter. The
concentrated log-likelihood is a strictly concave function of the shape
parameter which can easily maximised numerically.

## Value

A list with the following elements

- estimate:

  Parameter ML estimates.

- sd:

  Vector of (asymptotic) standard deviations for the estimates.

- loglik:

  The maximised log-likelihood.

- check.loglik:

  The checked log-likelihood.

- cov:

  The (asymptotic) covariance matrix computed from theoretical or
  observed information matrix.

- info:

  The information matrix.

## Author

Yves Deville

## Note

The distribution is fitted by using the `scale` parameter rather than
`rate` (inverse of `scale`).

## See also

[`GammaDist`](https://rdrr.io/r/stats/GammaDist.html) in the stats
package.

## Examples

``` r
set.seed(9876)
alpha <- 0.06
beta <- rexp(1)
n <- 30
x <- rgamma(n, shape = alpha, scale = beta)
fit <- fgamma(x, check.loglik = TRUE)

## compare with MASS results
if (require(MASS)) {
   fit.MASS <- fitdistr(x, densfun = "gamma")
   rate <- 1 / fit$estimate["scale"]
   est <- c(fit$estimate, rate = rate)
   der <- rate * rate ## derivative of rate w.r.t scale
   sdest <- c(fit$sd, rate = der * fit$sd["scale"])
   tab <- rbind(sprintf(" %10.8f ", est),
                sprintf("(%10.8f)", sdest))
   colnames(tab) <- c("shape", "scale", "rate")
   rownames(tab) <- c("est", "sd")
   noquote(tab)
}
#> Loading required package: MASS
#>     shape        scale        rate         
#> est  0.05534680   0.05686669   17.58498593 
#> sd  (0.01037116) (0.04540000) (14.03912135)
```
