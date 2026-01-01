# Derivation of probability functions with respect to the parameters

Derivation of probability functions with respect to the parameters by
using closed forms.

## Usage

``` r
parDeriv(par, x, distname, sum = TRUE)
```

## Arguments

- par:

  Vector of parameter values.

- x:

  Observations or data at which the derivatives are to be computed.

- distname:

  Name of the distribution. See **Details**.

- sum:

  Logical. If `TRUE`, a summation over the element of `x` is carried.
  Otherwise, the first dimension of the result corresponds to the
  elements of `x`.

## Details

Only a few distributions are and will be available. For now, these are:
the two-parameter Weibull `c("shape", "scale")`, the two-parameter
Generalised Pareto, `c("scale", "shape")` and the two-parameter Lomax
and maxlo distributions.

## Value

A list of arrays containing the first and second order derivatives.

- derLogdens, der2Logdens:

  Derivatives of the log-density \\\log f(x)\\.

- derSurv, der2Surv:

  Derivatives of the survival function \\S(x)\\.

When `x` has length \\n\\ and the distribution depends on \\p\\
parameters, the arrays of first and second order derivatives have
dimension \\n \times p\\ and \\n \times p \times p\\ when `sum` is
`FALSE`. If `sum` is `TRUE` the summation drops the first dimension and
the arrays are \\p\\ and \\p \times p\\.

## References

See the *Renext Computing Details* document.

## Author

Yves Deville

## See also

[`Maxlo`](https://irsn.github.io/Renext/reference/Maxlo.md) and
[`Lomax`](https://irsn.github.io/Renext/reference/Lomax.md).

## Examples

``` r
set.seed(1234)
distname <- "maxlo"
if (distname == "weibull") {
    logL <- function(par) {
        sum(dweibull(x, shape = par["shape"], scale = par["scale"], log = TRUE))
    }
    sumS <- function(par) {
        sum(pweibull(x, shape = par["shape"], scale = par["scale"],
                     lower.tail = FALSE))
    }
    pars <- c("shape" = rexp(1), "scale" = 1000 * rexp(1))
    x <- rweibull(n = 100, shape = pars["shape"], scale = pars["scale"])
    Der <- parDeriv(par = pars, x = x, distname = "weibull") 
} else if (distname == "gpd") {
    require(evd)
    logL <- function(par) {
        sum(dgpd(x, loc = 0, shape = par["shape"], scale = par["scale"],
                 log = TRUE))
    }
    sumS <- function(par) { 
        sum(pgpd(x, loc = 0, shape = par["shape"], scale = par["scale"],
                 lower.tail = FALSE))
    }
    pars <- c("scale" = 1000 * rexp(1),
              "shape" = runif(1, min = -0.4, max = 0.4))
    x <- rgpd(n = 100, loc = 0, shape = pars["shape"], scale = pars["scale"])
    Der <- parDeriv(par = pars, x = x, distname = "gpd")
} else if (distname == "lomax") {
    logL <- function(par) {
        sum(dlomax(x, shape = par["shape"], scale = par["scale"], log = TRUE))
    }
    sumS <- function(par) { 
        sum(plomax(x, shape = par["shape"], scale = par["scale"],
                   lower.tail = FALSE))
    }
    pars <- c( "shape" = 1 + rexp(1), "scale" = 1000 * rexp(1))
    x <- rlomax(n = 100, shape = pars["shape"], scale = pars["scale"])
    Der <- parDeriv(par = pars, x = x, distname = "lomax") 
} else if (distname == "maxlo") {
    logL <- function(par) {
        sum(dmaxlo(x, shape = par["shape"], scale = par["scale"], log = TRUE))
    }
    sumS <- function(par) { 
        sum(pmaxlo(x, shape = par["shape"], scale = par["scale"],
                   lower.tail = FALSE))
    }
    pars <- c( "shape" = 2.5 + runif(1), "scale" = 100 * rexp(1))
    x <- rmaxlo(n = 100, shape = pars["shape"], scale = pars["scale"])
    Der <- parDeriv(par = pars, x = x, distname = "maxlo") 
}

## check logdens
H <- numDeriv::hessian(func = logL, x = pars)
colnames(H) <- names(pars)
Grad <- numDeriv::grad(func = logL, x = pars)

cat("gradient for log density\n")
#> gradient for log density
print(cbind(parDeriv = Der$derLogdens, num = Grad))
#>         parDeriv        num
#> shape  1.7741976  1.7741976
#> scale -0.4560811 -0.4560811

cat("hessian for log density\n")
#> hessian for log density
print(cbind(exact = Der$der2Logdens, num = H))
#>            shape     scale      shape     scale
#> shape -14.638190  2.250876 -14.638190  2.250876
#> scale   2.250876 -0.348458   2.250876 -0.348458

## check survival
HS <- numDeriv::hessian(func = sumS, x = pars)
HS <- (HS + t(HS))/2
colnames(HS) <- names(pars)
GradS <- numDeriv::grad(func = sumS, x = pars)

cat("gradient for Survival\n")
#> gradient for Survival
print(cbind(parDeriv = Der$derSurv, num = GradS))
#>        parDeriv       num
#> shape -8.887358 -8.887358
#> scale  1.189740  1.189740

cat("hessian for Survival\n")
#> hessian for Survival
print(cbind(exact = Der$der2Surv, num = HS))
#>             shape       scale       shape       scale
#> shape  3.60711045 -0.08314355  3.60711045 -0.08314355
#> scale -0.08314355 -0.04585117 -0.08314355 -0.04585117
```
