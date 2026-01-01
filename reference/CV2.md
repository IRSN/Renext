# Squared Coefficient of Variation

Squared Coefficient of Variation.

## Usage

``` r
CV2(x)
```

## Arguments

- x:

  Numeric vector or matrix.

## Value

Numeric vector of the squared coefficients of variation.

## Details

Compute the squared Coefficient of Variation of one or several samples
provided as a numeric vector or matrix.

## Note

The squared coefficient of variation is the ratio \\S^2/\bar{X}^2\\
where \\\bar{X}\\ and \\S^2\\ are the sample mean and the sample
variance. The variance is computed using the sample size \\n\\ as
denominator, rather than the usual \\n-1\\.

## Examples

``` r
n <- 30; nSamp <- 500
X <- matrix(rexp(n * nSamp), nrow= nSamp, ncol = n)
W <- CV2(X)
plot(density(W), main = "CV2 of exponential samples")
```
