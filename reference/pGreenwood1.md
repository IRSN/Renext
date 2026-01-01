# Probability that the Greenwood's statistic is smaller than one

Probability that the Greenwood's statistic is smaller than one.

## Usage

``` r
pGreenwood1(n)
```

## Arguments

- n:

  Sample size.

## Value

Probability that the Greenwood's statistic is smaller than one. For a
random sample of an exponential distribution with size \\n\\, this is
the probability that the coefficient of variation is less than one, or
the probability that the ML estimate of the GPD shape parameter \\\xi\\
is negative.

## Details

The probability was computed by using the approximation of the quantile
function of the Greenwood's statistic returned by
[`qStat`](https://irsn.github.io/Renext/reference/qStat.md). The result
is found by interpolating the distribution function for \\x = 1\\.

## Author

Yves Deville

## Examples

``` r
n <- 8:500
plot(n, pGreenwood1(n), type = "l", col = "orangered", lwd = 2,
     log ="x", ylim =c(0.5, 0.7), main = "slow convergence to 0.5")
grid() ; abline(h = 0.5, col = "SpringGreen")
```
