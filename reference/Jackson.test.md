# Jackson's test of exponentiality

Jackson's test of exponentiality

## Usage

``` r
Jackson.test(x, method = c("num", "sim", "asymp"), nSamp = 15000)
```

## Arguments

- x:

  numeric vector or matrix.

- method:

  Character: choice of the method used to compute the \\p\\-value. See
  the **Details** section.

- nSamp:

  Number of samples used to compute the \\p\\-value if `method` is
  `"sim"`.

## Value

A list of results.

- statistic, p.value:

  The statistic and \\p\\-value.

- df:

  Number \\n\\ of observations.

- method:

  Description of the test implemented, regardless of how the \\p\\-value
  has been computed.

## Details

Compute the Jackson's test of exponentiality. The test statistic is the
ratio of weighted sums of the order statistics. Both sums can also be
written as weighted sums of the *scalings*.

The Jackson's statistic for a sample of size \\n\\ of the exponential
distribution can be shown to be approximately normal. More precisely
\\\sqrt{n}(J_n -2)\\ has approximately a standard normal distribution.
This distribution is used to compute the \\p\\-value when `method` is
`"asymp"`. When `method` is `"num"`, a numerical approximation of the
distribution is used. Finally, when `method` is `"sim"` the \\p\\-value
is computed by simulating `nSamp` samples of size `length(x)` and
estimating the probability to have a Jackson's statistic larger than
that of the 'observed' `x`.

## Note

Jackson's test of exponentiality works fine for a Lomax alternative (GPD
with heavy tail). It then reaches nearly the same power as a Likelihood
Ratio (LR) test, see Kozubowski et al. It can be implemented more easily
than the LR test because simulated values of the test statistic can be
obtained quickly enough to compute the \\p\\-value by simulation.

## Author

Yves Deville

## See also

The [`Jackson`](https://irsn.github.io/Renext/reference/Jackson.md)
function computing the statistic and the
[`LRExp.test`](https://irsn.github.io/Renext/reference/LRExp.test.md)
function.

## References

J. Beirlant and T. de Weit and Y. Goegebeur(2006) "A Goodness-of-fit
Statistic for Pareto-Type Behaviour", *J. Comp. Appl. Math.*, 186(1),
pp. 99-116.

T.J. Kozubowski, A. K. Panorska, F. Qeadan, A. Gershunov and D. Rominger
(2009) "Testing Exponentiality Versus Pareto Distribution via Likelihood
Ratio" *Comm. Statist. Simulation Comput.* 38(1), pp. 118-139.

## Examples

``` r
set.seed(1234)
x <- rGPD(n = 50, loc = 0, scale = 1, shape = 0.1)
Jackson.test(x, method = "num")$p.value
#> [1] 0.534
Jackson.test(x, method = "asymp")$p.value
#> [1] 0.775
Jackson.test(x, method = "sim")$p.value
#> [1] 0.539
```
