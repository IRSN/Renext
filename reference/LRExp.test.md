# Likelihood Ratio test of exponentiality vs. GPD

Likelihood Ratio test of exponentiality vs. GPD.

## Usage

``` r
LRExp.test(x,
              alternative = c("lomax", "GPD", "gpd", "maxlo"),
              method = c("num", "sim", "asymp"),
              nSamp = 15000,
              simW = FALSE)
```

## Arguments

- x:

  Numeric vector of positive sample values. For the POT context this
  should be the vector of excesses over the threshold.

- alternative:

  Character string describing the alternative distribution.

- method:

  Method used to compute the \\p\\-value.

- nSamp:

  Number of samples for a simulation, if `method` is `"sim"`.

- simW:

  Logical. If this is set to `TRUE` and `method` is `"sim"`, the
  simulated values are returned as an element `W` in the list.

## Value

A list of results with elements `statistic`, `p.value` and `method`.
Other elements are

- alternative:

  Character describing the alternative hypothesis.

- W:

  If `simW` is `TRUE` and `method` is `"sim"` only. A vector of `nSamp`
  simulated values of the statistic \\W := -2 \log \textrm{LR}\\.

## Details

The Lomax and maxlo alternatives correspond to a GPD alternative with
positive shape parameter \\\xi \> 0\\ (Lomax) and GPD with \\\xi \< 0\\
(maxlo).

The *asymptotic* distribution of the Likelihood-ratio statistic is
known. For the GPD alternative, this is a chi-square distribution with
one df. For the Lomax alternative, this is the distribution of a product
\\BC\\ where \\B\\ and \\C\\ are two independent random variables
following a Bernoulli distribution with probability parameter \\p =
0.5\\ and a chi-square distribution with one df.

- When `method` is `"num"`, a numerical approximation of the
  distribution is used. This method is not unlike that used by
  Kozubowski et al., but a different approximation is used. However, if
  `x` has a length \\n \> 500\\, the method is turned to `"asymp"`.

- When `method` is `"sim"`, `nSamp` samples of the exponential
  distribution with the same size as `x` are drawn and the LR statistic
  is computed for each sample. The \\p\\-value is simply the estimated
  probability that a simulated LR is greater than the observed LR.

- Finally when `method` is `"asymp"`, the asymptotic distribution is
  used.

## Note

For the Lomax alternative, the distribution of the test statistic has
*mixed type*: it can take any positive value as well as the value \\0\\
with a positive probability mass. The probability mass is the
probability that the ML estimate of the GPD shape parameter is negative,
and a good approximation of it is provided by the
[`pGreenwood1`](https://irsn.github.io/Renext/reference/pGreenwood1.md)
function. Note that this probability converges to its limit \\0.5\\
*very slowly*, which suggests that the asymptotic distribution provides
poor results for medium sample sizes, say \\\< 100\\.

Similarly for a maxlo alternative, the distribution of the test
statistic has mixed type: it can take any positive value as well as the
value \\0\\ with a positive probability mass approximately given by
`1 -pGreenwood1(n)` where \\n\\ is the sample size.

## Author

Yves Deville

## See also

[`Lomax`](https://irsn.github.io/Renext/reference/Lomax.md),
[`Maxlo`](https://irsn.github.io/Renext/reference/Maxlo.md),
[`GPD`](https://irsn.github.io/Renext/reference/GPD.md) for the
alternatives used here.

## References

T.J. Kozubowski, A. K. Panorska, F. Qeadan, A. Gershunov and D. Rominger
(2009) "Testing Exponentiality Versus Pareto Distribution via Likelihood
Ratio" *Comm. Statist. Simulation Comput.* 38(1), pp. 118-139.

The approximation method used is described in the *Renext Computing
Details* report.

## Examples

``` r
set.seed(1234)
x <- rGPD(n = 50, loc = 0, scale = 1, shape = 0.1)
LRExp.test(x, method = "num")$p.value
#> [1] 0.403011
LRExp.test(x, method = "asymp")$p.value
#> [1] 0.5
if (FALSE) { # \dontrun{
## requires much time
LRExp.test(x, method = "sim")$p.value
} # }
```
