# CV2 test of exponentiality

Test of exponentiality based on the squared coefficient of variation.

## Usage

``` r
CV2.test(x, method = c("num", "sim", "asymp"), nSamp = 15000)
```

## Arguments

- x:

  Numeric vector giving the sample.

- method:

  Method used to compute the \\p\\-value. Can be `"asymp"`, `"num"` or
  `"sim"` as in
  [`LRExp.test`](https://irsn.github.io/Renext/reference/LRExp.test.md).

- nSamp:

  Number of samples used to compute the \\p\\-value when `method` is
  `"sim"`.

## Value

A list of test results.

- statistic, p.value:

  The test statistic, i.e. the squared coefficient of variation
  \\\textrm{CV}^2\\ and the \\p\\-value.

- df:

  The sample size.

- method:

  Description of the test method.

## Note

This test is sometimes referred to as *Wilk's exponentiality test* or as
*WE1 test*. It works quite well for a Lomax alternative (i.e. GPD with
shape \\\xi \>0\\), and hence can be compared to Jackson's test and the
Likelihood-Ratio (LR) test of exponentiality. However, this test has
lower power that of the two others while having a comparable computation
cost due to the evaluation of the Greenwood's statistic distribution.

## Details

The distribution of \\\textrm{CV}^2\\ is that of *Greenwood's statistic*
up to normalising constants. It approximately normal with expectation
\\1\\ and standard deviation \\2/\sqrt{n}\\ for a large sample size `n`.
Yet the convergence to the normal is known to be *very slow*.

## References

S. Ascher (1990) "A Survey of Tests for Exponentiality" *Commun.
Statist. Theory Methods*, 19(5), pp. 1811-1525.

## Author

Yves Deville

## See also

The function [`CV2`](https://irsn.github.io/Renext/reference/CV2.md)
that computes the statistic and
[`LRExp.test`](https://irsn.github.io/Renext/reference/LRExp.test.md) or
[`Jackson.test`](https://irsn.github.io/Renext/reference/Jackson.test.md)
for functions implementing comparable tests or exponentiality with the
same arguments.

## Examples

``` r
n <- 30; nSamp <- 500
X <- matrix(rexp(n * nSamp), nrow = nSamp, ncol = n)
pVals <- apply(X, 1, function(x) CV2.test(x)$p.value)
plot(pVals)  ## should be uniform on (0, 1)
```
