# Likelihood Ratio test for the Gumbel distribution

Likelihood Ratio test of Gumbel vs. GEV

## Usage

``` r
LRGumbel.test(x,
                 alternative = c("frechet", "GEV"),
                 method = c("num", "sim", "asymp"),
                 nSamp = 1500,
                 simW = FALSE)
```

## Arguments

- x:

  Numeric vector of sample values.

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

The asymptotic distribution of the Likelihood-ratio statistic is known.
For the GEV alternative, this is a chi-square distribution with one df.
For the Fréchet alternative, this is the distribution of a product
\\XY\\ where \\X\\ and \\Y\\ are two independent random variables
following a Bernoulli distribution with probability parameter \\p =
0.5\\ and a chi-square distribution with one df.

- When `method` is `"num"`, a numerical approximation of the
  distribution is used.

- When `method` is `"sim"`, `nSamp` samples of the Gumbel distribution
  with the same size as `x` are drawn and the LR statistic is computed
  for each sample. The \\p\\-value is simply the estimated probability
  that a simulated LR is greater than the observed LR. This method
  requires more computation time than the tow others.

- Finally when `method` is `"asymp"`, the asymptotic distribution is
  used.

## Note

For the Fréchet alternative, the distribution of the test statistic has
*mixed type*: it can take any positive value as well as the value \\0\\
with a positive probability mass. The probability mass is the
probability that the ML estimate of the GEV shape parameter is negative.

When `method` is `"sim"`, the computation can be slow because each of
the `nSamp` simulated values requires two optimisations. The `"asymp"`
method provides an acceptable precision for \\n \geq 50\\, and may even
be used for \\n \geq 30\\.

## Author

Yves Deville

## Examples

``` r
set.seed(1234)
x <- rgumbel(60)
res <- LRGumbel.test(x)
```
