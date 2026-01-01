# Likelihood Ratio statistic for exponential vs. GPD

Likelihood Ratio statistic for the exponential distribution vs. GPD.

## Usage

``` r
LRExp(x, alternative = c("lomax", "GPD", "gpd", "maxlo"))
```

## Arguments

- x:

  Numeric vector of positive sample values. For the POT context, this
  should be the vector of excesses over the threshold.

- alternative:

  Character string describing the alternative hypothesis

## Value

The LR statistic value.

## Details

The Likelihood-Ratio statistic is actually \\W:=-2 \log \textrm{LR}\\
where LR is the ratio of the likelihoods *exponential* to *alternative
distribution*.

## Note

When the alternative is `"lomax"` or `"maxlo"`, the statistic has a
distribution of *mixed type* under the null hypothesis of
exponentiality. This is a mixture of a distribution of continuous type
(with positive values) and of a Dirac mass at LR = 0. The probability
mass \\\textrm{Pr}\\\textrm{LR} = 0\\\\ can be computed using the
[`pGreenwood1`](https://irsn.github.io/Renext/reference/pGreenwood1.md)
function. More precisely, the probability mass is `pGreenwood1(n)` for
the Lomax alternative and `1.0 - pGreenwood1(n)` for the maxlo
alternative, where `n` is the sample size `length(x)`.

## See also

[`LRExp.test`](https://irsn.github.io/Renext/reference/LRExp.test.md)
for the related LR test of exponentiality.
