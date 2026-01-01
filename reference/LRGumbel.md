# Likelihood Ratio statistic for Gumbel vs. GEV

Likelihood Ratio statistic for the Gumbel distribution vs. GEV.

## Usage

``` r
LRGumbel(x, alternative = c("frechet", "GEV"))
```

## Arguments

- x:

  Numeric vector of sample values.

- alternative:

  Character string describing the alternative.

## Value

The LR statistic value.

## Details

The Likelihood-Ratio statistic is actually \\W:=-2 \log \textrm{LR}\\
where LR is the ratio of the likelihoods *Gumbel* to *alternative
distribution*.

## Note

When the alternative is `"frechet"`, the statistic has a distribution of
mixed type under the null hypothesis of a Gumbel distribution.

## Author

Yves Deville

## See also

[`LRGumbel.test`](https://irsn.github.io/Renext/reference/LRGumbel.test.md)
for the related LR test of Gumbelity.
