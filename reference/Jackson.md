# Jackson's statistic

Jackson's statistic for the exponentiality test.

## Usage

``` r
Jackson(x, norm = FALSE)
```

## Arguments

- x:

  Numeric vector or matrix. In the second case, rows are considered as
  samples.

- norm:

  Logical: if `TRUE`, the statistic is normalized by using the
  *asymptotic* mean and standard deviation, respectively 2 and 1.

## Value

A numeric vector of length `1` when `x` is a vector, or with length
`nrow(x)` when `x` is a matrix.

## Details

The value(s) of the statistic are the ratio of two weighted means of the
order statistics.

## References

J. Beirlant and T. de Weit and Y. Goegebeur(2006) A Goodness-of-fit
Statistic for Pareto-Type Behaviour, *J. Comp. Appl. Math.*, 186(1), pp.
99-116
