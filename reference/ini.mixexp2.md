# Simple estimation for the mixture of two exponential distributions

Compute a simple (preliminary) estimation for the tree parameters of the
mixture of two exponential distributions

## Usage

``` r
ini.mixexp2(x, plot = FALSE)
```

## Arguments

- x:

  Sample: numerical vector with elements `>0`.

&nbsp;

- plot:

  Should a graphic be displayed?

## Details

This function gives estimators using several methods if necessary. The
goal is to find the rates `rate1`, `rate2` and the mixing probability
`prob1` with the 'feasibility' constraints `0 < rate1` `< rate2` and
`0 < prob1 < 1`.

First the method of moments is used. If the estimates are feasible they
are returned with `method` `=` `"moments"`. If not, the estimates are
derived using two linear regressions. A regression without constant
using only the smallest values gives an estimator of the mean rate. A
regression using only the largest values gives `rate1` and `prob1`. Yet
the constraints must be fulfilled. If they are, the estimates are
returned (together with `method =` `"Hreg"` suggesting a cumulative
hazard regression). If not, a (poor) default estimate is returned with
`method =` `"arbitrary"`.

## Value

A list

- estimate:

  A vector with named elements `"prob1"`, `"rate1"` and `"rate2"`.

- method:

  The method that really produced the estimators.

## Author

Yves Deville

## Note

The method of moments is implemented in `mom.mixexp2`. Further
investigations are needed to compare the estimators (moments or Hreg)
and select the best strategy.

Note that this function returns the estimate within a list and no longer
as a vector with named elements as was the case before.

## See also

See [`MixExp2`](https://irsn.github.io/Renext/reference/MixExp2.md),
[`mom.mixexp2`](https://irsn.github.io/Renext/reference/mom.mixexp2.md).

## Examples

``` r
set.seed(1234)
x <- rmixexp2(n = 100, prob1 = 0.5, rate2 = 4)
res <- ini.mixexp2(x, plot = TRUE)

```
