# Moment estimation for the mixture of two exponential distributions

Compute the moment estimation for the tree parameters of the mixture of
two exponential distributions

## Usage

``` r
mom.mixexp2(x)
```

## Arguments

- x:

  Sample. Vector containing values `>0`.

## Details

The three parameters (probability and the two rates) are computed from
the first three moments (theoretical and sample). It can be shown that
the inverse rates are obtained solving a quadratic equation. However the
roots can be negative or complex and the estimates are not valid ones.

## Value

A list with elements

- estimate:

  A vector with named elements `"prob1"`, `"rate1"` and `"rate2"`. When
  the moment estimators are not valid (negative or complex rates), a
  vector of three `NA` is returned.

- method:

  Character `"moments"`.

## References

Paul R. Rider. The Method of Moments Applied to a Mixture of Two
Exponential Distributions. *Ann. Math. Statist.* Vol. 32, Number 1
(1961), 143-147.

## Author

Yves Deville

## Note

The theoretical coefficient of variation (CV) of a mixture of two
exponential distributions always exceeds 100%. When the sample CV is
smallest than 100%, no valid estimates exist since the two first moments
can not be matched.

## See also

See
[`ini.mixexp2`](https://irsn.github.io/Renext/reference/ini.mixexp2.md)
for a more versatile initial estimation.

## Examples

``` r
x <- rmixexp2(n = 100, prob1 = 0.5, rate1 = 1.0, rate2 = 3.0)
est <- mom.mixexp2(x)
```
