# Add a small amount of noise to a numeric vector

Add a small amount of noise to a numeric vector keeping all the values
above the given threshold.

## Usage

``` r
OTjitter(x, threshold = NULL)
```

## Arguments

- x:

  The numeric vector to which *jitter* should be added.

- threshold:

  A threshold above which all elements of the modified vector must stay.

## Value

A vector with the same length and nearly the same values as `x`. As in
[`jitter`](https://rdrr.io/r/base/jitter.html), a small amount of noise
is added to each value of `x`. The noise level is adjusted so that every
noisy value remains above the specified threshold. When the a value is
very close to the threshold, only a very small amount of negative noise
can be added.

## Note

The aim of this function is to remove possible ties in experimental OT
data. Ties cause problems or warnings in some goodness-of-fit tests such
as Kolmogorov-Smirnov.

## Author

Yves Deville

## See also

[`jitter`](https://rdrr.io/r/base/jitter.html)

## Examples

``` r
## Garonne data (heavily rounded)
x <- Garonne$OTdata$Flow
min(x) 
#> [1] 2530
xmod <- OTjitter(x, threshold = 2500)
length(x)
#> [1] 151
nlevels(as.factor(x))
#> [1] 49
nlevels(as.factor(xmod))
#> [1] 151
max(abs(x-xmod))
#> [1] 5.867549
```
