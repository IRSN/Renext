# Compute return levels and confidence limits for a "Renouv" object

Compute return levels and confidence limits for an object of class
"Renouv".

## Usage

``` r
# S3 method for class 'Renouv'
predict(object,
        newdata = c(10, 20, 50, 100, 200, 500, 1000),
        cov.rate = TRUE,
        level = c(0.95, 0.7),
        prob = FALSE,
        trace = 1, eps = 1e-06,
        ...)
```

## Arguments

- object:

  An object of class `"Renouv"` typically created by using the `Renouv`
  function.

- newdata:

  The return period at which return levels and confidence bounds are
  wanted.

- cov.rate:

  If `FALSE`, the delta method will not take into account the
  uncertainty on the event rate `lambda` of the Poisson process. Note
  however that when `distname.y` is `"exponential"` and when no `MAX` or
  `OTS` data is used, the value of `cov.rate` has no impact for now,
  because the delta method is not used then.

- level:

  Confidence levels as in other 'predict' methods (not percentages).

- prob:

  If `TRUE` a `prob` column is found in the returned data frame. This
  column can be used to find which quantile was used to compute the
  return level.

- trace:

  Some details are printed when `trace` is not zero.

- eps:

  Level of perturbation used to compute the numerical derivatives in the
  delta method.

- ...:

  Further arguments passed to or from other methods.

## Details

Unless in some very special cases, the confidence limits are
approximated ones computed by using the delta method with numerical
derivatives.

## Value

A data frame with the expected return levels (col. named `"quant"`) at
the given return periods, and confidence limits. The returned object has
an `infer.method` attribute describing the method used to compute the
confidence limits.

## References

Coles S. (2001) *Introduction to Statistical Modelling of Extremes
Values*, Springer.

## Author

Yves Deville

## Note

Despite of its name, this method does not compute true predictions. A
return period is to be interpreted as an average interevent time rather
than the duration of a specific period of time. For instance, the
expected return level for a given return period with length 100 years is
the level that would be on average exceeded once every 100 years
(assuming that the model description in `object` is correct).

## See also

[`Renouv`](https://irsn.github.io/Renext/reference/Renouv.md) to fit
`Renouv` model.

## Examples

``` r
## Use Brest data
fit <- Renouv(Brest)
#> Special inference for the exponential case without history
#> Warning: uncertainty on the rate not taken into account yet  in the exponential with no history case

pred <- predict(fit, newdata = c(100, 125, 150, 175, 200),
                level = c(0.99, 0.95))
#> Special inference for the exponential case without history
#> Warning: uncertainty on the rate not taken into account yet  in the exponential with no history case
```
