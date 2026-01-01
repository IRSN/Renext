# Log-likelihood of a "Renouv" object

Log-likelihood, AIC, BIC and number of observations of an object of
class "Renouv".

## Usage

``` r
# S3 method for class 'Renouv'
AIC(object, ..., k = 2)
# S3 method for class 'Renouv'
BIC(object, ...)
# S3 method for class 'Renouv'
logLik(object, ...)
# S3 method for class 'Renouv'
nobs(object, ...)
```

## Arguments

- object:

  Object of class `"Renouv"`.

- k:

  See [`AIC`](https://rdrr.io/r/stats/AIC.html).

- ...:

  Not used yet.

## Author

Yves Deville

## Note

`logLik`, `AIC` and `BIC` can be used with an object of class `"Renouv"`
which makes use of historical data. In this case, the number of
observations may be misleading since a single historical observation may
concern dozens of years and thus have a much greater impact on the
estimation of the tail than an "ordinary" observation.

## Caution

Comparing log-likelihoods, AIC or BIC for different `Renouv` objects
makes sense only when these share the same data and the same threshold.

## See also

The [`AIC`](https://rdrr.io/r/stats/AIC.html),
[`nobs`](https://rdrr.io/r/stats/nobs.html) generic functions.
