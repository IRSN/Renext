# Variance-covariance matrix of the estimates of a "Renouv" object

Variance-covariance matrix of the estimates of a "Renouv" object.

## Usage

``` r
# S3 method for class 'Renouv'
vcov(object, ...)
```

## Arguments

- object:

  Object of class `"Renouv"`.

- ...:

  Not used at the time.

## Value

A variance-covariance matrix. The rows an columns correspond to the
parameters of the Renouv object. The are the rate `"lambda"` for the
Poisson process, and the parameters of the distribution for the excesses
over the threshold, with names depending on the chosen distribution.

## Author

Yves Deville

## See also

The [`vcov`](https://rdrr.io/r/stats/vcov.html) generic.
