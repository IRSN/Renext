# Translate a vector of Gumbel parameters into a vector of parameters for a renewal model

Translate a (named) vector of Gumbel parameters into a vector of
parameters for a renewal model.

## Usage

``` r
gumbel2Ren(parGumbel,
           threshold = NULL,
           lambda = NULL,
           w = 1,
           distname.y = c("exponential"),
           vcovGumbel = NULL,
           jacobian = TRUE,
           plot = FALSE)
```

## Arguments

- parGumbel:

  Named vector of Gumbel parameters, with name `"scale"` and `"shape"`.

- threshold:

  The threshold for the target Renouv parameters.

- lambda:

  The rate for the target Renouv parameters.

- w:

  A block duration for the conversion.

- distname.y:

  The distribution of the excesses.

- vcovGumbel:

  A covariance matrix for the Gumbel parameters.

- jacobian:

  Logical. If `TRUE`, the jacobian is used.

- plot:

  Logical. If `TRUE`, a rough plot will be drawn.

## Details

Given a vector of Gumbel parameters and a block duration, there exits an
infinity of `Renouv` models with exponential excesses leading to the
prescribed Gumbel distributions for the maximum of the marks on a block
with duration `w`. One of these models may be chosen by specifying
either a threshold or a rate `lambda`.

## Value

A vector of Renouv parameters, which can be used with
[`RenouvNoEst`](https://irsn.github.io/Renext/reference/RenouvNoEst.md).

## Author

Yves Deville

## Caution

All Renouv models lead to the same return level curve whatever be the
choice of `threshold` or `lambda`. However, when a covariance matrix is
given, the covariance matrix for the Renouv parameters and consequently
the confidence bounds **depend on the threshold or rate**. So the
computed covariance matrix must in general be considered as putative.

## See also

[`gev2Ren`](https://irsn.github.io/Renext/reference/gev2Ren.md) for a
similar translation from the GEV distribution.
