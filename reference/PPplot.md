# Diagnostic plots for Renouv objects

Diagnostic plots for `Renouv` objects.

## Usage

``` r
PPplot(x, ...)

QQplot(x, ...)

# S3 method for class 'Renouv'
PPplot(x,
       showHist = FALSE,
       legend = FALSE,
       par = NULL,
       posOptions = NULL,
       ...)

# S3 method for class 'Renouv'
QQplot(x,
       showHist = FALSE,
       legend = FALSE,
       par = NULL,
       posOptions = NULL,
       ...)
```

## Arguments

- x:

  Object containing a fitted model.

- legend:

  Should a legend be shown to identify historical blocks? NOT
  IMPLEMENTED YET.

- par:

  A list of graphical parameters as returned by
  [`RLpar`](https://irsn.github.io/Renext/reference/RLpar.md) and used
  to control the appearance of points. NOT IMPLEMENTED YET.

- posOptions:

  A pair list to be passed as list of formals to the
  [`SandT`](https://irsn.github.io/Renext/reference/SandT.md) function
  computing the plotting positions.

- showHist:

  If `TRUE`, historical information contained in the object `x` (if any)
  will be shown using special plotting positions as computed by
  [`SandT`](https://irsn.github.io/Renext/reference/SandT.md).

- ...:

  Other arguments to be passed to `plot`.

## Value

No value returned.

## Author

Yves Deville

## See also

[`SandT`](https://irsn.github.io/Renext/reference/SandT.md) for the
computation of the plotting positions used with historical data.
