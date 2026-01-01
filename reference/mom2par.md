# Parameters from moments

Compute parameters from (theoretical) moments

## Usage

``` r
mom2par(densfun = "exponential",
           mean,
           sd = NA)
```

## Arguments

- densfun:

  Name of the distribution. This can be at present time:
  `"exponential"`, `"weibull"`, `"gpd"`, `"gamma"`,
  `"negative binomial"`.

- mean:

  Theoretical mean (expectation) of the distribution. Can be a vector,
  in which case the parameters will be vectors.

- sd:

  Standard deviation.

## Details

For some distributions like Weibull, it is necessary to find a numerical
solution since the parameters have no closed form expression involving
the moments.

## Value

A named list containing the parameters values e.g. with names `shape`
and `scale`. When `mean` or `sd` is vector the list contains vectors.

## Author

Yves Deville

## Note

The name of the formal argument `densfun` is for compatibility with
`fitdistr` from the MASS package. However, unlike in `fitdistr` this
formal can not be given a density value, i.e. an object of the class
`"function"` such as `dnorm`.

## Examples

``` r
## Weibull
mom2par(densfun = "weibull", mean = 1, sd = 2) 
#> $shape
#> [1] 0.5426925
#> 
#> $scale
#> [1] 0.5752495
#> 
## Genrealised Pareto
mom2par(densfun = "gpd", mean = 1, sd = 2)
#> $shape
#> [1] 0.375
#> 
#> $scale
#> [1] 0.625
#> 
#> $loc
#> [1] 0
#> 
## Gamma
mom2par(densfun = "gamma", mean = 1:10, sd = 1)
#> $shape
#>  [1]   1   4   9  16  25  36  49  64  81 100
#> 
#> $scale
#>  [1] 1.0000000 0.5000000 0.3333333 0.2500000 0.2000000 0.1666667 0.1428571
#>  [8] 0.1250000 0.1111111 0.1000000
#> 
```
