# Summary and print methods for "Rendata" objects

Summary method for "Rendata" objects representing data to be used in
renouvellement models.

## Usage

``` r
# S3 method for class 'Rendata'
print(x, ...)
 
  # S3 method for class 'Rendata'
summary(object, ...)

  # S3 method for class 'summary.Rendata'
print(x, ...)
```

## Arguments

- object:

  An object with class `"Rendata"`.

- x:

  An object of class `"summary.Rendata"`, i.e. a result of a call to
  `summary.Rendata`.

- ...:

  Further arguments passed to or from other methods.

## Examples

``` r
## Brest example: no historical data
summary(Brest)
#> o Dataset Surge Heights at Brest (France)
#>    data 'Brest', variable 'Surge' (cm) 
#> 
#> o OT data (main sample) from  1846-01-01  to  2009-01-01  (eff. dur. 147.62 years)
#>  
#>          n       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#> 1289.00000   30.02200   33.64700   38.30600   41.76007   46.58100  143.94900 
#> 
#> o missing 'OT' periods, total 15.38 years 
#> 
#>            n         Min.      1st Qu.       Median         Mean      3rd Qu. 
#> 43.000000000  0.002737851  0.016427105  0.038329911  0.357639718  0.086242300 
#>         Max. 
#>  8.418891170 
#> 
#> o no 'MAX' historical data 
#> 
#> o no 'OTS' historical data 
#> 

## Garonne example:  with historical data
summary(Garonne)
#> o Dataset La Garonne river flow
#>    data 'Garonne', variable 'Flow' (m3/s) 
#> 
#> o OT data (main sample) from  1913-01-01  to  1978-01-01  (eff. dur.  65.00 years)
#>  
#>        n     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#>  151.000 2530.000 2900.000 3200.000 3591.675 3995.000 7500.000 
#> 
#> o no missing OT periods 
#> 
#> o 'MAX' historical info: 1 blocks, 12 obs., total duration = 143.09 years 
#> 
#> o no 'OTS' historical data 
#> 
```
