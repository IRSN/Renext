# Compute an analysis of deviance table for two nested Renouv objects

Compute an analysis of deviance table for two nested Renouv objects

## Usage

``` r
# S3 method for class 'Renouv'
anova(object, object1, trace = 1L, ...)
```

## Arguments

- object:

  A `Renouv` model as fitted with
  [`Renouv`](https://irsn.github.io/Renext/reference/Renouv.md).

- object1:

  A `Renouv` object such that `object` is nested in `object1`.

- trace:

  Level of verbosity. The value `0` prints nothing.

- ...:

  Not used yet.

## Value

An object of class `"anova"` inheriting from class `"data.frame"`.

## See also

[`anova`](https://rdrr.io/r/stats/anova.html),
[`LRExp.test`](https://irsn.github.io/Renext/reference/LRExp.test.md).

## Details

Of special interest is the case when the distribution of the excesses
used in `object` is exponential while `object1` uses a two-parameters
alternative in the GPD family. We know then that the convergence to the
asymptotic distribution is slow, and a numerical approximation of the
true distribution of the test statistic is used when possible, i.e. when
the objects do not use MAX or OTS data and the number of exceedances is
between 8 and 500.

## Note

The deviance of the models can not be interpreted: only the difference
of the deviance is meaningful.

## Examples

``` r
## test using historical data
fit1Exp <- Renouv(Garonne,  distname.y = "exponential", plot = FALSE)
fit1GPD <- Renouv(Garonne, distname.y = "GPD", plot = FALSE)
anova(fit1Exp, fit1GPD)
#> Models: 
#>   o 'fit1Exp' with exceedances dist. "exponential"
#>   o 'fit1GPD' with exceedances dist. "GPD"
#> 
#> Method used:  asymptotic approximation 
#> 
#> Analysis of Deviance Table
#> 
#>         df deviance      W  Pr(>W)  
#> fit1Exp  2   2399.0                 
#> fit1GPD  3   2393.4 5.5856 0.01811 *
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## test without using historical data
x <- Garonne$OTdata$Flow
dur <- Garonne$OTinfo$effDuration

fit2Exp <- Renouv(x,  threshold = 2700,  effDuration = dur,
                  distname.y = "exponential", plot = FALSE)
#> Special inference for the exponential case without history
#> Warning: uncertainty on the rate not taken into account yet  in the exponential with no history case
fit2GPD <- Renouv(x, threshold = 2700, effDuration = dur,
                  distname.y = "GPD", plot = FALSE)
anova(fit2Exp, fit2GPD)
#> Models: 
#>   o 'fit2Exp' with exceedances dist. "exponential"
#>   o 'fit2GPD' with exceedances dist. "GPD"
#> 
#> Method used:  numerical approximation 
#> 
#> Analysis of Deviance Table
#> 
#>         df deviance      W Pr(>W)
#> fit2Exp  2   2026.7              
#> fit2GPD  3   2024.7 2.0036 0.1743
```
