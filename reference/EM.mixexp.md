# Expectation-Maximisation for a mixture of exponential distributions

Experimental function for Expectation-Maximisation (EM) estimation

## Usage

``` r
EM.mixexp(x, m = 2)
```

## Arguments

- x:

  Sample vector with values `>0`.

- m:

  Number of mixture components.

## Details

The EM algorithm is very simple for exponential mixtures (as well as for
many other mixture models).

According to a general feature of EM, this iterative method leads to
successive estimates with increasing likelihood but which may converge
to a local maximum of the likelihood.

## Value

List with

- estimate:

  Estimated values as a named vector.

- logL:

  Vector giving the log-likelihood for successive iterations.

- Alpha:

  Matrix with `m` columns giving probability weights for successive
  iterations. Row with number `it` contains the `m` probabilities at
  iteration `it`.

- Theta:

  Matrix with `m` columns giving the estimates of the `m` expectations
  for the successive iterations

## Author

Yves Deville

## Note

The estimation is done for expectation (inverse rates) but the
`estimate` vector in the result contains rates for compatibility reasons
(e.g with exponential).

## See also

[`mom.mixexp2`](https://irsn.github.io/Renext/reference/mom.mixexp2.md)
and
[`ini.mixexp2`](https://irsn.github.io/Renext/reference/ini.mixexp2.md)
for "cheap" estimators when `m = 2`.

## Examples

``` r
set.seed(1234)
x <- rmixexp2(n = 100, prob1 = 0.5, rate2 = 4)
EM.mixexp(x) -> res
res$estimate
#>     prob1     prob2     rate1     rate2 
#> 0.5844083 0.4155917 1.2991665 4.7291873 
matplot(res$Theta, type = "l", lwd = 2,
        xlab = "iteration", ylab = "theta",
        main = "exponential inverse rates")
```
