# Plotting positions for exponential return levels

Plotting positions for exponential return level plots.

## Usage

``` r
Hpoints(n)
```

## Arguments

- n:

  Sample size.

## Value

Numeric vector of plotting positions with length `n`.

## Details

The plotting positions are numeric values to use as the abscissae
corresponding to the order statistics in an exponential return level
plot. They range from 1 to about \\\log n\\. They can be related to the
plotting positions given by
[`ppoints`](https://rdrr.io/r/stats/ppoints.html).

The returned vector \\\mathbf{H}\\ has elements \$\$H\_{i} =
\frac{1}{n} + \frac{1}{n-1} + \dots + \frac{1}{n + 1 -i}\$\$ for \\1
\leq i \leq n\\. This is the expectation of the \\i\\-th order statistic
for a sample of the standard exponential distribution, see e.g. chap. 4
of Embrechts et al.

## References

Embrechts P., Klüppelberg C. and Mikosch T. (1997) *Modelling Extremal
Events for Insurance and Finance*. Springer.

## Author

Yves Deville

## Note

For \\n\\ large enough, the largest value \\H_n\\ is approximately
\\\gamma + \log n\\ where \\\gamma\\ is the Euler-Mascheroni constant,
and \\\exp H_n\\ is about \\1.78 n\\. Thus if the Hpoints are used as
plotting positions on a return level plot, the largest observation has a
return period of about \\1.78 n\\ years.

## See also

[`ppoints`](https://rdrr.io/r/stats/ppoints.html).

## Examples

``` r
n <- 30
set.seed(1234)
x <- rGPD(n, shape = 0.2)
plot(exp(Hpoints(n)), sort(x), log = "x",
     main = "Basic return level plot")

```
