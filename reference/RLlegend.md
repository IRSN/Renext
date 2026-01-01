# Legend management for return level plots

Legend management for return level plots produced with the `plot` and
`lines` method of the `"Renouv"` class.

## Usage

``` r
RLlegend.ini(x = "topleft", bty = "n", ...)
    RLlegend.show()
```

## Arguments

- x:

  A possible value for the `x` argument of
  [`legend`](https://rdrr.io/r/graphics/legend.html). This will usually
  be a character giving the position e.g, `"topleft"` or `"bottomleft"`.
  See the [`legend`](https://rdrr.io/r/graphics/legend.html) function
  help.

- bty:

  As in [`legend`](https://rdrr.io/r/graphics/legend.html). The default
  value `"n"` differs from the default value of `legend`.

&nbsp;

- ...:

  Other arguments to be kept in the list and passed later to
  [`legend`](https://rdrr.io/r/graphics/legend.html). These arguments
  should be chosen among those of `legend` modifying the global legend
  appearance (e.g., `bg`) but not among those modifying the legend
  content (e.g. `col` `pt.bg`, `legend`, ...) since the content is here
  built semi-automatically.

## Details

This function is to be used in conjunction with
[`plot.Renouv`](https://irsn.github.io/Renext/reference/plot.Renouv.md)
and
[`lines.Renouv`](https://irsn.github.io/Renext/reference/plot.Renouv.md)
methods. It allows the construction of a legend in a semi-automatic
fashion, using the value of the `par` argument of the `plot` and `lines`
methods to specify the legend construction.

Each call to the
[`plot.Renouv`](https://irsn.github.io/Renext/reference/plot.Renouv.md)
or
[`lines.Renouv`](https://irsn.github.io/Renext/reference/plot.Renouv.md)
changes the content of a list variable named `.RLlegend` in a special
environment bound to the package. This list is re-created when
`RLlegend.ini` is called, and is used later to draw a legend on the
active device when `RLlegend.show` is called. Between these two calls,
the `plot` and `lines` methods should be used with their arg `legend`
set to `FALSE`.

## Value

`RLlegend.ini` returns a copy of the variable which is set.

`RLlegend.show` returns nothing.

## Author

Yves Deville

## Note

The size of symbols (i.e, *plotting characters*) can be set by using the
[`RLpar`](https://irsn.github.io/Renext/reference/RLpar.md) function and
the [`par`](https://rdrr.io/r/graphics/par.html) argument of the methods
[`plot.Renouv`](https://irsn.github.io/Renext/reference/plot.Renouv.md)
and
[`lines.Renouv`](https://irsn.github.io/Renext/reference/plot.Renouv.md).
However it can not be changed in the legend.

## See also

[`plot.Renouv`](https://irsn.github.io/Renext/reference/plot.Renouv.md)
and
[`lines.Renouv`](https://irsn.github.io/Renext/reference/plot.Renouv.md)
for and the [`RLpar`](https://irsn.github.io/Renext/reference/RLpar.md)
function to change the graphical parameters of the plot and the legend
by using the `par` argument.

## Examples

``` r
## use Garonne data
xG <- Garonne$OTdata$Flow
## use special "exponential" distribution 
fit1 <- Renouv(x = xG, threshold = 2500, distname.y = "exponential",
               effDuration = 65, plot = FALSE)
#> Special inference for the exponential case without history
#> Warning: uncertainty on the rate not taken into account yet  in the exponential with no history case

## use 'exp' in black box fashion, hence with delta method
fit2 <- Renouv(x = xG, , threshold = 2500, distname.y = "exp",
               effDuration = 65, start.par.y = c(rate = 1), plot = FALSE)
#> Warning: warning: distribution not in target list. Still EXPERIMENTAL
RLlegend.ini() ## initialise legend
## sample points only
plot(fit1, main = "Two types of confidence lims",
     show = list(OT = TRUE, quant = FALSE, conf = FALSE),
     label = "",
     legend = FALSE)
## quant and confidence lims
lines(fit1,
     show = list(OT = FALSE, quant = TRUE, conf = TRUE),
     label = "exact",
     legend = FALSE)
## quant (overplot) and confidence lims
lines(fit2,
      show = list(OT = FALSE, quant = TRUE, conf = TRUE),
      par = RLpar(quant.lty = 2, quant.col = "SpringGreen2",
        conf.conf1.col = "orangered", conf.conf1.lwd = 3,
        conf.conf2.col = "orangered", conf.conf2.lwd = 3),
      label = "delta",
      legend = FALSE)
RLlegend.show() ## now draw legend

```
