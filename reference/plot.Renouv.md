# Plot an object of class "Renouv"

Plot an object of class "Renouv". The plot is a return level plot with
some supplementary elements to display historical data.

## Usage

``` r
# S3 method for class 'Renouv'
plot(x,
     pct.conf = x$pct.conf,
     show = list(OT = TRUE, quant = TRUE, conf = TRUE,
                 MAX = TRUE, OTS = TRUE),
     mono = TRUE,
     predict = FALSE,
     par = NULL,
     legend = TRUE,
     label = NULL,
     problim = NULL,
     Tlim = NULL,
     main = NULL, xlab = "periods", ylab = "level",
     posOptions = NULL,
     byBlockStyle = NULL,
     ...)
# S3 method for class 'Renouv'
lines(x,
      pct.conf = x$pct.conf,
      show = NULL,
      mono = TRUE,
      predict = FALSE,
      par = NULL,
      legend = FALSE,
      label = NULL,
      posOptions = NULL,
      byBlockStyle = NULL,
      ...)
```

## Arguments

- x:

  Object of class `"Renouv"`.

- pct.conf:

  Percents for confidence limits (lower and upper). These levels should
  be found within those computed in the object `x`. By default, all
  computed levels will be used.

- show:

  A *list with named elements* specifying which parts of the return
  level plot must be drawn. Element `OT` is for the the sample points
  (Over the Threshold data), `quant` is for the quantile curve (or
  Return Level curve), `conf` is for the confidence limits. These three
  elements can be set to `TRUE` or `FALSE`. When the element `conf` is
  `TRUE`, only the percent levels given in `pct.conf` are drawn.
  Moreover, the levels not already computed in the object given in `x`
  will be drawn only if the predictions are recomputed with `predict`
  set to `TRUE`. Finally, the `MAX` and `OTS` elements are for the two
  possible types of historical data. They can be logical vectors with
  length one or with length equal to the corresponding of blocks if only
  some blocks are to be shown. These two elements can also be character
  vectors indicating the names of the blocks which are to be shown (by
  partial matching). These names should match one or several elements of
  the character vector named `blockNames` within the lists
  `x$history.MAX` or `x$history.OTS` respectively.

- mono:

  Logical, `TRUE` for a monochrome plot.

- predict:

  Logical. When `TRUE`, predictions are re-computed from the model
  before plotting. One effect is that the points used to draw the curves
  are designed to cover the whole range (if specified by the user). One
  other effect is that the confidence limits are recomputed in order to
  include the percent levels given on entry.

- par:

  A list such as returned by the
  [`RLpar`](https://irsn.github.io/Renext/reference/RLpar.md) function.

- legend:

  Logical. If `TRUE`, a legend is built and drawn on the graph.

&nbsp;

- label:

  A character label used to build the labels used in the legend. The
  default is to use the name of the `x` object. Using an empty string
  `""` can be better in some cases.

- problim:

  Limits for the x-axis in probability scale. Can be used as an
  alternative to `Tlim`.

- Tlim:

  Limits for the x-axis in return period scale. The values are given as
  a numeric vector of length 2, containing values \\\ge 1\\. The first
  element (minimal return period can be 0 in which case it will be
  replaced by a very small positive value.

- xlab:

  Label of the x-axis (time periods, with log scale).

- main:

  Main title (character).

- ylab:

  Label of the y-axis (labels).

- posOptions:

  A pair list to be passed as list of formals to the
  [`SandT`](https://irsn.github.io/Renext/reference/SandT.md) function
  computing the plotting positions.

&nbsp;

- byBlockStyle:

  Logical list (or named logical vector) with elements `MAX` and `OTS`.
  The value indicates if each (MAX or OTS) block must be plotted with a
  specific style (plotting character and color), or if instead a common
  style is used for all blocks of the same type (MAX or OTS). In the
  first case, each block will create a line in the legend with a label
  taken from the `history.MAX` element of the object given in `x`. These
  legend lines will not appear if `legend` is `FALSE` but can be shown
  later using
  [`RLlegend.show`](https://irsn.github.io/Renext/reference/RLlegend.md).
  In the second case, only one legend line will be generated. When the
  number of blocks is large for one type and the corresponding value of
  `byBlockStyle` is `TRUE`, the styles will be recycled and the
  plot/legend might not be clear. When `byBlockStyle` is `NULL` or does
  not contain all needed information, default choices are made.

- ...:

  Other arguments passed to the default
  [`plot`](https://rdrr.io/r/graphics/plot.default.html) function e.g.,
  `ylim` to adjust the y-axis.

## Details

Historical data blocks (MAX or OTS) embedded in the `x` object (if any)
can be plotted or not depending on the value of the corresponding
element in `show`.

- If the `MAX` element is `TRUE` and if `x` embeds historical data of
  type `MAX`, then these will be shown with a symbol differing from the
  one for ordinary points.

- If `OTS` element is `TRUE` and is `x` embeds historical data of type
  `OTS`, then these will be shown with a symbol differing from the one
  for ordinary points. An exception is when one or several OTS block
  have no data. Then each such block is shown as an horizontal segment;
  its right end-point shows the effective duration of the block and the
  ordinate shows the OTS threshold for this block. No data exceeded the
  threshold within the block.

This function acts on a list variable named `.RLlegend` and stored in a
special environment bound to the package. This variable is used to build
legends for plots produced with multiple commands. See the
[`RLlegend`](https://irsn.github.io/Renext/reference/RLlegend.md) help
page. Examples of possible combined uses of the argument of the `plot`
and `lines` together with the `RLlegend*` functions are given in the
"Renext Graphics" chapter of the *Renext Guide* document shipped with
this package.

## Value

No value returned.

## Author

Yves Deville

## Note

The return level plot is of exponential type i.e. uses a log-scale for
return periods. This contrasts with the Gumbel plot which is also used
in similar contexts.

## Caution

Remind that the methods `plot` and `lines` may change the value of the
variable `.RLlegend` in the environment `legendEnvir`. This variable
describes the material to be used in the legend at the next call of
`RLlegend.show`.

## See also

The [`RLlegend`](https://irsn.github.io/Renext/reference/RLlegend.md)
page for the legend construction and
[`RLpar`](https://irsn.github.io/Renext/reference/RLpar.md) to specify
the graphical parameters (colors, line types, ...) of the elements.

## Examples

``` r
## two fits for the Garonne data
fit.exp <- Renouv(x = Garonne, plot = FALSE)
fit.gpd <- Renouv(x = Garonne, distname.y = "gpd", plot = FALSE)

## simple plot (legend is TRUE here)
plot(fit.exp,
     main = "Two fits overlapped",
     label = "",
     ## Tlim = c(1, 5000),
     predict = TRUE)


## Now try 'lines' and RLlegend.xxx functions
plot(fit.exp,
     main = "Fancy legend",
     show = list(OT = FALSE, quant = FALSE, conf = FALSE,
                 OTS = FALSE, MAX = FALSE),
     legend = FALSE,
     Tlim = c(1, 5000))
RLlegend.ini(x = "bottomright", bg = "lightyellow") ## initialise legend
lines(fit.exp,
      show = list(quant = FALSE, conf = FALSE, OT = TRUE, MAX = TRUE),
      label = "expon",
      par = RLpar(quant.col = "orange", 
        OT.pch = 21, OT.cex = 1.2, OT.col = "SeaGreen", OT.bg = "yellow",
        MAX.block1.col = "purple", MAX.block1.bg = "mistyrose",
        MAX.block1.lwd = 1.4))
lines(fit.gpd,
      pct.conf = c(95, 70),
      show = list(quant = TRUE, conf = TRUE),
      label = "GPD",
      par = RLpar(quant.col = "darkcyan", conf.conf1.col = "red"))
RLlegend.show() ## now draw legend
```
