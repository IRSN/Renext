# Graphical parameters for Return Level plots

Build a hierarchical list of graphical parameters that can be used in
the methods plot or lines for the class `"Renouv"`.

## Usage

``` r
RLpar(mono = TRUE,
      trace = 0L,
      ...)
```

## Arguments

- mono:

  Logical. The default `TRUE` is for plots possibly using colors but
  that can be printed in grayscale. With the value `FALSE`, curves or
  symbols will appear distinctly on a color device but not necessarily
  when printed in grayscale.

- trace:

  Integer level of verbosity. The default value `0` prints nothing.

- ...:

  Arguments with names corresponding to the hierarchical structure and
  the graphical parameter to be changed.

## Details

The formals are in correspondence with the list hierarchy using a column
`"."` as separator to define the tree. Thus a `quant.col` formal
argument can be used to specify the color of the quantile (or return
level) curve, while `conf.conf1.col` will be used for the first
confidence limits (lower and upper).

## Value

A list containing lists in a hierarchical fashion. At the root level, an
element concerns a single curve (e.g. the return level curve), a single
scatterplot (e.g. sample used in POT), a group of curves (e.g. the
confidence limits) or a group of scatterplots (e.g. the collection of
`MAX` historical blocks). For single elements (curve or scatterplot) the
list contains graphical elements with values as they would be given in
`plot` or `lines` calls. For group elements, each element is a list of
such lists.

## Author

Yves Deville

## Note

A list of default parameter values is built first using the model
suitable for the `mono` value. Then the values provided by the user
overwrite the existing. Thus a curve can be coloured even if
`mono = TRUE`, if a colour specification is given for the corresponding
element.

When the same parameter name is used several times in `RLpar`, a warning
is thrown.

## See also

[`plot.Renouv`](https://irsn.github.io/Renext/reference/plot.Renouv.md)
and
[`lines.Renouv`](https://irsn.github.io/Renext/reference/plot.Renouv.md)
with which `RLpar` is to be used.

## Examples

``` r
## change color for quantile curve and type for confidence
## limits #1 (with largest confidence level).
newRLpar <- RLpar(quant.col = "red", conf.conf1.lty = "dashed")
newRLpar$quant
#> $type
#> [1] "l"
#> 
#> $col
#> [1] "red"
#> 
#> $lwd
#> [1] 2
#> 
#> $lty
#> [1] "solid"
#> 

## show the names of all possible editable parameters
names(unlist(RLpar()))
#>   [1] "quant.type"      "quant.col"       "quant.lwd"       "quant.lty"      
#>   [5] "OT.col"          "OT.pch"          "OT.cex"          "OT.bg"          
#>   [9] "conf.conf1.lty"  "conf.conf1.col"  "conf.conf1.lwd"  "conf.conf2.lty" 
#>  [13] "conf.conf2.col"  "conf.conf2.lwd"  "conf.conf3.lty"  "conf.conf3.col" 
#>  [17] "conf.conf3.lwd"  "conf.conf4.lty"  "conf.conf4.col"  "conf.conf4.lwd" 
#>  [21] "conf.conf5.lty"  "conf.conf5.col"  "conf.conf5.lwd"  "conf.conf6.lty" 
#>  [25] "conf.conf6.col"  "conf.conf6.lwd"  "MAX.block1.col"  "MAX.block1.pch" 
#>  [29] "MAX.block1.cex"  "MAX.block1.lwd"  "MAX.block1.bg"   "MAX.block2.col" 
#>  [33] "MAX.block2.pch"  "MAX.block2.cex"  "MAX.block2.lwd"  "MAX.block2.bg"  
#>  [37] "MAX.block3.col"  "MAX.block3.pch"  "MAX.block3.cex"  "MAX.block3.lwd" 
#>  [41] "MAX.block3.bg"   "MAX.block4.col"  "MAX.block4.pch"  "MAX.block4.cex" 
#>  [45] "MAX.block4.lwd"  "MAX.block4.bg"   "MAX.block5.col"  "MAX.block5.pch" 
#>  [49] "MAX.block5.cex"  "MAX.block5.lwd"  "MAX.block5.bg"   "MAX.block6.col" 
#>  [53] "MAX.block6.pch"  "MAX.block6.cex"  "MAX.block6.lwd"  "MAX.block6.bg"  
#>  [57] "MAX.block7.col"  "MAX.block7.pch"  "MAX.block7.cex"  "MAX.block7.lwd" 
#>  [61] "MAX.block7.bg"   "MAX.block8.col"  "MAX.block8.pch"  "MAX.block8.cex" 
#>  [65] "MAX.block8.lwd"  "MAX.block8.bg"   "MAX.block9.col"  "MAX.block9.pch" 
#>  [69] "MAX.block9.cex"  "MAX.block9.lwd"  "MAX.block9.bg"   "MAX.block10.col"
#>  [73] "MAX.block10.pch" "MAX.block10.cex" "MAX.block10.lwd" "MAX.block10.bg" 
#>  [77] "OTS.block1.col"  "OTS.block1.pch"  "OTS.block1.cex"  "OTS.block1.lwd" 
#>  [81] "OTS.block1.bg"   "OTS.block2.col"  "OTS.block2.pch"  "OTS.block2.cex" 
#>  [85] "OTS.block2.lwd"  "OTS.block2.bg"   "OTS.block3.col"  "OTS.block3.pch" 
#>  [89] "OTS.block3.cex"  "OTS.block3.lwd"  "OTS.block3.bg"   "OTS.block4.col" 
#>  [93] "OTS.block4.pch"  "OTS.block4.cex"  "OTS.block4.lwd"  "OTS.block4.bg"  
#>  [97] "OTS.block5.col"  "OTS.block5.pch"  "OTS.block5.cex"  "OTS.block5.lwd" 
#> [101] "OTS.block5.bg"   "OTS.block6.col"  "OTS.block6.pch"  "OTS.block6.cex" 
#> [105] "OTS.block6.lwd"  "OTS.block6.bg"   "OTS.block7.col"  "OTS.block7.pch" 
#> [109] "OTS.block7.cex"  "OTS.block7.lwd"  "OTS.block7.bg"   "OTS.block8.col" 
#> [113] "OTS.block8.pch"  "OTS.block8.cex"  "OTS.block8.lwd"  "OTS.block8.bg"  
#> [117] "OTS.block9.col"  "OTS.block9.pch"  "OTS.block9.cex"  "OTS.block9.lwd" 
#> [121] "OTS.block9.bg"   "OTS.block10.col" "OTS.block10.pch" "OTS.block10.cex"
#> [125] "OTS.block10.lwd" "OTS.block10.bg" 


```
