# Plot a Rendata object

Plot 'Rendata' datasets with OT and historical data

## Usage

``` r
# S3 method for class 'Rendata'
plot(x,
     textOver = quantile(x$OTdata[, x$info$varName], probs = 0.99),
     showHist = TRUE,
                 ...)
```

## Arguments

- x:

  Rendata object i.e. a list object as read with the `readXML` function.

- textOver:

  Mark values of the variable in the `OTdata` part of `x`. Values above
  the `textOver` value (if any) will be marked with the character
  version of the block, typically a year

- showHist:

  If `TRUE`, the historical periods (is any) are shown on the plot.

- ...:

  further args to be passed to `plot` function.

## Details

The plot shows the main data of the object `x` (the `OTdata` part) as
well as historical data `MAXdata` or `OTSdata` if any. Different colours
are used on the background. This function is not intended to produce
nice plots to be printed.

## Author

Yves Deville

## Note

This function is mainly a companion function of `readXML`. Its goal is
to check the content of the data read.

## See also

[`readXML`](https://irsn.github.io/Renext/reference/readXML.md)

## Examples

``` r
if (require(XML)) {
   ## use 'index.xml' file shipped with Renext
   dir1 <- system.file("Rendata", package = "Renext")
   BrestNew <- readXML(name = "Brest", dir = dir1)
   plot(BrestNew)
   GaronneNew <- readXML(name = "Garonne", dir = dir1)
   plot(GaronneNew)
   test1 <- readXML(name = "test1", dir = dir1)
   plot(test1)
}
#> Loading required package: XML


```
