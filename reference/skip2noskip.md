# Fix non-skipped periods from skipped ones

Compute non-skipped periods form start and end of skipped periods.

## Usage

``` r
skip2noskip(skip = NULL,
               start = NULL,
               end = NULL)
```

## Arguments

- skip:

  A data.frame object with `start` and `end` columns that can be coerced
  to `POSIXct`. Other columns can be present (and will be ignored). Each
  row describes a missing period. Rows must be sorted in chronological
  order and periods should not overlap. Validity checks are at the time
  very limited.

- start:

  Beginning of the whole period, to be used in `as.POSIXct`.

- end:

  End of the whole period to be used in `as.POSIXct`.

## Details

In a 'normal' use of this function `start` and `end` are given, and are
respectively *before the beginning* of the first `skip` period and
*after the end* of the last `skip` period. Thus the returned dataframe
will have `nrow(skip)+1` rows. However, `start` and `end` can be `NULL`
in which case only the `nrows(skip)-1` "inner" non-skipped periods will
be returned. If `start` and `end` are `NULL` and `skip` has only one
row, the returned result is `NULL`.

## Value

A data.frame object with two `POSIXct` columns named `start` and `end`.
Each row corresponds to a non-skipped period

## Author

Yves Deville

## See also

[`readXML`](https://irsn.github.io/Renext/reference/readXML.md) for
reading data from XML and csv files.

## Examples

``` r
## Brest data embeds a description of the gaps

ns <- skip2noskip(skip = Brest$OTmissing)

ns2 <- skip2noskip(skip = Brest$OTmissing,
                   start = Brest$OTinfo$start,
                   end = Brest$OTinfo$end)

## check durations. dur2 should be equal to the effective
## duration (with an error of a fraction of day)
dur <- as.numeric(sum(ns$end-ns$start))/365.25
dur2 <- as.numeric(sum(ns2$end-ns2$start))/365.25

```
