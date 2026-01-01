# Temporal aggregation of a Marked Process

Temporal aggregation of a Marked Process, leading to block maxima or
\\r\\-largest observations.

## Usage

``` r
OT2MAX(OTdata,
         OTmissing = NULL,
         start = NULL,
         end = NULL,
         MAX.r = 1L,
         blockDuration = "year",
         monthGapStat = TRUE,
         maxMissingFrac = 0.05,
         dataFrames = FALSE,
         infMAX = FALSE,
         plot = TRUE,
         plotType = c("max", "gaps"),
         jitterSeed = 123,
         trace = 0L,
         ...)
```

## Arguments

- OTdata:

  Data frame containing a `POSIXct` column `date` and the marks
  variable.

- OTmissing:

  Optional data frame with columns `start` and `end` (coerced to
  `POSIXct`) giving the beginning and the end of gaps.

- start:

  An object coerced to `POSIXct` indicating the beginning of
  reliable/usable information. Unless this is a beginning of block (1-st
  of January for years), the 1-st block will begin *after* `start` in
  order to use only qualified information.

- end:

  An object indicating the end of the reliable/usable information.
  Unless this is a end of block (1-st of January for years), the last
  block will end *before* `end` in order to use only qualified
  information.

- MAX.r:

  *Target* number of observations in the blocks. Can be of length one
  (same number of observations for all blocks) or of length equal to the
  number of blocks, the values being then for the blocks in the same
  order. In both cases, the target number may be impossible to reach
  because of a smaller number of events in the block. If `infMAX` is
  `TRUE`, the target number of observations will be reached by filling
  if needed with `-Inf` values. The rationale for this is that a
  non-existing event is assumed to have an arbitrarily small mark.

- blockDuration:

  Duration of the blocks. Can only be `"year"` for now.

- monthGapStat:

  Logical. Setting it to `TRUE` will compute statistics concerning the
  gaps and return them or show them on a plot.

- maxMissingFrac:

  Maximal fraction of a block duration (between 0 and 1) that can be
  missing without leading to a `NA` aggregated value.

- dataFrames:

  If `TRUE`, the result will contain data frames similar to those found
  in an object with class `"Rendata"`. If `FALSE` the result will
  contain *list* and *vector* objects, similar to those used as inputs
  in the [`Renouv`](https://irsn.github.io/Renext/reference/Renouv.md)
  function under the names `MAX.data` and `MAX.effDuration`. Note
  however, that `-Inf` values can be found in these objects when
  `infMAX` is `TRUE`.

- infMAX:

  If `FALSE`, the target number of values the blocks will generally not
  be reached, because the total number of events in a block can be lower
  than the target number. Then, the target number value is revised to
  the number of found values in each block. If `TRUE`, the target number
  of values is reached by filling the values with `-Inf` and the
  datetimes with (`POSIXct`) `NA`s.

- plot:

  If `TRUE` a simple plot is shown.

- plotType:

  Character controlling the plot. With `"max"`, the block maxima are
  shown. With `plotType = "gap"`, the daily and monthly gap rates are
  shown. This is possible when suitable information concerning gaps is
  provided in `OTmissing`. The plot then shows the probability that a
  given day of the year falls in a gap, as well as monthly gap rates.
  Most often one wants that the gap rate does not show a seasonal
  behaviour. Note that gap rates for month-year combinations are shown
  as grey segments after jitterizing them since the values `0` and `1`
  may be observed for several years. An alternative way to is using the
  `monthGapTS` multivariate time series returned by the function, see
  **Examples**.

- jitterSeed:

  Random seed for jittering. Used only when `plot` is `TRUE`, `plotType`
  is `"gap"` and when suitable information is provided in `OTmissing`.

- trace:

  Integer level of verbosity.

- ...:

  Other arguments to be passed to `plot`.

## Value

A list, the content of which depends on the value of `dataFrames`. If
this value is `TRUE`, the following elements are returned.

- MAXdata:

  A data frame of largest values by block with one row for each
  observation. The largest values are given as columns with names equal
  to those in the `OTdata` data frame.

- MAXinfo:

  A data frame describing the blocks, with one row by block. The two
  (`POSIXct`) columns `"start"` and `"end"` provide the beginning and
  the end of the block. The numeric column `duration` gives the
  *effective duration* (in year) within block.

- probMissing:

  A vector with values corresponding to the days in a block (year). Each
  value is a estimation of the probability that the day falls in a gap.

  If `dataFrames` is `FALSE`, the list still contains `probMissing` as
  before, as well as other lists as used in
  [`Renouv`](https://irsn.github.io/Renext/reference/Renouv.md).

- effDuration, r:

  Vectors containing the effective duration (*in years*) and number of
  value for the blocks.

- data:

  List of maxima or \\r\\-largest values for the blocks.

- monthGapStat, monthGapTS:

  Summary information concerning gaps, if `monthGapStat` is `TRUE` and
  if relevant information is provide via the the `OTmissing` formal. The
  element `monthGapTS` is a multivariate time series with yearly
  observations and one series (column) for each of the 12 months. Each
  series contains the missing fraction of the month for the considered
  year, ranging from `0.0` (no gap) to `1.0` (full gap). This object can
  be dealt with standard methods for time-series, but the `plot` method
  will require to select a reduced number of columns first, see
  **Examples**.

## Details

The data frame given in `OTdata` contains the *events* (or *arrivals*)
given by the `date` column, as well as one mark column. Depending on the
argument `MAX.r`, the maxima or the \\r\\-largest observations of the
marks is computed for each time block. When known gaps exist in the data
and when they are given in `OTmissing`, a block for which the total
duration of gaps is too large will be omitted.

## Note

Remind that even when `maxMissingFrac` is set to its maximum value 1.0,
there can still be blocks with no data. When the result is intended to
be used in the
[`Renouv`](https://irsn.github.io/Renext/reference/Renouv.md) function,
the formal `dataFrames` should be `FALSE`; the elements `data` and
`effDuration` can then be passed as `MAX.data` and `MAX.effDuration`. At
the time `infMAX` should also then be set to `FALSE` since `-Inf` values
are not yet allowed in the \\r\\-largest values.

## Examples
