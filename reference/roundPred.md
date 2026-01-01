# Round quantiles in a pseudo-prediction table

Round the quantiles of a pseudo prediction table such that computed by
`predict.Renouv`.

## Usage

``` r
roundPred(pred, dig.quant = NA)
```

## Arguments

- pred:

  The data.frame containing the predicted quantiles and return levels.

- dig.quant:

  Number of digits. Guessed if not provided.

## Details

Only the columns that can be considered as quantiles are rounded. These
are assumed to have names `"quant"` for the expected return level and
`"L."` or `"U."` followed by a percentage for lower and upper confidence
limits (e.g. `"L.95"` and `"U.95"` for 95% percent confidence limits.
The number of digits guessed is experimental.

## Value

A data.frame with the same structure as that given, but with some
columns rounded.
