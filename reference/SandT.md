# Compute empirical survivals (S) and return periods (T)

Compute the empirical survival values and the empirical return periods
at the observations of an object. These are used as plotting positions
in several plots.

## Usage

``` r
SandT(object, points = c("p", "H"), a = 0, naive = FALSE)
```

## Arguments

- object:

  The object containing the data, with class `"Renouv"` or `"Rendata"`.

- points:

  Option for the computation of the plotting positions. When `points` is
  set to `"p"`, the \\p\\-points formula is used with the selected value
  of `a`. This formula is used to compute the survival from which the
  return period is computed. When instead `points` is set to `"H"`,
  *Nelson's formula* is used to compute the return periods, the survival
  value still being given by the \\p\\-points formula. When the data is
  heterogeneous, i.e. when `object` contains `MAX` and/or `OTS` data,
  Nelson's formula is used only to compute the return periods of the
  upper slice of levels.

- a:

  Parameter used in the interpolation formula for the inverse return
  periods as in Hirsch and Stedinger (1987).

- naive:

  Logical. When `TRUE`, naive plotting positions are used to display
  `MAX` or `OTS` data. These can be defined only when a main sample
  exists in `object` as a `x.OT` element. For each `MAX` or `OTS` block,
  the positions use the number of events predicted using the rate of
  events as estimated from the main sample. When the main sample has a
  small durations, such predictions are likely to be misleading.

## Value

A list with the following elements

- x:

  Numeric vector containing the ordered values from all the available
  sources in the object: main sample, historical periods either 'MAX' or
  'OTS'.

- group, groupNames:

  Integer and character vectors giving the source of the values in `x`,
  in the same order of the values. For instance, `group[10]` gives the
  group form which `x[10]` was extracted, and the name of this group is
  `groupNames[group[10]]`.

- S, T:

  Numeric vectors of the same length as `x` and containing the
  corresponding estimation of the survival value and of the return
  period.

- thresh, lambda.thresh, S.thresh, T.thresh:

  Vector of thresholds and the corresponding estimation for the event
  rate, survival and return period. All the estimations are *for the
  threshold values*. The value of `T.thresh[i]` for a threshold
  `thresh[i]` results from a simple computation: divide the sum of the
  durations for blocks with thresholds `>= thresh[i]` by the number of
  events for these blocks.

## Details

When the object contains historical information (`MAX` or `OTS`), the
computation is an adaptation Hirsch and Stedinger (1987) for the Marked
Process (MP) context. The original method is devoted to block maxima and
interpolates the survival values at the thresholds which are computed
first. For MP, the interpolation is done for the inverse return periods,
and the survival values are deduced from those of the inverse return
periods.

Nelson's formula provides unbiased estimates for the values of the
cumulative hazard \\H(x)\\ at the order statistics, and thus can be used
to estimate the log-return periods as required on the return level plot.

## seealso

The [`ppoints`](https://rdrr.io/r/stats/ppoints.html) and
[`Hpoints`](https://irsn.github.io/Renext/reference/Hpoints.md)
functions.

## Warning

When using `points = "H"` the estimated values of the survival returned
in `S` and those for the return period `T` no longer verify
`T=1/S/lambda`, where `lambda` is the estimated rate. In this case, the
values in `T` should be used in the return level plot, while the values
in `S` should be used in the PP-plot.

## Author

Yves Deville

## References

The original method for block maxima is described in

- Hirsch R.M. and Stedinger J.R.(1887) Plotting Positions for Historical
  Floods and their precision. *Water Ressources Research*, vol. 23, N. 4
  pp. 715-727.

- Millard S. and Neerchal N. (2001). *Environmental Statistics with
  S-Plus*. CRC Press

The adaptation for the Marked Process context is described in the
*Renext Computing Details* document.

## Examples

``` r
## use an object with class "Rendata"
ST1 <- SandT(object = Garonne)
## basic return level plot
plot(ST1$T, ST1$x, col = ST1$group, log = "x")

## use an object with class "Renouv"
fit <- Renouv(x = Garonne, plot = FALSE)
ST2 <- SandT(object = fit)
plot(ST2$T, ST2$x, col = ST2$group, log = "x")
```
