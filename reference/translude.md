# Make translucient colors

Make translucient colors.

## Usage

``` r
translude(colors, alpha = 0.6)
```

## Arguments

- colors:

  A vector of colors in a format that can be understood by
  [`col2rgb`](https://rdrr.io/r/grDevices/col2rgb.html).

- alpha:

  Vector of level(s) of opacity between `0` and `1` (`0` means fully
  transparent and `1` means opaque). After recycling to reach the
  required length, this value or vector is used as `alpha` in
  [`rgb`](https://rdrr.io/r/grDevices/rgb.html).

## Value

A vector of translucient (or semi-transparent) colors.

## Note

Using the **RColorBrewer** package might be a better option that
transluding chosen colors!
