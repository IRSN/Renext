# Surge heights at Brest partial data

Surge heights at Brest (France)

## Usage

``` r
Brest.years
```

## Format

A data frame with 954 observations on the following 2 variables.

- `year`:

  Year e.g; 1980

- `Surge`:

  Surge heights above the threshold of 30 cm.

## Details

These data are a simplified version of
[`Brest`](https://irsn.github.io/Renext/reference/Brest.md). For each
surge event only the year is retained as timestamp. Years with missing
periods are available as a vector
[`Brest.years.missing`](https://irsn.github.io/Renext/reference/Brest.years.missing.md).

This dataset is useful for testing since similar data are sometimes met
in the analyses.

## Examples

``` r
names(Brest.years)
#> [1] "year"  "Surge"
```
