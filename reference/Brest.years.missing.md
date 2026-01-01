# Years with missing periods in 'Brest.year' dataset

Years with missing periods in the 'Brest.years' dataset

## Usage

``` r
Brest.years.missing
```

## Format

The format is: int \[1:49\] 1846 1847 1852 1857 1858 1859 1860 1861 1862
1863 ...

## Details

Vector of years containing missing periods in the
[`Brest.years`](https://irsn.github.io/Renext/reference/Brest.years.md)
dataset. This years should be ignored when computing yearly statistics
such as event rates, since time records are lost.

## Examples

``` r
print(Brest.years.missing)
#>  [1] 1846 1847 1852 1857 1858 1859 1860 1861 1862 1863 1864 1867 1868 1872 1874
#> [16] 1877 1878 1891 1897 1907 1910 1911 1915 1916 1917 1920 1922 1923 1937 1938
#> [31] 1940 1944 1945 1946 1947 1948 1949 1950 1951 1952 1980 1981 1982 1991 1992
#> [46] 1993 1999 2000 2008
```
