# Read data using an XML index file

Read one or several dataset(s) using an XML index file specifying the
data sets to read and the structure of each

## Usage

``` r
readXML(name,
           dir,
           index = "index.xml",
           TZ = "GMT",
           trace = 0)
```

## Arguments

- name:

  Name for the dataset that will be matched against the `name` attribute
  of datasets as they are given in the index file.

- dir:

  Path to the directory where the index file and all data files should
  be found.

- index:

  Name (short) of the index file. This file must be in the directory
  given by `dir`.

- TZ:

  A time zone as in [`strptime`](https://rdrr.io/r/base/strptime.html).
  The time zone `"GMT"` should be preferred, since it should work on all
  platforms and can cope with dates in the remote past.

- trace:

  Level of verbosity (integer). Can be used to trace the successive
  steps by short indications.

## Details

The XML index file is parsed within R. Then according to the indications
within the index file, other files are read (e.g. csv files). In this
way, data returned as a list can contain heterogeneous data: Over
Threshold (OT) data, missing periods, MAX data, etc. Various pieces of
information are also stored in list elements with name containing the
`"info"` string.

This function requires the CRAN package XML.

## Value

A list with the data read.

- info:

  General information about the data: `varName`, `varShorlLab` and
  `varUnit` give the variable name unit and short label.

- OTinfo:

  Information for the Over the Threshold (OT).

- OTdata:

  Over the Threshold (OT) data.

- OTmissing:

  Missing periods within the OTdata period.

- MAXinfo:

  Information for the MAX (\\r\\-largest) supplement data.

- MAXdata:

  MAX supplement data.

- OTSinfo:

  Information for the Over the Threshold Supplement (OTS) data.

- OTSdata:

  Over the Threshold (OT) supplement data.

## Author

Yves Deville

## Note

The flat files (usually `.csv` files) can also be read in a more
conventional way e.g. through `read.table`. However, conform to the
`index.xml` examples or to the `index.xsd` schema to see how to adjust
the reading of parameters such as `sep`, etc.

## See also

See [`Brest`](https://irsn.github.io/Renext/reference/Brest.md) for an
example of such a list.

## Examples

``` r
if (FALSE) { # \dontrun{
## Examples of datasets that can be read
## Browse should work for browsers with xslt support
browseURL(file.path(system.file("Rendata", package = "Renext"), "index.xml"))
if (require(XML)) {
   ## use 'index.xml' file shiped with Renext
   dir1 <- system.file("Rendata", package = "Renext")
   BrestNew1 <- readXML(name = "Brest", dir = dir1)
   test1 <- readXML(name = "test1", dir = dir1)
}
} # }
```
