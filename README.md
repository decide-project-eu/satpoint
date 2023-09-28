
<!-- README.md is generated from README.Rmd. Please edit that file -->

# satpoint

<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/decide-project-eu/satpoint/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/decide-project-eu/satpoint/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of satpoint is to allows users to automatically extract data
from a netCDF file for a specific geographical area and to aide in the
automatic processing of multiple netCDF files.

## Installation

You can install the development version of satpoint from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("decide-project-eu/satpoint")
```

This should install all the dependencies required.

## Usage

The package makes use of the existing
[ncdf4](https://cran.r-project.org/web/packages/ncdf4/index.html)
package to open and extract data from netCDF files but tries to simplify
the required interface for users, particularly where they are trying to
extract data for a geographic area - which they will then average
across.

For a full worked example of processing one or more netCDF files, please
see the `vignette("satpoint", package = "satpoint")` for a fully worked
example.

## Collecting netCDF files for analysis

At present the package does not contain any functions that will
download/collect netCDF files from any of the common data repositories.
However, to assist users who are new to using `R` to work with this type
of data we have put together the
`vignette("download-netcdf", package = "satpoint")` to demonstrate how
to download files from the [NASA
EARTHDATA](https://urs.earthdata.nasa.gov/) data repository.

## Final Words

This is a brand new package and is very much still in development. If
you have any problems, or would like to see any additional
functionality, please post an issue via
[github](https://github.com/decide-project-eu/satpoint/issues).
