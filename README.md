
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
see the [Processing netCDF files
article](https://decide-project-eu.github.io/satpoint/docs/articles/Processing-netcdf-files.html)
for a fully worked example.

## Collecting netCDF files for analysis

At present the package does not contain any functions that will
download/collect netCDF files from any of the common data repositories.
However, to assist users who are new to using `R` to work with this type
of data we have put together an
[article](https://decide-project-eu.github.io/satpoint/articles/download-netcdf.html)
from the [NASA EARTHDATA](https://urs.earthdata.nasa.gov/) data
repository.
