# Utility function to collect all the variable names from an open netCDF file.

Utility function to collect all the variable names from an open netCDF
file.

## Usage

``` r
collect_variables(nc_obj, nodes = FALSE)
```

## Arguments

- nc_obj:

  An open fvcom netCDF file to extract the data from.

- nodes:

  For a fvcom format netCDF file, whether to return if the variable
  makes use of the nodes or elements.

## Value

A tibble of variable names, longnames and (if specified) nodes or
elements.

## Examples

``` r
if (FALSE) { # \dontrun{

 the_nc_obj <- ncdf4::nc_open("path/to/netCDF/file.nc")

 collect_variables(the_nc_obj, nodes = TRUE)
} # }
```
