# Extract the values for the node variables present in an fvcom netCDF file.

Extract the values for the node variables present in an fvcom netCDF
file.

## Usage

``` r
extract_nodes_fvcom(
  nc_obj,
  nc_var_name,
  time_var_name = "time",
  all_sigma = FALSE,
  pretty_dates = TRUE
)
```

## Arguments

- nc_obj:

  An open fvcom netCDF file to extract the data from.

- nc_var_name:

  The name of the variable in the netCDF file that is the focus of the
  extraction.

- time_var_name:

  Name of time/date variable to extract from the netCDF file. Defaults
  to "time" as that is by far the most common variable name used.

- all_sigma:

  Boolean variable to allow users to specify whether all sigma layers or
  levels, often labelled siglay and siglev in fvcom files should be
  extracted. Defaults to FALSE, where the first (or top) layer/level
  will be extracted.

- pretty_dates:

  Boolean variable to control whether to tidy the fvcom date/times to
  the nearest hour or not. Defaults to TRUE.

## Value

A tibble of variable values, along with the corresponding dates, nodes
and (if specified) sigma layers/levels.

## Examples

``` r
if (FALSE) { # \dontrun{

 the_nc_obj <- ncdf4::nc_open("path/to/netCDF/file.nc")

 # taking a temp variable (presuming it exists within the fvcom netCDF file)
 extract_nodes_fvcom(the_nc_obj, nc_var_name = "temp")
} # }
```
