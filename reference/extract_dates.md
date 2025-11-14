# Extract all date or datetimes from an open netCDF file.

Extract all date or datetimes from an open netCDF file.

## Usage

``` r
extract_dates(nc_obj, time_var_name = "time", pretty = FALSE)
```

## Arguments

- nc_obj:

  Open netCDF file to retrieve time data from

- time_var_name:

  Name of time/date variable to extract from the netCDF file. Defaults
  to "time" as that is by far the most common variable name used.

- pretty:

  Boolean variable to round data to nearest hour. This is relevant for
  when hourly data has been recorded, particularly as part of an FVCOM
  file, where the timestamps are often not quite on the hour.

## Value

Vector of date or datetimes extracted from the netCDF file.

## Examples

``` r
if (FALSE) { # \dontrun{
 # open the netCDF file
 the_nc_obj <- ncdf4::nc_open("path/to/netCDF/file.nc")

 # extract all datetimes from the file
 extract_dates(the_nc_obj)

 # or if the date/time variable has a different name
 extract_dates(the_nc_obj, "datetime")
} # }
```
