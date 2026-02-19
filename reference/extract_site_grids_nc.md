# Extract all variable values from netCDF file for all provided geographical locations.

Extract all variable values from netCDF file for all provided
geographical locations.

## Usage

``` r
extract_site_grids_nc(
  nc_file,
  sites,
  nc_crs,
  sites_crs = NULL,
  use_bbox = FALSE,
  depth_var = "depth",
  depths_to_extract = NULL,
  id = "site",
  nc_var_name = NULL,
  dates_to_extract = NULL,
  time_present = TRUE,
  time_var_name = "time",
  time_value = NULL,
  grid_name_x = "",
  grid_name_y = "",
  site_message = TRUE
)
```

## Arguments

- nc_file:

  Filepath for the netCDF file to extract the data from.

- sites:

  Simple features (or spatial) data frame of geographical areas to
  extract the variable values for. The data frame should contain a
  single row per buffer and have a unique identifier.

- nc_crs:

  The numerical value for the crs of the netCDF file.

- sites_crs:

  The numerical value for the crs of sites, if different from the crs of
  the netCDF file.

- use_bbox:

  A boolean value to choose whether the boundary box of the sites will
  be used to speed up the extraction of data from the netCDF file. When
  a netCDF file cannot be reduced to a small area, or is particularly
  detailed, this can drastically speed up data extraction.

- depth_var:

  The name of the depth variable, if necessary, used in the netCDF file.
  Defaults to "depth".

- depths_to_extract:

  The specific depth values to extract, if applicable. By default, if
  any depth values are present, all of them will be extracted.

- id:

  The name of the identifier provided in the sites special features data
  frame.

- nc_var_name:

  The name of the variable in the netCDF file that is the focus of the
  extraction. If not specified, an attempt will be made to establish
  this from the netCDF file itself.

- dates_to_extract:

  Vector of dates to extract the variable for. If not provided, all
  available values will be returned.

- time_present:

  Boolean value that indicates whether a time, or similar value is
  present in the netCDF file.

- time_var_name:

  Name of time/date variable to extract from the netCDF file. Defaults
  to "time" as that is by far the most common variable name used.

- time_value:

  If there is no date/time value present in the netCDF file, a user can
  provide the date(s) or time(s) for this netCDF file.

- grid_name_x:

  Name of the geographical x coordinate to be extracted. If not
  specified, an attempt will be made to establish this from the netCDF
  file itself.

- grid_name_y:

  Name of the geographical y coordinate to be extracted. If not
  specified, an attempt will be made to establish this from the netCDF
  file itself.

- site_message:

  Boolean variable to indicate whether to print the messages about which
  site is being processed.

## Value

A tibble of each location id, with the corresponding variable values
depths and dates available.

## Examples

``` r
if (FALSE) { # \dontrun{

 the_nc_file <- "path/to/netCDF/file.nc"

 # presuming we already have a set of spatial buffers to collect data
 # from
 extract_site_grids_nc(the_nc_file, sites = buffers, nc_crs = 4326)
} # }
```
