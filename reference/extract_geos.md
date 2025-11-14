# Extract all geographical coordinates from a netCDF file and convert to simple features object.

Extract all geographical coordinates from a netCDF file and convert to
simple features object.

## Usage

``` r
extract_geos(
  nc_obj,
  orig_crs,
  buffer_bbox = NULL,
  crs = NULL,
  name_x = "",
  name_y = ""
)
```

## Arguments

- nc_obj:

  Open netCDF file to retrieve time data from.

- orig_crs:

  Numerical value of the crs used in the netCDF file.

- buffer_bbox:

  Result of sf::st_bbox() or similar to provide a vector of of the
  buffers. This enables extracting only a small part of the entire
  netCDF file if required. The order is assumed to be xmin, ymin, xmax
  and ymax.

- crs:

  Numerical value of the crs to change results into, if different from
  the original crs.

- name_x:

  Name of the geographical x coordinate to be extracted. If not
  specified, an attempt will be made to establish this from the netCDF
  file itself.

- name_y:

  Name of the geographical y coordinate to be extracted. If not
  specified, an attempt will be made to establish this from the netCDF
  file itself.

## Value

Special features data frame containing the geographic coordinates of the
provided netCDF file.

## Examples

``` r
if (FALSE) { # \dontrun{
 # open the netCDF file
 the_nc_obj <- ncdf4::nc_open("path/to/netCDF/file.nc")

 # extract all spatial points from the netCDF file
 extract_geos(the_nc_obj, crs = 4326)

 # if the names of the spatial coordindates aren't picked up automatically
 extract_geos(the_nc_obj, crs = 4326, name_x = "x", name_y = "y")
} # }
```
