# Internal function to extract the names of the geographical coordinates from a netCDF file.

Internal function to extract the names of the geographical coordinates
from a netCDF file.

## Usage

``` r
collect_geo_names(nc_obj, name_x, name_y)
```

## Arguments

- nc_obj:

  Open netCDF file to retrieve time data from.

- name_x:

  Name of the geographical x coordinate to be extracted. If not
  specified, an attempt will be made to establish this from the netCDF
  file itself.

- name_y:

  Name of the geographical y coordinate to be extracted. If not
  specified, an attempt will be made to establish this from the netCDF
  file itself.

## Value

A list containing the two names
