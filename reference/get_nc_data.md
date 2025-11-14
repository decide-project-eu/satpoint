# Internal function to extract the variable values from an open netCDF file.

Internal function to extract the variable values from an open netCDF
file.

## Usage

``` r
get_nc_data(
  ind.x,
  ind.y,
  nc_obj,
  depth_vals,
  nc_var,
  nc_times,
  site,
  swap_ind = FALSE
)
```

## Arguments

- ind.x:

  Index of the geographic x coordinate to start the variable extraction
  from.

- ind.y:

  Index of the geographic y coordinate to start the variable extraction
  from.

- nc_obj:

  Open netCDF file to extract the data from.

- depth_vals:

  The depth values (if present) to match the extracted data to.

- nc_var:

  Name of the variable contained in the netCDF file.

- nc_times:

  Vector of times contained with the netCDF file to match with the
  extracted data.

- site:

  Name of the location that these values will be attached to.

- swap_ind:

  Boolean identifier of whether to change the order of the x, y indices.

## Value

Tibble of all variable values, times and depths extracted from the
provided netCDF file.
