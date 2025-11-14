# Creates circular buffers around site locations to allow for different spatial averaging to occur when the values are extracted from a netCDF file.

Creates circular buffers around site locations to allow for different
spatial averaging to occur when the values are extracted from a netCDF
file.

## Usage

``` r
site_buffers(
  site_df,
  x_coord,
  y_coord,
  crs,
  buffers,
  unite = TRUE,
  id = "site"
)
```

## Arguments

- site_df:

  Data frame of locations to create buffers from. There is a presumption
  that each location has a (x, y) coordinates in separate columns and an
  identifier.

- x_coord:

  The name of the data frame column that details the x coordinate of the
  geographical location.

- y_coord:

  The name of the data frame column that details the y coordinate of the
  geographical location.

- crs:

  The numerical value of the to be used in the creation of the
  geographical buffer zones. This should match the form of the
  coordinates provided in the data frame.

- buffers:

  The radius of the buffers to be created in km.

- unite:

  Whether to unit the identifier of the location and the buffer.
  Defaults to TRUE as this creates a unique location/buffer per row when
  used in extract_site_grids_nc.

- id:

  The column name that details the location identifier. Defaults to
  "site".

## Value

Special features data frame with the original data frame's column, plus
a geometry column identifying the circular buffers around the individual
locations.

## Examples

``` r
# create data frame of sites
sites <- tibble::tibble(
  site = LETTERS[1:4],
  longitude = c(-5.837704, -6.592514, -7.885055, -3.421410),
  latitude = c(57.607846, 57.291280, 56.900022, 58.187849)
)

# create 10km, 15km and 20km buffers around the sites
site_buffers(sites, x_coord = "longitude", y_coord = "latitude",
             crs = 4326, buffers = c(10, 15, 20))
#> Simple feature collection with 12 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -8.218254 ymin: 56.71742 xmax: -3.076761 ymax: 58.36968
#> Geodetic CRS:  WGS 84
#> # A tibble: 12 × 2
#>    site                                                                 geometry
#>    <chr>                                                           <POLYGON [°]>
#>  1 A_10  ((-5.891903 57.52217, -5.889911 57.52227, -5.889591 57.52086, -5.88759…
#>  2 B_10  ((-6.72602 57.23765, -6.725663 57.23627, -6.724667 57.23632, -6.724486…
#>  3 C_10  ((-7.840913 56.81297, -7.836937 56.81323, -7.832962 56.81348, -7.83097…
#>  4 D_10  ((-3.585398 58.16313, -3.585212 58.1618, -3.584815 58.15896, -3.58333 …
#>  5 A_15  ((-5.655942 57.51381, -5.656095 57.51452, -5.653116 57.51465, -5.64914…
#>  6 B_15  ((-6.75816 57.19041, -6.757478 57.18778, -6.7535 57.188, -6.752773 57.…
#>  7 C_15  ((-8.071833 56.81163, -8.071189 56.80955, -8.069197 56.80967, -8.06876…
#>  8 D_15  ((-3.673967 58.16355, -3.673865 58.16284, -3.672874 58.16287, -3.67236…
#>  9 A_20  ((-5.545368 57.51908, -5.545443 57.51943, -5.541972 57.51958, -5.54257…
#> 10 B_20  ((-6.612155 57.47221, -6.612513 57.47361, -6.616525 57.4734, -6.620537…
#> 11 C_20  ((-8.182937 56.82204, -8.182502 56.82065, -8.180506 56.82079, -8.17920…
#> 12 D_20  ((-3.218432 58.33415, -3.22215 58.33405, -3.222172 58.33423, -3.22242 …
```
