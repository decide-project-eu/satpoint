# Internal function to assist the user in extracting only particular dates from the results of a netCDF file.

Internal function to assist the user in extracting only particular dates
from the results of a netCDF file.

## Usage

``` r
filter_dates(all_nc_data, dates_to_extract)
```

## Arguments

- all_nc_data:

  Data frame containing all the results extracted from a netCDF file.

- dates_to_extract:

  Vector of dates to be extracted.

## Value

Data frame containing the results filtered by date.
