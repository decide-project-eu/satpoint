# Downloading files from the NASA's Earth Data Service

In this article we will demonstrate how to download a collection of
`.nc4` files from NASA’s Earth Data resource.

## Collecting the Files

The first step, which you may well have already completed is to collect
the `.nc` or `.nc4` files that you wish to work with. In this example we
will collect a month of **precipitation** data from
<https://urs.earthdata.nasa.gov/>. To do this we will automate the
[instructions](https://urs.earthdata.nasa.gov/documentation/for_users/data_access/r)
provided by NASA themselves.

### Registering with EarthData

Prior to collecting the files you need to register with the service,
creating a username and password. You can then create the `.netrc` and
`.urs_cookies` files as recommended on the [Earth Data
Wiki](https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget).

### Creating List of Files to Download

Having created these files we recommend that you create a list of files
to download. To do this you can follow these steps as an example:

1.  Navigate to
    <https://disc.gsfc.nasa.gov/datasets/GPM_3IMERGDL_07/summary>
2.  From the Data Access menu, click Subset / Get Data
3.  From Download Method:, select “Get File Subsets using the GES DISC
    Subsetter”
4.  Choose your Date range, Region and Variables. We selected June, 2020
    and an area around Scotland. For example purposes our variable is
    just Precipitation.
5.  Click on Get Data. This will populate a list of links, which will
    take a minute or two.
6.  When complete, download the list of links.

This leaves us with a file with the following contents, first few lines
shown.

``` r
readLines("list_of_links.txt")[1:6]
#> [1] "https://arthurhou.pps.eosdis.nasa.gov/Documents/IMERG_V07_ATBD_final.pdf"                                                                                                                                            
#> [2] "https://gpm1.gesdisc.eosdis.nasa.gov/data/GPM_L3/doc/README.GPM.pdf"                                                                                                                                                 
#> [3] "https://gpm1.gesdisc.eosdis.nasa.gov/opendap/GPM_L3/GPM_3IMERGDL.07/2020/06/3B-DAY-L.MS.MRG.3IMERG.20200601-S000000-E235959.V07B.nc4.nc4?precipitation[0:0][1717:1759][1450:1491],time,lon[1717:1759],lat[1450:1491]"
#> [4] "https://gpm1.gesdisc.eosdis.nasa.gov/opendap/GPM_L3/GPM_3IMERGDL.07/2020/06/3B-DAY-L.MS.MRG.3IMERG.20200602-S000000-E235959.V07B.nc4.nc4?precipitation[0:0][1717:1759][1450:1491],time,lon[1717:1759],lat[1450:1491]"
#> [5] "https://gpm1.gesdisc.eosdis.nasa.gov/opendap/GPM_L3/GPM_3IMERGDL.07/2020/06/3B-DAY-L.MS.MRG.3IMERG.20200603-S000000-E235959.V07B.nc4.nc4?precipitation[0:0][1717:1759][1450:1491],time,lon[1717:1759],lat[1450:1491]"
#> [6] "https://gpm1.gesdisc.eosdis.nasa.gov/opendap/GPM_L3/GPM_3IMERGDL.07/2020/06/3B-DAY-L.MS.MRG.3IMERG.20200604-S000000-E235959.V07B.nc4.nc4?precipitation[0:0][1717:1759][1450:1491],time,lon[1717:1759],lat[1450:1491]"
```

### Download and Storing Files using `R`

We can now download these files. Our approach is that we’ll download all
of these files, and process them separately (possibly offline). We could
also download and process an individual file.

We’ll begin by loading the packages we’ll use. Note that we use the
`tidyverse` suite of packages but this is not compulsory, it is just our
preference.

``` r
library(tidyverse)
library(httr) # to GET the files
library(satpoint) # to process them later
```

We can now get the files to download into `R` and use the same to create
a list of output files.

``` r
# skip the first two lines as they are general info files
nc_urls <- read_lines("list_of_links.txt", skip = 2)

# use a quick regular expression to create list of outputs
nc_files_to_create <- str_extract(
  nc_urls,
  "3B-DAY-L.MS.MRG.3IMERG.[0-9]{8}-S000000-E235959.V07B.nc4"
)
```

Finally, we can set the file paths for the `.netrc` and `.urs_cookies`
files and use the
[`walk2()`](https://purrr.tidyverse.org/reference/map2.html) function to
download each file.
