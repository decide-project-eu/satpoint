% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geos.R
\name{collect_geo_names}
\alias{collect_geo_names}
\title{Internal function to extract the names of the geographical coordinates from a
  netCDF file.}
\usage{
collect_geo_names(nc_obj, name_x, name_y)
}
\arguments{
\item{nc_obj}{Open netCDF file to retrieve time data from.}

\item{name_x}{Name of the geographical x coordinate to be extracted.
If not specified, an attempt will be made to establish this from the netCDF
file itself.}

\item{name_y}{Name of the geographical y coordinate to be extracted.
If not specified, an attempt will be made to establish this from the netCDF
file itself.}
}
\value{
A list containing the two names
}
\description{
Internal function to extract the names of the geographical coordinates from a
  netCDF file.
}
