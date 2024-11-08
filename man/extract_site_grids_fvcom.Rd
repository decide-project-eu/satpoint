% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process.R
\name{extract_site_grids_fvcom}
\alias{extract_site_grids_fvcom}
\title{Extract a specific variable from a fvcom format netCDF file for all
  provided geographical locations.}
\usage{
extract_site_grids_fvcom(
  nc_file,
  sites,
  nc_crs,
  nc_var_name,
  sites_crs = NULL,
  id = "site",
  dates_to_extract = NULL,
  time_var_name = "time",
  all_sigma = FALSE,
  pretty_dates = TRUE,
  grid_name_x = "",
  grid_name_y = ""
)
}
\arguments{
\item{nc_file}{Filepath for the fvcom netCDF file to extract the data from.}

\item{sites}{Simple features (or spatial) data frame of geographical
areas to extract the variable values for. The data frame should contain a
single row per buffer and have a unique identifier.}

\item{nc_crs}{The numerical value for the crs of the netCDF file.}

\item{nc_var_name}{The name of the variable to extract from the file.}

\item{sites_crs}{The numerical value for the crs of sites, if different
from the crs of the netCDF file.}

\item{id}{The name of the identifier provided in the sites special
features data frame.}

\item{dates_to_extract}{Vector of dates to extract the variable for. If not
provided, all available values will be returned.}

\item{time_var_name}{Name of time/date variable to extract from the netCDF file.
Defaults to "time" as that is by far the most common variable name used.}

\item{all_sigma}{Boolean variable to allow users to specify whether all sigma
layers or levels, often labelled siglay and siglev in fvcom files should
be extracted. Defaults to FALSE, where the first (or top) layer/level will
be extracted.}

\item{pretty_dates}{Boolean variable to control whether to tidy the fvcom
date/times to the nearest hour or not. Defaults to TRUE.}

\item{grid_name_x}{Name of the geographical x coordinate to be extracted.
If not specified, an attempt will be made to establish this from the netCDF
file itself.}

\item{grid_name_y}{Name of the geographical y coordinate to be extracted.
If not specified, an attempt will be made to establish this from the netCDF
file itself.}
}
\value{
A tibble of each location id, with the corresponding variable values
  depths and dates available.
}
\description{
Extract a specific variable from a fvcom format netCDF file for all
  provided geographical locations.
}
\examples{
\dontrun{

 the_nc_file <- "path/to/netCDF/file.nc"

 # presuming we already have a set of spatial buffers to collect data
 # from and that the file contains a "temp" variable
 extract_site_grids_fvcom(the_nc_file, sites = buffers, nc_crs = 4326, nc_var_name = "temp")
}
}
