#' Extract all variable values from netCDF file for all provided geographical locations.
#'
#' @param nc_file Filepath for the netCDF file to extract the data from.
#' @param site_buffers Simple features (or spatial) data frame of geographical
#'   areas to extract the variable values for. The data frame should contain a
#'   single row per buffer and have a unique identifier.
#' @param nc_crs The numerical value for the crs of the netCDF file.
#' @param sites_crs The numerical value for the crs of site_buffers, if different
#'   from the crs of the netCDF file.
#' @param depth_var The name of the depth variable, if necessary, used in the
#'   netCDF file. Defaults to "depth".
#' @param id The name of the identifier provided in the site_buffers special
#'   features data frame.
#' @param nc_var_name The name of the variable in the netCDF file that is the focus
#'   of the extraction. If not specified, an attempt will be made to establish
#'   this from the netCDF file itself.
#' @param dates_to_extract Vector of dates to extract the variable for. If not
#'   provided, all available values will be returned.
#' @param time_var_name Name of time/date variable to extract from the netCDF file.
#'   Defaults to "time" as that is by far the most common variable name used.
#' @param grid_name_x Name of the geographical x coordinate to be extracted.
#'   If not specified, an attempt will be made to establish this from the netCDF
#'   file itself.
#' @param grid_name_y Name of the geographical y coordinate to be extracted.
#'   If not specified, an attempt will be made to establish this from the netCDF
#'   file itself.
#'
#' @return A tibble of each location id, with the corresponding variable values
#'   depths and dates available.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  the_nc_file <- "path/to/netCDF/file.nc"
#'
#'  # presuming we already have a set of spatial buffers to collect data
#'  # from
#'  extract_site_grids_nc(the_nc_file, site_buffers = buffers, nc_crs = 4326)
#' }
extract_site_grids_nc <- function(nc_file, site_buffers,
                                  nc_crs, sites_crs = NULL,
                                  depth_var = "depth", id = "site",
                                  nc_var_name = NULL, dates_to_extract = NULL,
                                  time_var_name = "time",
                                  grid_name_x = "", grid_name_y = "") {

  nc_obj <- ncdf4::nc_open(nc_file)

  all_times <- extract_dates(nc_obj, time_var_name)

  nc_grid <- extract_geos(nc_obj, orig_crs = nc_crs, crs = sites_crs,
                          name_x = grid_name_x, name_y = grid_name_y)

  if (is.na(sf::st_crs(nc_grid))) {
    nc_grid <- sf::st_set_crs(nc_grid, nc_crs)
    message("Setting crs for netCDF file as none provided directly from the file.")
  }

  if (sf::st_crs(site_buffers) != sf::st_crs(nc_grid)) {
    stop("Please make sure that site_locations has the same crs as supplied
         in the variable sites_crs.")
  }

  sites_df <- sf::st_intersection(nc_grid, site_buffers)

  # deal with depth once (as should be consistent across the nc_obj)
  depth_present <- any(grepl(depth_var, names(nc_obj$dim)))
  if (depth_present) {
    depth_values <- ncdf4::ncvar_get(nc_obj, depth_var)
    message(paste("Depth values are",
                  paste(depth_values, collapse = ", ")))
  } else {
    depth_values <- NULL
  }

  # extract variable name once
  if (is.null(nc_var_name)) {
    var_name <- names(nc_obj$var)[1]
    message(paste("Variable extracted from file is", var_name))
  } else {
    var_name <- nc_var_name
  }

  # check dimensions to ensure that the order of indices is correct
  nc_dims <- nc_obj$var[[var_name]][["size"]]
  # the first two parts should be the x, y dimensions but sometimes these are
  # swapped
  max_x_index <- max(nc_grid$ind.x)
  max_y_index <- max(nc_grid$ind.y)
  if (identical(c(max_x_index, max_y_index), nc_dims[c(1, 2)])) {
    swap_flag = FALSE
  } else if (identical(c(max_x_index, max_y_index), nc_dims[c(2, 1)])) {
    swap_flag = TRUE
  } else {
    stop("The dimensions of the geographical indices do not match the dimensions of the geographical variables.")
  }


  all_nc_sites <- sites_df |>
    split(sites_df[[id]]) |>
    lapply(function(df) {
      message(paste("Processing", id, unique(df[[id]])))
      results <- mapply(get_nc_data,
                        df$ind.x, df$ind.y,
                        MoreArgs = list(
                          nc_obj = nc_obj,
                          depth_vals = depth_values,
                          nc_var = var_name,
                          nc_times = all_times,
                          site = unique(df[[id]]),
                          swap_ind = swap_flag),
                        SIMPLIFY = FALSE)
      return(do.call("rbind", results))
    })

  all_nc_sites <- do.call("rbind", all_nc_sites) |>
    tibble::as_tibble(.name_repair = "minimal") |>
    dplyr::relocate(dplyr::all_of(id), .before = 1)

  # deal with times that need to be extracted, if required
  if (!is.null(dates_to_extract)) {
    all_nc_sites <- filter_dates(all_nc_sites, dates_to_extract)
  }


  ncdf4::nc_close(nc_obj)

  return(all_nc_sites)

}
