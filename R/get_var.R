#' Internal function to extract the variable values from an open netCDF file.
#'
#' @param ind.x Index of the geographic x coordinate to start the variable
#'   extraction from.
#' @param ind.y Index of the geographic y coordinate to start the variable
#'   extraction from.
#' @param nc_obj Open netCDF file to extract the data from.
#' @param depth_vals The depth values (if present) to match the extracted data
#'   to.
#' @param nc_var Name of the variable contained in the netCDF file.
#' @param nc_times Vector of times contained with the netCDF file to match with the
#'   extracted data.
#' @param site Name of the location that these values will be attached to.
#' @param swap_ind Boolean identifier of whether to change the order of the x, y
#'   indices.
#'
#' @return Tibble of all variable values, times and depths extracted from the
#'   provided netCDF file.
#'
get_nc_data <- function(ind.x, ind.y, nc_obj, depth_vals, nc_var, nc_times, site,
                        swap_ind = FALSE) {

  # swap indices if needed
  if (swap_ind) {
    first_ind <- ind.y
    second_ind <- ind.x
  } else {
    first_ind <- ind.x
    second_ind <- ind.y
  }


  # deal with
  if (!is.null(depth_vals) && length(nc_times) > 1) {
    nc_start <- c(first_ind , second_ind, 1, 1)
    nc_count <- c(1, 1, -1, -1)
    all_nc_times <- rep(nc_times, length(depth_vals))
  } else {
    nc_start <- c(first_ind , second_ind, 1)
    nc_count <- c(1, 1, -1)
    all_nc_times <- nc_times
  }

  # collect the data from the nc file and rearrange
  nc_data <- ncdf4::ncvar_get(nc_obj, nc_var, start = nc_start, count = nc_count) |>
    as.list() |>
    unlist() |>
    as.data.frame()

  colnames(nc_data) <- nc_var

  # add the times (for ease, using base R notation)
  nc_data$datetime <- all_nc_times

  # only adding a depth column if needed
  if (length(depth_vals) > 0) {
    nc_data$depth <- sort(rep(depth_vals, length(nc_times)))
  }

  nc_data <- dplyr::mutate(nc_data, site = site)

  return(nc_data)

}








