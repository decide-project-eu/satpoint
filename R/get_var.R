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
    all_nc_times <- rep(nc_times, each = length(depth_vals), times = 1)
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
    nc_data$depth <- rep(depth_vals, times = length(nc_times))
  }

  nc_data <- dplyr::mutate(nc_data, site = site)

  return(nc_data)

}

#' Extract the values for the node variables present in an fvcom netCDF file.
#'
#' @param nc_obj An open fvcom netCDF file to extract the data from.
#' @param nc_var_name The name of the variable in the netCDF file that is the focus
#'   of the extraction.
#' @param time_var_name Name of time/date variable to extract from the netCDF file.
#'   Defaults to "time" as that is by far the most common variable name used.
#' @param all_sigma Boolean variable to allow users to specify whether all sigma
#'   layers or levels, often labelled siglay and siglev in fvcom files should
#'   be extracted. Defaults to FALSE, where the first (or top) layer/level will
#'   be extracted.
#' @param pretty_dates Boolean variable to control whether to tidy the fvcom
#'   date/times to the nearest hour or not. Defaults to TRUE.
#' @return A tibble of variable values, along with the corresponding dates, nodes
#'   and (if specified) sigma layers/levels.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  the_nc_obj <- ncdf4::nc_open("path/to/netCDF/file.nc")
#'
#'  # taking a temp variable (presuming it exists within the fvcom netCDF file)
#'  extract_nodes_fvcom(the_nc_obj, nc_var_name = "temp")
#' }
extract_nodes_fvcom <- function(nc_obj, nc_var_name,
                                time_var_name = "time", all_sigma = FALSE,
                                pretty_dates = TRUE) {

  num_nodes <- nc_obj$dim$node$len

  # check if variable present
  if ( !(nc_var_name %in% names(nc_obj$var)) ) {
    stop("Variable specified not present in file, please choose another.")
  }

  # check if a node variable
  if (nc_obj$var[[nc_var_name]]$dim[[1]]$name != "node" ||
      length(nc_obj$var[[nc_var_name]]$dim[[1]]$vals) != num_nodes) {
    stop(paste(
      "This variable cannot be extracted as it is not associated with the nodes",
      "present in the netCDF file. Please choose another variable.")
    )
  }

  ndims <- length(nc_obj$var[[nc_var_name]]$dim)
  dates <- extract_dates(nc_obj, time_var_name, pretty = pretty_dates)

  # define start/end points for data extraction
  if (ndims == 2) {
    # add check that first is nodes/second is time
    start = c(1, 1)
    count = c(-1, -1)
  } else if (ndims == 3) {
    if (all_sigma) {
      start = c(1, 1, 1)
      count = c(-1, -1, -1)
    } else {
      start = c(1, 1, 1)
      count = c(-1, 1, -1)
    }
  }

  # get the data and convert to tibble for each of use later
  var_res <- ncdf4::ncvar_get(nc_obj, nc_var_name, start = start, count = count) |>
    as.list() |>
    unlist() |>
    tibble::as_tibble()

  # add other columns depending on dimensions and what was collected
  if (ndims == 3 && all_sigma) {
    second_var <- nc_obj$var[[nc_var_name]]$dim[[2]]$name
    lay_or_lev <- ncdf4::ncvar_get(nc_obj, second_var, start = c(1, 1), count = c(1, -1))
    var_res <- var_res |>
      dplyr::mutate({{second_var}} := rep(lay_or_lev, each = num_nodes, times = length(dates)),
                    date_time = rep(dates, each = length(lay_or_lev) * num_nodes),
                    node = rep(c(1:num_nodes), length(lay_or_lev) * length(dates)))
  } else {
    var_res <- var_res |>
      dplyr::mutate(date_time = rep(dates, each = num_nodes),
                    node = rep(c(1:num_nodes), length(dates)))
  }

  return(var_res)

}


