#' Extract all variable values from netCDF file for all provided geographical locations.
#'
#' @param nc_file Filepath for the netCDF file to extract the data from.
#' @param  sites Simple features (or spatial) data frame of geographical
#'   areas to extract the variable values for. The data frame should contain a
#'   single row per buffer and have a unique identifier.
#' @param nc_crs The numerical value for the crs of the netCDF file.
#' @param sites_crs The numerical value for the crs of sites, if different
#'   from the crs of the netCDF file.
#' @param use_bbox A boolean value to choose whether the boundary box of the
#'   sites will be used to speed up the extraction of data from the
#'   netCDF file. When a netCDF file cannot be reduced to a small area, or is
#'   particularly detailed, this can drastically speed up data extraction.
#' @param depth_var The name of the depth variable, if necessary, used in the
#'   netCDF file. Defaults to "depth".
#' @param depths_to_extract The specific depth values to extract, if applicable.
#'   By default, if any depth values are present, all of them will be extracted.
#' @param id The name of the identifier provided in the sites special
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
#'  extract_site_grids_nc(the_nc_file, sites = buffers, nc_crs = 4326)
#' }
extract_site_grids_nc <- function(nc_file, sites,
                                  nc_crs, sites_crs = NULL,
                                  use_bbox = FALSE,
                                  depth_var = "depth", depths_to_extract = NULL,
                                  id = "site",
                                  nc_var_name = NULL, dates_to_extract = NULL,
                                  time_var_name = "time",
                                  grid_name_x = "", grid_name_y = "") {

  nc_obj <- ncdf4::nc_open(nc_file)

  all_times <- extract_dates(nc_obj, time_var_name)

  # create buffer box if required
  # (will result in much quicker extraction when netCDF files are very detailed)
  if (use_bbox) {
    the_bbox = sf::st_bbox(sites)
  } else {
    the_bbox = NULL
  }

  nc_grid <- extract_geos(nc_obj, orig_crs = nc_crs, crs = sites_crs,
                          buffer_bbox = the_bbox,
                          name_x = grid_name_x, name_y = grid_name_y)

  if (is.na(sf::st_crs(nc_grid))) {
    nc_grid <- sf::st_set_crs(nc_grid, nc_crs)
    message("Setting crs for netCDF file as none provided directly from the file.")
  }

  if (sf::st_crs(sites) != sf::st_crs(nc_grid)) {
    stop("Please make sure that site_locations has the same crs as supplied
         in the variable sites_crs.")
  }

  sites_df <- sf::st_intersection(nc_grid, sites)

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

  # deal with extracting specific depths (if present)
  if (!is.null(depths_to_extract)) {
    if (is.null(depth_values)) {
      warning("The netCDF file does not include any depth values.")
    } else {
      depths_to_extract <- as.numeric(depths_to_extract)
      if (!any(depths_to_extract %in% depth_values)) {
        warning("Depth values specified do not appear in the file, all depth values will be returned.")
      } else {
        all_nc_sites <- dplyr::filter(all_nc_sites, .data$depth %in% depths_to_extract)
      }
    }
  }

  ncdf4::nc_close(nc_obj)

  return(all_nc_sites)

}

#' Extract a specific variable from a fvcom format netCDF file for all
#'   provided geographical locations.
#'
#' @param nc_file Filepath for the fvcom netCDF file to extract the data from.
#' @param  sites Simple features (or spatial) data frame of geographical
#'   areas to extract the variable values for. The data frame should contain a
#'   single row per buffer and have a unique identifier.
#' @param nc_crs The numerical value for the crs of the netCDF file.
#' @param nc_var_name The name of the variable to extract from the file.
#' @param sites_crs The numerical value for the crs of sites, if different
#'   from the crs of the netCDF file.
#' @param id The name of the identifier provided in the sites special
#'   features data frame.
#' @param dates_to_extract Vector of dates to extract the variable for. If not
#'   provided, all available values will be returned.
#' @param time_var_name Name of time/date variable to extract from the netCDF file.
#'   Defaults to "time" as that is by far the most common variable name used.
#' @param all_sigma Boolean variable to allow users to specify whether all sigma
#'   layers or levels, often labelled siglay and siglev in fvcom files should
#'   be extracted. Defaults to FALSE, where the first (or top) layer/level will
#'   be extracted.
#' @param pretty_dates Boolean variable to control whether to tidy the fvcom
#'   date/times to the nearest hour or not. Defaults to TRUE.
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
#'  # from and that the file contains a "temp" variable
#'  extract_site_grids_fvcom(the_nc_file, sites = buffers, nc_crs = 4326, nc_var_name = "temp")
#' }
extract_site_grids_fvcom <- function(nc_file, sites,
                                     nc_crs, nc_var_name,
                                     sites_crs = NULL,
                                     id = "site",
                                     dates_to_extract = NULL,
                                     time_var_name = "time",
                                     all_sigma = FALSE,
                                     pretty_dates = TRUE,
                                     grid_name_x = "", grid_name_y = "") {

  nc_obj <- ncdf4::nc_open(nc_file)
  nc_grid <- extract_geos_fvcom(nc_obj, orig_crs = nc_crs, crs = sites_crs,
                                name_x = grid_name_x, name_y = grid_name_y)

  if (is.na(sf::st_crs(nc_grid))) {
    nc_grid <- sf::st_set_crs(nc_grid, nc_crs)
    message("Setting crs for netCDF file as none provided directly from the file.")
  }

  if (sf::st_crs(sites) != sf::st_crs(nc_grid)) {
    stop("Please make sure that site_locations has the same crs as supplied
         in the variable sites_crs.")
  }

  nodes_to_select <- sf::st_intersection(nc_grid, sites) |>
    tibble::as_tibble() |>
    dplyr::select(-geometry)

  message("Collecting variable from file.")
  var_res <- extract_nodes_fvcom(nc_obj, nc_var_name, time_var_name, all_sigma,
                                 pretty_dates)

  message("Summarising variable across site")
  sum_var_res <- nodes_to_select |>
    dplyr::left_join(var_res, dplyr::join_by(node)) |>
    dplyr::group_by(site, date_time) |>
    dplyr::summarise("{nc_var_name}" := mean(value), .groups = "drop") |>
    dplyr::left_join(sites, dplyr::join_by(site)) |>
    sf::st_as_sf()

  return(sum_var_res)
}

node <- site <- geometry <- date_time  <- value <- NULL
