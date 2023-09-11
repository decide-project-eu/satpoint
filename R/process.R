#' Title
#'
#' @param nc_file
#' @param site_locations
#' @param nc_crs
#' @param sites_crs
#' @param depth_var
#' @param split_column
#' @param nc_var_name
#' @param grid_name_x
#' @param grid_name_y
#'
#' @return
#' @export
#'
#' @examples
extract_site_grids_nc <- function(nc_file, site_locations,
                                  nc_crs, sites_crs,
                                  depth_var = "depth", split_column = "site",
                                  nc_var_name = NULL,
                                  grid_name_x = "", grid_name_y = "") {

  nc_obj <- ncdf4::nc_open(nc_file)

  all_times <- extract_times(nc_obj)

  nc_grid <- extract_geos(nc_obj, orig_crs = nc_crs, crs = sites_crs,
                          name_x = grid_name_x, name_y = grid_name_y)

  if (sf::st_crs(site_locations) != sf::st_crs(nc_grid)) {
    stop("Please make sure that site_locations has the same crs as supplied
         in the variable sites_crs.")
  }

  sites_df <- sf::st_intersection(nc_grid, site_locations)

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
  # the first two parts should be the x, y dimensions but someimes these are
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
    split(sites_df[[split_column]]) |>
    lapply(function(df) {
      message(paste("Processing", split_column, unique(df[[split_column]])))
      results <- mapply(get_nc_data,
                        df$ind.x, df$ind.y,
                        MoreArgs = list(
                          nc_obj = nc_obj,
                          depth_vals = depth_values,
                          nc_var = var_name,
                          nc_times = all_times,
                          site = unique(df[[split_column]]),
                          swap_ind = swap_flag),
                        SIMPLIFY = FALSE)
      return(do.call("rbind", results))
    })


  all_nc_sites <- do.call("rbind", all_nc_sites) |>
    tibble::as_tibble(.name_repair = "minimal") |>
    dplyr::relocate(dplyr::all_of(split_column), .before = 1) |>
    dplyr::rename("{split_column}" := site)

  ncdf4::nc_close(nc_obj)

  return(all_nc_sites)

}
