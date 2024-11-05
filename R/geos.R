#' Extract all geographical coordinates from a netCDF file and convert to simple
#'   features object.
#'
#' @param nc_obj Open netCDF file to retrieve time data from.
#' @param orig_crs Numerical value of the crs used in the netCDF file.
#' @param buffer_bbox Result of sf::st_bbox() or similar to provide a vector of
#'   of the buffers. This enables extracting only a small part of the
#'   entire netCDF file if required. The order is assumed to be xmin, ymin,
#'   xmax and ymax.
#' @param crs Numerical value of the crs to change results into, if different
#'   from the original crs.
#' @param name_x Name of the geographical x coordinate to be extracted.
#'   If not specified, an attempt will be made to establish this from the netCDF
#'   file itself.
#' @param name_y Name of the geographical y coordinate to be extracted.
#'   If not specified, an attempt will be made to establish this from the netCDF
#'   file itself.
#'
#' @return Special features data frame containing the geographic coordinates of
#'   the provided netCDF file.
#' @export
#'
#' @examples
#' \dontrun{
#'  # open the netCDF file
#'  the_nc_obj <- ncdf4::nc_open("path/to/netCDF/file.nc")
#'
#'  # extract all spatial points from the netCDF file
#'  extract_geos(the_nc_obj, crs = 4326)
#'
#'  # if the names of the spatial coordindates aren't picked up automatically
#'  extract_geos(the_nc_obj, crs = 4326, name_x = "x", name_y = "y")
#' }
extract_geos <- function(nc_obj, orig_crs, buffer_bbox = NULL, crs = NULL, name_x = "", name_y = "") {

  coord_names <- collect_geo_names(nc_obj, name_x, name_y)

  x_coords <- ncdf4::ncvar_get(nc_obj, coord_names$x)
  x <- data.frame(x = x_coords, ind = c(1:length(x_coords)))

  y_coords <- ncdf4::ncvar_get(nc_obj, coord_names$y)
  y <- data.frame(y = y_coords, ind = c(1:length(y_coords)))

  # reduce the coords using the bounding box if present
  if (!is.null(buffer_bbox)) {
    x <- x[which(x$x > buffer_bbox[1] & x$x < buffer_bbox[3]), ]
    y <- y[which(y$y > buffer_bbox[2] & y$y < buffer_bbox[4]), ]
  }

  nc_geos <- expand.grid(x$ind, y$ind) |>
    dplyr::left_join(x, by = c(Var1 = "ind")) |>
    dplyr::left_join(y, by = c(Var2 = "ind")) |>
    dplyr::rename(ind.x = Var1, ind.y = Var2) |>
    sf::st_as_sf(coords = c("x", "y"),
                 crs = orig_crs)

  if(!is.null(crs) && orig_crs != crs) {
    nc_geos <- sf::st_transform(nc_geos, crs = crs)
  }

  return(nc_geos)
}

#' Internal function to extract the names of the geographical coordinates from a
#'   netCDF file.
#'
#' @param nc_obj Open netCDF file to retrieve time data from.
#' @param name_x Name of the geographical x coordinate to be extracted.
#'   If not specified, an attempt will be made to establish this from the netCDF
#'   file itself.
#' @param name_y Name of the geographical y coordinate to be extracted.
#'   If not specified, an attempt will be made to establish this from the netCDF
#'   file itself.
#'
#' @return A list containing the two names
collect_geo_names <- function(nc_obj, name_x, name_y) {

  if (name_x == ""|| name_y == "") {
    message("Extracting the coordinate names directly from the file")
    all_dims <- union(names(nc_obj$dim), names(nc_obj$var))
    name_x <- all_dims[grepl("lon|x_coord", all_dims)]
    name_y <- all_dims[grepl("lat|y_coord", all_dims)]
  }

  if (length(name_x) > 1 || length(name_y) > 1) {
    warning("More than one possible match for coordinate names, the first match will be used.")
    name_x <- name_x[1]
    name_y <- name_y[1]
  }

  return(list("x" = name_x, "y" = name_y))
}


#' Extract all geographical coordinates of the nodes from a fvcom netCDF file
#'   and convert to simple features object.
#'
#' @param nc_obj Open fvcom netCDF file to retrieve time data from.
#' @param orig_crs Numerical value of the crs used in the netCDF file.
#' @param crs Numerical value of the crs to change results into, if different
#'   from the original crs.
#' @param name_x Name of the geographical x coordinate to be extracted.
#'   If not specified, an attempt will be made to establish this from the netCDF
#'   file itself.
#' @param name_y Name of the geographical y coordinate to be extracted.
#'   If not specified, an attempt will be made to establish this from the netCDF
#'   file itself.
#'
#' @return Special features data frame containing the geographic coordinates of
#'   the nodes in the provided fcvom netCDF file.
#' @export
#'
#' @examples
#' \dontrun{
#'  # open the netCDF file
#'  the_nc_obj <- ncdf4::nc_open("path/to/netCDF/file.nc")
#'
#'  # extract all spatial points from the netCDF file
#'  extract_geos_fvcom(the_nc_obj, crs = 4326)
#'
#'  # if the names of the spatial coordindates aren't picked up automatically
#'  extract_geos_fvcom(the_nc_obj, crs = 4326, name_x = "lon", name_y = "lat")
#' }
extract_geos_fvcom <- function(nc_obj, orig_crs, crs = NULL, name_x = "", name_y = "") {

  coord_names <- collect_geo_names(nc_obj, name_x, name_y)
  num_nodes <- nc_obj$dim$node$len

  nc_geos <- tibble::tibble(
    node = c(1:num_nodes),
    lon = ncdf4::ncvar_get(nc_obj, coord_names$x),
    lat = ncdf4::ncvar_get(nc_obj, coord_names$y)
  ) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = orig_crs)

  if(!is.null(crs) && orig_crs != crs) {
    nc_geos <- sf::st_transform(nc_geos, crs = crs)
  }

  return(nc_geos)
}


Var1 <- Var2 <- NULL
