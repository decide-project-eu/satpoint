#' Title
#'
#' @param nc_obj
#' @param orig_crs
#' @param crs
#' @param name_x
#' @param name_y
#'
#' @return
#' @export
#'
#' @examples
extract_geos <- function(nc_obj, orig_crs, crs, name_x = "", name_y = "") {

  if (name_x == ""|| name_y == "") {
    message("Extracting the coordinate names directly from the file")
    all_dims <- names(nc_obj$dim)
    name_x <- all_dims[grepl("lon|x_coord", all_dims)]
    name_y <- all_dims[grepl("lat|y_coord", all_dims)]
  }

  x_coords <- ncdf4::ncvar_get(nc_obj, name_x)
  x <- data.frame(x = x_coords, ind = c(1:length(x_coords)))

  y_coords <- ncdf4::ncvar_get(nc_obj, name_y)
  y <- data.frame(y = y_coords, ind = c(1:length(y_coords)))

  nc_geos <- expand.grid(x$x, y$y) |>
    dplyr::left_join(x, by = c(Var1 = "x")) |>
    dplyr::left_join(y, by = c(Var2 = "y")) |>
    dplyr::rename(x = Var1, y = Var2) |>
    sf::st_as_sf(coords = c("x", "y"),
                 crs = orig_crs)

  if(orig_crs != crs) {
    nc_geos <- sf::st_transform(nc_geos, crs = crs)
  }

  return(nc_geos)
}
