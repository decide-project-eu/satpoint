#' Creates circular buffers around site locations to allow for different
#' spatial averaging to occur when the values are extracted from a netCDF file.
#'
#' @param site_df Data frame of locations to create buffers from. There is a
#'   presumption that each location has a (x, y) coordinates in separate columns
#'   and an identifier.
#' @param x_coord The name of the data frame column that details the x
#'   coordinate of the geographical location.
#' @param y_coord The name of the data frame column that details the y
#'   coordinate of the geographical location.
#' @param crs The numerical value of the to be used in the creation of the
#'   geographical buffer zones. This should match the form of the coordinates
#'   provided in the data frame.
#' @param buffers The radius of the buffers to be created in km.
#' @param unite Whether to unit the identifier of the location and the buffer.
#'   Defaults to TRUE as this creates a unique location/buffer per row when
#'   used in extract_site_grids_nc.
#' @param id The column name that details the location identifier.
#'   Defaults to "site".
#'
#' @return Special features data frame with the original data frame's column,
#'   plus a geometry column identifying the circular buffers around the
#'   individual locations.
#' @export
#'
#' @examples
#' # create data frame of sites
#' sites <- tibble::tibble(
#'   site = LETTERS[1:4],
#'   longitude = c(-5.837704, -6.592514, -7.885055, -3.421410),
#'   latitude = c(57.607846, 57.291280, 56.900022, 58.187849)
#' )
#'
#' # create 10km, 15km and 20km buffers around the sites
#' site_buffers(sites, x_coord = "longitude", y_coord = "latitude",
#'              crs = 4326, buffers = c(10, 15, 20))
#'
site_buffers <- function(site_df, x_coord, y_coord, crs, buffers,
                         unite = TRUE, id = "site") {

  if (any(buffers >= 1000)) {
    stop("Some buffer values are greater than 1000. Please provide buffers in km, rather than m.")
  }

  all_buffers <- lapply(buffers, function(x) {
    site_df |>
      sf::st_as_sf(coords = c(x_coord, y_coord), crs = crs) |>
      sf::st_buffer(dist = x * 1000) |>
      dplyr::mutate(buffer = x)
  })
  all_buffers <- do.call("rbind", all_buffers)

  if (unite) {
    if (!(id %in% colnames(site_df))) {
      stop("Please provide an id column that is present in the dataset, if you wish to unite the buffer and id columns.")
    }
    all_buffers <- all_buffers |>
      dplyr::mutate("{id}" := paste(.data[[id]], buffer, sep = "_")) |>
      dplyr::select(-buffer)
  }


  return(all_buffers)

}

buffer <- NULL
