#' Title
#'
#' @param site_df
#' @param x_coord
#' @param y_coord
#' @param crs
#' @param buffers
#' @param unite
#' @param id
#'
#' @return
#' @export
#'
#' @examples
site_buffers <- function(site_df, x_coord, y_coord, crs, buffers, unite = FALSE,
                         id = "site") {

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
