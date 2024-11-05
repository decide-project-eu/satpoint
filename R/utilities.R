#' Utility function to collect all the variable names from an open netCDF file.
#'
#' @param nc_obj An open fvcom netCDF file to extract the data from.
#' @param nodes For a fvcom format netCDF file, whether to return if the variable
#'   makes use of the nodes or elements.
#' @return A tibble of variable names, longnames and (if specified) nodes or
#'   elements.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  the_nc_obj <- ncdf4::nc_open("path/to/netCDF/file.nc")
#'
#'  collect_variables(the_nc_obj, nodes = TRUE)
#' }
collect_variables <- function(nc_obj, nodes = FALSE) {

  # loop through the variables and extract the names/longnames
  all_vars <- lapply(nc_obj$var, function(x) {
    nc_names <- tibble::tibble(
      name = x$name,
      longname = x$longname
    )
    # add the nodes/elements if required, NA if neither
    if (nodes) {
      node_or_elem = ifelse(
        is.null(x$dim[[1]]$name) || !(x$dim[[1]]$name %in% c("node", "nele")),
        yes = NA,
        no = x$dim[[1]]$name
      )

      nc_names <- dplyr::bind_cols(nc_names,tibble::tibble(node_or_elem))
    }

    return(nc_names)

  })

  return(do.call(rbind, all_vars))
}

