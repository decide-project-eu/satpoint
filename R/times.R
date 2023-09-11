#' Title
#'
#' @param nc_obj
#' @param time_var_name
#'
#' @return
#' @export
#'
#' @examples
extract_times <- function(nc_obj, time_var_name = "time") {

  # check input name (if provided)
  rlang::arg_match0(time_var_name, values = names(nc_obj$dim))

  # collect the values from the file
  time_var <- ncdf4::ncatt_get(nc_obj, time_var_name, "units")


  # collect time information from file and use various parts of it
  # this assumes the info is stored as
  # {period} since {date origin}
  # so the second element "since" isn't used
  time_info <- unlist(strsplit(time_var$value, split = " +"))
  time_period <- time_info[1]
  # create base time as Date time object for time arithmetic in next step
  time_base <- lubridate::as_datetime(
    paste(
      time_info[-c(1, 2)],
      collapse = " ")
  )

  # collect all the times stored in the NetCDF file
  stored_times <- ncdf4::ncvar_get(nc_obj, time_var_name)

  # use the time info to change things to something more readable
  if (grepl("sec", time_period)) {
    all_times <- time_base + lubridate::seconds(stored_times)
  } else if (grepl("hours", time_period)) {
    all_times <- time_base + lubridate::hours(stored_times)
  } else if (grepl("days", time_period)) {
    all_times <- time_base + lubridate::days(stored_times)
  } else {
    all_times <- stored_times
    warning("The time period utilised in the files is not seconds, hours or days.
             Times will be returned without conversion.")
  }

  return(all_times)
}
