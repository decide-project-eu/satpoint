#' Extract all date or datetimes from an nc file.
#'
#' @param nc_obj Open nc file to retrieve time data from
#' @param time_var_name Name of time/date variable to extract from the nc file.
#'   Defaults to "time" as that is by far the most common variable name used.
#'
#' @return Vector of date or datetimes extracted from the nc file.
#' @export
#'
extract_dates <- function(nc_obj, time_var_name = "time") {

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
    all_dates <- time_base + lubridate::seconds(stored_times)
  } else if (grepl("hours", time_period)) {
    all_dates <- time_base + lubridate::hours(stored_times)
  } else if (grepl("days", time_period)) {
    all_dates <- time_base + lubridate::days(stored_times)
  } else {
    all_dates <- stored_times
    warning("The time period utilised in the files is not seconds, hours or days.
             Times will be returned without conversion.")
  }

  return(all_dates)
}



#' Internal function to assist the user in extracting only particular dates
#' from the results of an nc file.
#'
#' @param all_nc_data Data frame containing all the results extracted from an
#' nc file.
#' @param dates_to_extract Vector of dates to be extracted.
#'
#' @return Data frame containing the results filtered by date.
filter_dates <- function(all_nc_data, dates_to_extract) {

  dates_for_join <- tibble::tibble(
    date = lubridate::as_date(dates_to_extract)
  )

  filter_nc_data <- all_nc_data |>
    dplyr::mutate(date = lubridate::as_date(.data$datetime)) |>
    dplyr::inner_join(dates_for_join)

  dates_missing <- dates_for_join$date |>
    setdiff(filter_nc_data$date) |>
    lubridate::as_date()

  filter_nc_data <- dplyr::select(filter_nc_data, -date)

  if (nrow(filter_nc_data) == 0) {
    warning("The requested date(s) are not present in the nc file.
            All dates have been returned.")
    filter_nc_data <- all_nc_data
  } else if (length(dates_missing) != 0) {
    warning(paste("The following dates were not present in the dataset:",
                  dates_missing, "."))
  }

  return(filter_nc_data)

}




