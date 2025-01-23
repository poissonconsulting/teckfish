#' Classify Water Temperature Data
#'
#' A wrapper on `classify_time_series_data()` with the arguments set for
#' water temperature data.
#' 
#' @inheritParams params
#'
#' @return A data frame
#' @export
#' @examples
#' data <- data.frame(
#'   temperature_date_time =
#'     as.POSIXct(c(
#'       "2021-05-07 08:00:00", "2021-05-07 09:00:00",
#'       "2021-05-07 10:00:00", "2021-05-07 11:00:00", "2021-05-07 12:00:00",
#'       "2021-05-07 13:00:00"
#'     )),
#'   water_temperature = c(4.124, 4.078, 4.102, 4.189, 4.243, 6.578)
#' )
#'
#' classified_data <- classify_water_temp_data(data)
classify_water_temp_data <- function(data,
                                     questionable_min = 0,
                                     questionable_max = 30,
                                     erroneous_min = -0.5,
                                     erroneous_max = 40,
                                     questionable_rate = 2,
                                     erroneous_rate = 5,
                                     questionable_buffer = 1,
                                     erroneous_buffer = 1,
                                     gap_range = 5,
                                     date_time = "temperature_date_time",
                                     value = "water_temperature") {
  
  classify_time_series_data(
    data, 
    date_time = date_time,
    value = value,
    questionable_min = questionable_min,
    questionable_max = questionable_max,
    erroneous_min = erroneous_min,
    erroneous_max = erroneous_max,
    questionable_rate = questionable_rate,
    erroneous_rate = erroneous_rate,
    questionable_buffer = questionable_buffer,
    erroneous_buffer = erroneous_buffer,
    gap_range = gap_range)
}