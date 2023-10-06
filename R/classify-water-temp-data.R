#' Classify Water Temperature Data
#'
#' Water temperature data will be either classified as reasonable (1),
#' questionable (2), or erroneous (3) by adding a new column to the data frame
#' called status_id with the vales of 1, 2, or 3 for each point.
#'
#' @param data A data frame.
#' @param questionable_min A numeric value indicating the lower bound of the
#'   questionable range of temperature values.
#' @param questionable_max A numeric value indicating the upper bound of the
#'   questionable range of temperature values.
#' @param erroneous_min A numeric value indicating the lower bound of the
#'   erroneous range of temperature values.
#' @param erroneous_max A numeric value indicating the upper bound of the
#'   erroneous range of temperature values.
#' @param questionable_rate A numeric value indicating the rate of change
#'   (temperature per hour) of temperature values that is considered
#'   questionable.
#' @param erroneous_rate A numeric value indicating the rate of change
#'   (temperature per hour) of temperature values that is considered erroneous.
#' @param questionable_hours A numeric value indicating a time buffer for
#'   questionable values.
#' @param erroneous_hours A numeric value indicating a time buffer for erroneous
#'   values.
#' @param gap_range A numeric value indicating the range of hours between two
#'   non reasonable values that will be coded as questionable or erroneous.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' data <- data.frame(temperature_date_time = as.POSIXct(c("2021-05-07
#' 08:00:00", "2021-05-07 09:00:00", "2021-05-07 10:00:00", "2021-05-07
#' 11:00:00", "2021-05-07 12:00:00")), water_temperature = c(
#'   4.124, 4.078,
#'   4.102, 4.189, 4.243
#' ))
#'
#' classified_data <- classify_water_temp_data(data)
classify_water_temp_data <- function(data,
                                     questionable_min = 0,
                                     questionable_max = 30,
                                     erroneous_min = -0.5,
                                     erroneous_max = 40,
                                     questionable_rate = 2,
                                     erroneous_rate = 5,
                                     questionable_hours = 1,
                                     erroneous_hours = 1,
                                     gap_range = 5) {

}
