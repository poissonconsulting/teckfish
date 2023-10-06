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
#' data <- data.frame(
#'   temperature_date_time =
#'     as.POSIXct(c(
#'       "2021-05-07 08:00:00", "2021-05-07 09:00:00",
#'       "2021-05-07 10:00:00", "2021-05-07 11:00:00", "2021-05-07 12:00:00"
#'     )),
#'   water_temperature = c(4.124, 4.078, 4.102, 4.189, 4.243)
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
                                     questionable_hours = 1,
                                     erroneous_hours = 1,
                                     gap_range = 5) {
  chk::check_data(
    data,
    values = list(
      temperature_date_time = as.POSIXct("2021-05-07 08:00:00"),
      water_temperature = 1.5
    )
  )
  ### TODO Add all intermediate columns to chk_not_subset and the corresponding tests
  chk::chk_not_subset(colnames(data), c("status_id"))
  chk::chk_number(questionable_min)
  chk::chk_number(questionable_max)
  chk::chk_number(erroneous_min)
  chk::chk_number(erroneous_max)
  chk::chk_number(questionable_rate)
  chk::chk_number(erroneous_rate)
  chk::chk_number(questionable_hours)
  chk::chk_number(erroneous_hours)
  chk::chk_number(gap_range)

  data <-
    data |>
    dplyr::arrange(.data$temperature_date_time) |>
    dplyr::mutate(
      status_id = 1L,

      # questionable ranges
      status_id = dplyr::case_when(
        .data$water_temperature < questionable_min ~ 2L,
        .data$water_temperature > questionable_max ~ 2L,
        TRUE ~ .data$status_id
      ),
      # erroneous ranges
      status_id = dplyr::case_when(
        .data$water_temperature < erroneous_min ~ 3L,
        .data$water_temperature > erroneous_max ~ 3L,
        TRUE ~ .data$status_id
      ),
      # rate of change
      lag_temp = dplyr::lag(water_temperature),
      diff_temp = abs(water_temperature - lag_temp),
      lag_time = dplyr::lag(temperature_date_time),
      diff_time = as.numeric(difftime(temperature_date_time, lag_time, units = "hours")),
      rate_temp_per_time = abs(diff_temp / diff_time),
      status_id = dplyr::case_when(
        # erroneous rate of change
        rate_temp_per_time > erroneous_rate ~ 3L,
        # questionable rate of change
        rate_temp_per_time > questionable_rate ~ 2L, 
        TRUE ~ status_id
      )
    )

  data
}
