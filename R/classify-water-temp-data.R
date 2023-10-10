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
      lag_temp = dplyr::lag(.data$water_temperature),
      diff_temp = abs(.data$water_temperature - .data$lag_temp),
      lag_time = dplyr::lag(.data$temperature_date_time),
      diff_time = as.numeric(difftime(
        .data$temperature_date_time,
        .data$lag_time,
        units = "hours"
      )),
      rate_temp_per_time = abs(.data$diff_temp / .data$diff_time),
      status_id = dplyr::case_when(
        # erroneous rate of change
        .data$rate_temp_per_time > erroneous_rate ~ 3L,
        # questionable rate of change
        .data$rate_temp_per_time > questionable_rate ~ 2L,
        TRUE ~ .data$status_id
      )
    ) |>
    dplyr::select(
      -"lag_temp", -"diff_temp", -"lag_time", -"diff_time",
      -"rate_temp_per_time"
    )
  
  questionable_rows <- which(data$status_id == 2)
  error_rows <- which(data$status_id == 3)
  
  data <- 
    data |>
    dplyr::mutate(
      id = dplyr::row_number()
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      # find closest questionable/erroneous value above and below
      quest_id_above = list(
        questionable_rows[which(questionable_rows > .data$id)]
      ),
      quest_id_below = list(
        questionable_rows[which(questionable_rows < .data$id)]
      ),
      
      error_id_above = list(
        error_rows[which(error_rows > .data$id)]
      ),
      error_id_below = list(
        error_rows[which(error_rows < .data$id)]
      )
      
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      quest_id_above = purrr::map_int(.data$quest_id_above, min2),
      quest_id_below = purrr::map_int(.data$quest_id_below, max2),
      
      error_id_above = purrr::map_int(.data$error_id_above, min2),
      error_id_below = purrr::map_int(.data$error_id_below, max2),
      
      quest_id_above2 = .data$temperature_date_time[.data$quest_id_above],
      quest_id_below2 = .data$temperature_date_time[.data$quest_id_below],
      error_id_above2 = .data$temperature_date_time[.data$error_id_above],
      error_id_below2 = .data$temperature_date_time[.data$error_id_below],
      
      quest_id_above3 = diff_hours(.data$quest_id_above2, .data$temperature_date_time),
      quest_id_below3 = diff_hours(.data$temperature_date_time, .data$quest_id_below2),
      error_id_above3 = diff_hours(.data$error_id_above2, .data$temperature_date_time),
      error_id_below3 = diff_hours(.data$temperature_date_time, .data$error_id_below2),
      
      # anything within an hour of a questionable value is questionable
      status_id = dplyr::if_else(
        .data$status_id == 1 & .data$quest_id_above3 <= questionable_hours, 
        2, 
        .data$status_id, 
        .data$status_id
      ),
      status_id = dplyr::if_else(
        .data$status_id == 1 & .data$quest_id_below3 <= questionable_hours, 
        2, 
        .data$status_id, 
        .data$status_id
      ),
      
      # anything within an hour of an erroneous value is erroneous
      status_id = dplyr::if_else(
        .data$status_id %in% c(1, 2) & .data$error_id_above3 <= erroneous_hours, 
        3, 
        .data$status_id, 
        .data$status_id
      ),
      status_id = dplyr::if_else(
        .data$status_id %in% c(1, 2) & .data$error_id_below3 <= erroneous_hours, 
        3, 
        .data$status_id, 
        .data$status_id
      ),
      
      # Fill in gap between questionable/erroneous values 
      gap_above = pmin(.data$error_id_above2, .data$quest_id_above2, na.rm = TRUE),
      gap_above_type = dplyr::case_when(
        .data$gap_above == .data$error_id_above2 ~ "err",
        .data$gap_above == .data$quest_id_above2 ~ "quest",
        TRUE ~ NA_character_
      ),
      gap_below = pmax(.data$error_id_below2, .data$quest_id_below2, na.rm = TRUE),
      gap_below_type = dplyr::case_when(
        .data$gap_below == .data$error_id_below2 ~ "err",
        .data$gap_below == .data$quest_id_below2 ~ "quest",
        TRUE ~ NA_character_
      ),
      gap_diff = diff_hours(.data$gap_above, .data$gap_below),
      
      status_id = dplyr::case_when(
        # if the gap less then 5 and at least one value is erroneous code the gap as erroneous
        .data$status_id == 1 & .data$gap_diff <= 5 & (.data$gap_above_type == "err" | .data$gap_below_type == "err")   ~ 3,
        # if the gap less then 5 (and not touching erroneous) then code as questionable 
        .data$status_id == 1 & .data$gap_diff <= 5   ~ 2,
        TRUE ~ .data$status_id
      )
      
    )

  data
}




diff_hours <- function(x, y) {
  as.numeric(difftime(x, y, units = "hours"))
}


min2 <- function(x) {
  if (length(x) == 0) {
    NA_integer_
  } else {
    min(x)
  }
}

max2 <- function(x) {
  if (length(x) == 0) {
    NA_integer_
  } else {
    max(x)
  }
}