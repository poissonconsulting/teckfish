#' Classify Water Temperature Data
#'
#' Water temperature data will be either classified as reasonable, questionable,
#' or erroneous in the status_id column.
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
#' @param questionable_buffer A numeric value indicating a time buffer for
#'   questionable values.
#' @param erroneous_buffer A numeric value indicating a time buffer for
#'   erroneous values.
#' @param gap_range A numeric value indicating the range of hours between two
#'   non reasonable values that will be coded as questionable or erroneous.
#'
#' @return A data frame
#' @export
#' @details The function only works on a single deployment of a logger. The
#'   table output will be sorted by temperature_date_time.
#'
#'   The function will error if you have columns with the following names as
#'   they are used internally: status_id,  ".lag_temp", ".diff_temp",
#'   ".lag_time", ".diff_time", ".rate_temp_per_time", ".lag_id", ".lead_id",
#'   ".id_row", ".quest_higher_next_id", ".quest_lower_next_id",
#'   ".error_higher_next_id", ".error_lower_next_id", ".quest_higher_next_time",
#'   ".quest_lower_next_time", ".error_higher_next_time",
#'   ".error_lower_next_time", ".quest_higher_time_diff_h",
#'   ".quest_lower_time_diff_h", ".error_higher_time_diff_h",
#'   ".error_lower_time_diff_h", ".gap_fill_higher_time",
#'   ".gap_fill_higher_type", ".gap_fill_lower_time", ".gap_fill_lower_type",
#'   ".gap_diff_time_h"
#'
#'
#'   The function will error if there are missing temperature_date_time values
#'   missing. Missing values in water_temperature are ignored and treated as if
#'   they are not present. If you want to drop these values you can do that to
#'   the output by using [tidyr::drop_na()].
#'
#'   The data is processed by:
#'
#'   1. Classifying the temperature values based on their absolute values
#'   (questionable_min, questionable_max, erroneous_min, erroneous_max). 2. The
#'   rate of change between adjacent values is calculate and values are
#'   classified based on the rate parameters (questionable_rate,
#'   erroneous_rate). 3. Adjacent values to questionable/erroneous are coded as
#'   questionable/erroneous. 4. A buffer is applied that any value within the
#'   buffer is classified as questionable/erroneous based on the buffer
#'   parameters (questionable_buffer, erroneous_buffer). 5. Reasonable values
#'   identified between two questionable/erroneous values are coded as
#'   questionable/erroneous based on the gap hour difference allowed
#'   (gap_range).
#'
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
                                     gap_range = 5) {
  chk::check_data(
    data,
    values = list(
      temperature_date_time = as.POSIXct("2021-05-07 08:00:00"),
      water_temperature = c(1.5, NA_real_)
    )
  )
  chk::chk_unique(data$temperature_date_time)

  chk::chk_not_subset(colnames(data), c("status_id"))
  chk::chk_not_subset(
    colnames(data), 
    c(
      ".lag_temp", ".diff_temp", ".lag_time", ".diff_time", 
      ".rate_temp_per_time", ".lag_id", ".lead_id", ".id_row",
      ".quest_higher_next_id", ".quest_lower_next_id",
      ".error_higher_next_id", ".error_lower_next_id",
      ".quest_higher_next_time", ".quest_lower_next_time",
      ".error_higher_next_time", ".error_lower_next_time",
      ".quest_higher_time_diff_h", ".quest_lower_time_diff_h",
      ".error_higher_time_diff_h", ".error_lower_time_diff_h",
      ".gap_fill_higher_time", ".gap_fill_higher_type",
      ".gap_fill_lower_time", ".gap_fill_lower_type",
      ".gap_diff_time_h"
    )
  )
  
  chk::chk_number(questionable_min)
  chk::chk_number(questionable_max)
  chk::chk_gt(questionable_max, questionable_min)

  chk::chk_number(erroneous_min)
  chk::chk_number(erroneous_max)
  chk::chk_gt(erroneous_max, erroneous_min)

  chk::chk_gt(erroneous_max, questionable_max)
  chk::chk_lt(erroneous_min, questionable_min)

  chk::chk_number(questionable_rate)
  chk::chk_gte(questionable_rate, 0)

  chk::chk_number(erroneous_rate)
  chk::chk_gte(erroneous_rate, 0)

  chk::chk_gte(erroneous_rate, questionable_rate)

  chk::chk_number(questionable_buffer)
  chk::chk_gte(questionable_buffer, 0)

  chk::chk_number(erroneous_buffer)
  chk::chk_gte(erroneous_buffer, 0)

  chk::chk_number(gap_range)
  chk::chk_gte(gap_range, 0)

  if (nrow(data) == 0) {
    data <-
      data |>
      dplyr::mutate(
        status_id = integer(),
        status_id = factor(
          .data$status_id,
          levels = c("reasonable", "questionable", "erroneous"),
          ordered = TRUE
        )
      ) |>
      tibble::as_tibble()
    return(data)
  }

  missing_rows <-
    data |>
    dplyr::filter(is.na(.data$water_temperature)) |>
    dplyr::mutate(
      status_id = NA_integer_,
      status_id = factor(
        .data$status_id,
        levels = c("reasonable", "questionable", "erroneous"),
        ordered = TRUE
      )
    )

  data <-
    data |>
    dplyr::filter(!is.na(.data$water_temperature)) |>
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
      .lag_temp = dplyr::lag(.data$water_temperature),
      .diff_temp = abs(.data$water_temperature - .data$.lag_temp),
      .lag_time = dplyr::lag(.data$temperature_date_time),
      .diff_time = diff_hours(.data$temperature_date_time, .data$.lag_time),
      .rate_temp_per_time = abs(.data$.diff_temp / .data$.diff_time),
      status_id = dplyr::case_when(
        # erroneous rate of change
        .data$.rate_temp_per_time > erroneous_rate ~ 3L,
        # questionable rate of change
        .data$.rate_temp_per_time > questionable_rate ~ 2L,
        TRUE ~ .data$status_id
      ),
      # classify adjacent values
      .lag_id = dplyr::lag(.data$status_id),
      .lead_id = dplyr::lead(.data$status_id)
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      status_id = max(
        .data$status_id, .data$.lag_id, .data$.lead_id,
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      -".lag_temp", -".diff_temp", -".lag_time", -".diff_time",
      -".rate_temp_per_time", -".lag_id", -".lead_id"
    )

  questionable_rows <- which(data$status_id == 2)
  error_rows <- which(data$status_id == 3)

  data <-
    data |>
    dplyr::mutate(
      .id_row = dplyr::row_number()
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      # find closest questionable/erroneous value above and below
      .quest_higher_next_id = list(
        questionable_rows[which(questionable_rows > .data$.id_row)]
      ),
      .quest_lower_next_id = list(
        questionable_rows[which(questionable_rows < .data$.id_row)]
      ),
      .error_higher_next_id = list(
        error_rows[which(error_rows > .data$.id_row)]
      ),
      .error_lower_next_id = list(
        error_rows[which(error_rows < .data$.id_row)]
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      .quest_higher_next_id = purrr::map_int(.data$.quest_higher_next_id, min2),
      .quest_lower_next_id = purrr::map_int(.data$.quest_lower_next_id, max2),
      .error_higher_next_id = purrr::map_int(.data$.error_higher_next_id, min2),
      .error_lower_next_id = purrr::map_int(.data$.error_lower_next_id, max2),
      .quest_higher_next_time = .data$temperature_date_time[.data$.quest_higher_next_id],
      .quest_lower_next_time = .data$temperature_date_time[.data$.quest_lower_next_id],
      .error_higher_next_time = .data$temperature_date_time[.data$.error_higher_next_id],
      .error_lower_next_time = .data$temperature_date_time[.data$.error_lower_next_id],
      .quest_higher_time_diff_h = diff_hours(.data$.quest_higher_next_time, .data$temperature_date_time),
      .quest_lower_time_diff_h = diff_hours(.data$temperature_date_time, .data$.quest_lower_next_time),
      .error_higher_time_diff_h = diff_hours(.data$.error_higher_next_time, .data$temperature_date_time),
      .error_lower_time_diff_h = diff_hours(.data$temperature_date_time, .data$.error_lower_next_time),

      # anything within an hour of a questionable value is questionable
      status_id = dplyr::if_else(
        .data$status_id == 1L & .data$.quest_higher_time_diff_h <= questionable_buffer,
        2L,
        .data$status_id,
        .data$status_id
      ),
      status_id = dplyr::if_else(
        .data$status_id == 1L & .data$.quest_lower_time_diff_h <= questionable_buffer,
        2L,
        .data$status_id,
        .data$status_id
      ),

      # anything within an hour of an erroneous value is erroneous
      status_id = dplyr::if_else(
        .data$status_id %in% c(1L, 2L) & .data$.error_higher_time_diff_h <= erroneous_buffer,
        3L,
        .data$status_id,
        .data$status_id
      ),
      status_id = dplyr::if_else(
        .data$status_id %in% c(1L, 2L) & .data$.error_lower_time_diff_h <= erroneous_buffer,
        3L,
        .data$status_id,
        .data$status_id
      ),

      # Fill in gap between questionable/erroneous values
      .gap_fill_higher_time = pmin(
        .data$.error_higher_next_time, .data$.quest_higher_next_time,
        na.rm = TRUE
      ),
      .gap_fill_higher_type = dplyr::case_when(
        .data$.gap_fill_higher_time == .data$.error_higher_next_time ~ "err",
        .data$.gap_fill_higher_time == .data$.quest_higher_next_time ~ "quest",
        TRUE ~ NA_character_
      ),
      .gap_fill_lower_time = pmax(
        .data$.error_lower_next_time, .data$.quest_lower_next_time,
        na.rm = TRUE
      ),
      .gap_fill_lower_type = dplyr::case_when(
        .data$.gap_fill_lower_time == .data$.error_lower_next_time ~ "err",
        .data$.gap_fill_lower_time == .data$.quest_lower_next_time ~ "quest",
        TRUE ~ NA_character_
      ),
      .gap_diff_time_h = diff_hours(.data$.gap_fill_higher_time, .data$.gap_fill_lower_time),
      status_id = dplyr::case_when(
        # if the gap less then gap range and at least one value is erroneous code the gap as erroneous
        .data$status_id == 1L & .data$.gap_diff_time_h <= gap_range & (.data$.gap_fill_higher_type == "err" | .data$.gap_fill_lower_type == "err") ~ 3L,
        # if the gap less then gap range (and not touching erroneous) then code as questionable
        .data$status_id == 1L & .data$.gap_diff_time_h <= gap_range ~ 2L,
        TRUE ~ .data$status_id
      ),
      status_id = dplyr::case_when(
        .data$status_id == 3L ~ "erroneous",
        .data$status_id == 2L ~ "questionable",
        .data$status_id == 1L ~ "reasonable"
      ),
      status_id = factor(
        .data$status_id,
        levels = c("reasonable", "questionable", "erroneous"),
        ordered = TRUE
      )
    ) |>
    dplyr::select(
      -".id_row",
      -".quest_higher_next_id", -".quest_lower_next_id",
      -".error_higher_next_id", -".error_lower_next_id",
      -".quest_higher_next_time", -".quest_lower_next_time",
      -".error_higher_next_time", -".error_lower_next_time",
      -".quest_higher_time_diff_h", -".quest_lower_time_diff_h",
      -".error_higher_time_diff_h", -".error_lower_time_diff_h",
      -".gap_fill_higher_time", -".gap_fill_higher_type",
      -".gap_fill_lower_time", -".gap_fill_lower_type",
      -".gap_diff_time_h"
    ) |>
    tibble::as_tibble()

  data <-
    dplyr::bind_rows(data, missing_rows) |>
    dplyr::arrange(.data$temperature_date_time) |>
    tibble::as_tibble()

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



data <- data.frame(
   temperature_date_time =
     as.POSIXct(c(
       "2021-05-07 08:00:00", "2021-05-07 09:00:00",
       "2021-05-07 10:00:00", "2021-05-07 11:00:00", "2021-05-07 12:00:00",
       "2021-05-07 13:00:00"
     )),
   water_temperature = c(4.124, 4.078, 4.102, 4.189, 4.243, 6.578)
)

classified_data <- classify_water_temp_data(data)
