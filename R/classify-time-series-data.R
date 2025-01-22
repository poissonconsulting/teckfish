reserved_colnames <- function() {
  c(
    ".date_time", ".value", ".lag_temp", ".diff_temp", ".lag_time", ".diff_time",
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
}

set_status_id <- function(data) {
  if(!rlang::has_name(data, "status_id")) {
    data <- data |>
      duckplyr::mutate(status_id = rep(NA_integer_, nrow(data)))
  }
  data |>
    duckplyr::mutate(
      status_id = duckplyr::case_when(
        .data$status_id == 3L ~ "erroneous",
        .data$status_id == 2L ~ "questionable",
        .data$status_id == 1L ~ "reasonable",
        TRUE ~ NA_character_),
      status_id = ordered(
        .data$status_id,
        levels = c("reasonable", "questionable", "erroneous"),
      )
    ) |>
    duckplyr::as_tibble()
}

check_time_series_args <- function(data,
                                   ...,
                                   date_time = "date_time",
                                   value = "value",
                                   questionable_min = 0,
                                   questionable_max = 30,
                                   erroneous_min = -0.5,
                                   erroneous_max = 40,
                                   questionable_rate = 2,
                                   erroneous_rate = 5,
                                   questionable_buffer = 1,
                                   erroneous_buffer = 1,
                                   gap_range = 5) {
  
  chk::chk_data(data)
  chk::chk_unused(...)
  
  chk::chk_string(date_time)
  chk::chk_string(value)
  
  values <- list(
    as.POSIXct("2021-05-07 08:00:00"), 
    c(1, NA_real_)) |>
    setNames(c(date_time, value))
  
  chk::check_data(data, values = values)
  chk::chk_unique(data[[date_time]])
  
  chk::chk_not_subset(colnames(data), "status_id")
  chk::chk_not_subset(colnames(data), reserved_colnames())
  
  chk::chk_number(questionable_min)
  chk::chk_number(questionable_max)
  chk::chk_gt(questionable_max, questionable_min)
  
  chk::chk_number(erroneous_min)
  chk::chk_number(erroneous_max)
  chk::chk_gt(erroneous_max, erroneous_min)
  
  chk::chk_gte(erroneous_max, questionable_max)
  chk::chk_lte(erroneous_min, questionable_min)
  
  chk::chk_number(questionable_rate)
  chk::chk_gt(questionable_rate)
  
  chk::chk_number(erroneous_rate)
  chk::chk_gt(erroneous_rate)
  
  chk::chk_gte(erroneous_rate, questionable_rate)
  
  chk::chk_number(questionable_buffer)
  chk::chk_gte(questionable_buffer)
  
  chk::chk_number(erroneous_buffer)
  chk::chk_gte(erroneous_buffer)
  
  chk::chk_number(gap_range)
  chk::chk_gte(gap_range)
}

#' Classify Time Series Data
#'
#' Time series data will be either classified as reasonable, questionable,
#' or erroneous in the status_id column.
#'
#' @inheritParams params
#' @return The original data frame sorted by date_time with a status_id column.
#' @export
#' @details The function only works on a single deployment of a logger. The
#'   table output will be sorted by date_time.
#'
#'   The function will error if there are missing date_time values
#'   missing.
#'
#'   The data is processed by:
#'
#'   1. Classifying the temperature values based on their values
#'   (questionable_min, questionable_max, erroneous_min, erroneous_max). 
#'   
#'   2. The
#'   rate of change between adjacent values is calculate and values are
#'   classified based on the rate parameters (questionable_rate,
#'   erroneous_rate). 
#'   
#'   3. Adjacent values to questionable/erroneous are coded as
#'   questionable/erroneous. 
#'   
#'   4. A buffer is applied that any value within the
#'   buffer is classified as questionable/erroneous based on the buffer
#'   parameters (questionable_buffer, erroneous_buffer). 
#'   
#'   5. Reasonable values
#'   identified between two questionable/erroneous values are coded as
#'   questionable/erroneous based on the gap hour difference allowed
#'   (gap_range).
#'
#' @examples
#' data <- data.frame(
#'   date_time =
#'     as.POSIXct(c(
#'       "2021-05-07 08:00:00", "2021-05-07 09:00:00",
#'       "2021-05-07 10:00:00", "2021-05-07 11:00:00", "2021-05-07 12:00:00",
#'       "2021-05-07 13:00:00"
#'     )),
#'   water_temperature = c(4.124, 4.078, 4.102, 4.189, 4.243, 6.578)
#' )
#'
#' classify_time_series_data(data, value = "water_temperature")
classify_time_series_data <- function(data,
                                      ...,
                                      date_time = "date_time",
                                      value = "value",
                                      questionable_min = 0,
                                      questionable_max = 30,
                                      erroneous_min = -0.5,
                                      erroneous_max = 40,
                                      questionable_rate = 2,
                                      erroneous_rate = 5,
                                      questionable_buffer = 1,
                                      erroneous_buffer = 1,
                                      gap_range = 5) {
  
  check_time_series_args(
    data,
    ...,
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
  
  data <- data |>
    duckplyr::rename(.date_time = date_time, .value = value) |>
    duckplyr::arrange(.data$.date_time) |>
    set_status_id()
  
  missing_rows <- data |>
    duckplyr::filter(is.na(.data$.value))
  
  lookup <- c(".date_time", ".value") |>
    setNames(c(date_time, value))
  
  if(identical(nrow(missing_rows), nrow(data))) {
    data <- data |>
      duckplyr::rename(duckplyr::all_of(lookup))
      return(data)    
  }
  
  tz <- attr(data$.date_time, "tzone")
  
  data <- data |>
    duckplyr::filter(!is.na(.data$.value)) |>
    duckplyr::mutate(
      .date_time = as.numeric(.data$.date_time),
      .rate = c(NA_real_, diff(.data$.value) / diff(.data$.date_time)),
      .rate = abs(.rate) / 3600,
      status_id = duckplyr::case_when(
        .data$.value <= erroneous_min ~ 3L,
        .data$.value >= erroneous_max ~ 3L,
        .data$.rate >= erroneous_rate ~ 3L,
        .data$.value <= questionable_min ~ 2L,
        .data$.value >= questionable_max ~ 2L,
        .data$.rate >= questionable_rate ~ 2L,
        TRUE ~ 1L
      ),
      status_id = pmax(
        .data$status_id, 
        duckplyr::lag(.data$status_id), 
        duckplyr::lead(.data$status_id),
        na.rm = TRUE
      )
    ) |>
    duckplyr::select(
      !".rate"
    )
  
  questionable_range <- data |>
    duckplyr::filter(status_id == 2L) |>
    duckplyr::mutate(.start = .date_time + questionable_buffer,
                     .end = .date_time - questionable_buffer)
  
  erroneous_range <- data |>
    duckplyr::filter(status_id == 3L) |>
    duckplyr::mutate(.start = .date_time + erroneous_buffer,
                     .end = .date_time - erroneous_buffer)
  
  data <- data |>
    #   duckplyr::::mutate(
    #     .id_row = duckplyr::::row_number()
    #   ) |>
    #   duckplyr::::rowwise() |>
    #   duckplyr::::mutate(
    #     # find closest questionable/erroneous value above and below
    #     .quest_higher_next_id = list(
    #       questionable_rows[which(questionable_rows > .data$.id_row)]
    #     ),
    #     .quest_lower_next_id = list(
    #       questionable_rows[which(questionable_rows < .data$.id_row)]
    #     ),
    #     .error_higher_next_id = list(
    #       error_rows[which(error_rows > .data$.id_row)]
    #     ),
    #     .error_lower_next_id = list(
    #       error_rows[which(error_rows < .data$.id_row)]
    #     )
    #   ) |>
    #   duckplyr::::ungroup() |>
    duckplyr::mutate(
      #     .quest_higher_next_id = purrr::map_int(.data$.quest_higher_next_id, min2),
      #     .quest_lower_next_id = purrr::map_int(.data$.quest_lower_next_id, max2),
      #     .error_higher_next_id = purrr::map_int(.data$.error_higher_next_id, min2),
      #     .error_lower_next_id = purrr::map_int(.data$.error_lower_next_id, max2),
      #     .quest_higher_next_time = .data$temperature_date_time[.data$.quest_higher_next_id],
      #     .quest_lower_next_time = .data$temperature_date_time[.data$.quest_lower_next_id],
      #     .error_higher_next_time = .data$temperature_date_time[.data$.error_higher_next_id],
      #     .error_lower_next_time = .data$temperature_date_time[.data$.error_lower_next_id],
      #     .quest_higher_time_diff_h = diff_hours(.data$.quest_higher_next_time, .data$temperature_date_time),
      #     .quest_lower_time_diff_h = diff_hours(.data$temperature_date_time, .data$.quest_lower_next_time),
      #     .error_higher_time_diff_h = diff_hours(.data$.error_higher_next_time, .data$temperature_date_time),
      #     .error_lower_time_diff_h = diff_hours(.data$temperature_date_time, .data$.error_lower_next_time),
      #     
      #     # anything within questionable_buffer of a questionable value is questionable
      #     status_id = duckplyr::::if_else(
      #       .data$status_id == 1L & .data$.quest_higher_time_diff_h <= questionable_buffer,
      #       2L,
      #       .data$status_id,
      #       .data$status_id
      #     ),
      #     status_id = duckplyr::::if_else(
      #       .data$status_id == 1L & .data$.quest_lower_time_diff_h <= questionable_buffer,
      #       2L,
      #       .data$status_id,
      #       .data$status_id
      #     ),
      #     
      #     # anything within erroneous_buffer of an erroneous value is erroneous
      #     status_id = duckplyr::::if_else(
      #       .data$status_id %in% c(1L, 2L) & .data$.error_higher_time_diff_h <= erroneous_buffer,
      #       3L,
      #       .data$status_id,
      #       .data$status_id
      #     ),
      #     status_id = duckplyr::::if_else(
      #       .data$status_id %in% c(1L, 2L) & .data$.error_lower_time_diff_h <= erroneous_buffer,
      #       3L,
      #       .data$status_id,
      #       .data$status_id
      #     ),
      #     
      #     # Fill in gap between questionable/erroneous values
      #     .gap_fill_higher_time = pmin(
      #       .data$.error_higher_next_time, .data$.quest_higher_next_time,
      #       na.rm = TRUE
      #     ),
      #     .gap_fill_higher_type = duckplyr::::case_when(
      #       .data$.gap_fill_higher_time == .data$.error_higher_next_time ~ "err",
      #       .data$.gap_fill_higher_time == .data$.quest_higher_next_time ~ "quest",
      #       TRUE ~ NA_character_
      #     ),
      #     .gap_fill_lower_time = pmax(
      #       .data$.error_lower_next_time, .data$.quest_lower_next_time,
      #       na.rm = TRUE
      #     ),
      #     .gap_fill_lower_type = duckplyr::::case_when(
      #       .data$.gap_fill_lower_time == .data$.error_lower_next_time ~ "err",
      #       .data$.gap_fill_lower_time == .data$.quest_lower_next_time ~ "quest",
      #       TRUE ~ NA_character_
      #     ),
      #     .gap_diff_time_h = diff_hours(.data$.gap_fill_higher_time, .data$.gap_fill_lower_time),
      #     # if gap is less then gap range then code as questionable
      #     status_id = duckplyr::::if_else(
      #       .data$status_id == 1L & .data$.gap_diff_time_h <= gap_range,
      #       2L,
      #       .data$status_id,
      #       .data$status_id
      #     ),
      status_id = duckplyr::case_when(
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
    #   dplyr::select(
    #     -".id_row",
    #     -".quest_higher_next_id", -".quest_lower_next_id",
    #     -".error_higher_next_id", -".error_lower_next_id",
    #     -".quest_higher_next_time", -".quest_lower_next_time",
    #     -".error_higher_next_time", -".error_lower_next_time",
    #     -".quest_higher_time_diff_h", -".quest_lower_time_diff_h",
    #     -".error_higher_time_diff_h", -".error_lower_time_diff_h",
    #     -".gap_fill_higher_time", -".gap_fill_higher_type",
    #     -".gap_fill_lower_time", -".gap_fill_lower_type",
    #     -".gap_diff_time_h"
    #   ) |>
    duckplyr::mutate(.date_time = as.POSIXct(.data$.date_time, tz = tz)) |>
    duckplyr::bind_rows(missing_rows) |>
    duckplyr::arrange(.data$.date_time) |>
    duckplyr::rename(duckplyr::all_of(lookup)) |>
    duckplyr::as_tibble()
  
  data
}
