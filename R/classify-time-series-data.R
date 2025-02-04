#' Classify Time Series Data
#'
#' Time series data will be either classified as reasonable, questionable,
#' or erroneous in the status_id column or NA if the value is missing.
#'
#' @inheritParams params
#' @return The original data frame sorted by the date time with a status_id column.
#' @export
#' @details The function only works on a single time series.
#'
#'   The function will error if there are missing or duplicated date time.
#'
#'   The data is processed by:
#'
#'   1. Classifying the time series values based on their values
#'   (`questionable_min, questionable_max, erroneous_min, erroneous_max).
#'
#'   2. The
#'   rate of change to each value is then calculated and the values are
#'   classified based on the absolute rate of change (questionable_rate,
#'   erroneous_rate).
#'
#'   3. Adjacent values to all questionable/erroneous are then coded as
#'   questionable/erroneous.
#'
#'   4. Next any value within the time
#'   buffer of a questionable/erroneous value is classified as questionable/erroneous
#'    (questionable_buffer, erroneous_buffer).
#'
#'   5. In addition, ignoring the buffer, reasonable values
#'   between two questionable/erroneous values are coded as
#'   questionable if the hourly duration of the gap is within the 
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
    gap_range = gap_range
  )

  data <- data |>
    dplyr::rename(.date_time = dplyr::all_of(date_time), 
                     .value = dplyr::all_of(value)) |>
    dplyr::arrange(.data$.date_time) |>
    dplyr::mutate(status_id = rep(NA_integer_, nrow(data))) |>
    set_status_id()

  missing_rows <- data |>
    dplyr::filter(is.na(.data$.value))

  lookup <- c(".date_time", ".value") |>
    rlang::set_names(c(date_time, value))

  if (identical(nrow(missing_rows), nrow(data))) {
    data <- data |>
      dplyr::rename(dplyr::all_of(lookup))
    return(data)
  }

  tz <- attr(data$.date_time, "tzone")

  data <- data |>
    dplyr::filter(!is.na(.data$.value)) |>
    dplyr::mutate(
      .date_time = as.integer(.data$.date_time),
      .rate = c(NA_real_, diff(.data$.value) / diff(.data$.date_time)),
      .rate = abs(.data$.rate) * 3600,
      status_id = dplyr::case_when(
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
        dplyr::lag(.data$status_id),
        dplyr::lead(.data$status_id),
        na.rm = TRUE
      )
    ) |>
    dplyr::select(
      !".rate"
    )

  questionable_range <- data |>
    dplyr::filter(.data$status_id == 2L) |>
    dplyr::mutate(
      .start_date_time = .data$.date_time - questionable_buffer * 3600,
      .end_date_time = .data$.date_time + questionable_buffer * 3600,
      .keep = "none"
    )

  erroneous_range <- data |>
    dplyr::filter(.data$status_id == 3L) |>
    dplyr::mutate(
      .start_date_time = .data$.date_time - erroneous_buffer * 3600,
      .end_date_time = .data$.date_time + erroneous_buffer * 3600,
      .keep = "none"
    )

  gap <- data |>
    dplyr::filter(.data$status_id != 1L) |>
    dplyr::mutate(
      .status_id = pmax(.data$status_id, dplyr::lead(.data$status_id), na.rm = TRUE),
      .status_id = 2L, # TODO 2L, pmin or pmax?? - pmax
      .start_date_time = .data$.date_time,
      .end_date_time = dplyr::lead(.data$.date_time),
      .keep = "none"
    ) |>
    dplyr::filter(.data$.end_date_time - .data$.start_date_time <= gap_range * 3600)

  data <- data |>
    dplyr::left_join(questionable_range, by = dplyr::join_by(closest(x$.date_time >= y$.start_date_time))) |>
    dplyr::mutate(status_id = dplyr::if_else(.data$status_id == 1L & .data$.date_time <= .data$.end_date_time, 2L, .data$status_id, .data$status_id)) |>
    dplyr::select(!c(".start_date_time", ".end_date_time")) |>
    dplyr::left_join(erroneous_range, by = dplyr::join_by(closest(x$.date_time >= y$.start_date_time))) |>
    dplyr::mutate(status_id = dplyr::if_else(.data$status_id != 3L & .data$.date_time <= .data$.end_date_time, 3L, .data$status_id, .data$status_id)) |>
    dplyr::select(!c(".start_date_time", ".end_date_time")) |>
    dplyr::left_join(gap, by = dplyr::join_by(closest(x$.date_time >= y$.start_date_time))) |>
    dplyr::mutate(status_id = dplyr::if_else(.data$status_id < .data$.status_id & .data$.date_time <= .data$.end_date_time, .data$.status_id, .data$status_id, .data$status_id)) |>
    dplyr::select(!c(".status_id", ".start_date_time", ".end_date_time")) |>
    set_status_id() |>
    dplyr::mutate(.date_time = as.POSIXct(.data$.date_time, tz = tz)) |>
    dplyr::bind_rows(missing_rows) |>
    dplyr::arrange(.data$.date_time) |>
    dplyr::rename(dplyr::all_of(lookup)) |>
    dplyr::as_tibble()
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
    c(1, NA_real_)
  ) |>
    rlang::set_names(c(date_time, value))

  chk::check_data(data, values = values)
  
  chk::chk_unique(as.integer(data[[date_time]]), x_name = paste0("`data$", date_time, "`"))

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

set_status_id <- function(data) {
  data |>
    dplyr::mutate(
      status_id = dplyr::case_when(
        .data$status_id == 3L ~ "erroneous",
        .data$status_id == 2L ~ "questionable",
        .data$status_id == 1L ~ "reasonable",
        TRUE ~ NA_character_
      ),
      status_id = ordered(
        .data$status_id,
        levels = c("reasonable", "questionable", "erroneous"),
      )
    ) |>
    dplyr::as_tibble()
}

reserved_colnames <- function() {
  c(
    ".rate", ".status_id", ".start_date_time", ".end_date_time"
  )
}
