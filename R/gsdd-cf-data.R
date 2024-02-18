#' Calculate Growing Season Degree Days (GSDD) from a Data Frame
#' 
#' The GSDD is calculated for each study year from a data frame of 
#' with a `date` and `water_temperature` values. 
#' `date`, which must be of class `Date` provides the date and 
#' `water_temperature` which must be a numeric vector provides the water temperature
#' in degrees centigrade. For additional information on 
#' GSDD and the various arguments that can be passed via `...` see [`gsdd_cf()`].
#'
#' @param x A data frame with two columns `date` and `water_temperature`. 
#' @param start_month A whole number of the first month to include.
#' @param end_month A whole number of the last month to include.
#' @param ... Additional arguments passed to [`gsdd_cf()`].
#' @inheritParams interpolate_numeric_vector
#' @return A tibble with two columns `year` and `gsdd`.
#' `year`, which is an integer, indicates the year in which the window
#' began and `gsdd` which is a non-negative real number provides the GSDD
#' or a missing value if it cannot be calculated.
#' @export
#'
#' @examples
#' x <- data.frame(water_temperature = teckfish::simulated_data$synthetic)
#' x$date <- dttr2::dtt_seq(as.Date("1970-01-01"), as.Date("1970-12-31"))
#' gsdd_cf_data(x)
#' gsdd_cf_data(x, ignore_truncation = TRUE)
#' gsdd_cf_data(x, end_month = 11)
gsdd_cf_data <- function(
    x, 
    start_month = 3, 
    end_month = 10,
    ...,
    span = 3, 
    tails = FALSE) {
  
  check_data(x, list(date = dttr2::dtt_date("1970-01-01"), water_temperature = c(1, NA)))
  chk_whole_number(start_month)
  chk_range(start_month, c(1, 12)) 
  chk_whole_number(end_month)
  chk_range(end_month, c(1, 12))
  chk_lt(start_month, end_month)
  chk_whole_number(span)
  chk_gte(span)
  
  x$date <- dttr2::dtt_date(x$date)
  check_key(x, "date")
  x$year <- dttr2::dtt_year(x$date)
  years <- range(x$year)
  dates <- years |> paste0(c("-01-01", "-12-31")) |> dttr2::dtt_date()
  dates <- tibble::tibble(date = dttr2::dtt_seq(dates[1], to = dates[2]))
  
  x <- x |> 
    dplyr::right_join(dates, by = "date") |>
    dplyr::arrange(.data$date)
  
  if(span > 0) {
    x$water_temperature <- x$water_temperature |>
      interpolate_numeric_vector(span = span, tails = tails)
  }
  x |> 
    dplyr::mutate(month = dttr2::dtt_month(date)) |>
    dplyr::filter(.data$month >= start_month, .data$month <= end_month) |>
    dplyr::mutate(year = dttr2::dtt_year(date)) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(gsdd = gsdd_cf(.data$water_temperature, ...), .groups = "keep") |>
    dplyr::ungroup()
}
