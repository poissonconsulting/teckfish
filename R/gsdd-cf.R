#' Calculate Growing Season Degree Days (GSDD)
#'
#' Growing Season Degree Days (GSDD) is a water temperature metric
#' that is a useful predictor of Cutthroat trout size at the
#' beginning of winter. It is the accumulated thermal units (in C) 
#' during the growing season based on the mean daily water temperature values.
#'
#' By default the growing season is as defined by
#' Coleman and Fausch (2007) who stated that
#'
#' We defined the start of the growing season as the
#' beginning of the first week that average stream temperatures exceeded and
#' remained above 5C for the season;
#' the end of the growing season was defined as
#' the last day of the first week that
#' average stream temperature dropped below 4C.
#'
#' For the purposes of the calculation week is assumed to refer to a seven day
#' rolling average as opposed to the calendar week and if there are multiple
#' start and/or end dates the growing season is assumed to be the period of time
#' with the highest GSDD between the start and end dates.
#'
#' @param x A numeric vector of mean daily water temperature data from
#' before to after the growing season in C. It must be at least 55
#' and no more than 366 days in length.
#' @param ignore_truncation A flag specifying whether to ignore truncation
#' when calculating the GSDD or a string of "left", "right", "none" or "both"
#' specifying which type of truncation to ignore.
#' @param start_temp A number of the average water temperature
#' at the start of the growing season in C.
#' @param end_temp A number of the average water temperature
#' at the end of the growing season in C.
#' @param window_width A positive whole number of the
#' width of the rolling mean window in days.
#' @param quiet A flag specifying whether to suppress warnings.
#'
#' @return A number of the GSDD.
#' @export
#'
#' @examples
#' x <- c(rep(1, 10), rep(10, 20), rep(1, 200))
#' gsdd_cf(x)
#'
#' x <- teckfish::simulated_data$synthetic
#' output <- gsdd_cf(x)
#' print(output)
gsdd_cf <- function(x,
                    ignore_truncation = FALSE,
                    start_temp = 5,
                    end_temp = 4,
                    window_width = 7,
                    quiet = FALSE) {
  chk_numeric(x)
  chk_vector(x)
  chk_length(x, 55, 366)

  chkor_vld(vld_flag(ignore_truncation), vld_string(ignore_truncation))
  if (isTRUE(ignore_truncation)) {
    ignore_truncation <- "both"
  } else if (isFALSE(ignore_truncation)) {
    ignore_truncation <- "none"
  }
  chk_subset(ignore_truncation, c("none", "left", "right", "both"))
  chk_number(start_temp)
  chk_number(end_temp)
  chk_gte(start_temp, end_temp)
  chk_count(window_width)
  chk_range(window_width, c(3, 14))
  if (is_even(window_width)) {
    abort_chk("`window_width` must be odd.")
  }

  # create rolling mean vector from x and window width
  rollmean <- zoo::rollmean(x = x, k = window_width)

  # pick which indices have values above start temp that begin runs
  index_start <- index_begin_run(rollmean > start_temp)

  # no GSDD if season never starts
  if (!length(index_start)) {
    return(0)
  }
  truncated <- FALSE
  # if season starts on first day, ignore_truncation left
  if (index_start[1] == 1L) {
    truncated <- TRUE
    if (!quiet) {
      warning("Growing season truncated.")
    }
    if (ignore_truncation %in% c("none", "right")) {
      return(NA_real_)
    }
  }
  # pick which indices have values above and temp that begin runs
  index_end <- index_begin_run(rollmean < end_temp)
  # if season doesnt end ignore_truncation right
  if (!length(index_end) || max(index_start) > max(index_end)) {
    if (!truncated && !quiet) {
      warning("Growing season truncated.")
    }
    if (ignore_truncation %in% c("none", "left")) {
      return(NA_real_)
    }
    index_end <- c(index_end, length(rollmean))
  }

  data <- tidyr::expand_grid(
    index_start = index_start,
    index_end = index_end
  ) |>
    dplyr::filter(.data$index_start <= .data$index_end) |>
    dplyr::group_by(.data$index_start) |>
    dplyr::arrange(.data$index_end) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$index_end) |>
    dplyr::arrange(.data$index_start) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      index_end = .data$index_end + (window_width - 1),
      ndays = .data$index_end - .data$index_start + 1
    ) |>
    dplyr::mutate(gsdd = purrr::map2_dbl(
      .x = .data$index_start,
      .y = .data$index_end,
      .f = sum_vector,
      ..vector = x
    )) |>
    dplyr::arrange(
      dplyr::desc(.data$gsdd),
      dplyr::desc(.data$ndays),
      dplyr::desc(.data$index_start)
    ) |>
    dplyr::slice(1)

  data$gsdd
}
