#' Calculate Growing Season Degree Days (GSDD)
#'
#' Growing Season Degree Days (GSDD) is a water temperature metric
#' that is a useful predictor of Cutthroat trout size at the
#' beginning of winter. It is the accumulated thermal units (in C)
#' during the growing season.
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
#' For the purposes of the calculation week is assumed to refer to a
#' seven day rolling average as opposed to the calendar week and
#' if there are multiple start and/or end dates the growing season is
#' assumed to be the longest period of time between the start and end dates.
#'
#' @param x A numeric vector of mean daily water temperature data from
#' before to after the growing season in C. It must be at least 180
#' and no more than 366 days in length.
#' @param entire A flag specifying whether to only calculate GSDD
#' for the entire season.
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
#' set.seed(13)
#' day <- 1:365
#' x <- pmax(-15 * cos((2*pi / 365) * (day-10)) + rnorm(365, sd = .5), 0)
gsdd_cf <- function(x,
                    entire = TRUE,
                    start_temp = 5,
                    end_temp = 4,
                    window_width = 7,
                    quiet = FALSE) {
  chk_numeric(x)
  chk_vector(x)
  chk_not_any_na(x)
  chk_length(x, 28, 366)

  chk_flag(entire)

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

  if (index_start[1] == 1L) {
    if (!quiet) {
      warning("growing season left truncated")
    }
    if (entire) {
      return(NA_real_)
    }
  }

  # pick which indices have values above and temp that begin runs
  index_end <- index_begin_run(rollmean < end_temp)

  if (!length(index_end) || max(index_start) > max(index_end)) {
    if (!quiet) {
      warning("growing season right truncated")
    }
    if (entire) {
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
