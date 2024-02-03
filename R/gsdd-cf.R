#' Calculate Growing Season Degree Days (GSDD)
#'
#' Growing Season Degree Days (GSDD) is a water temperature metric
#' that is a useful predictor of Cutthroat trout size at the
#' beginning of winter. 
#' It is the accumulated thermal units (in C) 
#' during the growing season based on the mean daily water temperature values.
#' 
#' The GSDD is calculated across the longest consecutive sequence of non-missing
#' values which must be at least 184 elements in length otherwise a 
#' missing value is returned.
#' If the time series includes missing values it is recommended that they are
#' replaced by estimates of the actual values using linear
#' interpolation (`[interpolate_numeric_vector()]`) or other predictive methods. 
#' If the user removes the missing values then the returned GSDD value 
#' will be less than the actual GSDD.
#' 
#' Truncation occurs when the start and/or end
#' of the time series is part way through a growing season.
#' If the user chooses to ignore truncation then the returned value
#' will be less than the actual GSDD.
#'
#' By default the growing season is based on the interpretation of
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
#' rolling average as opposed to the calendar week.
#' 
#' If there are multiple growing 'seasons' within the same year then by
#' default the returned value is the sum of the GSDD values for `"all"` seasons.
#' 
#' The user also has the option to pick the `"first"`/`"last"` or
#'  `"longest"`/`"shortest"` season or the season with 
#'  the `"biggest"`/`"smallest"` GSDD.
#'  If the user picks the `"longest"` season but there are multiple seasons
#' with the longest length then the candidate 
#' season with the `"biggest"` GSDD is selected.
#' Conversely in the case of multiple `"shortest"` seasons then the
#' candidate with the `"smallest"` GSDD is selected.
#'
#' @param x A numeric vector of the
#' mean daily water temperature values for the period
#' of interest in C. It must be consist of no more than 
#' 366 values.
#' @param ignore_truncation A flag specifying whether to ignore truncation
#' of the mean daily water temperature vector 
#' or a string of "start", "end", "none" or "both"
#' specifying which type of truncation to ignore.
#' @param start_temp A positive real number of the average water temperature
#' at the start of the growing season in C.
#' @param end_temp A positive real number of the average water temperature
#' at the end of the growing season in C. It must be greater than or equal to
#' the start temperature.
#' @param window_width A positive whole number of the
#' width of the rolling mean window in days. By default 7.
#' @param pick A string specifying whether to pick the
#' "longest", "shortest", "first" or "last" 'season' or the season with the
#' "biggest" or "smallest" GSDD. By default the returned value is the
#' sum of the GSDD values for "all" 'seasons'.
#' @param quiet A flag specifying whether to suppress warnings.
#'
#' @return A non-negative real number of the GSDD.
#' @export
#'
#' @examples
#' gsdd_cf(c(rep(1, 10), rep(10, 20), rep(1, 200)))
#' gsdd_cf(teckfish::simulated_data$synthetic)
gsdd_cf <- function(x,
                    ignore_truncation = FALSE,
                    start_temp = 5,
                    end_temp = 4,
                    window_width = 7,
                    pick = "all",
                    quiet = FALSE) {
  chk_numeric(x)
  chk_vector(x)
  chk_length(x, 0, 366)

  chkor_vld(vld_flag(ignore_truncation), vld_string(ignore_truncation))
  if (isTRUE(ignore_truncation)) {
    ignore_truncation <- "both"
  } else if (isFALSE(ignore_truncation)) {
    ignore_truncation <- "none"
  }
  chk_subset(ignore_truncation, c("none", "start", "end", "both"))
  chk_number(start_temp)
  chk_number(end_temp)
  chk_gt(start_temp)
  chk_gte(start_temp, end_temp)
  chk_count(window_width)
  chk_range(window_width, c(3, 14))
  if (is_even(window_width)) {
    abort_chk("`window_width` must be odd.")
  }
  chk_string(pick)
  chk_subset(
    pick, 
    c("biggest", "smallest", "longest", "shortest", "first", "last", "all"))
  chk_flag(quiet)

  if(length(x) < 184) {
    return(NA_real_)
  }
  x <- longest_run(x)
  if(length(x) < 184 || anyNA(x)) {
    return(NA_real_)
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
    if (ignore_truncation %in% c("none", "end")) {
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
    if (ignore_truncation %in% c("none", "start")) {
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
    pick_season(pick)
  
  sum(data$gsdd)
}
