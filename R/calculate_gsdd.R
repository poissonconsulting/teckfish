#' Calculate Growing Season Degree Days (GSDD)
#' 
#' GSDD is calculated by summing daily temperatures during the growing season.
#' The start of the growing season is defined by the rolling mean temperature remaining above the start_temp for n_consecutive days.
#' The end of the growing season is defined by the rolling mean temperature remaining below the end_temp for n_consecutive days.
#'
#' @param x A vector of numeric temperature data.
#' @param window_width A positive whole number indicating the width of rolling mean window.
#' @param start_temp A number indicating the threshold rolling average temperature to define the start of the growing season.
#' @param end_temp A number indicating the weekly temperature rolling average threshold to define the end of growing season.
#' @param n_consecutive A positive whole number indicating the number of consecutive rolling average days above and below cut off temperatures to begin GSDD calculations.
#'
#' @return A number of the cumulative GSDD.
#' @export
#'
#' @examples
#' set.seed(13)
#' day <- 1:365
#' temperature <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
#' calculate_gsdd(x = temperature, window_width = 7, start_temp = 5, end_temp = 4, n_consecutive = 5)
#'
calculate_gsdd <-
  function(x,
           window_width = 7,
           start_temp = 5,
           end_temp = 4,
           n_consecutive = 5) {
    
    chk_numeric(x)
    chk_number(start_temp)
    chk_number(end_temp)
    chk_count(window_width)
    chk_count(n_consecutive)
    chk_true(length(x) > window_width)
    chk_true(length(x) > n_consecutive)
    chk_not_any_na(x)
    
    if (start_temp < end_temp) {
      abort_chk("`start_temp` must be greater than or equal to `end_temp`")
    }
    if (max(x) <= start_temp) {
      abort_chk("`start_temp` is higher than max temperature in `x`")
    }
    
    index_adjust <- stats::median(seq(1:window_width)) - 1
    
    rollmean <- zoo::rollmean(x = x, k = window_width)
    
    index_start_temps <- which(rollmean > start_temp)
    
    matches <-
      zoo::rollapply(
        index_start_temps,
        width = n_consecutive,
        FUN = function(index_start_temps)
          all(diff(index_start_temps) == 1)
      )
    
    first_match_index <- min(index_start_temps[matches])
    
    index_end <- which(which(rollmean < end_temp) > first_match_index)
    
    if (length(index_end) == 0) {
      end_match_index <- length(rollmean)
      warning("end_temp never reached, gsdd calculated for remainder of values")
    }
    else {
      index_end <- index_end
      matches <-
        zoo::rollapply(
          index_end,
          width = n_consecutive,
          FUN = function(rollmean)
            all(diff(rollmean) == 1)
        )
      end_match_index <- suppressWarnings(min(index_end[matches]))
    }
    
    adjusted_index_start <- first_match_index - index_adjust
    if (adjusted_index_start < 0) {
      select_start <- 1
    } else {
      select_start <- adjusted_index_start
    }
    
    adjusted_index_end <- end_match_index + index_adjust
    
    if (adjusted_index_end > length(x)) {
      select_end <- length(x)
    } else{
      select_end <- adjusted_index_end
    }
    
    x <- x[select_start:select_end]
    
    sum(x)
  }
