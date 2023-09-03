#' Calculate Growing Season Degree Days (GSDD)
#'
#' GSDD is a continuous variable that is a measurement of thermal energy available for growth. GSDD is calculated by summing daily temperatures during the growing season.
#' The start of the growing season is defined by when the rolling mean temperature remains above the start_temp for n_consecutive days.
#' The end of the growing season is defined by when the rolling mean temperature remains below the end_temp for n_consecutive days.
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
#'
#' x <- c(rep(1,10),rep(10,20), rep(1,10))
#' calculate_gsdd(x, window_width = 3, start_temp = 9, end_temp = 9, n_consecutive = 3)
#'
#'#' set.seed(13)
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
      abort_chk(
        "`start_temp` is higher than of equal to max temperature in `x`, choose a lower temperature"
      )
    }
    # calculating the length of sides in rolling window - expect 1 from 3 because 1 on each side.
    index_adjust <- stats::median(seq(1:window_width)) - 1
    
    # create rolling mean vector from x and window width
    rollmean <- zoo::rollmean(x = x, k = window_width)
    
    # pick which indices have values above start temp
    index_start_temps <- which(rollmean > start_temp)
    
    # which of those indices are n_consecutive in a row
    matches <-
      zoo::rollapply(
        index_start_temps,
        width = n_consecutive,
        FUN = function(index_start_temps)
          all(diff(index_start_temps) == 1)
      )
    
    # grab the smallest of the indices that are n_consecutive in a row
    first_match_index <- min(index_start_temps[matches])
    
    # which indices in roll mean are below end_temp
    index_end <- which(rollmean < end_temp)
    
    # remove indices that are lower than the first_match_index
    index_end <- index_end[index_end > first_match_index]
    
    # choosing smallest end index
    if (length(index_end) < 1) {
      end_match_index <- length(rollmean)
      warning("end_temp never reached, gsdd calculated for remainder of values")
    } else {
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
    
    # adjust start index to subset x by the index adjustment for the rolling mean
    adjusted_index_start <- first_match_index - index_adjust
    
    if (adjusted_index_start < 1) {
      select_start <- 1
    } else {
      select_start <- adjusted_index_start
    }
    
    #adjust the ending index to subset x
    adjusted_index_end <- end_match_index + index_adjust
    
    if (adjusted_index_end > length(x)) {
      select_end <- length(x)
    } else{
      select_end <- adjusted_index_end
    }
    
    x <- x[select_start:select_end]
    
    sum(x)
  }
