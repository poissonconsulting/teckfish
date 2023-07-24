#' Calculate growing season degree days for a vector of temperature values.
#' Growing season degree day metric is calculated by summing daily temperatures during the growing season.
#' The start of the growing season is defined by the rolling mean temperature remaining above the start_temp for n_consecutive days.
#' The end of the growing season is defined by the rolling mean temperature remaining below the end_temp for n_consecutive days.
#'
#' @param x a vector of numeric temperature data.
#' @param rollmean_units a positive whole number, width of rolling mean window.
#' @param starttemp a single numeric value, threshold rolling average temperature to define start of the growing season
#' @param endtemp a single numeric value, weekly temperature rolling average threshold to define end of growing season.
#' @param n_consecutive a single positive whole number. Number of consecutive rolling average days above and below cut off temperatures to begin gsdd calculations
#'
#' @return
#' @export
#'
#' @examples
#' set.seed(13)
#' day <- 1:365
#' temperature <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
#' calculate_gsdd(x = temperature, rollmean_units = 7, start_temp = 5, end_temp = 4, n_consecutive = 5)
#'
calculate_gsdd <-
  function(x,
           rollmean_units = 7,
           start_temp = 5,
           end_temp = 4,
           n_consecutive = 5) {
    chk::chk_vector(x)
    chk::chk_all(x, chk::chk_number)
    chk::chk_length(start_temp, length = 1)
    chk::chk_length(end_temp, length = 1)
    chk::chk_count(rollmean_units)
    chk::chk_numeric(start_temp)
    chk::chk_numeric(end_temp)
    chk::chk_count(n_consecutive)
    chk::chk_true(length(x) > rollmean_units)
    chk::chk_true(length(x) > n_consecutive)
    chk::chk_false(anyNA(x))
    if (start_temp < end_temp) {
      stop("Error: start temp must be greater than or equal to end_temp")
    }
    if (max(x) <= start_temp) {
      stop("Error: start_temp higher than max temperature in vector")
    }
    
    index_adjust <- median(seq(1:rollmean_units)) - 1
    
    rollmean <- zoo::rollmean(x = x, k = rollmean_units)
    
    index_start_temps <- which(rollmean > start_temp)
    
    matches <-
      zoo::rollapply(
        index_start_temps,
        width = n_consecutive,
        FUN = function(index_start_temps)
          all(diff(index_start_temps) == 1)
      )
    
    first_match_index <- min(index_start_temps[matches])
    
    rollmean <- rollmean[first_match_index:length(rollmean)]
    
    index_end <- which(rollmean < end_temp)
    
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
    
    cumulative_gsdd <- sum(x)
    return(cumulative_gsdd)
  }
