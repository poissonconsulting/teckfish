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
#' seven day rolling average as opposed to the calendar week.
#'
#' @param x A numeric vector of mean daily water temperature data from 
#' before to after the growing season in C.
#' @param window_width A positive whole number of the 
#' width of the rolling mean window in days.
#' @param start_temp A number of the average water temperature 
#' at the start of the growing season in C.
#' @param end_temp A number of the average water temperature
#'  at the end of the growing season in C.
#' @param n_consecutive A positive whole number indicating the 
#' number of consecutive rolling average days above and below cut off 
#' temperatures to begin GSDD calculations.
#' temperatures to begin GSDD calculations.
#' temperatures to begin GSDD calculations.
#'
#' @return A number of the GSDD.
#' @export
#'
#' @examples
#' x <- c(rep(1, 10), rep(10, 20), rep(1, 10))
#' calculate_gsdd(x)
#'
#' set.seed(13)
#' day <- 1:365
# x <- pmax(-15 * cos((2*pi / 365) * (day-10)) + rnorm(365, sd = .5), 0)
calculate_gsdd <-
  function(x,
           window_width = 7,
           start_temp = 5,
           end_temp = 4,
           n_consecutive = 5) {
    chk_numeric(x)
    chk_number(start_temp)
    chk_number(end_temp)
    chk_gte(start_temp, end_temp)
    chk_count(window_width)
    chk_count(n_consecutive)
    chk_true(length(x) > window_width)
    chk_true(length(x) > n_consecutive)
    chk_not_any_na(x)
  
    if (max(x) <= start_temp) {
      abort_chk(
        "`start_temp` is higher than of equal to max temperature in `x`, choose a lower temperature"
      )
    }
    # calculating the length of sides in rolling window - expect 1 from 3 because 1 on each side.
    index_adjust <- (mean(window_width) - 1)/2

    # create rolling mean vector from x and window width
    rollmean <- zoo::rollmean(x = x, k = window_width)

    # pick which indices have values above start temp
    index_start_temps <- which(rollmean > start_temp)

    # which of those indices are n_consecutive in a row
    matches <-
      zoo::rollapply(
        index_start_temps,
        width = n_consecutive,
        FUN = function(index_start_temps) {
          all(diff(index_start_temps) == 1)
        }
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
          FUN = function(rollmean) {
            all(diff(rollmean) == 1)
          }
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

    # adjust the ending index to subset x
    adjusted_index_end <- end_match_index + index_adjust

    if (adjusted_index_end > length(x)) {
      select_end <- length(x)
    } else {
      select_end <- adjusted_index_end
    }

    x <- x[select_start:select_end]

    sum(x)
  }
