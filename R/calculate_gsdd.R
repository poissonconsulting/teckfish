#' calculate growing season degree days for a vector of temperature values. 
#'
#' @param x vector of numeric temperature data. 
#' @param rollmean_units positive whole number, width of rolling mean window. 
#' @param starttemp single numeric value, threshold rolling average temperature to define start of the growing season
#' @param endtemp single numeric value, weekly temperature rolling average threshold to define end of growing season. 
#' @param n_consecutive single positive whole number. Number of consecutive rolling average days above and below cut off temperatures to begin gsdd calculations
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
calculate_gsdd <- function(x, rollmean_units = 7, start_temp = 5, end_temp = 4, n_consecutive = 5) {
  chk::chk_vector(x)
  chk::chk_all(x, chk::chk_number)
  chk::chk_length(start_temp, length = 1)
  chk::chk_length(end_temp, length = 1)
  chk::chk_count(rollmean_units)
  chk::chk_numeric(start_temp)
  chk::chk_numeric(end_temp)
  chk::chk_count(n_consecutive)
  if(max(x) <= start_temp){
    stop("Error: start temp never reached in vector")
  }
  
  rollmean <- zoo::rollmean(x=x, k=rollmean_units)
  
  indices <- which(rollmean > start_temp)
  matches <- zoo::rollapply(indices, width = n_consecutive, FUN = function(rollmean) all(diff(rollmean) == 1))
  first_match_index <- min(indices[matches])
  rollmean <- rollmean[(first_match_index-3):length(rollmean)]
  
  indices <- which(rollmean < end_temp)
  
  if (length(indices) == 0) {
    indices <- (length(rollmean) - 3)
    warning("end_temp never reached, gsdd calculated for remainder of values")
    x <- x[(first_match_index-3):length(x)]
  } else {
  indices <- indices
  matches <- zoo::rollapply(indices, width = n_consecutive, FUN = function(rollmean) all(diff(rollmean) == 1))
  end_match_index <- min(indices[matches])
  x <- x[(first_match_index-3):(end_match_index+3)]
  }
  
  cumulative_gsdd <- sum(x)
  return(cumulative_gsdd)
}
