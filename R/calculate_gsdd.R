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
#' temperature <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
#' calculate_gsdd(x = temperature, rollmean_units = 7, start_temp = 5, end_temp = 4, n_consecutive = 5)
#' 
calculate_gsdd <- function(x, rollmean_units = 7, start_temp = 5, end_temp = 4, n_consecutive = 5) {
  chk_vector(x)
  chk_all(temperature, chk_number)
  chk_length(start_temp, length = 1)
  chk_length(end_temp, length = 1)
  chk_count(rollmean_units)
  chk_numeric(start_temp)
  chk_numeric(end_temp)
  chk_count(n_consecutive)
  
  x <- zoo::rollmean(x=x, k=rollmean_units)
  
  indices <- which(x > start_temp)
  matches <- zoo::rollapply(indices, width = n_consecutive, FUN = function(x) all(diff(x) == 1))
  first_match_index <- min(indices[matches])
  
  x <- x[(first_match_index-3):length(x)]
  
  indices <- which(x < end_temp)
  matches <- zoo::rollapply(indices, width = n_consecutive, FUN = function(x) all(diff(x) == 1))
  end_match_index <- min(indices[matches])
  
  x <- x[1:(end_match_index+3)]
 
  cumulative_gsdd <- sum(x)
  return(cumulative_gsdd)
}
