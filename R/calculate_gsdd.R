#' calculate growing season degree days.
#'
#' @param x vector of numeric temperature data. 
#' @param rollmean_units positive whole number, width of rolling mean window. 
#' @param starttemp single numeric value, threshold rolling average temperature to define start of the growing season
#' @param endtemp single numeric value, weekly temperature rolling average threshold to define end of growing season. 
#' 
#'
#' @return
#' @export
#'
#' @examples
#' day <- 1:365
#' temperature <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
#' calculate_gsdd(x = temperature, rollmean_units = 7, start_temp = 5, end_temp = 4)
#' 
calculate_gsdd <- function(x, rollmean_units, start_temp = 5, end_temp = 4) {
  chk_vector(x)
  chk_all(temperature, chk_number)
  chk_length(start_temp, length = 1)
  chk_length(end_temp, length = 1)
  chk_count(rollmean_units)
  chk_numeric(start_temp)
  chk_numeric(end_temp)

  x <- zoo::rollmean(x=x, rollmean_units=rollmean_units)
  
  start <- match(TRUE, x > start_temp)
  x <- x[(start-3):length(x)]
  
  end <- match(TRUE, x < end_temp)
  temperature_filtered <- x[1:(end+3)]

  cumulative_gsdd <- sum(temperature_filtered)
  return(cumulative_gsdd)
}


