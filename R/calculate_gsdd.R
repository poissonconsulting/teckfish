#' calculate growing season degree days.
#'
#' @param x vector of integer temperature data of DAILY VALUES
#' @param k integer width of rolling mean window
#' @param endtemp minimum weekly temperature to define end of growing season
#' @param starttemp threshold temperature to define start of the growing season
#'
#' @return
#' @export
#'
#' @examples
#' day <- 1:365
#' temperature <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
#' calculate_gsdd(x = temperature, k = 7, start_temp = 5, end_temp = 4)
#' 
calculate_gsdd <- function(x, rollmean_units, start_temp = 5, end_temp = 4) {
  chk_vector(x)
  chk_all(temperature, chk_number)
  chk_length(start_temp, length = 1)
  chk_length(end_temp, length = 1)
  chk_count(rollmean_units)
  chk_numeric(start_temp)
  chk_numeric(end_temp)

  # calculate rolling average
  x <- zoo::rollmean(x=x, rollmean_units=rollmean_units)
  
  # remove days that are above start temp and below end temp
  start <- match(TRUE, x > start_temp)
  x <- x[(start-3):length(x)]
  
  end <- match(TRUE, x < end_temp)
  temperature_filtered <- x[1:(end+3)]
  
  # Subtract base temp (start temp) and sum for each season. 
  cumulative_gsdd <- sum(temperature_filtered)
  return(cumulative_gsdd)
}


