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
calculate_gsdd <- function(x, k, starttemp, endtemp) {
  # calculate rolling average
  rollingmean <- zoo::rollmean(x=x, k=k)
  
  # remove days that are above startttemp and below endtemp
  start <- match(TRUE, x > starttemp)
  end <- match(TRUE, x[start:length(x)] < endtemp)
  temperature_filtered <- x[start:end]
  
  # Subtract base temp (starttemp) and sum for each season. 
  gsdd <- sum(temperature - starttemp)
}


# Simulate annual temp data for dev - will remove
# day <- 1:365
# temperature <- -15 * cos((2*pi / 365) * (day-60)) + rnorm(365, mean = 10, sd = 1)
# 
# plot(day, temperature, type = "l",
#      xlab = "Day",
#      ylab = "Temperature (°C)")
# x <- temperature
# k <- 7

starttemp <- 5
endtemp <- 4


