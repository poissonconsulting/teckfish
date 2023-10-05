#' Title
#'
#' @param data 
#' @param questionable_min 
#' @param questionable_max 
#' @param erroneous_min 
#' @param erroneous_max 
#' @param questionable_rate 
#' @param erroneous_rate 
#' @param questionable_hours 
#' @param erroneous_hours 
#' @param gap_range 
#'
#' @return
#' @export
#'
#' @examples
classify_water_temp_data <- function(data, 
                                     questionable_min = 0, 
                                     questionable_max = 30, 
                                     erroneous_min = -0.5,
                                     erroneous_max = 40,
                                     questionable_rate = 2,
                                     erroneous_rate = 5,
                                     questionable_hours = 1,
                                     erroneous_hours = 1,
                                     gap_range = 5
                                     ) {
  
}