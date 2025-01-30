#' Parameter Descriptions
#'
#' Default parameter descriptions which may be overridden in individual
#' functions.
#'
#' A flag is a non-missing logical scalar.
#'
#' A string is a non-missing character scalar.
#
#' @inheritParams rlang::args_dots_empty
#' @param data A data frame.
#' @param date_time A string indicating the column name of the POSIXct vector.
#' @param gap_range A numeric value indicating the number of hours between two
#'   non reasonable values that will be coded as questionable or erroneous.
#' @param erroneous_buffer A numeric value indicating the number of hours buffer for
#'   erroneous values.
#' @param erroneous_max A numeric value indicating the upper bound of the
#'   erroneous range of temperature values.
#' @param erroneous_min A numeric value indicating the lower bound of the
#'   erroneous range of temperature values.
#' @param erroneous_rate A numeric value indicating the rate of change
#'   (temperature per hour) of temperature values that is considered erroneous.
#' @param questionable_buffer A numeric value indicating the buffer in hours for
#'   questionable values.
#' @param questionable_min A numeric value indicating the lower bound of the
#'   questionable range of temperature values.
#' @param questionable_max A numeric value indicating the upper bound of the
#'   questionable range of temperature values.
#' @param questionable_rate A numeric value indicating the rate of change
#'   (temperature per hour) of temperature values that is considered
#'   questionable.
#' @param value A string indicating the column name of the value vector.
#' @keywords internal
#' @aliases parameters arguments args
#' @usage NULL
# nocov start
params <- function(...) NULL
# nocov end
