which_replace <- function(x, max_span, min_gap = 0L, ends = TRUE) {
  x <- is.na(x)
  x <- diff(c(FALSE, x, FALSE))
  df <- data.frame(start = which(x == 1))
  df$end <- which(x == -1)
  if (!ends) {
    if (df$start[1] == 1) {
      df <- df[-1, ]
    }
    if (!nrow(df)) {
      return(integer(0))
    }
    if (df$end[nrow(df)] == length(x)) {
      df <- df[-nrow(df), ]
    }
  }
  df <- df[df$end - df$start <= max_span, ]
  if (!nrow(df)) {
    return(integer(0))
  }
  df$start[df$start != 1] <- df$start[df$start != 1] + min_gap
  df$end[df$end != length(x)] <- df$end[df$end != length(x)] -
    min_gap
  df <- df[df$end - df$start > 0, ]
  if (!nrow(df)) {
    return(integer(0))
  }
  df$end <- df$end - 1L
  which <- mapply(seq, df$start, df$end, USE.NAMES = FALSE)
  which <- unlist(which)
  which <- sort(which)
  which
}

#' Interpolate Numeric Vector
#'
#' Useful for filling in short runs of missing values in a time series.
#'
#' `interpolate_numeric_vector()` is essentially a wrapper on [`stats::approx()`].
#'
#' @param x A double or integer vector of with missing values to fill
#' in using linear interpolation.
#' @param span A whole number of the maximum span of missing values to interpolate.
#' If a gap exceeds the span none of the values are interpolate.
#' @param tails A flag specifying whether to fill in missing values at the
#' start and end by setting them to be the same value as the closest
#' adjacent non-missing value.
#'
#' @return A double or integer vector.
#' @export
#'
#' @examples
#' interpolate_numeric_vector(c(1, NA, 4))
#' interpolate_numeric_vector(c(1L, NA, 4L))
#' interpolate_numeric_vector(c(1, NA, NA, NA, NA, 3))
#' interpolate_numeric_vector(c(1, NA, NA, NA, NA, 3), span = 4)
#' interpolate_numeric_vector(c(NA, NA, 10, 1, NA))
#' interpolate_numeric_vector(c(NA, NA, 10, 1, NA), tails = TRUE)
interpolate_numeric_vector <- function(x, span = 3, tails = FALSE) {
  chk_vector(x)
  chk_numeric(x)
  chk_whole_number(span)
  chk_gte(span)
  chk_flag(tails)

  if (!length(x) | !anyNA(x) | span == 0 | all(is.na(x))) {
    return(x)
  }
  which <- which_replace(x, max_span = span, ends = tails)
  if (!length(which)) {
    return(x)
  }
  is_integer <- is.integer(x)
  x[which] <- stats::approx(x, xout = which, rule = 2)$y
  if (is_integer) {
    x <- as.integer(round(x))
  }
  x
}
