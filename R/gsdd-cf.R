#' Calculate Growing Season Degree Days (GSDD)
#' 
#' Soft-deprecated for [gsdd::gsdd_vctr()].
#'
#' @inheritParams gsdd::gsdd_vctr
#' @return A non-negative real number of the GSDD.
#' @seealso [gsdd_vctr()]
#' @export
#'
#' @examples
#' gsdd_cf(c(rep(1, 10), rep(10, 20), rep(1, 200)))
#' gsdd_cf(gsdd::temperature_data$temperature)
gsdd_cf <- function(
    x,
    ignore_truncation = FALSE,
    min_length = 120,
    msgs = TRUE
) {
  lifecycle::deprecate_soft("0.1.2", "gsdd_cf()", with = "gsdd::gsdd_vctr()")
  chk_whole_number(min_length)
  chk_range(min_length, c(14, 274))
  
  gsdd::gsdd_vctr(
    x, 
    ignore_truncation = ignore_truncation,
    min_length = min_length, 
    msgs = msgs)
}
