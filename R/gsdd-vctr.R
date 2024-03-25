#' Calculate Growing Season Degree Days (GSDD)
#'
#' @inheritParams gsdd::gsdd_vctr
#' @return A non-negative real number of the GSDD.
#' @seealso [gsdd::gsdd_vctr()]
#' @export
#'
#' @examples
#' gsdd_vctr(c(rep(1, 10), rep(10, 20), rep(1, 200)))
#' gsdd_vctr(teckfish::simulated_data$synthetic)
gsdd_vctr <- function(
    x,
    ignore_truncation = FALSE,
    min_length = 274,
    msgs = TRUE
) {
  gsdd::gsdd_vctr(
    x, 
    ignore_truncation = ignore_truncation, 
    min_length = min_length, 
    pick = "longest",
    msgs = msgs)
}
