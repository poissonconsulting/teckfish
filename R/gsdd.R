#' Calculate Growing Season Degree Days (GSDD)
#'
#' @inheritParams gsdd::gsdd
#' @seealso [gsdd::gsdd()]
#' @export
#'
#' @examples
#' gsdd(gsdd::temperature_data)
gsdd <- function(
    x,
    ignore_truncation = FALSE,
    min_length = 274,
    msgs = TRUE
) {
  chk_range(min_length, c(14, 274))
  gsdd::gsdd(
    x, 
    ignore_truncation = ignore_truncation, 
    min_length = min_length, 
    pick = "longest",
    msgs = msgs)
}
