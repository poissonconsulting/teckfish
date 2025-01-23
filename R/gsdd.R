#' Calculate Growing Season Degree Days (GSDD)
#'
#' A wrapper on [gsdd::gsdd()] to get the Growing Season Degree Days for
#' the longest growing season.
#'
#' @inheritParams gsdd::gsdd
#' @seealso [gsdd::gsdd()], [gsdd()] and [gss()].
#' @export
#'
#' @examples
#' gsdd(gsdd::temperature_data)
gsdd <- function(
    x,
    min_length = 120,
    msgs = TRUE) {
  chk_whole_number(min_length)
  chk_range(min_length, c(14, 274))

  gsdd::gsdd(
    x,
    min_length = min_length,
    pick = "longest",
    msgs = msgs
  )
}
