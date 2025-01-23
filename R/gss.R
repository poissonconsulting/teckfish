#' Calculate Growing Seasons (GSS)
#'
#' A wrapper on [gsdd::gss()] to by default
#' get all Growing Seasons ignoring truncation.
#' For more information see [gsdd::gss()].
#'
#' @inheritParams gsdd::gdd
#' @seealso [gsdd::gss()], [gsdd()] and [gss()].
#' @export
#'
#' @examples
#' gss(gsdd::temperature_data)
gss <- function(
    x,
    min_length = 120,
    ignore_truncation = TRUE,
    pick = "all",
    msgs = TRUE) {
  chk_whole_number(min_length)
  chk_range(min_length, c(14, 274))

  gsdd::gss(
    x,
    min_length = min_length,
    ignore_truncation = ignore_truncation,
    pick = pick,
    msgs = msgs
  )
}
