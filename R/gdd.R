#' Calculate Growing Degree Days (GDD)
#' 
#' A wrapper on [gsdd::gdd()] to get the Growing Degree Days up to a date for
#' the longest growing season.
#'
#' @inheritParams gsdd::gdd
#' @seealso [gsdd::gdd()] and [gsdd()].
#' @export
#'
#' @examples
#' gdd(gsdd::temperature_data)
gdd <- function(
    x,
    end_date = as.Date("1972-09-30"),
    ignore_truncation = FALSE,
    min_length = NULL,
    msgs = TRUE
) {
  gsdd::gdd(
    x, 
    end_date = end_date,
    ignore_truncation = ignore_truncation, 
    min_length = min_length, 
    pick = "longest",
    msgs = msgs)
}
