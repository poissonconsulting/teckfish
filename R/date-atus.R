#' Calculate Date of Accumulated Thermal Units (ATUs)
#'
#' A wrapper on [gsdd::date_atus()] to calculats the date on which a
#' specified number of Accumulated Thermal Units (ATUs) are exceeded.
#'
#' @inheritParams gsdd::date_atus
#' @return A tibble with four columns `year`, `start_date`, `end_date` and `atus`.
#' @export
#'
#' @examples
#' date_atus(gsdd::temperature_data)
date_atus <- function(
    x,
    atus = 600,
    start_date = as.Date("1972-03-01")) {
  gsdd::date_atus(
    x,
    atus = atus,
    start_date = start_date
  )
}
