#' Calculate Growing Seasons from a vector
#'
#' @param x A numeric vector of the
#' mean daily water temperature values for the period
#' of interest in C. It must be consist of no more than 
#' 366 values.
#' @inheritParams gsdd_cf
#' @return A tibble of the growing seasons with three columns.
#' `start_index` an integer vector of the start index, 
#' `end_index` an integer vector of the end index and 
#' `gsdd` an real vector of the growing season degree days.
#' @seealso [gsdd_cf()]
#' @export
#'
#' @examples
#' gss_cf(c(rep(1, 10), rep(10, 20), rep(1, 200)))
gss_cf <- function(x,
                   ignore_truncation = FALSE,
                   start_temp = 5,
                   end_temp = 4,
                   window_width = 7,
                   msgs = TRUE) {
  data <- .gss(x, ignore_truncation = ignore_truncation, 
               start_temp = start_temp, end_temp = end_temp,
               window_width = window_width, msgs = msgs)
  
  if(vld_scalar(data)) {
    return(tibble::tibble(start_index = integer(0), end_index = integer(0), gsdd = numeric(0)))
  }
  data |>
    dplyr::select(start_index = .data$index_start,
                  end_index = .data$index_end,
                  "gsdd") |>
    dplyr::arrange(.data$start_index)
}
