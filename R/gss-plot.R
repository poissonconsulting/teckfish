#' Plot Growing Seasons (GSS)
#' 
#' A wrapper on [gsdd::gss_plot()] to by default 
#' plot all Growing Seasons ignoring truncation.
#' For more information see [gsdd::gss_plot()].
#'
#' @inheritParams gsdd::gss_plot
#' @seealso [gsdd::gss_plot()] and [gss()].
#' @export
#'
#' @examples
#' gss_plot(gsdd::temperature_data)
gss_plot <- function(
    x,
    min_length = 60,
    ignore_truncation = TRUE,
    pick = "all",
    latex = FALSE,
    nrow = NULL,
    ncol = NULL,
    msgs = TRUE
) {
  chk_whole_number(min_length)
  chk_range(min_length, c(14, 274))
  
  gsdd::gss_plot(
    x, 
    min_length = min_length, 
    ignore_truncation = ignore_truncation,
    pick = pick,
    latex = latex,
    nrow = nrow,
    ncol = ncol,
    msgs = msgs)
}
