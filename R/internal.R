pick_season <- function(x, pick) {
  x <- if(pick == "biggest") {
    x |> dplyr::filter(.data$gsdd == max(.data$gsdd))
  } else if(pick == "smallest") {
    x |> dplyr::filter(.data$gsdd == min(.data$gsdd))
  } else if(pick == "longest") {
    x |> dplyr::filter(.data$ndays == max(.data$ndays))
  } else if(pick == "shortest") {
    x |> dplyr::filter(.data$ndays == min(.data$ndays))
  } else if(pick == "last") {
    x |> dplyr::filter(.data$index_start == max(.data$index_start))
  } else if(pick == "first") {
    x |> dplyr::filter(.data$index_start == min(.data$index_start))
  } else if(pick == "all") {
    return(x)
  } else {
    stop("pick unrecognized")
  }
  
  x |>
    dplyr::arrange(dplyr::desc(.data$gsdd),
                   dplyr::desc(.data$ndays),
                   dplyr::desc(.data$index_start)) |>
    dplyr::slice(1)
}