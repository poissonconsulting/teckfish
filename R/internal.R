pick_season <- function(x, pick) {
  if(pick == "all") {
    return(x)
  }
  
  x <- if(pick == "biggest") {
    x |> dplyr::filter(.data$gsdd == max(.data$gsdd))
  } else if(pick == "smallest") {
    x |> dplyr::filter(.data$gsdd == min(.data$gsdd))
  } else if(pick == "longest") {
    x |> dplyr::filter(.data$ndays == max(.data$ndays)) |>
      dplyr::arrange(dplyr::desc(.data$gsdd))
  } else if(pick == "shortest") {
    x |> dplyr::filter(.data$ndays == min(.data$ndays))  |>
      dplyr::arrange(.data$gsdd)
  } else if(pick == "last") {
    x |> dplyr::filter(.data$index_start == max(.data$index_start))
  } else if(pick == "first") {
    x |> dplyr::filter(.data$index_start == min(.data$index_start))
  }
  x |>
    dplyr::slice(1)
}