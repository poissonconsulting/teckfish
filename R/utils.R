longest_run <- function(x) {
  rle <- rle(is.na(x))
  wch <- which.max(rle$length)
  if (rle$length[wch] < 184 | rle$values[wch]) {
    return(NA_real_)
  }
  cumsum <- cumsum(rle$lengths)
  to <- cumsum[wch]
  from <- if (wch == 1) 1L else cumsum[wch - 1] + 1L
  x[from:to]
}

sum_vector <- function(from, to, ..vector) {
  sum(..vector[from:to])
}

is_even <- function(x) {
  !(x %% 2)
}

index_begin_run <- function(x) {
  index <- which(x)
  if (length(index) <= 1) {
    return(index)
  }
  index[c(TRUE, diff(index) > 1)]
}
