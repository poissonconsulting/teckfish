trim_na <- function(x) {
  indices <- which(!is.na(x))
  if(!length(indices)) {
    return(x[0])
  }
  first <- indices[1]
  last <- indices[length(indices)]
  x[first:last]
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

.data <- NULL
