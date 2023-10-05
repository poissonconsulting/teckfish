sum_vector <- function(from, to, ..vector) {
  sum(..vector[from:to])
}

is_even <- function(x) {
  !(x %% 2)
}

index_begin_run <- function(x) {
  index <- which(x)
  if(length(index) <= 1) return(index)
  index[c(TRUE, diff(index) == 1)]
}

.data <- NULL
