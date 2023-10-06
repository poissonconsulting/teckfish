## code to prepare `DATASET` dataset goes here
set.seed(13)
day <- 1:365
x <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
x[x < 0] <- 0
usethis::use_data(DATASET, overwrite = TRUE)
