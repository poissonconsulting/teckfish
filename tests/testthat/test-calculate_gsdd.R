testthat::test_that("output is a numeric value", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
  output <- calculate_gsdd(x, rollmean_units = 7, start_temp = 5, end_temp = 4, n_consecutive = 5)
  testthat::expect_true(is.numeric(output))
})

testthat::test_that("vector must be longer than rollmean_units", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
  testthat::expect_error(calculate_gsdd(x, rollmean_units = 369, start_temp = 5, end_temp = 4, n_consecutive = 5))
})

testthat::test_that("vector must be longer than n_consecutive", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
  testthat::expect_error(calculate_gsdd(x, rollmean_units = 7, start_temp = 5, end_temp = 4, n_consecutive = 369))
})

testthat::test_that("vector must not contain NA values", {
  set.seed(13)  
  day <- 1:365
  x <- -15 * cos((2 * pi / 365) * (day - 10)) + rnorm(365, mean = 10, sd = 0.5)
  random_indices <- sample(1:length(x), 40)
  x[random_indices] <- NA
  testthat::expect_error(calculate_gsdd(x, rollmean_units = 7, start_temp = 5, end_temp = 4, n_consecutive = 5))
})

testthat::test_that("start temp must be greater than or equal to end temp", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
  testthat::expect_error(calculate_gsdd(x, rollmean_units = 7, start_temp = 5, end_temp = 40, n_consecutive = 5))
})

testthat::test_that("if max temp in vector is lower than start_temp the function will error", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
  testthat::expect_error(calculate_gsdd(x = x, rollmean_units = 7, start_temp = 50, end_temp = 4, n_consecutive = 5))
})

testthat::test_that("if end_temp is not reached, gsdd calculated to end of vector and warning is shown", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
  testthat::expect_warning(calculate_gsdd(x, rollmean_units = 7, start_temp = 5, end_temp = -40, n_consecutive = 5))
})

testthat::test_that("if end_temp is reached at end of vector x, indicies do not fall off the edge", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
  gsdd <- calculate_gsdd(x, rollmean_units = 7, start_temp = 5, end_temp = -4, n_consecutive = 5)
  testthat::expect_equal(gsdd, 3827.5905)
})


testthat::test_that("if start_temp is reached at start of vector x, indicies do not fall off the edge", {
  set.seed(13)
  x <- -15 * cos((2 * pi / 365) * (day - 10)) + rnorm(365, mean = 10, sd = 0.5)
  x <- x[163:length(x)]
  gsdd <- calculate_gsdd(x, rollmean_units = 7, start_temp = 5, end_temp = 4, n_consecutive = 7)
  testthat::expect_equal(gsdd, 2678.3522)
})



