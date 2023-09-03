test_that("output is a numeric value", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
  output <- calculate_gsdd(x, window_width = 7, start_temp = 5, end_temp = 4, n_consecutive = 5)
  expect_true(is.numeric(output))
})

test_that("vector must be longer than window_width", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
  expect_chk_error(calculate_gsdd(x, window_width = 369, start_temp = 5, end_temp = 4, n_consecutive = 5))
})

test_that("vector must be longer than n_consecutive", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
  expect_chk_error(calculate_gsdd(x, window_width = 7, start_temp = 5, end_temp = 4, n_consecutive = 369))
})

test_that("vector must not contain NA values", {
  set.seed(13)  
  day <- 1:365
  x <- -15 * cos((2 * pi / 365) * (day - 10)) + rnorm(365, mean = 10, sd = 0.5)
  random_indices <- sample(1:length(x), 40)
  x[random_indices] <- NA
  expect_chk_error(calculate_gsdd(x, window_width = 7, start_temp = 5, end_temp = 4, n_consecutive = 5))
})

test_that("start temp must be greater than or equal to end temp", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
  expect_chk_error(calculate_gsdd(x, window_width = 7, start_temp = 5, end_temp = 40, n_consecutive = 5))
})

test_that("if max temp in vector is lower than start_temp the function will error", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
  expect_chk_error(calculate_gsdd(x = x, window_width = 7, start_temp = 50, end_temp = 4, n_consecutive = 5))
})

test_that("if end_temp is not reached, gsdd calculated to end of vector and warning is shown", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
  expect_warning(calculate_gsdd(x, window_width = 7, start_temp = 5, end_temp = -40, n_consecutive = 5))
})

test_that("if end_temp is reached at end of vector x, indicies do not fall off the edge", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
  gsdd <- calculate_gsdd(x, window_width = 7, start_temp = 5, end_temp = -4, n_consecutive = 5)
  expect_equal(gsdd, 3827.5905)
})

test_that("if start_temp is reached at start of vector x, indicies do not fall off the edge", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2 * pi / 365) * (day - 10)) + rnorm(365, mean = 10, sd = 0.5)
  x <- x[163:length(x)]
  gsdd <- calculate_gsdd(x, window_width = 7, start_temp = 5, end_temp = 4, n_consecutive = 7)
  expect_equal(gsdd, 2678.3522)
})

test_that("output is consistent with expected value",{
  x <- c(rep(0,10),rep(10,20), rep(0,10))
  gsdd<-calculate_gsdd(x, window_width = 3, start_temp = 9, end_temp = 9, n_consecutive = 3)
  expect_equal(gsdd, 200)
  })

test_that("output is consistent with expected value if x dips below end_temp for n_consecutive - 1",{
  x <- c(rep(0,10), rep(10,10), rep(8,2), rep(10,9), rep(0,10))
  gsdd<-calculate_gsdd(x, window_width = 3, start_temp = 9, end_temp = 9, n_consecutive = 3)
  expect_equal(gsdd, 206)
})

test_that("if window_width for rolling mean is even and index adjust is not a whole number, final index will round down",{
  x <- c(rep(0,10),rep(10,20), rep(0,10))
  gsdd<-calculate_gsdd(x, window_width = 3, start_temp = 9, end_temp = 9, n_consecutive = 4)
  expect_equal(gsdd, 200)
})
