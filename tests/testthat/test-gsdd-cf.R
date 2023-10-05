test_that("output is a numeric value", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2 * pi / 365) * (day - 10)) + rnorm(365, mean = 10, sd = .5)
  output <- gsdd_cf(x)
  #  expect_equal(3901.01849569098) this is what calculate_gsdd gets
  expect_equal(output, 3902.33018879598) 
})

test_that("vector must be longer than window_width", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2 * pi / 365) * (day - 10)) + rnorm(365, mean = 10, sd = .5)
  expect_chk_error(gsdd_cf(x, window_width = 369))
})

test_that("vector must not contain NA values", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2 * pi / 365) * (day - 10)) + rnorm(365, mean = 10, sd = 0.5)
  random_indices <- sample(seq_along(x), 40)
  x[random_indices] <- NA
  expect_chk_error(gsdd_cf(x, end_temp = 4))
})

test_that("start temp must be greater than or equal to end temp", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2 * pi / 365) * (day - 10)) + rnorm(365, mean = 10, sd = .5)
  expect_chk_error(gsdd_cf(x, end_temp = 40))
})

test_that("if max temp in vector is lower than start_temp the function will error", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2 * pi / 365) * (day - 10)) + rnorm(365, mean = 10, sd = .5)
  #  expect_chk_error(gsdd_cf(x = x, start_temp = 50, end_temp = 4))
  output <- gsdd_cf(x, start_temp = 50)
  expect_identical(output, 0) 
})

test_that("if end_temp is not reached, gsdd calculated to end of vector and warning is shown", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2 * pi / 365) * (day - 10)) + rnorm(365, mean = 10, sd = .5)
  expect_warning(gsdd_cf(x, end_temp = -40))
})

test_that("if end_temp is reached at end of vector x, indicies do not fall off the edge", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2 * pi / 365) * (day - 10)) + rnorm(365, mean = 10, sd = .5)
  gsdd <- gsdd_cf(x, end_temp = -4)
  #  expect_equal(gsdd, 3827.5905) this is what calculate_gsdd gets
  expect_equal(gsdd, 3828.24748908639) 
})

test_that("if start_temp is reached at start of vector x, indicies do not fall off the edge", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2 * pi / 365) * (day - 10)) + rnorm(365, mean = 10, sd = 0.5)
  x <- x[163:length(x)]
  gsdd <- gsdd_cf(x, end_temp = 4, quiet = TRUE)
  #  expect_equal(gsdd, 2678.3522) this is what calculate_gsdd gets
  expect_equal(gsdd, NA_real_) 
  gsdd <- gsdd_cf(x, end_temp = 4, quiet = TRUE, entire = FALSE)
  expect_equal(gsdd, 2687.98160174586) 
})

test_that("x must have a length between 180 and 366", {
  x <- c(rep(0, 10), rep(10, 20), rep(0, 10))
  expect_error(gsdd_cf(x, window_width = 3, start_temp = 9, end_temp = 9),
               "`x` must have a length between 180 and 366 not 40\\.")
})

test_that("If temperature jumps above start_temp but for too short a period of time for rolling mean to be above start_temp for longer than n_consecutive it will not start counting gsdd until rollmean remains above start_temp for n_consecutive days.",{
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2*pi / 365) * (day-10))
  x[99] <- 9
  gsdd<-gsdd_cf(x, window_width = 3, start_temp = 9, end_temp = 9)
  #  expect_equal(gsdd, 1422.9838) this what calculate_gsdd gets
  expect_equal(gsdd, 1431.4310554115) 
})

test_that("Gets growth period with higher GSDD even though shorter period.",{
  x <- c(rep(0, 100), rep(10, 50), rep(0, 50), rep(20, 40), rep(0, 115))
  gsdd<-gsdd_cf(x, window_width = 3, start_temp = 9, end_temp = 9)
  expect_equal(gsdd, 800)
})

test_that("Gets growth period with higher GSDD even though shorter period.",{
  x <- c(rep(10, 50),rep(0, 255), rep(20, 40))
  gsdd<-gsdd_cf(x, window_width = 3, start_temp = 9, end_temp = 9, quiet = TRUE)
  expect_equal(gsdd, NA_real_)
  gsdd<-gsdd_cf(x, window_width = 3, start_temp = 9, end_temp = 9, quiet = TRUE,
                entire = FALSE)
  expect_equal(gsdd, 800)
})
