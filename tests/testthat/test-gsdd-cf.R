test_that("output is a numeric value", {
  x <- simulated_data$synthetic
  output <- gsdd_cf(x)
  expect_equal(output, 3898.80557580767)
})

test_that("vector must be longer than window_width", {
  x <- simulated_data$synthetic
  expect_chk_error(gsdd_cf(x, window_width = 369))
})

test_that("vector must not contain NA values", {
  x <- simulated_data$synthetic
  random_indices <- sample(seq_along(x), 40)
  x[random_indices] <- NA
  expect_chk_error(gsdd_cf(x, end_temp = 4))
})

test_that("start temp must be greater than or equal to end temp", {
  x <- simulated_data$synthetic
  expect_chk_error(gsdd_cf(x, end_temp = 40, start_temp = 30))
})

test_that("if max temp in vector is lower than start_temp the function return 0", {
  x <- simulated_data$synthetic
  output <- gsdd_cf(x, start_temp = 50)
  expect_identical(output, 0)
})

test_that("if end_temp is not reached, gsdd calculated to end of vector and warning is shown", {
  x <- simulated_data$synthetic
  expect_warning(gsdd_cf(x, end_temp = -40, ))
})

test_that("if end_temp is reached at end of vector x, indicies do not fall off the edge", {
  x <- simulated_data$synthetic
  gsdd <- gsdd_cf(x, end_temp = -4, quiet = TRUE, ignore_truncation = TRUE)
  expect_equal(gsdd, 3921.63308)
})

test_that("if start_temp is reached at start of vector x, indicies do not fall off the edge", {
  x <- simulated_data$synthetic
  x <- x[163:length(x)]
  gsdd <- gsdd_cf(x, end_temp = 4, quiet = TRUE)
  expect_equal(gsdd, NA_real_)
  gsdd <- gsdd_cf(x, end_temp = 4, quiet = TRUE, ignore_truncation = TRUE)
  expect_equal(gsdd, 2687.98160174586)
})

test_that("x must have a length between 28 and 366", {
  x <- c(rep(0, 1), rep(10, 20), rep(0, 1))
  expect_error(
    gsdd_cf(x, window_width = 3, start_temp = 9, end_temp = 9),
    "`x` must have a length between 28 and 366 not 22\\."
  )
})

test_that("Gets growth period with higher GSDD even though shorter period.", {
  x <- c(rep(0, 100), rep(10, 50), rep(0, 50), rep(20, 40), rep(0, 115))
  gsdd <- gsdd_cf(x, window_width = 3, start_temp = 9, end_temp = 9)
  expect_equal(gsdd, 800)
})

test_that("Gets growth period with higher GSDD even though shorter period.", {
  x <- c(rep(10, 50), rep(0, 255), rep(20, 40))
  gsdd <- gsdd_cf(x, window_width = 3, start_temp = 9, end_temp = 9, quiet = TRUE)
  expect_equal(gsdd, NA_real_)
  gsdd <- gsdd_cf(x,
                  window_width = 3, start_temp = 9, end_temp = 9, quiet = TRUE,
                  ignore_truncation = TRUE
  )
  expect_equal(gsdd, 800)
})

test_that("Gets growth gives warnings with truncation.", {
  x <- c(rep(10, 50), rep(0, 255), rep(20, 40))
  expect_warning(expect_identical(gsdd_cf(x), NA_real_), "Growing season truncated\\.")
  expect_warning(expect_identical(gsdd_cf(x, ignore_truncation = "left"), NA_real_), "Growing season truncated\\.")
})

test_that("Gets gsdd with single boiling day.", {
  x <- c(rep(0, 100), rep(100, 1), rep(0, 100))
  expect_identical(gsdd_cf(x), 100)
})

test_that("Gets gsdd with single hot day.", {
  x <- c(rep(0, 100), rep(36, 1), rep(0, 100))
  expect_identical(gsdd_cf(x), 36)
})

test_that("Gets 0 gsdd with single warm day.", {
  x <- c(rep(0, 100), rep(35, 1), rep(0, 100))
  expect_identical(gsdd_cf(x), 0)
})

test_that("gsdd with two weeks", {
  x <- c(rep(0, 100), rep(5.1, 7), rep(3.8, 7), rep(0, 100))
  expect_gte(mean(c(rep(5.1, 2), rep(3.8, 5))), 4)
  expect_lt(mean(c(rep(5.1, 1), rep(3.8, 6))), 4)
  expect_equal(gsdd_cf(x), 5.1 * 7 + 3.8 * 6)
})

test_that("Gets with two weeks and 3 day window width - great test", {
  x <- c(rep(0, 100), rep(5.1, 7), rep(3.8, 7), rep(0, 100))
  expect_gte(mean(c(rep(5.1, 2), rep(3.8, 1))), 4)
  expect_lt(mean(c(rep(5.1, 0), rep(3.8, 3))), 4)
  expect_equal(gsdd_cf(x, window_width = 3), 5.1 * 7 + 3.8 * 3)
})

test_that("Gets with two weeks and 3 day window and smaller", {
  x <- c(rep(0, 100), rep(5.1, 7), rep(3, 7), rep(0, 100))
  expect_lt(mean(c(rep(5.1, 6), 0)), 5)
  expect_gte(mean(c(rep(5.1, 2), rep(3, 1))), 4)
  expect_lt(mean(c(rep(5.1, 1), rep(3, 2))), 4)
  expect_equal(gsdd_cf(x, window_width = 3), 5.1 * 7 + 3 * 2)
})

test_that("Gets with two weeks and 3 day window and smaller", {
  x <- c(rep(0, 100), rep(5.1, 7), rep(0, 100))
  expect_equal(gsdd_cf(x), 5.1 * 7)
})

test_that("Gets one week with end day after of 0", {
  x <- c(rep(0, 100), rep(5.1, 7), rep(1, 0))
  expect_equal(gsdd_cf(x, ignore_truncation = "right", quiet = TRUE), 5.1 * 7)
})

test_that("Gets one week with end day after of 1", {
  x <- c(rep(0, 100), rep(5.1, 7), rep(1, 1))
  expect_equal(gsdd_cf(x, ignore_truncation = "right", quiet = TRUE), 5.1 * 7 + 1)
})

test_that("Gets with two weeks and 3 day window and smaller", {
  x <- c(rep(0, 100), rep(5.1, 7))
  expect_equal(gsdd_cf(x, ignore_truncation = "right", quiet = TRUE), 5.1 * 7)
})

test_that("Gets triangle", {
  x <- c(seq(-5,9),10,seq(9,-5))
  ma <- zoo::rollmean(x, k = 7, align = "center", na.pad = TRUE)
  data <- tibble::tibble(index = 1:length(x), x = x, ma = ma)
  expect_equal(data,
               tibble::tribble(
                 ~index, ~x,              ~ma,
                 1L, -5,               NA,
                 2L, -4,               NA,
                 3L, -3,               NA,
                 4L, -2,               -2,
                 5L, -1,               -1,
                 6L,  0,                0,
                 7L,  1,                1,
                 8L,  2,                2,
                 9L,  3,                3,
                 10L,  4,                4,
                 11L,  5,                5,
                 12L,  6,                6,
                 13L,  7,                7,
                 14L,  8, 7.71428571428571,
                 15L,  9, 8.14285714285714,
                 16L, 10, 8.28571428571429,
                 17L,  9, 8.14285714285714,
                 18L,  8, 7.71428571428571,
                 19L,  7,                7,
                 20L,  6,                6,
                 21L,  5,                5,
                 22L,  4,                4,
                 23L,  3,                3,
                 24L,  2,                2,
                 25L,  1,                1,
                 26L,  0,                0,
                 27L, -1,               -1,
                 28L, -2,               -2,
                 29L, -3,               NA,
                 30L, -4,               NA,
                 31L, -5,               NA
               )
  )
  
  expect_equal(gsdd_cf(x), sum(x[9:26]))
})
