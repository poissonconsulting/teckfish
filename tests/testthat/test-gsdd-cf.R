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
  expect_identical(gsdd_cf(x), NA_real_)
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
  x <- c(seq(-5, 9), 10, seq(9, -5))
  ma <- zoo::rollmean(x, k = 7, align = "center", na.pad = TRUE)
  data <- tibble::tibble(index = 1:length(x), x = x, ma = ma)
  expect_equal(
    data,
    tibble::tribble(
      ~index, ~x, ~ma,
      1L, -5, NA,
      2L, -4, NA,
      3L, -3, NA,
      4L, -2, -2,
      5L, -1, -1,
      6L, 0, 0,
      7L, 1, 1,
      8L, 2, 2,
      9L, 3, 3,
      10L, 4, 4,
      11L, 5, 5,
      12L, 6, 6,
      13L, 7, 7,
      14L, 8, 7.71428571428571,
      15L, 9, 8.14285714285714,
      16L, 10, 8.28571428571429,
      17L, 9, 8.14285714285714,
      18L, 8, 7.71428571428571,
      19L, 7, 7,
      20L, 6, 6,
      21L, 5, 5,
      22L, 4, 4,
      23L, 3, 3,
      24L, 2, 2,
      25L, 1, 1,
      26L, 0, 0,
      27L, -1, -1,
      28L, -2, -2,
      29L, -3, NA,
      30L, -4, NA,
      31L, -5, NA
    )
  )

  expect_equal(gsdd_cf(x), sum(x[9:26]))
})

test_that("Gets asymmetric triangle", {
  x <- c(seq(-5, 9), 10, seq(9.5, -5.5))
  ma <- zoo::rollmean(x, k = 7, align = "center", na.pad = TRUE)
  data <- tibble::tibble(index = 1:length(x), x = x, ma = ma)
  expect_equal(
    data,
    tibble::tribble(
      ~index, ~x, ~ma,
      1L, -5, NA,
      2L, -4, NA,
      3L, -3, NA,
      4L, -2, -2,
      5L, -1, -1,
      6L, 0, 0,
      7L, 1, 1,
      8L, 2, 2,
      9L, 3, 3,
      10L, 4, 4,
      11L, 5, 5,
      12L, 6, 6,
      13L, 7, 7,
      14L, 8, 7.78571428571429,
      15L, 9, 8.28571428571429,
      16L, 10, 8.5,
      17L, 9.5, 8.42857142857143,
      18L, 8.5, 8.07142857142857,
      19L, 7.5, 7.42857142857143,
      20L, 6.5, 6.5,
      21L, 5.5, 5.5,
      22L, 4.5, 4.5,
      23L, 3.5, 3.5,
      24L, 2.5, 2.5,
      25L, 1.5, 1.5,
      26L, 0.5, 0.5,
      27L, -0.5, -0.5,
      28L, -1.5, -1.5,
      29L, -2.5, -2.5,
      30L, -3.5, NA,
      31L, -4.5, NA,
      32L, -5.5, NA
    )
  )

  expect_equal(gsdd_cf(x), sum(x[9:26]))
})

test_that("2 asymetric triangles, first one longer but lower, second should be chosen.", {
  x <- c(
    rep(0, 3),
    seq(0, 10, by = 0.5),
    seq(10, 2, by = -0.5),
    seq(2, 25, by = 2),
    seq(21, 0, by = -5),
    rep(0, 5)
  )
  ma <- zoo::rollmean(x, k = 7, align = "center", na.pad = TRUE)
  data <- tibble::tibble(index = 1:length(x), x = x, ma = ma)
  expect_equal(data,
    tibble::tribble(
      ~index, ~x, ~ma,
      1L, 0.0, NA,
      2L, 0.0, NA,
      3L, 0.0, NA,
      4L, 0.0, 0.42857143,
      5L, 0.5, 0.71428571,
      6L, 1.0, 1.07142857,
      7L, 1.5, 1.5000000,
      8L, 2.0, 2.0000000,
      9L, 2.5, 2.5000000,
      10L, 3.0, 3.0000000,
      11L, 3.5, 3.5000000,
      12L, 4.0, 4.0000000,
      13L, 4.5, 4.5000000,
      14L, 5.0, 5.0000000,
      15L, 5.5, 5.5000000,
      16L, 6.0, 6.0000000,
      17L, 6.5, 6.5000000,
      18L, 7.0, 7.0000000,
      19L, 7.5, 7.5000000,
      20L, 8.0, 8.0000000,
      21L, 8.5, 8.5000000,
      22L, 9.0, 8.9285714,
      23L, 9.5, 9.2142857,
      24L, 10.0, 9.3571429,
      25L, 10.0, 9.3571429,
      26L, 9.5, 9.2142857,
      27L, 9.0, 8.9285714,
      28L, 8.5, 8.5000000,
      29L, 8.0, 8.0000000,
      30L, 7.5, 7.5000000,
      31L, 7.0, 7.0000000,
      32L, 6.5, 6.5000000,
      33L, 6.0, 6.0000000,
      34L, 5.5, 5.5000000,
      35L, 5.0, 5.0000000,
      36L, 4.5, 4.5000000,
      37L, 4.0, 4.0000000,
      38L, 3.5, 3.5000000,
      39L, 3.0, 3.0714286,
      40L, 2.5, 3.0000000,
      41L, 2.0, 3.2857143,
      42L, 2.0, 3.9285714,
      43L, 4.0, 4.9285714,
      44L, 6.0, 6.2857143,
      45L, 8.0, 8.0000000,
      46L, 10.0, 10.0000000,
      47L, 12.0, 12.0000000,
      48L, 14.0, 14.0000000,
      49L, 16.0, 16.0000000,
      50L, 18.0, 18.0000000,
      51L, 20.0, 19.2857143,
      52L, 22.0, 19.5714286,
      53L, 24.0, 18.8571429,
      54L, 21.0, 17.1428571,
      55L, 16.0, 14.4285714,
      56L, 11.0, 11.2857143,
      57L, 6.0, 7.8571429,
      58L, 1.0, 4.8571429,
      59L, 0.0, 2.5714286,
      60L, 0.0, 1.000,
      61L, 0.0, NA,
      62L, 0.0, NA,
      63L, 0.0, NA
    ),
    tolerance = 1e-3
  )
  expect_equal(gsdd_cf(x), sum(x[41:61]))
})

test_that("2 asymetric triangles, second one longer but lower, first one should be chosen.", {
  x <- c(
    rep(0, 3),
    seq(2, 25, by = 2),
    seq(21, 0, by = -5),
    seq(0, 10, by = 0.5),
    seq(10, 0, by = -0.5),
    rep(0, 5)
  )
  ma <- zoo::rollmean(x, k = 7, align = "center", na.pad = TRUE)
  data <- tibble::tibble(index = 1:length(x), x = x, ma = ma)
  expect_equal(data,
    tibble::tribble(
      ~index, ~x, ~ma,
      1L, 0, NA,
      2L, 0, NA,
      3L, 0, NA,
      4L, 2, 2.85714285714286,
      5L, 4, 4.28571428571429,
      6L, 6, 6,
      7L, 8, 8,
      8L, 10, 10,
      9L, 12, 12,
      10L, 14, 14,
      11L, 16, 16,
      12L, 18, 18,
      13L, 20, 19.2857142857143,
      14L, 22, 19.5714285714286,
      15L, 24, 18.8571428571429,
      16L, 21, 17.1428571428571,
      17L, 16, 14.4285714285714,
      18L, 11, 11.2857142857143,
      19L, 6, 7.92857142857143,
      20L, 1, 5.07142857142857,
      21L, 0, 3,
      22L, 0.5, 1.71428571428571,
      23L, 1, 1.21428571428571,
      24L, 1.5, 1.5,
      25L, 2, 2,
      26L, 2.5, 2.5,
      27L, 3, 3,
      28L, 3.5, 3.5,
      29L, 4, 4,
      30L, 4.5, 4.5,
      31L, 5, 5,
      32L, 5.5, 5.5,
      33L, 6, 6,
      34L, 6.5, 6.5,
      35L, 7, 7,
      36L, 7.5, 7.5,
      37L, 8, 8,
      38L, 8.5, 8.5,
      39L, 9, 8.92857142857143,
      40L, 9.5, 9.21428571428571,
      41L, 10, 9.35714285714286,
      42L, 10, 9.35714285714286,
      43L, 9.5, 9.21428571428571,
      44L, 9, 8.92857142857143,
      45L, 8.5, 8.5,
      46L, 8, 8,
      47L, 7.5, 7.5,
      48L, 7, 7,
      49L, 6.5, 6.5,
      50L, 6, 6,
      51L, 5.5, 5.5,
      52L, 5, 5,
      53L, 4.5, 4.5,
      54L, 4, 4,
      55L, 3.5, 3.5,
      56L, 3, 3,
      57L, 2.5, 2.5,
      58L, 2, 2,
      59L, 1.5, 1.5,
      60L, 1, 1.07142857142857,
      61L, 0.5, 0.714285714285714,
      62L, 0, 0.428571428571429,
      63L, 0, 0.214285714285714,
      64L, 0, 0.0714285714285714,
      65L, 0, NA,
      66L, 0, NA,
      67L, 0, NA
    ),
    tolerance = 1e-3
  )
  expect_equal(gsdd_cf(x), sum(x[3:24]))
})

test_that("Right truncated triangle", {
  x <- c(
    rep(0, 15),
    seq(2, 25, by = 2),
    seq(21, 5, by = -2)
  )
  ma <- zoo::rollmean(x, k = 7, align = "center", na.pad = TRUE)
  data <- tibble::tibble(index = 1:length(x), x = x, ma = ma)
  expect_equal(data,
    tibble::tribble(
      ~index, ~x, ~ma,
      1L, 0, NA,
      2L, 0, NA,
      3L, 0, NA,
      4L, 0, 0,
      5L, 0, 0,
      6L, 0, 0,
      7L, 0, 0,
      8L, 0, 0,
      9L, 0, 0,
      10L, 0, 0,
      11L, 0, 0,
      12L, 0, 0,
      13L, 0, 0.285714285714286,
      14L, 0, 0.857142857142857,
      15L, 0, 1.71428571428571,
      16L, 2, 2.85714285714286,
      17L, 4, 4.28571428571429,
      18L, 6, 6,
      19L, 8, 8,
      20L, 10, 10,
      21L, 12, 12,
      22L, 14, 14,
      23L, 16, 16,
      24L, 18, 18,
      25L, 20, 19.2857142857143,
      26L, 22, 20,
      27L, 24, 20.1428571428571,
      28L, 21, 19.7142857142857,
      29L, 19, 18.7142857142857,
      30L, 17, 17.1428571428571,
      31L, 15, 15,
      32L, 13, 13,
      33L, 11, 11,
      34L, 9, NA,
      35L, 7, NA,
      36L, 5, NA
    ),
    tolerance = 1e-3
  )
  expect_equal(gsdd_cf(x, quiet = TRUE), NA_real_)
  expect_equal(gsdd_cf(x, ignore_truncation = "right", quiet = TRUE), sum(x[15:length(x)]))
})

test_that("Left truncated triangle", {
  x <- c(
    seq(6, 25, by = 2),
    seq(25, 0, by = -2),
    rep(0, 15)
  )
  ma <- zoo::rollmean(x, k = 7, align = "center", na.pad = TRUE)
  data <- tibble::tibble(index = 1:length(x), x = x, ma = ma)
  expect_equal(data,
    tibble::tribble(
      ~index, ~x, ~ma,
      1L, 6, NA,
      2L, 8, NA,
      3L, 10, NA,
      4L, 12, 12,
      5L, 14, 14,
      6L, 16, 16,
      7L, 18, 18,
      8L, 20, 19.8571428571429,
      9L, 22, 21.1428571428571,
      10L, 24, 21.8571428571429,
      11L, 25, 22,
      12L, 23, 21.5714285714286,
      13L, 21, 20.5714285714286,
      14L, 19, 19,
      15L, 17, 17,
      16L, 15, 15,
      17L, 13, 13,
      18L, 11, 11,
      19L, 9, 9,
      20L, 7, 7,
      21L, 5, 5.14285714285714,
      22L, 3, 3.57142857142857,
      23L, 1, 2.28571428571429,
      24L, 0, 1.28571428571429,
      25L, 0, 0.571428571428571,
      26L, 0, 0.142857142857143,
      27L, 0, 0,
      28L, 0, 0,
      29L, 0, 0,
      30L, 0, 0,
      31L, 0, 0,
      32L, 0, 0,
      33L, 0, 0,
      34L, 0, 0,
      35L, 0, 0,
      36L, 0, NA,
      37L, 0, NA,
      38L, 0, NA
    ),
    tolerance = 1e-3
  )
  expect_equal(gsdd_cf(x, quiet = TRUE), NA_real_)
  expect_equal(gsdd_cf(x, ignore_truncation = "left", quiet = TRUE), sum(x[0:25]))
})
