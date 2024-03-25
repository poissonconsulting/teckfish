test_that("output is a numeric value", {
  x <- simulated_data$synthetic
  output <- gsdd_cf(x)
  expect_equal(output, 3898.80557580767)
})

test_that("gsdd_cf returns NA when missing summer", {
  x <- simulated_data$synthetic
  x[85:320] <- NA_real_
  expect_identical(gsdd_cf(x, msgs = FALSE), NA_real_)
})

test_that("vector must not contain NA values", {
  x <- simulated_data$synthetic
  random_indices <- sample(seq_along(x), 40)
  x[random_indices] <- NA
  expect_identical(gsdd_cf(x, msgs = FALSE), NA_real_)
})

test_that("gsdd_cf trims missing values", {
  x <- simulated_data$synthetic
  x[c(1,length(x))] <- NA_real_
  expect_equal(gsdd_cf(x), 3898.80557580767)
})

test_that("x must have a length less than 366", {
  expect_error(gsdd_cf(rep(5,367)))
})

test_that("Gets growth gives messages with truncation.", {
  x <- c(rep(10, 50), rep(0, 255), rep(20, 40))
  expect_message(expect_identical(gsdd_cf(x), NA_real_), "The growing season is truncated at the start of the sequence.")
  expect_message(expect_identical(gsdd_cf(x, ignore_truncation = "start"), NA_real_), "The growing season is truncated at the end of the sequence.")
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

test_that("Gets with two weeks and 3 day window and smaller", {
  x <- c(rep(0, 100), rep(5.1, 7), rep(0, 100))
  expect_equal(gsdd_cf(x), 5.1 * 7)
})

test_that("Gets one week with end day after of 0", {
  x <- c(rep(0, 180), rep(5.1, 7), rep(1, 0))
  expect_equal(gsdd_cf(x, ignore_truncation = "end", msgs = FALSE), 5.1 * 7)
})

test_that("Gets one week with end day after of 1", {
  x <- c(rep(0, 180), rep(5.1, 7), rep(1, 1))
  expect_equal(gsdd_cf(x, ignore_truncation = "end", msgs = FALSE), 5.1 * 7 + 1)
})

test_that("Gets with two weeks and 3 day window and smaller", {
  x <- c(rep(0, 180), rep(5.1, 7))
  expect_equal(gsdd_cf(x, ignore_truncation = "end", msgs = FALSE), 5.1 * 7)
})

test_that("NA if less than 184 values after trimming trailing NAs", {
  x <- c(rep(1,183), rep(NA,100))
  expect_message(expect_identical(gsdd_cf(x),NA_real_), "The length of the longest non-missing sequence in `x` must be at least 184.")
  x <- c(rep(1,184), rep(NA,100))
  expect_identical(gsdd_cf(x),0)
})

test_that("extracts longest non-missing sequence (not just trim tails)", {
  x <- c(NA,1,NA,rep(1,183),NA,1,NA)
  expect_identical(gsdd_cf(x, msgs = FALSE),NA_real_) 
  x <- c(NA,1,NA,rep(1,184),NA,1,NA)
  expect_identical(gsdd_cf(x),0) 
})
