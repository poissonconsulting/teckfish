test_that("output is a numeric value", {
  x <- simulated_data$synthetic
  output <- gsdd_cf(x)
  expect_equal(output, 3898.80557580767)
})

test_that("vector must be longer than window_width", {
  x <- simulated_data$synthetic[180]
  expect_chk_error(gsdd_cf(x, window_width = 181))
})

test_that("window_width must be odd", {
  x <- simulated_data$synthetic
  expect_chk_error(gsdd_cf(x, window_width = 6), "`window_width` must be odd.")
})

test_that("gsdd_cf returns NA when missing summer", {
  x <- simulated_data$synthetic
  x[85:320] <- NA_real_
  expect_identical(gsdd_cf(x), NA_real_)
})

test_that("vector must not contain NA values", {
  x <- simulated_data$synthetic
  random_indices <- sample(seq_along(x), 40)
  x[random_indices] <- NA
  expect_identical(gsdd_cf(x), NA_real_)
})

test_that("gsdd_cf can trims missing values", {
  x <- simulated_data$synthetic
  x[c(1,length(x))] <- NA_real_
  expect_identical(gsdd_cf(x, na_trim = FALSE), NA_real_)
  x <- simulated_data$synthetic
  x[c(1,length(x))] <- NA_real_
  expect_equal(gsdd_cf(x, na_trim = TRUE), 3898.80557580767)
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

test_that("x must have a length between 55 and 366", {
  x <- c(rep(0, 1), rep(10, 20), rep(0, 1))
  expect_error(
    gsdd_cf(x, window_width = 3, start_temp = 9, end_temp = 9),
    "`x` must have a length between 180 and 366 not 22\\."
  )
})

test_that("Gets growth period with biggest GSDD even though shorter period.", {
  x <- c(rep(0, 100), rep(10, 50), rep(0, 50), rep(20, 40), rep(0, 115))
  gsdd <- gsdd_cf(x, window_width = 3, start_temp = 9, end_temp = 9, pick = "biggest")
  expect_equal(gsdd, 800)
})

test_that("Gets growth period with biggest GSDD even though shorter period.", {
  x <- c(rep(0, 100), rep(10, 50), rep(0, 50), rep(20, 40), rep(0, 115))
  gsdd <- gsdd_cf(x, window_width = 3, start_temp = 9, end_temp = 9, pick = "last")
  expect_equal(gsdd, 800)
})

test_that("Gets growth period with biggest GSDD even though shorter period.", {
  x <- c(rep(0, 100), rep(10, 50), rep(0, 50), rep(20, 40), rep(0, 115))
  gsdd <- gsdd_cf(x, window_width = 3, start_temp = 9, end_temp = 9, pick = "first")
  expect_equal(gsdd, 500)
})

test_that("Gets growth period with smallest GSDD.", {
  x <- c(rep(0, 100), rep(10, 50), rep(0, 50), rep(20, 40), rep(0, 115))
  gsdd <- gsdd_cf(x, window_width = 3, start_temp = 9, end_temp = 9, pick = "smallest")
  expect_equal(gsdd, 500)
})

test_that("Gets growth period with all GSDD.", {
  x <- c(rep(0, 100), rep(10, 50), rep(0, 50), rep(20, 40), rep(0, 115))
  gsdd <- gsdd_cf(x, window_width = 3, start_temp = 9, end_temp = 9)
  expect_equal(gsdd, 1300)
})

test_that("Gets growth period with higher GSDD even though shorter period.", {
  x <- c(rep(10, 50), rep(0, 255), rep(20, 40))
  gsdd <- gsdd_cf(x, window_width = 3, start_temp = 9, end_temp = 9, quiet = TRUE)
  expect_equal(gsdd, NA_real_)
  gsdd <- gsdd_cf(x,
    window_width = 3, start_temp = 9, end_temp = 9, quiet = TRUE,
    ignore_truncation = TRUE, pick = "biggest"
  )
  expect_equal(gsdd, 800)
})

test_that("Gets growth period longest period.", {
  x <- c(rep(10, 50), rep(0, 255), rep(20, 40))
  gsdd <- gsdd_cf(x, window_width = 3, start_temp = 9, end_temp = 9, quiet = TRUE)
  expect_equal(gsdd, NA_real_)
  gsdd <- gsdd_cf(x,
                  window_width = 3, start_temp = 9, end_temp = 9, quiet = TRUE,
                  ignore_truncation = TRUE, pick = "longest"
  )
  expect_equal(gsdd, 500)
})

test_that("Gets growth period all gsdd.", {
  x <- c(rep(10, 50), rep(0, 255), rep(20, 40))
  gsdd <- gsdd_cf(x, window_width = 3, start_temp = 9, end_temp = 9, quiet = TRUE)
  expect_equal(gsdd, NA_real_)
  gsdd <- gsdd_cf(x,
                  window_width = 3, start_temp = 9, end_temp = 9, quiet = TRUE,
                  ignore_truncation = TRUE, pick = "all"
  )
  expect_equal(gsdd, 1300)
})

test_that("Gets growth period shortest", {
  x <- c(rep(10, 50), rep(0, 255), rep(20, 40))
  gsdd <- gsdd_cf(x, window_width = 3, start_temp = 9, end_temp = 9, quiet = TRUE)
  expect_equal(gsdd, NA_real_)
  gsdd <- gsdd_cf(x,
                  window_width = 3, start_temp = 9, end_temp = 9, quiet = TRUE,
                  ignore_truncation = TRUE, pick = "shortest"
  )
  expect_equal(gsdd, 800)
})

test_that("Gets growth period longest", {
  x <- c(rep(10, 50), rep(0, 255), rep(20, 40))
  gsdd <- gsdd_cf(x, window_width = 3, start_temp = 9, end_temp = 9, quiet = TRUE)
  expect_equal(gsdd, NA_real_)
  gsdd <- gsdd_cf(x,
                  window_width = 3, start_temp = 9, end_temp = 9, quiet = TRUE,
                  ignore_truncation = TRUE, pick = "longest"
  )
  expect_equal(gsdd, 500)
})

test_that("Gets growth gives warnings with truncation.", {
  x <- c(rep(10, 50), rep(0, 255), rep(20, 40))
  expect_warning(expect_identical(gsdd_cf(x), NA_real_), "Growing season truncated\\.")
  expect_warning(expect_identical(gsdd_cf(x, ignore_truncation = "start"), NA_real_), "Growing season truncated\\.")
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
  x <- c(rep(0, 180), rep(5.1, 7), rep(1, 0))
  expect_equal(gsdd_cf(x, ignore_truncation = "end", quiet = TRUE), 5.1 * 7)
})

test_that("Gets one week with end day after of 1", {
  x <- c(rep(0, 180), rep(5.1, 7), rep(1, 1))
  expect_equal(gsdd_cf(x, ignore_truncation = "end", quiet = TRUE), 5.1 * 7 + 1)
})

test_that("Gets with two weeks and 3 day window and smaller", {
  x <- c(rep(0, 180), rep(5.1, 7))
  expect_equal(gsdd_cf(x, ignore_truncation = "end", quiet = TRUE), 5.1 * 7)
})

test_that("Gets triangle", {
  x <- c(seq(-5, 9), 10, seq(9, -5), rep(-1, 149))
  ma <- zoo::rollmean(x, k = 7, align = "center", na.pad = TRUE)
  
  testthat::expect_snapshot({
    tibble::tibble(index = 1:length(x), x = x, ma = ma)
  })
  
  expect_equal(gsdd_cf(x), sum(x[9:26]))
})

test_that("Gets asymmetric triangle", {
  x <- c(seq(-5, 9), 10, seq(9.5, -5.5), rep(-6, 148))
  ma <- zoo::rollmean(x, k = 7, align = "center", na.pad = TRUE)
  
  testthat::expect_snapshot({
    tibble::tibble(index = 1:length(x), x = x, ma = ma)
  })
  

  expect_equal(gsdd_cf(x), sum(x[9:26]))
})

test_that("2 asymetric triangles, first one longer but lower, second should be chosen.", {
  x <- c(
    rep(0, 3),
    seq(0, 10, by = 0.5),
    seq(10, 2, by = -0.5),
    seq(2, 25, by = 2),
    seq(21, 0, by = -5),
    rep(0, 122)
  )
  ma <- zoo::rollmean(x, k = 7, align = "center", na.pad = TRUE)

  testthat::expect_snapshot({
    tibble::tibble(index = 1:length(x), x = x, ma = ma)
  })
  
  expect_equal(gsdd_cf(x, pick = "biggest"), sum(x[41:61]))
})

test_that("2 asymetric triangles, first one longer but lower, second should be chosen unless longest.", {
  x <- c(
    rep(0, 3),
    seq(0, 10, by = 0.5),
    seq(10, 2, by = -0.5),
    seq(2, 25, by = 2),
    seq(21, 0, by = -5),
    rep(0, 122)
  )
  ma <- zoo::rollmean(x, k = 7, align = "center", na.pad = TRUE)
  
  testthat::expect_snapshot({
    tibble::tibble(index = 1:length(x), x = x, ma = ma)
  })
  
  expect_equal(gsdd_cf(x, pick = "longest"), 193)
})

test_that("2 asymetric triangles, second one longer but lower, first one should be chosen.", {
  x <- c(
    rep(0, 3),
    seq(2, 25, by = 2),
    seq(21, 0, by = -5),
    seq(0, 10, by = 0.5),
    seq(10, 0, by = -0.5),
    rep(0, 118)
  )
  ma <- zoo::rollmean(x, k = 7, align = "center", na.pad = TRUE)
  testthat::expect_snapshot({
    tibble::tibble(index = 1:length(x), x = x, ma = ma)
  })
  
  expect_equal(gsdd_cf(x, pick = "biggest"), sum(x[3:24]))
})

test_that("Right truncated triangle", {
  x <- c(
    rep(0, 159),
    seq(2, 25, by = 2),
    seq(21, 5, by = -2)
  )
  ma <- zoo::rollmean(x, k = 7, align = "center", na.pad = TRUE)
  data <- tibble::tibble(index = 1:length(x), x = x, ma = ma)
  testthat::expect_snapshot({
    tibble::tibble(index = 1:length(x), x = x, ma = ma)
  })
  
  expect_equal(gsdd_cf(x, quiet = TRUE), NA_real_)
  expect_equal(gsdd_cf(x, ignore_truncation = "end", quiet = TRUE), sum(x[15:length(x)]))
})

test_that("Left truncated triangle", {
  x <- c(
    seq(6, 25, by = 2),
    seq(25, 0, by = -2),
    rep(0,157)
  )
  ma <- zoo::rollmean(x, k = 7, align = "center", na.pad = TRUE)
  
  testthat::expect_snapshot({
    tibble::tibble(index = 1:length(x), x = x, ma = ma)
  })
  
  expect_equal(gsdd_cf(x, quiet = TRUE), NA_real_)
  expect_equal(gsdd_cf(x, ignore_truncation = "start", quiet = TRUE), sum(x[0:25]))
})
