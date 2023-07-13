test_that("output is a numeric value", {
  set.seed(13)
  day <- 1:365
  x <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
  output <- calculate_gsdd(x, rollmean_units = 7, start_temp = 5, end_temp = 4, n_consecutive = 5)
  expect_equal(is.numeric(output), TRUE)
})
