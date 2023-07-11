test_that("output is a numeric value", {
  day <- 1:365
  temperature <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)
  gsdd <- calculate_gsdd(x = temperature, k = 7, starttemp = 5, endtemp =4)
  expect_true(is.numeric(gsdd))
})
