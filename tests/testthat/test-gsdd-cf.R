test_that("output is a numeric value", {
  x <- simulated_data$synthetic
  lifecycle::expect_deprecated(output <- gsdd_cf(x))
  expect_equal(output, 3898.80557580767)
})
