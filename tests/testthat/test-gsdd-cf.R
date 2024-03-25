test_that("output is a numeric value", {
  lifecycle::expect_deprecated(output <- gsdd_cf(gsdd::temperature_data$temperature))
  expect_equal(output, 3898.80557580767)
})
