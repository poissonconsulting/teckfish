test_that("column names of simulated_data are correct", {
  expect_identical(c("date", "value"), colnames(simulated_data))
  })
