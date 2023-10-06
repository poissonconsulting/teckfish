test_that("column names of simulated_data are correct", {
  expect_identical(c("date", "synthetic"), colnames(simulated_data))
  })
