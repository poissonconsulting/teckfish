test_that("gsdd works", {
  gsdd <- gsdd(gsdd::temperature_data)
  expect_snapshot({
    gsdd
  })
})

test_that("gsdd works t2", {
  data <- gsdd::temperature_data
  data$temperature <- data$temperature2
  gsdd <- gsdd(data)
  expect_snapshot({
    gsdd
  })
})
