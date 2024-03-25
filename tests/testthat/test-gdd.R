test_that("gdd works", {
  gdd <- gdd(gsdd::temperature_data)
  expect_snapshot({
    gdd
  })
})

test_that("gsdd works t2", {
  data <- gsdd::temperature_data
  data$temperature <- data$temperature2
  gdd <- gdd(data)
  expect_snapshot({
    gdd
  })
})
