test_that("gdd_cf_data works", {
  data <- simulated_data
  data$temperature <- data$synthetic
  gdd <- gdd_cf_data(data)
  expect_snapshot({
    gdd
  })
})

test_that("gdd_cf_data curtails if curtailed", {
  data <- simulated_data
  data$temperature <- data$synthetic
  data$temperature[data$date == as.Date("2019-09-30")] <- NA_real_
  gdd <- gdd_cf_data(data)
  expect_snapshot({
    gdd
  })
})
