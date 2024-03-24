test_that("gdd_cf_data works", {
  data <- simulated_data
  data$temperature <- data$synthetic
  gdd <- gdd_cf_data(data)
  expect_snapshot({
    gdd
  })
})

test_that("gdd_cf_data works", {
  data <- simulated_data
  data$temperature <- data$synthetic
  gdd <- gdd_cf_data(data, window_width = 13)
  expect_snapshot({
    gdd
  })
})

test_that("gdd_cf_data works shortened", {
  data <- simulated_data
  data$temperature <- data$synthetic
  gdd <- gdd_cf_data(data, end_date = as.Date("1972-09-29"))
  expect_snapshot({
    gdd
  })
})

test_that("gdd_cf_data works very shortened", {
  data <- simulated_data
  data$temperature <- data$synthetic
  gdd <- gdd_cf_data(data, end_date = as.Date("1972-04-01"))
  expect_snapshot({
    gdd
  })
})

test_that("gdd_cf_data NA if stops before", {
  data <- simulated_data
  data$temperature <- data$synthetic
  data <- data[data$date < as.Date("2019-09-30"),]
  expect_message(gdd <- gdd_cf_data(data))
  expect_snapshot({
    gdd
  })
})

test_that("gdd_cf_data NA if missing", {
  data <- simulated_data
  data$temperature <- data$synthetic
  data$temperature[data$date == as.Date("2019-09-30")] <- NA_real_
  gdd <- gdd_cf_data(data)
  expect_snapshot({
    gdd
  })
})
