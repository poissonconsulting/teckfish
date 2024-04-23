test_that("gss works", {
  gss <- gss(gsdd::temperature_data)
  expect_snapshot({
    gss
  })
})

test_that("gss works t2", {
  data <- gsdd::temperature_data
  data$temperature <- data$temperature2
  gss <- gss(data)
  expect_snapshot({
    gss
  })
})

test_that("gss truncation off", {
  data <- gsdd::temperature_data
  data$temperature <- data$temperature2
  data$temperature[data$date >= as.Date("2019-08-28")] <- NA_real_
  gss <- gss(data, ignore_truncation = FALSE, msgs = FALSE)
  expect_snapshot({
    gss
  })
})

test_that("gss truncation on", {
  data <- gsdd::temperature_data
  data$temperature <- data$temperature2
  data$temperature[data$date >= as.Date("2019-08-28")] <- NA_real_
  gss <- gss(data)
  expect_snapshot({
    gss
  })
})
