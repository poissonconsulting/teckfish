test_that("date_atus works", {
  date_atus <- date_atus(gsdd::temperature_data)
  expect_snapshot({
    date_atus
  })
})

test_that("date_atus change atu and date", {
  data <- gsdd::temperature_data
  date_atus <- date_atus(data, start_date = as.Date("2019-01-01"), atu = 300)
  expect_snapshot({
    date_atus
  })
})
