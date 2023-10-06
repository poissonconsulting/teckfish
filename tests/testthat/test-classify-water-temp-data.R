test_that("errors when temperature_date_time column is missing", {
  data <- data.frame(
    temperature_date = as.POSIXct(c("2021-05-07 08:00:00")),
    water_temperature = c(4.124)
  )

  expect_error(
    classify_water_temp_data(data)
  )
})

test_that("errors when temperature_date_time column type is not a date time", {
  data <- data.frame(
    temperature_date_time = as.Date(c("2021-05-07")),
    water_temperature = c(4.124)
  )

  expect_error(
    classify_water_temp_data(data)
  )
})

test_that("errors when water_temperature column is missing", {
  data <- data.frame(
    temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
    water_temp = c(4.124)
  )

  expect_error(
    classify_water_temp_data(data)
  )
})

test_that("errors when water_temperature column type is not numeric", {
  data <- data.frame(
    temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
    water_temperature = c("cold")
  )

  expect_error(
    classify_water_temp_data(data)
  )
})

test_that("additional columns are retained in the output", {
  data <- data.frame(
    temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
    water_temperature = c(4.124),
    comments = c("")
  )

  classified_data <- classify_water_temp_data(data)

  expect_equal(
    classified_data,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(4.124),
      comments = c(""),
      status_id = c(1L)
    )
  )
})

test_that("errors when reserved columns are already present in the data", {
  ### TODO: update to all the internal/temp column names you use
  data <- data.frame(
    temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
    water_temperature = c(4.2),
    status_id = c(1L)
  )

  expect_error(
    classify_water_temp_data(data)
  )
})

test_that("dataset with no rows is returned with no rows", {
  data <- data.frame(
    temperature_date_time = as.POSIXct(c()),
    water_temperature = numeric()
  )

  classified_data <- classify_water_temp_data(data)

  expect_equal(
    classified_data,
    data.frame(
      temperature_date_time = as.POSIXct(c()),
      water_temperature = numeric(),
      status_id = integer()
    )
  )
})

test_that("errors when no data is passed", {
  expect_error(
    classify_water_temp_data()
  )
})

### TODO Add tests for each of the input parameters to the function to test the type is correct

test_that("questionable temperatures are classified correctly with default values", {
  data <- data.frame(
    temperature_date_time = as.POSIXct(c(
      "2021-05-07 08:00:00", "2021-05-07 09:00:00", "2021-05-07 10:00:00",
      "2021-05-07 11:00:00", "2021-05-07 12:00:00", "2021-05-07 13:00:00"
    )),
    water_temperature = c(4.124, -0.1, 0, 30, 31, 6.712)
  )

  classified_data <- classify_water_temp_data(data)

  expect_equal(
    classified_data,
    data.frame(
      temperature_date_time = as.POSIXct(c(
        "2021-05-07 08:00:00", "2021-05-07 09:00:00", "2021-05-07 10:00:00",
        "2021-05-07 11:00:00", "2021-05-07 12:00:00", "2021-05-07 13:00:00"
      )),
      water_temperature = c(4.124, -0.1, 0, 30, 31, 6.712),
      status_id = c(1L, 2L, 1L, 1L, 2L, 1L)
    )
  )
})

test_that("erroneous temperatures are classified correctly with default values", {
  data <- data.frame(
    temperature_date_time = as.POSIXct(c(
      "2021-05-07 08:00:00", "2021-05-07 09:00:00", "2021-05-07 10:00:00",
      "2021-05-07 11:00:00", "2021-05-07 12:00:00", "2021-05-07 13:00:00"
    )),
    water_temperature = c(4.124, -0.6, -1, 41, 60, 6.712)
  )

  classified_data <- classify_water_temp_data(data)

  expect_equal(
    classified_data,
    data.frame(
      temperature_date_time = as.POSIXct(c(
        "2021-05-07 08:00:00", "2021-05-07 09:00:00", "2021-05-07 10:00:00",
        "2021-05-07 11:00:00", "2021-05-07 12:00:00", "2021-05-07 13:00:00"
      )),
      water_temperature = c(4.124, -0.6, -1, 41, 60, 6.712),
      status_id = c(1L, 3L, 3L, 3L, 3L, 1L)
    )
  )
})

test_that("erroneous and questionable temperature lower bounds are classified correctly with default values", {
  data <- data.frame(
    temperature_date_time = as.POSIXct(c(
      "2021-05-07 08:00:00", "2021-05-07 09:00:00", "2021-05-07 10:00:00",
      "2021-05-07 11:00:00", "2021-05-07 12:00:00", "2021-05-07 13:00:00"
    )),
    water_temperature = c(0, -0.3, -0.5, -.6, -1, -10)
  )

  classified_data <- classify_water_temp_data(data)

  expect_equal(
    classified_data,
    data.frame(
      temperature_date_time = as.POSIXct(c(
        "2021-05-07 08:00:00", "2021-05-07 09:00:00", "2021-05-07 10:00:00",
        "2021-05-07 11:00:00", "2021-05-07 12:00:00", "2021-05-07 13:00:00"
      )),
      water_temperature = c(0, -0.3, -0.5, -.6, -1, -10),
      status_id = c(1L, 2L, 2L, 3L, 3L, 3L)
    )
  )
})

test_that("erroneous and questionable temperature upper bounds are classified correctly with default values", {
  data <- data.frame(
    temperature_date_time = as.POSIXct(c(
      "2021-05-07 08:00:00", "2021-05-07 09:00:00", "2021-05-07 10:00:00",
      "2021-05-07 11:00:00", "2021-05-07 12:00:00", "2021-05-07 13:00:00"
    )),
    water_temperature = c(25, 30, 35, 40, 45, 70)
  )

  classified_data <- classify_water_temp_data(data)

  expect_equal(
    classified_data,
    data.frame(
      temperature_date_time = as.POSIXct(c(
        "2021-05-07 08:00:00", "2021-05-07 09:00:00", "2021-05-07 10:00:00",
        "2021-05-07 11:00:00", "2021-05-07 12:00:00", "2021-05-07 13:00:00"
      )),
      water_temperature = c(25, 30, 35, 40, 45, 70),
      status_id = c(1L, 1L, 2L, 2L, 3L, 3L)
    )
  )
})

test_that("erroneous and questionable temperatures are classified correctly with default values", {
  data <- data.frame(
    temperature_date_time = as.POSIXct(c(
      "2021-05-07 08:00:00", "2021-05-07 09:00:00", "2021-05-07 10:00:00",
      "2021-05-07 11:00:00", "2021-05-07 12:00:00", "2021-05-07 13:00:00"
    )),
    water_temperature = c(4.7, -0.4, -1, 22, 32, 45)
  )

  classified_data <- classify_water_temp_data(data)

  expect_equal(
    classified_data,
    data.frame(
      temperature_date_time = as.POSIXct(c(
        "2021-05-07 08:00:00", "2021-05-07 09:00:00", "2021-05-07 10:00:00",
        "2021-05-07 11:00:00", "2021-05-07 12:00:00", "2021-05-07 13:00:00"
      )),
      water_temperature = c(4.7, -0.4, -1, 22, 32, 45),
      status_id = c(1L, 2L, 3L, 1L, 2L, 3L)
    )
  )
})

test_that("erroneous and questionable temperatures are classified correctly not using default values", {
  data <- data.frame(
    temperature_date_time = as.POSIXct(c(
      "2021-05-07 08:00:00", "2021-05-07 09:00:00", "2021-05-07 10:00:00",
      "2021-05-07 11:00:00", "2021-05-07 12:00:00", "2021-05-07 13:00:00"
    )),
    water_temperature = c(4.7, -0.9, -2, 23, 15, 45)
  )

  classified_data <- classify_water_temp_data(
    data,
    questionable_min = -0.8,
    questionable_max = 20,
    erroneous_min = -1,
    erroneous_max = 25
  )

  expect_equal(
    classified_data,
    data.frame(
      temperature_date_time = as.POSIXct(c(
        "2021-05-07 08:00:00", "2021-05-07 09:00:00", "2021-05-07 10:00:00",
        "2021-05-07 11:00:00", "2021-05-07 12:00:00", "2021-05-07 13:00:00"
      )),
      water_temperature = c(4.7, -0.9, -2, 23, 15, 45),
      status_id = c(1L, 2L, 3L, 2L, 1L, 3L)
    )
  )
})
