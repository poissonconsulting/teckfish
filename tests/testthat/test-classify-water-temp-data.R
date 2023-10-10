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
    tibble::tibble(
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
    tibble::tibble(
      temperature_date_time = as.POSIXct(c()),
      water_temperature = numeric(),
      status_id = integer()
    )
  )
})

test_that("errors when no data is passed", {
  expect_error(
    classify_water_temp_data(),
    regexp = 'argument "data" is missing, with no default'
  )
})

### TODO Add tests for each of the input parameters to the function to test the type is correct

test_that("questionable temperatures are classified correctly with default values", {
  output_1 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(0)
    )
  )

  output_2 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(-0.1)
    )
  )

  output_3 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(30.01)
    )
  )

  output_4 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(30)
    )
  )

  output_5 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(39.9998124)
    )
  )

  output_6 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(34.24)
    )
  )

  expect_equal(
    output_1,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(0),
      status_id = c(1L)
    )
  )

  expect_equal(
    output_2,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(-0.1),
      status_id = c(2L)
    )
  )

  expect_equal(
    output_3,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(30.01),
      status_id = c(2L)
    )
  )

  expect_equal(
    output_4,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(30),
      status_id = c(1L)
    )
  )

  expect_equal(
    output_5,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(39.9998124),
      status_id = c(2L)
    )
  )

  expect_equal(
    output_6,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(34.24),
      status_id = c(2L)
    )
  )
})

test_that("erroneous temperatures are classified correctly with default values", {
  output_1 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.6)
    )
  )

  output_2 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-1)
    )
  )

  output_3 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(41)
    )
  )

  output_4 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(60)
    )
  )

  output_5 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(6.712)
    )
  )

  expect_equal(
    output_1,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.6),
      status_id = c(3L)
    )
  )

  expect_equal(
    output_2,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-1),
      status_id = c(3L)
    )
  )

  expect_equal(
    output_3,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(41),
      status_id = c(3L)
    )
  )

  expect_equal(
    output_4,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(60),
      status_id = c(3L)
    )
  )

  expect_equal(
    output_5,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(6.712),
      status_id = c(1L)
    )
  )
})

test_that("erroneous and questionable temperature lower bounds are classified correctly with default values", {
  output_1 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(0)
    )
  )

  output_2 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.3)
    )
  )

  output_3 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.5)
    )
  )

  output_4 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.6)
    )
  )

  output_5 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-1)
    )
  )

  output_6 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-10)
    )
  )

  expect_equal(
    output_1,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(0),
      status_id = c(1L)
    )
  )

  expect_equal(
    output_2,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.3),
      status_id = c(2L)
    )
  )

  expect_equal(
    output_3,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.5),
      status_id = c(2L)
    )
  )

  expect_equal(
    output_4,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.6),
      status_id = c(3L)
    )
  )

  expect_equal(
    output_5,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-1),
      status_id = c(3L)
    )
  )

  expect_equal(
    output_6,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-10),
      status_id = c(3L)
    )
  )
})

test_that("erroneous and questionable temperature upper bounds are classified correctly with default values", {
  output_1 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(25)
    )
  )

  output_2 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(30)
    )
  )

  output_3 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(35)
    )
  )

  output_4 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(40)
    )
  )

  output_5 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(45)
    )
  )

  output_6 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(60)
    )
  )

  expect_equal(
    output_1,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(25),
      status_id = c(1L)
    )
  )

  expect_equal(
    output_2,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(30),
      status_id = c(1L)
    )
  )

  expect_equal(
    output_3,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(35),
      status_id = c(2L)
    )
  )

  expect_equal(
    output_4,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(40),
      status_id = c(2L)
    )
  )

  expect_equal(
    output_5,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(45),
      status_id = c(3L)
    )
  )

  expect_equal(
    output_6,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(60),
      status_id = c(3L)
    )
  )
})

test_that("erroneous and questionable temperatures are classified correctly not using default values", {
  output_1 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(4.7)
    ),
    questionable_min = -0.8,
    questionable_max = 20,
    erroneous_min = -1,
    erroneous_max = 25
  )

  output_2 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.9)
    ),
    questionable_min = -0.8,
    questionable_max = 20,
    erroneous_min = -1,
    erroneous_max = 25
  )

  output_3 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-2)
    ),
    questionable_min = -0.8,
    questionable_max = 20,
    erroneous_min = -1,
    erroneous_max = 25
  )

  output_4 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(23)
    ),
    questionable_min = -0.8,
    questionable_max = 20,
    erroneous_min = -1,
    erroneous_max = 25
  )

  output_5 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(15)
    ),
    questionable_min = -0.8,
    questionable_max = 20,
    erroneous_min = -1,
    erroneous_max = 25
  )

  output_6 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(45)
    ),
    questionable_min = -0.8,
    questionable_max = 20,
    erroneous_min = -1,
    erroneous_max = 25
  )

  expect_equal(
    output_1,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(4.7),
      status_id = c(1L)
    )
  )

  expect_equal(
    output_2,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.9),
      status_id = c(2L)
    )
  )

  expect_equal(
    output_3,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-2),
      status_id = c(3L)
    )
  )

  expect_equal(
    output_4,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(23),
      status_id = c(2L)
    )
  )

  expect_equal(
    output_5,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(15),
      status_id = c(1L)
    )
  )

  expect_equal(
    output_6,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(45),
      status_id = c(3L)
    )
  )
})

test_that("small rates of changes are classified as resonable", {
  data <-
    tibble::tribble(
      ~temperature_date_time, ~water_temperature,
      "2021-05-07 08:00:00",  2.877,
      "2021-05-07 08:15:00",  3.012,
      "2021-05-07 08:30:00",  3.147,
      "2021-05-07 08:45:00",  3.124,
      "2021-05-07 09:00:00",  3.268,
      "2021-05-07 09:15:00",  3.115,
      "2021-05-07 09:30:00",  3.048,
      "2021-05-07 09:45:00",  2.987
    ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))

  classified_data <- classify_water_temp_data(data)

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 08:00:00",  2.877,              1L,
      "2021-05-07 08:15:00",  3.012,              1L,
      "2021-05-07 08:30:00",  3.147,              1L,
      "2021-05-07 08:45:00",  3.124,              1L,
      "2021-05-07 09:00:00",  3.268,              1L,
      "2021-05-07 09:15:00",  3.115,              1L,
      "2021-05-07 09:30:00",  3.048,              1L,
      "2021-05-07 09:45:00",  2.987,              1L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("erroneous values are classifed and spread, erroneous values start at end of series", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 07:30:00",  2.817,
    "2021-05-07 07:45:00",  2.867,
    "2021-05-07 08:00:00",  2.877,
    "2021-05-07 08:15:00",  3.012,
    "2021-05-07 08:30:00",  3.147,
    "2021-05-07 08:45:00",  3.124,
    "2021-05-07 09:00:00",  3.268,
    "2021-05-07 09:15:00",  4.789, 
    "2021-05-07 09:30:00",  6.257,
    "2021-05-07 09:45:00",  8.657
  ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))

  classified_data <- classify_water_temp_data(data)

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 07:30:00",  2.817,              1L,
      "2021-05-07 07:45:00",  2.867,              1L,
      "2021-05-07 08:00:00",  2.877,              1L,
      "2021-05-07 08:15:00",  3.012,              3L,
      "2021-05-07 08:30:00",  3.147,              3L,
      "2021-05-07 08:45:00",  3.124,              3L,
      "2021-05-07 09:00:00",  3.268,              3L,
      "2021-05-07 09:15:00",  4.789,              3L,
      "2021-05-07 09:30:00",  6.257,              3L,
      "2021-05-07 09:45:00",  8.657,              3L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("erroneous values are classifed and spread, erroneous values start at start of series", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 08:00:00",  4.789,
    "2021-05-07 08:15:00",  6.257,
    "2021-05-07 08:30:00",  3.147,
    "2021-05-07 08:45:00",  3.124,
    "2021-05-07 09:00:00",  3.068,
    "2021-05-07 09:15:00",  2.877,
    "2021-05-07 09:30:00",  2.987,
    "2021-05-07 09:45:00",  3.012,
    "2021-05-07 10:15:00",  3.112,
    "2021-05-07 10:35:00",  3.042
  ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))

  classified_data <- classify_water_temp_data(data)

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 08:00:00",  4.789,              3L,
      "2021-05-07 08:15:00",  6.257,              3L,
      "2021-05-07 08:30:00",  3.147,              3L,
      "2021-05-07 08:45:00",  3.124,              3L,
      "2021-05-07 09:00:00",  3.068,              3L,
      "2021-05-07 09:15:00",  2.877,              3L,
      "2021-05-07 09:30:00",  2.987,              3L,
      "2021-05-07 09:45:00",  3.012,              1L,
      "2021-05-07 10:15:00",  3.112,              1L,
      "2021-05-07 10:35:00",  3.042,              1L 
      
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("erroneous values are classifed and spread, erroneous values start at middle of series", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 07:30:00",  3.589,
    "2021-05-07 07:45:00",  3.489,
    "2021-05-07 08:00:00",  3.589,
    "2021-05-07 08:15:00",  3.324,
    "2021-05-07 08:30:00",  3.147,
    "2021-05-07 08:45:00",  10.124,
    "2021-05-07 09:00:00",  3.068,
    "2021-05-07 09:15:00",  2.877,
    "2021-05-07 09:30:00",  2.987,
    "2021-05-07 09:45:00",  3.012,
    "2021-05-07 10:00:00",  3.122,
    "2021-05-07 10:15:00",  3.100,
    "2021-05-07 10:30:00",  3.120
  ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))

  classified_data <- classify_water_temp_data(data)

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 07:30:00",  3.589,              1L,
      "2021-05-07 07:45:00",  3.489,              1L,
      "2021-05-07 08:00:00",  3.589,              3L,
      "2021-05-07 08:15:00",  3.324,              3L,
      "2021-05-07 08:30:00",  3.147,              3L,
      "2021-05-07 08:45:00",  10.124,             3L,
      "2021-05-07 09:00:00",  3.068,              3L,
      "2021-05-07 09:15:00",  2.877,              3L,
      "2021-05-07 09:30:00",  2.987,              3L,
      "2021-05-07 09:45:00",  3.012,              3L,
      "2021-05-07 10:00:00",  3.122,              3L,
      "2021-05-07 10:15:00",  3.100,              1L,
      "2021-05-07 10:30:00",  3.120,              1L
      
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("questionable values are classifed and spread, questionable values start at end of series", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 07:30:00",  2.817,
    "2021-05-07 07:45:00",  2.867,
    "2021-05-07 08:00:00",  2.877,
    "2021-05-07 08:15:00",  3.012,
    "2021-05-07 08:30:00",  3.147,
    "2021-05-07 08:45:00",  3.124,
    "2021-05-07 09:00:00",  3.268,
    "2021-05-07 09:15:00",  3.989,
    "2021-05-07 09:30:00",  4.557,
    "2021-05-07 09:45:00",  5.657
  ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))

  classified_data <- classify_water_temp_data(data)

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 07:30:00",  2.817,              1L,
      "2021-05-07 07:45:00",  2.867,              1L,
      "2021-05-07 08:00:00",  2.877,              1L,
      "2021-05-07 08:15:00",  3.012,              2L,
      "2021-05-07 08:30:00",  3.147,              2L,
      "2021-05-07 08:45:00",  3.124,              2L,
      "2021-05-07 09:00:00",  3.268,              2L,
      "2021-05-07 09:15:00",  3.989,              2L,
      "2021-05-07 09:30:00",  4.557,              2L,
      "2021-05-07 09:45:00",  5.657,              2L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("questionable values are classifed and spread, questionable values start at start of series", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 08:00:00",  4.189,
    "2021-05-07 08:15:00",  3.657,
    "2021-05-07 08:30:00",  3.147,
    "2021-05-07 08:45:00",  3.124,
    "2021-05-07 09:00:00",  3.068,
    "2021-05-07 09:15:00",  2.877,
    "2021-05-07 09:30:00",  2.987,
    "2021-05-07 09:45:00",  3.012,
    "2021-05-07 10:00:00",  3.122,
    "2021-05-07 10:15:00",  3.100,
    "2021-05-07 10:30:00",  3.120,
  ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))

  classified_data <- classify_water_temp_data(data)

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 08:00:00",  4.189,              2L,
      "2021-05-07 08:15:00",  3.657,              2L,
      "2021-05-07 08:30:00",  3.147,              2L,
      "2021-05-07 08:45:00",  3.124,              2L,
      "2021-05-07 09:00:00",  3.068,              2L,
      "2021-05-07 09:15:00",  2.877,              2L,
      "2021-05-07 09:30:00",  2.987,              2L,
      "2021-05-07 09:45:00",  3.012,              1L,
      "2021-05-07 10:00:00",  3.122,              1L,
      "2021-05-07 10:15:00",  3.100,              1L,
      "2021-05-07 10:30:00",  3.120,              1L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("questionable values are classifed and spread, questionable values start at middle of series", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 07:30:00",  3.589,
    "2021-05-07 07:45:00",  3.489,
    "2021-05-07 08:00:00",  3.589,
    "2021-05-07 08:15:00",  3.324,
    "2021-05-07 08:30:00",  3.147,
    "2021-05-07 08:45:00",  3.724,
    "2021-05-07 09:00:00",  3.068,
    "2021-05-07 09:15:00",  2.877,
    "2021-05-07 09:30:00",  2.987,
    "2021-05-07 09:45:00",  3.012,
    "2021-05-07 10:00:00",  3.122,
    "2021-05-07 10:15:00",  3.100,
    "2021-05-07 10:30:00",  3.120
  ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))

  classified_data <- classify_water_temp_data(data)

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 07:30:00",  3.589,              1L,
      "2021-05-07 07:45:00",  3.489,              2L,
      "2021-05-07 08:00:00",  3.589,              2L,
      "2021-05-07 08:15:00",  3.324,              2L,
      "2021-05-07 08:30:00",  3.147,              2L,
      "2021-05-07 08:45:00",  3.724,              2L,
      "2021-05-07 09:00:00",  3.068,              2L,
      "2021-05-07 09:15:00",  2.877,              2L,
      "2021-05-07 09:30:00",  2.987,              2L,
      "2021-05-07 09:45:00",  3.012,              2L,
      "2021-05-07 10:00:00",  3.122,              2L,
      "2021-05-07 10:15:00",  3.100,              1L,
      "2021-05-07 10:30:00",  3.120,              1L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

### HERE

test_that("questionable and erronous rates of change with default values", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 08:00:00",  20.124,
    "2021-05-07 08:15:00",  18.782,
    "2021-05-07 08:30:00",  14.579,
    "2021-05-07 08:45:00",  3.724,
    "2021-05-07 09:00:00",  3.068,
    "2021-05-07 09:15:00",  2.877,
    "2021-05-07 09:30:00",  2.987,
    "2021-05-07 09:45:00",  3.012,
    "2021-05-07 10:00:00",  3.122
  ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))

  classified_data <- classify_water_temp_data(data)

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 08:00:00",  20.124,             3L,
      "2021-05-07 08:15:00",  18.782,             3L,
      "2021-05-07 08:30:00",  14.579,             3L,
      "2021-05-07 08:45:00",  3.724,              3L,
      "2021-05-07 09:00:00",  3.068,              3L,
      "2021-05-07 09:15:00",  2.877,              3L,
      "2021-05-07 09:30:00",  2.987,              3L,
      "2021-05-07 09:45:00",  3.012,              3L,
      "2021-05-07 10:00:00",  3.122,              2L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("questionable and erronous rates of change set with parameter", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 08:00:00",  20.124,
    "2021-05-07 08:15:00",  18.782,
    "2021-05-07 08:30:00",  14.579,
    "2021-05-07 08:45:00",  3.724,
    "2021-05-07 09:00:00",  3.068,
    "2021-05-07 09:15:00",  2.877,
    "2021-05-07 09:30:00",  2.987,
    "2021-05-07 09:45:00",  3.012,
    "2021-05-07 10:00:00",  3.122
  ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))

  classified_data <- classify_water_temp_data(
    data,
    questionable_rate = 5,
    erroneous_rate = 20
  )

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 08:00:00",  20.124,             3L,
      "2021-05-07 08:15:00",  18.782,             3L,
      "2021-05-07 08:30:00",  14.579,             3L,
      "2021-05-07 08:45:00",  3.724,              3L,
      "2021-05-07 09:00:00",  3.068,              3L,
      "2021-05-07 09:15:00",  2.877,              3L,
      "2021-05-07 09:30:00",  2.987,              3L,
      "2021-05-07 09:45:00",  3.012,              3L,
      "2021-05-07 10:00:00",  3.122,              1L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("questionable rate of change set with parameter", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 08:00:00",  20.124,
    "2021-05-07 08:15:00",  18.782,
    "2021-05-07 08:30:00",  14.579,
    "2021-05-07 08:45:00",  3.724,
    "2021-05-07 09:00:00",  3.168,
    "2021-05-07 09:15:00",  2.877,
    "2021-05-07 09:30:00",  2.987,
    "2021-05-07 09:45:00",  3.012,
    "2021-05-07 10:00:00",  3.122
  ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))

  classified_data <- classify_water_temp_data(
    data,
    questionable_rate = 1
  )

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 08:00:00",  20.124,             3L,
      "2021-05-07 08:15:00",  18.782,             3L,
      "2021-05-07 08:30:00",  14.579,             3L,
      "2021-05-07 08:45:00",  3.724,              3L,
      "2021-05-07 09:00:00",  3.168,              3L,
      "2021-05-07 09:15:00",  2.877,              3L,
      "2021-05-07 09:30:00",  2.987,              3L,
      "2021-05-07 09:45:00",  3.012,              3L,
      "2021-05-07 10:00:00",  3.122,              2L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("erronous rate of change set with parameter", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 08:00:00",  20.124,
    "2021-05-07 08:15:00",  18.782,
    "2021-05-07 08:30:00",  14.579,
    "2021-05-07 08:45:00",  3.724,
    "2021-05-07 09:00:00",  3.168,
    "2021-05-07 09:15:00",  2.877,
    "2021-05-07 09:30:00",  2.987,
    "2021-05-07 09:45:00",  3.012,
    "2021-05-07 10:00:00",  3.122
  ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))

  classified_data <- classify_water_temp_data(
    data,
    erroneous_rate = 15
  )

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 08:00:00",  20.124,             3L,
      "2021-05-07 08:15:00",  18.782,             3L,
      "2021-05-07 08:30:00",  14.579,             3L,
      "2021-05-07 08:45:00",  3.724,              3L,
      "2021-05-07 09:00:00",  3.168,              3L,
      "2021-05-07 09:15:00",  2.877,              3L,
      "2021-05-07 09:30:00",  2.987,              3L,
      "2021-05-07 09:45:00",  3.012,              3L,
      "2021-05-07 10:00:00",  3.122,              2L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})




# questionable_min <- 0
# questionable_max <- 30
# erroneous_min <- -0.5
# erroneous_max <- 40
# questionable_rate <- 2
# erroneous_rate <- 5
# questionable_hours <- 1
# erroneous_hours <- 1
#
#
# data <- tibble::tribble(
#   ~temperature_date_time, ~water_temperature,
#   "2021-05-07 08:00:00",  20.124,
#   "2021-05-07 08:15:00",  18.782,
#   "2021-05-07 08:30:00",  14.579,
#   "2021-05-07 08:45:00",  3.724,
#   "2021-05-07 09:00:00",  3.168,
#   "2021-05-07 09:15:00",  2.877,
#   "2021-05-07 09:30:00",  2.987,
#   "2021-05-07 09:45:00",  3.012,
#   "2021-05-07 10:00:00",  3.122,
#   "2021-05-07 10:15:00",  18.782,
#   "2021-05-07 10:30:00",  14.579,
#   "2021-05-07 10:45:00",  3.724,
#   "2021-05-07 11:00:00",  3.168,
#   "2021-05-07 11:15:00",  2.877,
#   "2021-05-07 11:30:00",  2.987,
#   "2021-05-07 11:45:00",  3.012,
#   "2021-05-07 12:00:00",  3.122
# ) |>
#   dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
#
#
# data <-
#   data |>
#   dplyr::arrange(.data$temperature_date_time) |>
#   dplyr::mutate(
#     status_id = 1L,
#
#     # questionable ranges
#     status_id = dplyr::case_when(
#       .data$water_temperature < questionable_min ~ 2L,
#       .data$water_temperature > questionable_max ~ 2L,
#       TRUE ~ .data$status_id
#     ),
#     # erroneous ranges
#     status_id = dplyr::case_when(
#       .data$water_temperature < erroneous_min ~ 3L,
#       .data$water_temperature > erroneous_max ~ 3L,
#       TRUE ~ .data$status_id
#     ),
#     # rate of change
#     lag_temp = dplyr::lag(.data$water_temperature),
#     diff_temp = abs(.data$water_temperature - .data$lag_temp),
#     lag_time = dplyr::lag(.data$temperature_date_time),
#     diff_time = as.numeric(difftime(
#       .data$temperature_date_time,
#       .data$lag_time,
#       units = "hours"
#     )),
#     rate_temp_per_time = abs(.data$diff_temp / .data$diff_time),
#     status_id = dplyr::case_when(
#       # erroneous rate of change
#       .data$rate_temp_per_time > erroneous_rate ~ 3L,
#       # questionable rate of change
#       .data$rate_temp_per_time > questionable_rate ~ 2L,
#       TRUE ~ .data$status_id
#     )
#   ) |>
#   dplyr::select(
#     -"lag_temp", -"diff_temp", -"lag_time", -"diff_time",
#     -"rate_temp_per_time"
#   )
#
# questionable_rows <- which(data$status_id == 2L)
# error_rows <- which(data$status_id == 3L)
#
# data <-
#   data |>
#   dplyr::mutate(
#     id = dplyr::row_number()
#   ) |>
#   dplyr::rowwise() |>
#   dplyr::mutate(
#     # find closest questionable/erroneous value above and below
#     quest_id_above = list(
#       questionable_rows[which(questionable_rows > .data$id)]
#     ),
#     quest_id_below = list(
#       questionable_rows[which(questionable_rows < .data$id)]
#     ),
#
#     error_id_above = list(
#       error_rows[which(error_rows > .data$id)]
#     ),
#     error_id_below = list(
#       error_rows[which(error_rows < .data$id)]
#     )
#
#   ) |>
#   dplyr::ungroup() |>
#   dplyr::mutate(
#     quest_id_above = purrr::map_int(quest_id_above, min2),
#     quest_id_below = purrr::map_int(quest_id_below, max2),
#
#     error_id_above = purrr::map_int(error_id_above, min2),
#     error_id_below = purrr::map_int(error_id_below, max2),
#
#     quest_id_above2 = temperature_date_time[quest_id_above],
#     quest_id_below2 = temperature_date_time[quest_id_below],
#     error_id_above2 = temperature_date_time[error_id_above],
#     error_id_below2 = temperature_date_time[error_id_below],
#
#     quest_id_above3 = as.numeric(
#       difftime(quest_id_above2, temperature_date_time, units = "hours")
#     ),
#     quest_id_below3 = diff_hours(temperature_date_time, quest_id_below2),
#     error_id_above3 = diff_hours(error_id_above2, temperature_date_time),
#     error_id_below3 = diff_hours(temperature_date_time, error_id_below2),
#
#     # anything within an hour of a questionable value is questionable
#     status_id = dplyr::if_else(
#       status_id == 1 & quest_id_above3 <= questionable_hours,
#       2,
#       status_id,
#       status_id
#     ),
#     status_id = dplyr::if_else(
#       status_id == 1 & quest_id_below3 <= questionable_hours,
#       2,
#       status_id,
#       status_id
#     ),
#
#     # anything within an hour of an erroneous value is erroneous
#     status_id = dplyr::if_else(
#       status_id %in% c(1, 2) & error_id_above3 <= erroneous_hours,
#       3,
#       status_id,
#       status_id
#     ),
#     status_id = dplyr::if_else(
#       status_id %in% c(1, 2) & error_id_below3 <= erroneous_hours,
#       3,
#       status_id,
#       status_id
#     ),
#
#     # Fill in gap between questionable/erroneous values
#     gap_above = pmin(error_id_above2, quest_id_above2, na.rm = TRUE),
#     gap_above_type = dplyr::case_when(
#       gap_above == error_id_above2 ~ "err",
#       gap_above == quest_id_above2 ~ "quest",
#       TRUE ~ NA_character_
#     ),
#     gap_below = pmax(error_id_below2, quest_id_below2, na.rm = TRUE),
#     gap_below_type = dplyr::case_when(
#       gap_below == error_id_below2 ~ "err",
#       gap_below == quest_id_below2 ~ "quest",
#       TRUE ~ NA_character_
#     ),
#     gap_diff = diff_hours(gap_above, gap_below),
#
#     status_id = dplyr::case_when(
#       # if the gap less then 5 and at least one value is erroneous code the gap as erroneous
#       status_id == 1 & gap_diff <= 5 & (gap_above_type == "err" | gap_below_type == "err")   ~ 3,
#       # if the gap less then 5 (and not touching erroneous) then code as questionable
#       status_id == 1 & gap_diff <= 5   ~ 2,
#       TRUE ~ status_id
#     )
#
#   )
#
# data





