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
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(0),
      status_id = c(1L)
    )
  )
  
  expect_equal(
    output_2,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(-0.1),
      status_id = c(2L)
    )
  )
  
  expect_equal(
    output_3,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(30.01),
      status_id = c(2L)
    )
  )
  
  expect_equal(
    output_4,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(30),
      status_id = c(1L)
    )
  )
  
  expect_equal(
    output_5,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(39.9998124),
      status_id = c(2L)
    )
  )

  expect_equal(
    output_6,
    data.frame(
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
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.6),
      status_id = c(3L)
    )
  )
  
  expect_equal(
    output_2,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-1),
      status_id = c(3L)
    )
  )
  
  expect_equal(
    output_3,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(41),
      status_id = c(3L)
    )
  )
  
  expect_equal(
    output_4,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(60),
      status_id = c(3L)
    )
  )
  
  expect_equal(
    output_5,
    data.frame(
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
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(0),
      status_id = c(1L)
    )
  )
  
  expect_equal(
    output_2,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.3),
      status_id = c(2L)
    )
  )
  
  expect_equal(
    output_3,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.5),
      status_id = c(2L)
    )
  )
  
  expect_equal(
    output_4,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.6),
      status_id = c(3L)
    )
  )
  
  expect_equal(
    output_5,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-1),
      status_id = c(3L)
    )
  )
  
  expect_equal(
    output_6,
    data.frame(
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
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(25),
      status_id = c(1L)
    )
  )
  
  expect_equal(
    output_2,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(30),
      status_id = c(1L)
    )
  )
  
  expect_equal(
    output_3,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(35),
      status_id = c(2L)
    )
  )
  
  expect_equal(
    output_4,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(40),
      status_id = c(2L)
    )
  )
  
  expect_equal(
    output_5,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(45),
      status_id = c(3L)
    )
  )
  
  expect_equal(
    output_6,
    data.frame(
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
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(4.7),
      status_id = c(1L)
    )
  )
  
  expect_equal(
    output_2,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.9),
      status_id = c(2L)
    )
  )
  
  expect_equal(
    output_3,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-2),
      status_id = c(3L)
    )
  )
  
  expect_equal(
    output_4,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(23),
      status_id = c(2L)
    )
  )
  
  expect_equal(
    output_5,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(15),
      status_id = c(1L)
    )
  )
  
  expect_equal(
    output_6,
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(45),
      status_id = c(3L)
    )
  )
})

test_that("small rates of changes are not classified as not resonable", {
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

test_that("erroneous rates of change are classifed with default values at end of values", {
  data <- tibble::tribble(
      ~temperature_date_time, ~water_temperature,
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
      "2021-05-07 08:00:00",  2.877,              1L,
      "2021-05-07 08:15:00",  3.012,              1L,
      "2021-05-07 08:30:00",  3.147,              1L,
      "2021-05-07 08:45:00",  3.124,              1L,
      "2021-05-07 09:00:00",  3.268,              1L,
      "2021-05-07 09:15:00",  4.789,              3L,
      "2021-05-07 09:30:00",  6.257,              3L,
      "2021-05-07 09:45:00",  8.657,              3L
    ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("erroneous rates of change are classifed with default values at start of values", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 08:00:00",  4.789,
    "2021-05-07 08:15:00",  6.257,
    "2021-05-07 08:30:00",  3.147,
    "2021-05-07 08:45:00",  3.124,
    "2021-05-07 09:00:00",  3.068, 
    "2021-05-07 09:15:00",  2.877,
    "2021-05-07 09:30:00",  2.987,
    "2021-05-07 09:45:00",  3.012
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
      "2021-05-07 08:45:00",  3.124,              1L,
      "2021-05-07 09:00:00",  3.068,              1L,
      "2021-05-07 09:15:00",  2.877,              1L,
      "2021-05-07 09:30:00",  2.987,              1L,
      "2021-05-07 09:45:00",  3.012,              1L
    ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("erroneous rates of change are classifed with default values in the middle of values", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 08:00:00",  3.589,
    "2021-05-07 08:15:00",  3.324,
    "2021-05-07 08:30:00",  3.147,
    "2021-05-07 08:45:00",  10.124,
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
      "2021-05-07 08:00:00",  3.589,              1L,
      "2021-05-07 08:15:00",  3.324,              1L,
      "2021-05-07 08:30:00",  3.147,              1L,
      "2021-05-07 08:45:00",  10.124,             3L,
      "2021-05-07 09:00:00",  3.068,              3L,
      "2021-05-07 09:15:00",  2.877,              1L,
      "2021-05-07 09:30:00",  2.987,              1L,
      "2021-05-07 09:45:00",  3.012,              1L,
      "2021-05-07 10:00:00",  3.122,              1L
    ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("questionable rates of change are classifed with default values at end of values", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
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
      "2021-05-07 08:00:00",  2.877,              1L,
      "2021-05-07 08:15:00",  3.012,              1L,
      "2021-05-07 08:30:00",  3.147,              1L,
      "2021-05-07 08:45:00",  3.124,              1L,
      "2021-05-07 09:00:00",  3.268,              1L,
      "2021-05-07 09:15:00",  3.989,              2L,
      "2021-05-07 09:30:00",  4.557,              2L,
      "2021-05-07 09:45:00",  5.657,              2L
    ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("questionable rates of change are classifed with default values at start of values", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 08:00:00",  4.189,
    "2021-05-07 08:15:00",  3.657,
    "2021-05-07 08:30:00",  3.147,
    "2021-05-07 08:45:00",  3.124,
    "2021-05-07 09:00:00",  3.068, 
    "2021-05-07 09:15:00",  2.877,
    "2021-05-07 09:30:00",  2.987,
    "2021-05-07 09:45:00",  3.012
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
      "2021-05-07 08:45:00",  3.124,              1L,
      "2021-05-07 09:00:00",  3.068,              1L,
      "2021-05-07 09:15:00",  2.877,              1L,
      "2021-05-07 09:30:00",  2.987,              1L,
      "2021-05-07 09:45:00",  3.012,              1L
    ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("questionable rates of change are classifed with default values in the middle of values", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 08:00:00",  3.589,
    "2021-05-07 08:15:00",  3.324,
    "2021-05-07 08:30:00",  3.147,
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
      "2021-05-07 08:00:00",  3.589,              1L,
      "2021-05-07 08:15:00",  3.324,              1L,
      "2021-05-07 08:30:00",  3.147,              1L,
      "2021-05-07 08:45:00",  3.724,              2L,
      "2021-05-07 09:00:00",  3.068,              2L,
      "2021-05-07 09:15:00",  2.877,              1L,
      "2021-05-07 09:30:00",  2.987,              1L,
      "2021-05-07 09:45:00",  3.012,              1L,
      "2021-05-07 10:00:00",  3.122,              1L
    ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

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
      "2021-05-07 09:00:00",  3.068,              2L,
      "2021-05-07 09:15:00",  2.877,              1L,
      "2021-05-07 09:30:00",  2.987,              1L,
      "2021-05-07 09:45:00",  3.012,              1L,
      "2021-05-07 10:00:00",  3.122,              1L
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
      "2021-05-07 08:00:00",  20.124,             2L,
      "2021-05-07 08:15:00",  18.782,             2L,
      "2021-05-07 08:30:00",  14.579,             2L,
      "2021-05-07 08:45:00",  3.724,              3L,
      "2021-05-07 09:00:00",  3.068,              1L,
      "2021-05-07 09:15:00",  2.877,              1L,
      "2021-05-07 09:30:00",  2.987,              1L,
      "2021-05-07 09:45:00",  3.012,              1L,
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
      "2021-05-07 09:00:00",  3.168,              2L,
      "2021-05-07 09:15:00",  2.877,              2L,
      "2021-05-07 09:30:00",  2.987,              1L,
      "2021-05-07 09:45:00",  3.012,              1L,
      "2021-05-07 10:00:00",  3.122,              1L
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
      "2021-05-07 08:00:00",  20.124,             2L,
      "2021-05-07 08:15:00",  18.782,             2L,
      "2021-05-07 08:30:00",  14.579,             3L,
      "2021-05-07 08:45:00",  3.724,              3L,
      "2021-05-07 09:00:00",  3.168,              2L,
      "2021-05-07 09:15:00",  2.877,              1L,
      "2021-05-07 09:30:00",  2.987,              1L,
      "2021-05-07 09:45:00",  3.012,              1L,
      "2021-05-07 10:00:00",  3.122,              1L
    ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

# 
# data
# 
# data |>
#   dplyr::arrange(.data$temperature_date_time) |>
#   dplyr::mutate(
#     status_id = 1L,
#     
#     # rate of change
#     lag_temp = dplyr::lag(water_temperature),
#     diff_temp = abs(water_temperature - lag_temp),
#     lag_time = dplyr::lag(temperature_date_time),
#     diff_time = as.numeric(difftime(temperature_date_time, lag_time, units = "hours")),
#     rate_temp_per_time = abs(diff_temp / diff_time),
#     status_id = dplyr::case_when(
#       # erroneous rate of change
#       rate_temp_per_time > 5 ~ 3L,
#       # questionable rate of change
#       rate_temp_per_time > 2 ~ 2L, 
#       TRUE ~ status_id
#     )
#   ) 
