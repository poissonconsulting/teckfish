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

  classified_data <- classify_water_temp_data(data) |>
    dplyr::mutate(status_id = as.integer(status_id))

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
  expect_error(
    classify_water_temp_data(data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(4.2),
      .rate = c(1L)
    )),
    regexp = "`colnames\\(data\\)` must not have any values matching"
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
    ) |>
      dplyr::mutate(
        status_id = factor(
          status_id,
          levels = c("reasonable", "questionable", "erroneous"),
          ordered = TRUE
        )
      )
  )
})

test_that("errors when no data is passed", {
  expect_error(
    classify_water_temp_data(),
    regexp = 'argument "data" is missing, with no default'
  )
})

test_that("status_id column is output as a factor", {
  output_1 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(0)
    )
  )

  expect_type(output_1$status_id, "integer")
  expect_s3_class(output_1$status_id, c("ordered"))
  expect_s3_class(output_1$status_id, c("factor"))
})

test_that("table output type is tibble", {
  output_1 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(0)
    )
  )

  expect_s3_class(output_1, c("tbl_df"))
  expect_s3_class(output_1, c("tbl"))
  expect_s3_class(output_1, c("data.frame"))
})

test_that("errors when date times are duplicated", {
  expect_error(
    classify_water_temp_data(
      data.frame(
        temperature_date_time = as.POSIXct(
          c("2021-05-07 08:00:00", "2021-05-07 08:00:00", "2021-05-07 08:30:00")
        ),
        water_temperature = c(0, 0.123, 0.345)
      )
    ),
    regexp = "`data\\$temperature_date_time` must be unique."
  )
})

test_that("errors when missing a date time", {
  expect_error(
    classify_water_temp_data(
      data.frame(
        temperature_date_time = as.POSIXct(
          c("2021-05-07 08:00:00", NA_character_, "2021-05-07 08:30:00")
        ),
        water_temperature = c(0, 0.123, 0.345)
      )
    ),
    regexp = "`data\\$temperature_date_time` must not have any missing values."
  )
})

test_that("questionable temperatures are classified correctly with default values", {
  output_1 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(0)
    )
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    output_1,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(0),
      status_id = c(2L)
    )
  )

  output_2 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(-0.1)
    )
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    output_2,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(-0.1),
      status_id = c(2L)
    )
  )

  output_3 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(29.45)
    )
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    output_3,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(29.45),
      status_id = c(1L)
    )
  )

  output_4 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(30)
    )
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    output_4,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(30),
      status_id = c(2L)
    )
  )

  output_5 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(39.9998124)
    )
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    output_5,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(39.9998124),
      status_id = c(2L)
    )
  )

  output_6 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 08:00:00")),
      water_temperature = c(34.24)
    )
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

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
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  output_2 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-1)
    )
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  output_3 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(41)
    )
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  output_4 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(60)
    )
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  output_5 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(6.712)
    )
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

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
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    output_1,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(0),
      status_id = c(2L)
    )
  )

  output_2 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.3)
    )
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    output_2,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.3),
      status_id = c(2L)
    )
  )

  output_3 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.5)
    )
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    output_3,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.5),
      status_id = c(3L)
    )
  )

  output_4 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.6)
    )
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    output_4,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.6),
      status_id = c(3L)
    )
  )

  output_5 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-1)
    )
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    output_5,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-1),
      status_id = c(3L)
    )
  )

  output_6 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-10)
    )
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

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
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    output_1,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(25),
      status_id = c(1L)
    )
  )

  output_2 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(30)
    )
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    output_2,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(30),
      status_id = c(2L)
    )
  )

  output_3 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(35)
    )
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    output_3,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(35),
      status_id = c(2L)
    )
  )

  output_4 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(40)
    )
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    output_4,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(40),
      status_id = c(3L)
    )
  )

  output_5 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(45)
    )
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    output_5,
    tibble::tibble(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(45),
      status_id = c(3L)
    )
  )

  output_6 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(60)
    )
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

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
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  output_2 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-0.9)
    ),
    questionable_min = -0.8,
    questionable_max = 20,
    erroneous_min = -1,
    erroneous_max = 25
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  output_3 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(-2)
    ),
    questionable_min = -0.8,
    questionable_max = 20,
    erroneous_min = -1,
    erroneous_max = 25
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  output_4 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(23)
    ),
    questionable_min = -0.8,
    questionable_max = 20,
    erroneous_min = -1,
    erroneous_max = 25
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  output_5 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(15)
    ),
    questionable_min = -0.8,
    questionable_max = 20,
    erroneous_min = -1,
    erroneous_max = 25
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  output_6 <- classify_water_temp_data(
    data.frame(
      temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
      water_temperature = c(45)
    ),
    questionable_min = -0.8,
    questionable_max = 20,
    erroneous_min = -1,
    erroneous_max = 25
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

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

test_that("questionable and erroneous ranges align with checks", {
  expect_error(
    classify_water_temp_data(
      data.frame(
        temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
        water_temperature = c(45)
      ),
      questionable_min = -4,
      erroneous_min = -2,
    ),
    regexp = "`erroneous_min` must be less than or equal to -4, not -2."
  )

  expect_error(
    classify_water_temp_data(
      data.frame(
        temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
        water_temperature = c(45)
      ),
      questionable_max = 20,
      erroneous_max = 10,
    ),
    regexp = "`erroneous_max` must be greater than or equal to 20, not 10."
  )

  expect_error(
    classify_water_temp_data(
      data.frame(
        temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
        water_temperature = c(45)
      ),
      questionable_min = 40,
      questionable_max = 20
    ),
    regexp = "`questionable_max` must be greater than 40, not 20."
  )

  expect_error(
    classify_water_temp_data(
      data.frame(
        temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
        water_temperature = c(45)
      ),
      questionable_min = -2,
      questionable_max = -30
    ),
    regexp = "`questionable_max` must be greater than -2, not -30."
  )

  expect_error(
    classify_water_temp_data(
      data.frame(
        temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
        water_temperature = c(45)
      ),
      erroneous_min = 40,
      erroneous_max = 20
    ),
    regexp = "`erroneous_max` must be greater than 40, not 20."
  )

  expect_error(
    classify_water_temp_data(
      data.frame(
        temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
        water_temperature = c(45)
      ),
      erroneous_min = -4,
      erroneous_max = -10
    ),
    regexp = "`erroneous_max` must be greater than -4, not -10."
  )
})

test_that("rates parameter checks", {
  expect_error(
    classify_water_temp_data(
      data.frame(
        temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
        water_temperature = c(45)
      ),
      questionable_rate = -2
    ),
    regexp = "`questionable_rate` must be greater than 0, not -2."
  )

  expect_error(
    classify_water_temp_data(
      data.frame(
        temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
        water_temperature = c(45)
      ),
      erroneous_rate = -2
    ),
    regexp = "`erroneous_rate` must be greater than 0, not -2."
  )

  expect_error(
    classify_water_temp_data(
      data.frame(
        temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
        water_temperature = c(45)
      ),
      questionable_rate = 5,
      erroneous_rate = 3
    ),
    regexp = "`erroneous_rate` must be greater than or equal to 5, not 3."
  )
})

test_that("buffer parameter checks", {
  expect_error(
    classify_water_temp_data(
      data.frame(
        temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
        water_temperature = c(45)
      ),
      questionable_buffer = -2
    ),
    regexp = "`questionable_buffer` must be greater than or equal to 0, not -2."
  )

  expect_error(
    classify_water_temp_data(
      data.frame(
        temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
        water_temperature = c(45)
      ),
      erroneous_buffer = -2
    ),
    regexp = "`erroneous_buffer` must be greater than or equal to 0, not -2."
  )
})

test_that("gap range parameter checks", {
  expect_error(
    classify_water_temp_data(
      data.frame(
        temperature_date_time = as.POSIXct(c("2021-05-07 13:00:00")),
        water_temperature = c(45)
      ),
      gap_range = -2
    ),
    regexp = "`gap_range` must be greater than or equal to 0, not -2."
  )
})

test_that("small rates of changes are classified as reasonable", {
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

  classified_data <- classify_water_temp_data(
    data,
    questionable_buffer = 0,
    erroneous_buffer = 0,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

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

test_that("erroneous rates are classifed, bad values at end of series", {
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

  classified_data <- classify_water_temp_data(
    data,
    questionable_buffer = 0,
    erroneous_buffer = 0,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 07:30:00",  2.817,              1L,
      "2021-05-07 07:45:00",  2.867,              1L,
      "2021-05-07 08:00:00",  2.877,              1L,
      "2021-05-07 08:15:00",  3.012,              1L,
      "2021-05-07 08:30:00",  3.147,              1L,
      "2021-05-07 08:45:00",  3.124,              1L,
      "2021-05-07 09:00:00",  3.268,              3L,
      "2021-05-07 09:15:00",  4.789,              3L,
      "2021-05-07 09:30:00",  6.257,              3L,
      "2021-05-07 09:45:00",  8.657,              3L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("erroneous rates are classifed, bad values at beginning", {
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

  classified_data <- classify_water_temp_data(
    data,
    questionable_buffer = 0,
    erroneous_buffer = 0,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 08:00:00",  4.789,              3L,
      "2021-05-07 08:15:00",  6.257,              3L,
      "2021-05-07 08:30:00",  3.147,              3L,
      "2021-05-07 08:45:00",  3.124,              3L,
      "2021-05-07 09:00:00",  3.068,              1L,
      "2021-05-07 09:15:00",  2.877,              1L,
      "2021-05-07 09:30:00",  2.987,              1L,
      "2021-05-07 09:45:00",  3.012,              1L,
      "2021-05-07 10:15:00",  3.112,              1L,
      "2021-05-07 10:35:00",  3.042,              1L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("erroneous rates are classifed, bad values in middle of series", {
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

  classified_data <- classify_water_temp_data(
    data,
    questionable_buffer = 0,
    erroneous_buffer = 0,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 07:30:00",  3.589,              1L,
      "2021-05-07 07:45:00",  3.489,              1L,
      "2021-05-07 08:00:00",  3.589,              1L,
      "2021-05-07 08:15:00",  3.324,              1L,
      "2021-05-07 08:30:00",  3.147,              3L,
      "2021-05-07 08:45:00",  10.124,             3L,
      "2021-05-07 09:00:00",  3.068,              3L,
      "2021-05-07 09:15:00",  2.877,              3L,
      "2021-05-07 09:30:00",  2.987,              1L,
      "2021-05-07 09:45:00",  3.012,              1L,
      "2021-05-07 10:00:00",  3.122,              1L,
      "2021-05-07 10:15:00",  3.100,              1L,
      "2021-05-07 10:30:00",  3.120,              1L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("questionable rates classifed, bad values at end of series", {
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

  classified_data <- classify_water_temp_data(
    data,
    questionable_buffer = 0,
    erroneous_buffer = 0,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 07:30:00",  2.817,              1L,
      "2021-05-07 07:45:00",  2.867,              1L,
      "2021-05-07 08:00:00",  2.877,              1L,
      "2021-05-07 08:15:00",  3.012,              1L,
      "2021-05-07 08:30:00",  3.147,              1L,
      "2021-05-07 08:45:00",  3.124,              1L,
      "2021-05-07 09:00:00",  3.268,              2L,
      "2021-05-07 09:15:00",  3.989,              2L,
      "2021-05-07 09:30:00",  4.557,              2L,
      "2021-05-07 09:45:00",  5.657,              2L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("questionable rates are classifed, bad values at beginning", {
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

  classified_data <- classify_water_temp_data(
    data,
    questionable_buffer = 0,
    erroneous_buffer = 0,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 08:00:00",  4.189,              2L,
      "2021-05-07 08:15:00",  3.657,              2L,
      "2021-05-07 08:30:00",  3.147,              2L,
      "2021-05-07 08:45:00",  3.124,              2L,
      "2021-05-07 09:00:00",  3.068,              1L,
      "2021-05-07 09:15:00",  2.877,              1L,
      "2021-05-07 09:30:00",  2.987,              1L,
      "2021-05-07 09:45:00",  3.012,              1L,
      "2021-05-07 10:00:00",  3.122,              1L,
      "2021-05-07 10:15:00",  3.100,              1L,
      "2021-05-07 10:30:00",  3.120,              1L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("questionable rates are classifed, bad values in middle of series", {
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

  classified_data <- classify_water_temp_data(
    data,
    questionable_buffer = 0,
    erroneous_buffer = 0,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 07:30:00",  3.589,              1L,
      "2021-05-07 07:45:00",  3.489,              1L,
      "2021-05-07 08:00:00",  3.589,              1L,
      "2021-05-07 08:15:00",  3.324,              1L,
      "2021-05-07 08:30:00",  3.147,              2L,
      "2021-05-07 08:45:00",  3.724,              2L,
      "2021-05-07 09:00:00",  3.068,              2L,
      "2021-05-07 09:15:00",  2.877,              2L,
      "2021-05-07 09:30:00",  2.987,              1L,
      "2021-05-07 09:45:00",  3.012,              1L,
      "2021-05-07 10:00:00",  3.122,              1L,
      "2021-05-07 10:15:00",  3.100,              1L,
      "2021-05-07 10:30:00",  3.120,              1L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("questionable and erroneous rates classfied", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 06:45:00",  3.142,
    "2021-05-07 07:00:00",  3.042,
    "2021-05-07 07:15:00",  3.142,
    "2021-05-07 07:30:00",  3.345,
    "2021-05-07 07:45:00",  3.478,
    "2021-05-07 08:00:00",  20.124,
    "2021-05-07 08:15:00",  18.782,
    "2021-05-07 08:30:00",  14.579,
    "2021-05-07 08:45:00",  3.724,
    "2021-05-07 09:00:00",  3.068,
    "2021-05-07 09:15:00",  2.877,
    "2021-05-07 09:30:00",  2.987,
    "2021-05-07 09:45:00",  3.012,
    "2021-05-07 10:00:00",  3.122,
    "2021-05-07 10:15:00",  3.022
  ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))

  classified_data <- classify_water_temp_data(
    data,
    questionable_buffer = 0,
    erroneous_buffer = 0,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 06:45:00",  3.142,              1L,
      "2021-05-07 07:00:00",  3.042,              1L,
      "2021-05-07 07:15:00",  3.142,              1L,
      "2021-05-07 07:30:00",  3.345,              1L,
      "2021-05-07 07:45:00",  3.478,              3L,
      "2021-05-07 08:00:00",  20.124,             3L,
      "2021-05-07 08:15:00",  18.782,             3L,
      "2021-05-07 08:30:00",  14.579,             3L,
      "2021-05-07 08:45:00",  3.724,              3L,
      "2021-05-07 09:00:00",  3.068,              3L,
      "2021-05-07 09:15:00",  2.877,              2L,
      "2021-05-07 09:30:00",  2.987,              1L,
      "2021-05-07 09:45:00",  3.012,              1L,
      "2021-05-07 10:00:00",  3.122,              1L,
      "2021-05-07 10:15:00",  3.022,              1L,
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("questionable and erroneous rates of change set with parameter", {
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
    erroneous_rate = 20,
    questionable_buffer = 0,
    erroneous_buffer = 0,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 08:00:00",  20.124,             2L,
      "2021-05-07 08:15:00",  18.782,             2L,
      "2021-05-07 08:30:00",  14.579,             3L,
      "2021-05-07 08:45:00",  3.724,              3L,
      "2021-05-07 09:00:00",  3.068,              3L,
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
    questionable_rate = 1,
    questionable_buffer = 0,
    erroneous_buffer = 0,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 08:00:00",  20.124,             3L,
      "2021-05-07 08:15:00",  18.782,             3L,
      "2021-05-07 08:30:00",  14.579,             3L,
      "2021-05-07 08:45:00",  3.724,              3L,
      "2021-05-07 09:00:00",  3.168,              3L,
      "2021-05-07 09:15:00",  2.877,              2L,
      "2021-05-07 09:30:00",  2.987,              2L,
      "2021-05-07 09:45:00",  3.012,              1L,
      "2021-05-07 10:00:00",  3.122,              1L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("erroneous rate of change set with parameter", {
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
    erroneous_rate = 15,
    questionable_buffer = 0,
    erroneous_buffer = 0,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 08:00:00",  20.124,             2L,
      "2021-05-07 08:15:00",  18.782,             3L,
      "2021-05-07 08:30:00",  14.579,             3L,
      "2021-05-07 08:45:00",  3.724,              3L,
      "2021-05-07 09:00:00",  3.168,              3L,
      "2021-05-07 09:15:00",  2.877,              2L,
      "2021-05-07 09:30:00",  2.987,              1L,
      "2021-05-07 09:45:00",  3.012,              1L,
      "2021-05-07 10:00:00",  3.122,              1L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("erroneous rates are buffered, bad values at end of series", {
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

  classified_data <- classify_water_temp_data(
    data,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 07:30:00",  2.817,              1L,
      "2021-05-07 07:45:00",  2.867,              1L,
      "2021-05-07 08:00:00",  2.877,              3L,
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

test_that("erroneous rates are buffered, bad values at beginning", {
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

  classified_data <- classify_water_temp_data(
    data,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

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
      "2021-05-07 09:45:00",  3.012,              3L,
      "2021-05-07 10:15:00",  3.112,              1L,
      "2021-05-07 10:35:00",  3.042,              1L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("erroneous rates are buffered, bad values in middle of series", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 07:15:00",  3.489,
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

  classified_data <- classify_water_temp_data(
    data,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 07:15:00",  3.489,              1L,
      "2021-05-07 07:30:00",  3.589,              3L,
      "2021-05-07 07:45:00",  3.489,              3L,
      "2021-05-07 08:00:00",  3.589,              3L,
      "2021-05-07 08:15:00",  3.324,              3L,
      "2021-05-07 08:30:00",  3.147,              3L,
      "2021-05-07 08:45:00",  10.124,             3L,
      "2021-05-07 09:00:00",  3.068,              3L,
      "2021-05-07 09:15:00",  2.877,              3L,
      "2021-05-07 09:30:00",  2.987,              3L,
      "2021-05-07 09:45:00",  3.012,              3L,
      "2021-05-07 10:00:00",  3.122,              3L,
      "2021-05-07 10:15:00",  3.100,              3L,
      "2021-05-07 10:30:00",  3.120,              1L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("questionable rates are buffered, bad values at end of series", {
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

  classified_data <- classify_water_temp_data(
    data,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 07:30:00",  2.817,              1L,
      "2021-05-07 07:45:00",  2.867,              1L,
      "2021-05-07 08:00:00",  2.877,              2L,
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

test_that("questionable rates are buffered, bad values at beginning", {
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

  classified_data <- classify_water_temp_data(
    data,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

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
      "2021-05-07 09:45:00",  3.012,              2L,
      "2021-05-07 10:00:00",  3.122,              1L,
      "2021-05-07 10:15:00",  3.100,              1L,
      "2021-05-07 10:30:00",  3.120,              1L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("questionable rates are buffered, bad values in middle of series", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 07:15:00",  3.489,
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

  classified_data <- classify_water_temp_data(
    data,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 07:15:00",  3.489,              1L,
      "2021-05-07 07:30:00",  3.589,              2L,
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
      "2021-05-07 10:15:00",  3.100,              2L,
      "2021-05-07 10:30:00",  3.120,              1L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("questionable and erroneous rates buffered", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 06:30:00",  3.042,
    "2021-05-07 06:45:00",  3.142,
    "2021-05-07 07:00:00",  3.042,
    "2021-05-07 07:15:00",  3.142,
    "2021-05-07 07:30:00",  3.345,
    "2021-05-07 07:45:00",  3.478,
    "2021-05-07 08:00:00",  20.124,
    "2021-05-07 08:15:00",  18.782,
    "2021-05-07 08:30:00",  14.579,
    "2021-05-07 08:45:00",  3.724,
    "2021-05-07 09:00:00",  3.068,
    "2021-05-07 09:15:00",  2.877,
    "2021-05-07 09:30:00",  2.987,
    "2021-05-07 09:45:00",  3.012,
    "2021-05-07 10:00:00",  3.122,
    "2021-05-07 10:15:00",  3.022,
    "2021-05-07 10:30:00",  3.122
  ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))

  classified_data <- classify_water_temp_data(
    data,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 06:30:00",  3.042,              1L,
      "2021-05-07 06:45:00",  3.142,              3L,
      "2021-05-07 07:00:00",  3.042,              3L,
      "2021-05-07 07:15:00",  3.142,              3L,
      "2021-05-07 07:30:00",  3.345,              3L,
      "2021-05-07 07:45:00",  3.478,              3L,
      "2021-05-07 08:00:00",  20.124,             3L,
      "2021-05-07 08:15:00",  18.782,             3L,
      "2021-05-07 08:30:00",  14.579,             3L,
      "2021-05-07 08:45:00",  3.724,              3L,
      "2021-05-07 09:00:00",  3.068,              3L,
      "2021-05-07 09:15:00",  2.877,              3L,
      "2021-05-07 09:30:00",  2.987,              3L,
      "2021-05-07 09:45:00",  3.012,              3L,
      "2021-05-07 10:00:00",  3.122,              3L,
      "2021-05-07 10:15:00",  3.022,              2L,
      "2021-05-07 10:30:00",  3.122,              1L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("questionable buffer set with parameter to be smaller", {
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

  classified_data <- classify_water_temp_data(
    data,
    gap_range = 0,
    questionable_buffer = 0.5
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

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
      "2021-05-07 09:30:00",  2.987,              1L,
      "2021-05-07 09:45:00",  3.012,              1L,
      "2021-05-07 10:00:00",  3.122,              1L,
      "2021-05-07 10:15:00",  3.100,              1L,
      "2021-05-07 10:30:00",  3.120,              1L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("questionable buffer set with parameter to be larger", {
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
    "2021-05-07 10:45:00",  3.152,
    "2021-05-07 11:00:00",  3.100,
    "2021-05-07 11:15:00",  3.120
  ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))

  classified_data <- classify_water_temp_data(
    data,
    gap_range = 0,
    questionable_buffer = 2
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

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
      "2021-05-07 09:45:00",  3.012,              2L,
      "2021-05-07 10:00:00",  3.122,              2L,
      "2021-05-07 10:15:00",  3.100,              2L,
      "2021-05-07 10:30:00",  3.120,              2L,
      "2021-05-07 10:45:00",  3.152,              2L,
      "2021-05-07 11:00:00",  3.100,              1L,
      "2021-05-07 11:15:00",  3.120,              1L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("erroneous buffer set with parameter to be smaller", {
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

  classified_data <- classify_water_temp_data(
    data,
    erroneous_buffer = 0.25,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 07:30:00",  2.817,              1L,
      "2021-05-07 07:45:00",  2.867,              1L,
      "2021-05-07 08:00:00",  2.877,              1L,
      "2021-05-07 08:15:00",  3.012,              1L,
      "2021-05-07 08:30:00",  3.147,              1L,
      "2021-05-07 08:45:00",  3.124,              3L,
      "2021-05-07 09:00:00",  3.268,              3L,
      "2021-05-07 09:15:00",  4.789,              3L,
      "2021-05-07 09:30:00",  6.257,              3L,
      "2021-05-07 09:45:00",  8.657,              3L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("erroneous buffer set with parameter to be larger", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 06:45:00",  2.917,
    "2021-05-07 07:00:00",  2.817,
    "2021-05-07 07:15:00",  2.917,
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

  classified_data <- classify_water_temp_data(
    data,
    erroneous_buffer = 2,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 06:45:00",  2.917,              1L,
      "2021-05-07 07:00:00",  2.817,              3L,
      "2021-05-07 07:15:00",  2.917,              3L,
      "2021-05-07 07:30:00",  2.817,              3L,
      "2021-05-07 07:45:00",  2.867,              3L,
      "2021-05-07 08:00:00",  2.877,              3L,
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

test_that("gaps are filled in", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 05:45:00",  6.257,
    "2021-05-07 06:00:00",  2.817,
    "2021-05-07 06:15:00",  2.917,
    "2021-05-07 06:30:00",  2.817,
    "2021-05-07 06:45:00",  2.917,
    "2021-05-07 07:00:00",  2.817,
    "2021-05-07 07:15:00",  2.917,
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

  classified_data <- classify_water_temp_data(
    data,
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 05:45:00",  6.257,              3L,
      "2021-05-07 06:00:00",  2.817,              3L,
      "2021-05-07 06:15:00",  2.917,              3L,
      "2021-05-07 06:30:00",  2.817,              3L,
      "2021-05-07 06:45:00",  2.917,              3L,
      "2021-05-07 07:00:00",  2.817,              3L,
      "2021-05-07 07:15:00",  2.917,              3L,
      "2021-05-07 07:30:00",  2.817,              2L,
      "2021-05-07 07:45:00",  2.867,              2L,
      "2021-05-07 08:00:00",  2.877,              3L,
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

test_that("gaps not filled in when set to zero", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 05:45:00",  6.257,
    "2021-05-07 06:00:00",  2.817,
    "2021-05-07 06:15:00",  2.917,
    "2021-05-07 06:30:00",  2.817,
    "2021-05-07 06:45:00",  2.917,
    "2021-05-07 07:00:00",  2.817,
    "2021-05-07 07:15:00",  2.917,
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

  classified_data <- classify_water_temp_data(
    data,
    gap_range = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 05:45:00",  6.257,              3L,
      "2021-05-07 06:00:00",  2.817,              3L,
      "2021-05-07 06:15:00",  2.917,              3L,
      "2021-05-07 06:30:00",  2.817,              3L,
      "2021-05-07 06:45:00",  2.917,              3L,
      "2021-05-07 07:00:00",  2.817,              3L,
      "2021-05-07 07:15:00",  2.917,              3L,
      "2021-05-07 07:30:00",  2.817,              1L,
      "2021-05-07 07:45:00",  2.867,              1L,
      "2021-05-07 08:00:00",  2.877,              3L,
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

test_that("gaps does not over fill gap difference is 2.5 and parameter set to 1", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 05:45:00",  6.257,
    "2021-05-07 06:00:00",  2.817,
    "2021-05-07 06:15:00",  2.917,
    "2021-05-07 06:30:00",  2.817,
    "2021-05-07 06:45:00",  2.917,
    "2021-05-07 07:00:00",  2.817,
    "2021-05-07 07:15:00",  2.917,
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

  classified_data <- classify_water_temp_data(
    data,
    gap_range = 1
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 05:45:00",  6.257,              3L,
      "2021-05-07 06:00:00",  2.817,              3L,
      "2021-05-07 06:15:00",  2.917,              3L,
      "2021-05-07 06:30:00",  2.817,              3L,
      "2021-05-07 06:45:00",  2.917,              3L,
      "2021-05-07 07:00:00",  2.817,              3L,
      "2021-05-07 07:15:00",  2.917,              3L,
      "2021-05-07 07:30:00",  2.817,              1L,
      "2021-05-07 07:45:00",  2.867,              1L,
      "2021-05-07 08:00:00",  2.877,              3L,
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

test_that("gaps fills with questionable values when erroneous and questionable nexts", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 05:45:00",  6.257,
    "2021-05-07 06:00:00",  2.817,
    "2021-05-07 06:15:00",  2.917,
    "2021-05-07 06:30:00",  2.817,
    "2021-05-07 06:45:00",  2.917,
    "2021-05-07 07:00:00",  2.817,
    "2021-05-07 07:15:00",  2.917,
    "2021-05-07 07:30:00",  2.817,
    "2021-05-07 07:45:00",  2.867,
    "2021-05-07 08:00:00",  2.877,
    "2021-05-07 08:15:00",  3.012,
    "2021-05-07 08:30:00",  3.147,
    "2021-05-07 08:45:00",  3.124,
    "2021-05-07 09:00:00",  3.268,
    "2021-05-07 09:15:00",  4.189,
    "2021-05-07 09:30:00",  4.157,
    "2021-05-07 09:45:00",  4.957
  ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))

  classified_data <- classify_water_temp_data(
    data,
    questionable_buffer = 0,
    erroneous_buffer = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 05:45:00",  6.257,              3L,
      "2021-05-07 06:00:00",  2.817,              3L,
      "2021-05-07 06:15:00",  2.917,              3L,
      "2021-05-07 06:30:00",  2.817,              2L,
      "2021-05-07 06:45:00",  2.917,              2L,
      "2021-05-07 07:00:00",  2.817,              2L,
      "2021-05-07 07:15:00",  2.917,              2L,
      "2021-05-07 07:30:00",  2.817,              2L,
      "2021-05-07 07:45:00",  2.867,              2L,
      "2021-05-07 08:00:00",  2.877,              2L,
      "2021-05-07 08:15:00",  3.012,              2L,
      "2021-05-07 08:30:00",  3.147,              2L,
      "2021-05-07 08:45:00",  3.124,              2L,
      "2021-05-07 09:00:00",  3.268,              2L,
      "2021-05-07 09:15:00",  4.189,              2L,
      "2021-05-07 09:30:00",  4.157,              2L,
      "2021-05-07 09:45:00",  4.957,              2L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("gaps fills with questionable values", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 05:45:00",  3.557,
    "2021-05-07 06:00:00",  2.817,
    "2021-05-07 06:15:00",  2.917,
    "2021-05-07 06:30:00",  2.817,
    "2021-05-07 06:45:00",  2.917,
    "2021-05-07 07:00:00",  2.817,
    "2021-05-07 07:15:00",  2.917,
    "2021-05-07 07:30:00",  2.817,
    "2021-05-07 07:45:00",  2.867,
    "2021-05-07 08:00:00",  2.877,
    "2021-05-07 08:15:00",  3.012,
    "2021-05-07 08:30:00",  3.147,
    "2021-05-07 08:45:00",  3.124,
    "2021-05-07 09:00:00",  3.268,
    "2021-05-07 09:15:00",  4.189,
    "2021-05-07 09:30:00",  4.157,
    "2021-05-07 09:45:00",  4.957
  ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))

  classified_data <- classify_water_temp_data(
    data,
    questionable_buffer = 0,
    erroneous_buffer = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 05:45:00",  3.557,              2L,
      "2021-05-07 06:00:00",  2.817,              2L,
      "2021-05-07 06:15:00",  2.917,              2L,
      "2021-05-07 06:30:00",  2.817,              2L,
      "2021-05-07 06:45:00",  2.917,              2L,
      "2021-05-07 07:00:00",  2.817,              2L,
      "2021-05-07 07:15:00",  2.917,              2L,
      "2021-05-07 07:30:00",  2.817,              2L,
      "2021-05-07 07:45:00",  2.867,              2L,
      "2021-05-07 08:00:00",  2.877,              2L,
      "2021-05-07 08:15:00",  3.012,              2L,
      "2021-05-07 08:30:00",  3.147,              2L,
      "2021-05-07 08:45:00",  3.124,              2L,
      "2021-05-07 09:00:00",  3.268,              2L,
      "2021-05-07 09:15:00",  4.189,              2L,
      "2021-05-07 09:30:00",  4.157,              2L,
      "2021-05-07 09:45:00",  4.957,              2L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("gaps fills only reasonable", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 05:45:00",  6.257,
    "2021-05-07 06:00:00",  2.817,
    "2021-05-07 06:15:00",  2.917,
    "2021-05-07 06:30:00",  2.817,
    "2021-05-07 06:45:00",  2.917,
    "2021-05-07 07:00:00",  2.817,
    "2021-05-07 07:15:00",  2.917,
    "2021-05-07 07:30:00",  2.817,
    "2021-05-07 07:45:00",  2.867,
    "2021-05-07 08:00:00",  2.877,
    "2021-05-07 08:15:00",  3.012,
    "2021-05-07 08:30:00",  3.147,
    "2021-05-07 08:45:00",  3.124,
    "2021-05-07 09:00:00",  3.268,
    "2021-05-07 09:15:00",  4.189,
    "2021-05-07 09:30:00",  4.157,
    "2021-05-07 09:45:00",  4.957,
    "2021-05-07 10:00:00",  7.957,
  ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))

  classified_data <- classify_water_temp_data(
    data,
    questionable_buffer = 0,
    erroneous_buffer = 0
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 05:45:00",  6.257,              3L,
      "2021-05-07 06:00:00",  2.817,              3L,
      "2021-05-07 06:15:00",  2.917,              3L,
      "2021-05-07 06:30:00",  2.817,              2L,
      "2021-05-07 06:45:00",  2.917,              2L,
      "2021-05-07 07:00:00",  2.817,              2L,
      "2021-05-07 07:15:00",  2.917,              2L,
      "2021-05-07 07:30:00",  2.817,              2L,
      "2021-05-07 07:45:00",  2.867,              2L,
      "2021-05-07 08:00:00",  2.877,              2L,
      "2021-05-07 08:15:00",  3.012,              2L,
      "2021-05-07 08:30:00",  3.147,              2L,
      "2021-05-07 08:45:00",  3.124,              2L,
      "2021-05-07 09:00:00",  3.268,              2L,
      "2021-05-07 09:15:00",  4.189,              2L,
      "2021-05-07 09:30:00",  4.157,              2L,
      "2021-05-07 09:45:00",  4.957,              3L,
      "2021-05-07 10:00:00",  7.957,              3L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("missing values in water temp are retained", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 05:45:00",  6.257,
    "2021-05-07 06:00:00",  2.817,
    "2021-05-07 06:15:00",  2.917,
    "2021-05-07 06:30:00",  2.817,
    "2021-05-07 06:45:00",  NA_real_,
    "2021-05-07 07:00:00",  2.817,
    "2021-05-07 07:15:00",  2.917,
    "2021-05-07 07:30:00",  2.817,
    "2021-05-07 07:45:00",  2.867,
    "2021-05-07 08:00:00",  2.877,
    "2021-05-07 08:15:00",  3.012,
    "2021-05-07 08:30:00",  NA_real_,
    "2021-05-07 08:45:00",  3.124,
    "2021-05-07 09:00:00",  3.268,
    "2021-05-07 09:15:00",  4.789,
    "2021-05-07 09:30:00",  6.257,
    "2021-05-07 09:45:00",  8.657
  ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))

  classified_data <- classify_water_temp_data(data) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 05:45:00", 6.257, 3L,
      "2021-05-07 06:00:00", 2.817, 3L,
      "2021-05-07 06:15:00", 2.917, 3L,
      "2021-05-07 06:30:00", 2.817, 3L,
      "2021-05-07 06:45:00", NA_real_, NA_integer_,
      "2021-05-07 07:00:00", 2.817, 3L,
      "2021-05-07 07:15:00", 2.917, 3L,
      "2021-05-07 07:30:00", 2.817, 2L,
      "2021-05-07 07:45:00", 2.867, 2L,
      "2021-05-07 08:00:00", 2.877, 3L,
      "2021-05-07 08:15:00", 3.012, 3L,
      "2021-05-07 08:30:00", NA_real_, NA_integer_,
      "2021-05-07 08:45:00", 3.124, 3L,
      "2021-05-07 09:00:00", 3.268, 3L,
      "2021-05-07 09:15:00", 4.789, 3L,
      "2021-05-07 09:30:00", 6.257, 3L,
      "2021-05-07 09:45:00", 8.657, 3L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})

test_that("status id values are coded as correct factor levels and integer values", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 05:45:00",  6.257,
    "2021-05-07 06:00:00",  2.817,
    "2021-05-07 06:15:00",  2.917,
    "2021-05-07 06:30:00",  2.817,
    "2021-05-07 06:45:00",  NA_real_,
    "2021-05-07 07:00:00",  2.817,
    "2021-05-07 07:15:00",  2.917,
    "2021-05-07 07:30:00",  2.817,
    "2021-05-07 07:45:00",  2.867,
    "2021-05-07 08:00:00",  2.877,
    "2021-05-07 08:15:00",  3.012,
    "2021-05-07 08:30:00",  NA_real_,
    "2021-05-07 08:45:00",  3.124,
    "2021-05-07 09:00:00",  3.268,
    "2021-05-07 09:15:00",  3.789,
    "2021-05-07 09:30:00",  4.104,
    "2021-05-07 09:45:00",  4.178
  ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))

  classified_data <- classify_water_temp_data(
    data,
    gap_range = 0
  )

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 05:45:00",  6.257,              "erroneous",
      "2021-05-07 06:00:00",  2.817,              "erroneous",
      "2021-05-07 06:15:00",  2.917,              "erroneous",
      "2021-05-07 06:30:00",  2.817,              "erroneous",
      "2021-05-07 06:45:00",  NA_real_,           NA_character_,
      "2021-05-07 07:00:00",  2.817,              "erroneous",
      "2021-05-07 07:15:00",  2.917,              "erroneous",
      "2021-05-07 07:30:00",  2.817,              "reasonable",
      "2021-05-07 07:45:00",  2.867,              "reasonable",
      "2021-05-07 08:00:00",  2.877,              "questionable",
      "2021-05-07 08:15:00",  3.012,              "questionable",
      "2021-05-07 08:30:00",  NA_real_,           NA_character_,
      "2021-05-07 08:45:00",  3.124,              "questionable",
      "2021-05-07 09:00:00",  3.268,              "questionable",
      "2021-05-07 09:15:00",  3.789,              "questionable",
      "2021-05-07 09:30:00",  4.104,              "questionable",
      "2021-05-07 09:45:00",  4.178,              "questionable"
    ) |>
      dplyr::mutate(
        temperature_date_time = as.POSIXct(temperature_date_time),
        status_id = factor(
          status_id,
          levels = c("reasonable", "questionable", "erroneous"),
          ordered = TRUE
        )
      )
  )

  classified_data <- classified_data |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 05:45:00",  6.257,              3L,
      "2021-05-07 06:00:00",  2.817,              3L,
      "2021-05-07 06:15:00",  2.917,              3L,
      "2021-05-07 06:30:00",  2.817,              3L,
      "2021-05-07 06:45:00",  NA_real_,           NA_integer_,
      "2021-05-07 07:00:00",  2.817,              3L,
      "2021-05-07 07:15:00",  2.917,              3L,
      "2021-05-07 07:30:00",  2.817,              1L,
      "2021-05-07 07:45:00",  2.867,              1L,
      "2021-05-07 08:00:00",  2.877,              2L,
      "2021-05-07 08:15:00",  3.012,              2L,
      "2021-05-07 08:30:00",  NA_real_,           NA_integer_,
      "2021-05-07 08:45:00",  3.124,              2L,
      "2021-05-07 09:00:00",  3.268,              2L,
      "2021-05-07 09:15:00",  3.789,              2L,
      "2021-05-07 09:30:00",  4.104,              2L,
      "2021-05-07 09:45:00",  4.178,              2L
    ) |>
      dplyr::mutate(
        temperature_date_time = as.POSIXct(temperature_date_time)
      )
  )
})

test_that("check against hourly values", {
  data <- tibble::tribble(
    ~temperature_date_time, ~water_temperature,
    "2021-05-07 01:00:00",  6.257,
    "2021-05-07 02:00:00",  2.817,
    "2021-05-07 03:00:00",  2.917,
    "2021-05-07 04:00:00",  2.817,
    "2021-05-07 05:00:00",  2.917,
    "2021-05-07 06:00:00",  2.817,
    "2021-05-07 07:00:00",  2.917,
    "2021-05-07 08:00:00",  2.817,
    "2021-05-07 09:00:00",  2.867,
    "2021-05-07 10:00:00",  2.877,
    "2021-05-07 11:00:00",  10.012,
    "2021-05-07 12:00:00",  3.147,
    "2021-05-07 13:00:00",  3.124,
    "2021-05-07 14:00:00",  3.268,
    "2021-05-07 15:00:00",  4.189,
    "2021-05-07 16:00:00",  4.157,
    "2021-05-07 17:00:00",  4.957,
    "2021-05-07 18:00:00",  10.957,
  ) |>
    dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))

  classified_data <- classify_water_temp_data(
    data
  ) |>
    dplyr::mutate(status_id = as.integer(status_id))

  expect_equal(
    classified_data,
    tibble::tribble(
      ~temperature_date_time, ~water_temperature, ~status_id,
      "2021-05-07 01:00:00",  6.257,              2L,
      "2021-05-07 02:00:00",  2.817,              2L,
      "2021-05-07 03:00:00",  2.917,              2L,
      "2021-05-07 04:00:00",  2.817,              2L,
      "2021-05-07 05:00:00",  2.917,              1L,
      "2021-05-07 06:00:00",  2.817,              1L,
      "2021-05-07 07:00:00",  2.917,              1L,
      "2021-05-07 08:00:00",  2.817,              1L,
      "2021-05-07 09:00:00",  2.867,              3L,
      "2021-05-07 10:00:00",  2.877,              3L,
      "2021-05-07 11:00:00",  10.012,             3L,
      "2021-05-07 12:00:00",  3.147,              3L,
      "2021-05-07 13:00:00",  3.124,              3L,
      "2021-05-07 14:00:00",  3.268,              3L,
      "2021-05-07 15:00:00",  4.189,              2L,
      "2021-05-07 16:00:00",  4.157,              3L,
      "2021-05-07 17:00:00",  4.957,              3L,
      "2021-05-07 18:00:00",  10.957,             3L
    ) |>
      dplyr::mutate(temperature_date_time = as.POSIXct(temperature_date_time))
  )
})
