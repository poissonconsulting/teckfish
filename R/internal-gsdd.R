.gsdd_data <- function(
    x, 
    start_date, 
    end_date, 
    ignore_truncation,
    ...,
    span, 
    tails,
    gdd = FALSE) {
  check_data(x, list(date = dttr2::dtt_date("1970-01-01"), temperature = c(1, NA)))
  chk_date(start_date)
  chk_date(end_date)
  chk_whole_number(span)
  chk_gte(span)
  
  end_dayte <- dttr2::dtt_dayte(end_date, start = start_date)
  start_dayte <- dttr2::dtt_dayte(start_date, start = start_date)
  
  x <- x |>
    dplyr::mutate(
      date = dttr2::dtt_date(.data$date)) |>
    check_key("date", x_name = "x") |>
    dplyr::arrange(.data$date)
  
  if(span > 0) {
    x <- x |>
      dplyr::mutate(temperature = interpolate_numeric_vector(.data$temperature, span = span, tails = tails))
  }
  
  x <- x |>
    dplyr::mutate(
      year = dttr2::dtt_study_year(.data$date, start = start_date),
      year = stringr::str_extract(.data$year, "^\\d{4,4}"),
      year = as.integer(.data$year),
      dayte = dttr2::dtt_dayte(.data$date, start = start_date)) |>
    dplyr::filter(.data$dayte >= start_dayte, .data$dayte <= end_dayte) |>
    dplyr::group_by(.data$year) |>
    dplyr::arrange(.data$dayte)
  
  if(!gdd) {
    x <- x |>
      dplyr::summarise(gsdd = gsdd_cf(
        .data$temperature,     
        ignore_truncation = ignore_truncation, ...), .groups = "keep") |>
      dplyr::ungroup()
    return(x)
  }
  .NotYetImplemented()
}