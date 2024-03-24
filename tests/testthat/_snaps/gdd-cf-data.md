# gdd_cf_data works

    Code
      gdd
    Output
      # A tibble: 1 x 4
      # Groups:   year [1]
         year start_dayte end_dayte   gsdd
        <int> <date>      <date>     <dbl>
      1  2019 1972-03-19  1972-12-16 3605.

---

    Code
      gdd
    Output
      # A tibble: 1 x 4
      # Groups:   year [1]
         year start_dayte end_dayte   gsdd
        <int> <date>      <date>     <dbl>
      1  2019 1972-03-16  1972-12-13 3617.

# gdd_cf_data works shortened

    Code
      gdd
    Output
      # A tibble: 1 x 4
      # Groups:   year [1]
         year start_dayte end_dayte   gsdd
        <int> <date>      <date>     <dbl>
      1  2019 1972-03-19  1972-12-15 3592.

# gdd_cf_data works very shortened

    Code
      gdd
    Output
      # A tibble: 1 x 4
      # Groups:   year [1]
         year start_dayte end_dayte   gsdd
        <int> <date>      <date>     <dbl>
      1  2019 1972-03-19  1972-06-17  77.0

# gdd_cf_data NA if stops before

    Code
      gdd
    Output
      # A tibble: 1 x 4
      # Groups:   year [1]
         year start_dayte end_dayte   gsdd
        <int> <date>      <date>     <dbl>
      1  2019 1972-03-19  1972-12-15 3592.

# gdd_cf_data NA if missing

    Code
      gdd
    Output
      # A tibble: 1 x 4
      # Groups:   year [1]
         year start_dayte end_dayte   gsdd
        <int> <date>      <date>     <dbl>
      1  2019 1972-03-19  1972-12-15 3592.

