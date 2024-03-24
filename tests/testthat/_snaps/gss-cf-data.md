# gss_cf_data works

    Code
      gdd
    Output
      # A tibble: 1 x 4
      # Groups:   year [1]
         year start_dayte end_dayte   gsdd
        <int> <date>      <date>     <dbl>
      1  2019 1972-03-19  1973-01-23 3899.

---

    Code
      gdd
    Output
      # A tibble: 1 x 4
      # Groups:   year [1]
         year start_dayte end_dayte   gsdd
        <int> <date>      <date>     <dbl>
      1  2019 1972-03-16  1973-01-23 3920.

# gss_cf_data shifts by 10

    Code
      gdd
    Output
      # A tibble: 1 x 4
      # Groups:   year [1]
         year start_dayte end_dayte   gsdd
        <int> <date>      <date>     <dbl>
      1  2019 1972-03-09  1973-01-03 3899.

# gss_cf_data preserves if shift start date

    Code
      gdd
    Output
      # A tibble: 1 x 4
      # Groups:   year [1]
         year start_dayte end_dayte   gsdd
        <int> <date>      <date>     <dbl>
      1  2019 1972-03-19  1972-12-23 3899.

---

    Code
      gdd
    Output
      # A tibble: 1 x 4
      # Groups:   year [1]
         year start_dayte end_dayte   gsdd
        <int> <date>      <date>     <dbl>
      1  2019 1971-03-20  1971-11-07 3899.

# gss_cf_data one message

    Code
      gdd
    Output
      # A tibble: 0 x 4
      # i 4 variables: year <int>, start_dayte <date>, end_dayte <date>, gsdd <dbl>

# gss_cf_data works shortened

    Code
      gdd
    Output
      # A tibble: 0 x 4
      # i 4 variables: year <int>, start_dayte <date>, end_dayte <date>, gsdd <dbl>

# gss_cf_data works very shortened

    Code
      gdd
    Output
      # A tibble: 0 x 4
      # i 4 variables: year <int>, start_dayte <date>, end_dayte <date>, gsdd <dbl>

# gss_cf_data NA if stops before

    Code
      gdd
    Output
      # A tibble: 0 x 4
      # i 4 variables: year <int>, start_dayte <date>, end_dayte <date>, gsdd <dbl>

# gss_cf_data NA if missing

    Code
      gdd
    Output
      # A tibble: 0 x 4
      # i 4 variables: year <int>, start_dayte <date>, end_dayte <date>, gsdd <dbl>

