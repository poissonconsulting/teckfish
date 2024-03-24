# gdd_cf_data works

    Code
      gdd
    Output
      # A tibble: 1 x 4
      # Groups:   year [1]
         year start_index end_index  gsdd
        <int>       <int>     <dbl> <dbl>
      1  2019          79       273 3605.

# gdd_cf_data works shortened

    Code
      gdd
    Output
      # A tibble: 1 x 4
      # Groups:   year [1]
         year start_index end_index  gsdd
        <int>       <int>     <dbl> <dbl>
      1  2019          79       272 3592.

# gdd_cf_data works very shortened

    Code
      gdd
    Output
      # A tibble: 1 x 4
      # Groups:   year [1]
         year start_index end_index  gsdd
        <int>       <int>     <dbl> <dbl>
      1  2019          79        91  77.0

# gdd_cf_data NA if stops before

    Code
      gdd
    Output
      # A tibble: 1 x 4
      # Groups:   year [1]
         year start_index end_index  gsdd
        <int>       <int>     <dbl> <dbl>
      1  2019          79       272 3592.

# gdd_cf_data NA if missing

    Code
      gdd
    Output
      # A tibble: 1 x 4
      # Groups:   year [1]
         year start_index end_index  gsdd
        <int>       <int>     <dbl> <dbl>
      1  2019          79       272 3592.

