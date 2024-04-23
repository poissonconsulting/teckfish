# gss works

    Code
      gss
    Output
      # A tibble: 1 x 5
      # Groups:   year [1]
         year start_dayte end_dayte   gsdd truncation
        <int> <date>      <date>     <dbl> <chr>     
      1  2019 1971-03-20  1971-11-07 3899. none      

# gss works t2

    Code
      gss
    Output
      # A tibble: 2 x 5
      # Groups:   year [1]
         year start_dayte end_dayte   gsdd truncation
        <int> <date>      <date>     <dbl> <chr>     
      1  2019 1971-04-08  1971-06-04   500 none      
      2  2019 1971-07-15  1971-09-03   800 none      

# gss truncation off

    Code
      gss
    Output
      # A tibble: 0 x 5
      # i 5 variables: year <int>, start_dayte <date>, end_dayte <date>, gsdd <dbl>,
      #   truncation <chr>

# gss truncation on

    Code
      gss
    Output
      # A tibble: 2 x 5
      # Groups:   year [1]
         year start_dayte end_dayte   gsdd truncation
        <int> <date>      <date>     <dbl> <chr>     
      1  2019 1971-04-08  1971-06-04   500 none      
      2  2019 1971-07-15  1971-08-27   780 end       

