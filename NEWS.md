<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

# teckfish 0.0.1.9002

- Added `interpolate_numeric_vector()` to fill in missing values using linear interpolation.


# teckfish 0.0.1.9001

- `gsdd_cf()` now requires at least 90 values.
- Renamed "left" and "right" truncation to "start" and "end" truncation.
- Added `na_trim = TRUE` argument to `gsdd_cf()` to trim leading or trailing missing values.
- `gsdd_cf()` by default now returns the sum of all the growing seasons within the period of interest as this is expected to be the most reliable predictor of growth.
- Added `pick` argument to specify whether to pick the "biggest", "smallest", "longest", "shortest", "first" or "last" 'season' or "all" 'seasons'.

# teckfish 0.0.1.9000

- `gsdd_cf()` now requires at least 180 values and returns `NA_real` if any values are `NA_real`.
-  Added `fishbc::freshwaterfish` and `fishbc::fbc_common_name` data.

# teckfish 0.0.1

- Added `gsdd_cf()` function. (#5) which implements Growing Season Degree Days
algorithm as described by Coleman and Fausch (2007)
- Added `classify_water_temp_data()` function to classify raw water temperature
data as `reasonable`, `questionable`, or `unreasonable` based on 
simple criteria
- Added `simulated_data` data set.
