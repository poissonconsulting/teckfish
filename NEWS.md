<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

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
