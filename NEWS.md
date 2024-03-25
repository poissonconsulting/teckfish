<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

# teckfish 0.1.1.9000

- Added `gsdd()` and `gdd()`.

- Removed `simulated_data`.

- Soft-deprecated `gsdd_cf()` for `gsdd_vctr()`.

- Switch to gsdd package to calculate gsdd.

- Switched default `pick` argument to `longest` following recommendations from Ecofish and Lotic.


# teckfish 0.1.1

- Switched default `pick` argument to `longest` following recommendations from Ecofish and Lotic.


# teckfish 0.1.0

## Additions

- Added `interpolate_numeric_vector()` function to fill in missing values using linear interpolation.
- Added `freshwaterfish` data from `fishbc` package to provide British Columbian and Alberta fish codes as well as taxonomy and conservation status.

## Modifications

### `gsdd_cf()` Function

- Uses the longest sequence on non-missing values, which must be at least 184 elements (otherwise returns `NA`), to calculate the Growing Season Degree Days.
- Returns the sum of the Growing Season Degree Days for all the growing 'seasons' although the user can use the `pick` argument to specify whether to instead return the GSDD value for the `"biggest"`, `"smallest"`, `"longest"`, `"shortest"`, `"first"` or `"last"` 'season' within the sequence.
- `ignore_truncation` argument of `gssd_cf()` now accepts `"start"` and `"end"` instead of
`"left"` and `"right"`.
- Replaced `quiet = FALSE` argument with `messages = TRUE`.

# teckfish 0.0.1

- Added `gsdd_cf()` function which implements Growing Season Degree Days
algorithm as described by Coleman and Fausch (2007)
- Added `classify_water_temp_data()` function to classify raw water temperature
data as `reasonable`, `questionable`, or `unreasonable` based on 
simple criteria
- Added `simulated_data` data set.
