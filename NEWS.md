# wqbc 0.3.1.9000

## Major changes
- Water Quality Index calculation has been split into a new package `wqindex` (#82)

## Minor changes and bugfixes
- Fixed bug where only outliers were retained in `clean_wqdata(delete_outliers = TRUE)` (#79)
- Retain `ResultLetter` column in `clean_wqdata` and `calc_limits` (#78)
- Better options for dealing with values below detection limits more robustly
- More columns retained and option to keep even more in `tidy_ems_data()` and 
`tidy_ec_data()` (#83)
- Fully duplicated records are removed in `tidy_ec_data()` and `tidy_ems_data()`
- New argument `remove_blanks` in `clean_wqdata()` to remove blank records (#90)

# wqbc 0.3.1

- Exported previously internal-only function `convert_values` to do unit conversions (#70).

# wqbc 0.3.0

- Added function `set_non_detects()` to flexibily deal with values below the 
detection limit (#73)
- Added flexibility in dealing with non-detects to `tidy_ems_data()` and `tidy_ec_data()` (#73)

# wqbc 0.2.1

- Fixed a bug where `substitute_variables()` matched too many variables (#72)

# wqbc 0.2.0

- Many changes including functionality for trends and importing and cleaning 
data from the rems package.

# wqbc 0.1.0

- Initial release of package
