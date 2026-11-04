# ratlas 3.0.0

## Breaking changes

* Renaming of ids: id_observations, id_regions, id_datasets
* within_quebec deprecated, as only quebec observations are included
* get_timeseries() function removed
* db_read_table() and db_call_function() are now internal functions

## New features

* Spatial filtering of observations by custom polygon (#62)

## Minor improvements and fixes

* Stricter validation of input parameters.
* Updated tests to match the new endpoints.

# ratlas 2.1.2

* Last release before the NEWS file was introduced.
