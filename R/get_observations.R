#' Get observations
#'
#' The function downloads and returns either an sf or tibble containing
#' observations with a single record per row. The function returns
#' all records if no parameters are specified (STRONGLY DISCOURAGED
#' DUE TO LARGE AMOUNT OF OBSERVATIONS CONTAINED WITHIN ATLAS).
#' The function filters returned records by attributes corresponding to atlas
#' table columns specified as parameters (ie. `id_observations`, `year`,
#' `id_taxa_obs`, `id_datasets`, `id_variables`, `etc`) with accepted values
#' either being scalar or vector for single or multiple records.
#'
#' @param id_observations Optional. `integer` scalar or vector. Returns a dataframe for the
#' observation with the specified id
#' @param year Optional. `integer` scalar or vector. Returns a dataframe for
#' the observations related to the specified `year`
#' @param id_datasets Optional. `uuid` scalar or vector. Returns a dataframe for
#' the observations related to the specified `id_datasets`. The parameter accepts
#' the output of ratlas::get_datasets()
#' @param id_taxa_obs Optional. `integer` scalar or vector. Returns a dataframe
#' for the taxa_obs record related to the value. The parameters accepts
#' the output of ratlas::get_taxa()
#' @param scientific_name Optional. `char` scalar or vector. Returns a dataframe
#' for the scientific_name records related to the value. The parameter accepts
#' the output of ratlas::get_taxa()
#' @param region_fid Optional. `integer` scalar or vector. Returns observations
#' for the region with the specified region_fid. The parameter accepts the
#' output of ratlas::get_regions()
#' @param polygon Optional. `sf, sfc, bbox` objects accepted and a valid associated
#' CRS is required. Multiple features will be merged, and points on the boundaries
#' will be included with an intersect.
#' @param geometry Optional. `logical` default `TRUE`. If `TRUE`,
#' returns an `sf` object using the `geometry` column from the table. IF `FALSE`
#' returns a tibble.
#' @param ... Optional. scalar or vector. Returns a dataframe filtered by the
#' atlas `observations_mv` table columns specified as parameter
#' (ie. `id_variables`, `month_obs`, `day_obs`)
#'
#' @return An `sf` object when `geometry = TRUE`, a `tibble` otherwise.
#' A 0-row `tibble` is returned when no observations match.
#' @export

get_observations <- function(id_observations = NULL, year = NULL, id_datasets = NULL,
                             id_taxa_obs = NULL, scientific_name = NULL,
                             region_fid = NULL, polygon = NULL,
                             geometry = TRUE, ...) {

  # Verify that conflicting parameters are not provided together
  if (!is.null(region_fid) && !is.null(polygon)) stop("Bad input: use either `region_fid` or `polygon`, not both")
  if (!is.null(id_taxa_obs) && !is.null(scientific_name)) stop("Bad input: use either `id_taxa_obs` or `scientific_name`, not both")

  # Allow input parameter to be the output of other ratlas functions
  # like get_taxa(), get_datasets() and get_regions()
  if (is.data.frame(scientific_name)) {
    if (!"valid_scientific_name" %in% names(scientific_name)) stop("Bad input: `scientific_name` data frame must have a `valid_scientific_name` column")
    scientific_name <- unique(scientific_name$valid_scientific_name)
  }
  if (is.data.frame(id_taxa_obs)) {
    if (!"id_taxa_obs" %in% names(id_taxa_obs)) stop("Bad input: `id_taxa_obs` data frame must have an `id_taxa_obs` column")
    id_taxa_obs <- unique(id_taxa_obs$id_taxa_obs)
  }
  if (is.data.frame(id_datasets)) {
    if (!"id_datasets" %in% names(id_datasets)) stop("Bad input: `id_datasets` data frame must have an `id_datasets` column")
    id_datasets <- unique(id_datasets$id_datasets)
  }
  if (is.data.frame(region_fid)) {
    if (!"region_fid" %in% names(region_fid)) stop("Bad input: `region_fid` data frame must have a `region_fid` column")
    region_fid <- unique(region_fid$region_fid)
  }

  # Build the filters
  filters <- list(...)
  if (!is.null(id_observations)) filters$id_observations <- id_observations
  if (!is.null(year)) filters$year_obs <- year
  if (!is.null(id_datasets)) filters$id_datasets <- id_datasets
  if (!is.null(id_taxa_obs)) filters$id_taxa_obs <- id_taxa_obs
  if (!is.null(scientific_name)) filters$valid_scientific_name <- scientific_name
  filters$order <- "id_observations"

  # Safeguard if any parameters are provided empty
  if (!is.null(region_fid) && length(region_fid) == 0) stop("Bad input: empty value for region_fid")
  empty <- names(Filter(function(v) length(v) == 0, filters))
  if (length(empty) > 0) stop("Bad input: empty value for ", paste(empty, collapse = ", "))

  if (!geometry) {
    sample <- db_read_table("observations_mv", schema = "api", limit = 1)
    filters$select <- grep("^geom", colnames(sample), value = TRUE, invert = TRUE)
  }

  if (!is.null(polygon)) {
    db_call_function("filter_observations_wkt", schema = "api",
                     output_geometry = geometry,
                     wkt = as_wkt_polygon(polygon),
                     .filters = filters, .paginate = TRUE)
  } else if (!is.null(region_fid)) {
    db_call_function("filter_observations_region", schema = "api",
                     output_geometry = geometry,
                     region_fid = as.list(region_fid),
                     .filters = filters, .paginate = TRUE)
  } else {
    do.call(db_read_table, c(list(table_name = "observations_mv", schema = "api",
                                  output_geometry = geometry), filters))
  }
}
