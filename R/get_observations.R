#' Get observations
#'
#' The function downloads and returns a list or dataframe containing
#' observations with a single record per row. The function returns a dataframe
#' containing all records if no parameters are specified (STRONGLY DISCOURAGED
#' DUE TO LARGE AMOUNT OF OBSERVATIONS CONTAINED WITHIN ATLAS).
#' The function filters returned records by attributes corresponding to atlas
#' table columns specified as parameters (ie. `id`, `year_obs`, `id_taxa_obs`,
#' `id_datasets`, `id_variables`, `fid_region`, `etc`) with accepted values
#' either being scalar or vector for single or multiple records.
#'
#' @param id Optional. `integer` scalar or vector. Returns a dataframe for the
#' observation with the specified id
#' @param year Optional. `integer` scalar or vector. Returns a dataframe for
#' the observations related to the specified `year`
#' @param id_taxa_obs Optional. `integer` scalar or vector. Returns a dataframe
#' for the taxa_obs record related to the value.
#' @param region_fid Optional. `integer` scalar or vector. Returns observations
#' for the region with the specified id
#' @param polygon Optional. `sf, sfc, bbox` objects accepted and a valid associated
#' CRS is required. Multiple features will be merged, and points on the boundaries
#' will be included with an intersect.
#' @param geometry Optional. `logical` default `TRUE`. If `TRUE`,
#' returns an `sf` object using the `geometry` column from the table. IF `FALSE`
#' returns a tibble.
#' @param within_quebec Optional. `logical` default NULL. If `TRUE`, returns
#' only observations within Quebec. If `FALSE`, returns only observations
#' outside Quebec. If `NULL`, returns all observations.
#' @param ... Optional. scalar or vector. Returns a dataframe filtered by the
#' atlas `observations` table columns specified as parameter
#' (ie. `id_datasets`, `id_variables`, `month_obs`, `day_obs`)
#'
#' @return An `sf` object when `geometry = TRUE`, a `tibble` otherwise.
#' A 0-row `tibble` is returned when no observations match.
#' @export

get_observations <- function(id = NULL, year = NULL, id_taxa_obs = NULL,
                             region_fid = NULL, polygon = NULL,
                             geometry = TRUE, within_quebec = NULL, ...) {

  if (!is.null(region_fid) && !is.null(polygon)) stop("Bad input: use either `region_fid` or `polygon`, not both")

  # Filters (assigning NULL leaves them out)
  filters <- list(...)
  filters$id <- id
  filters$id_taxa_obs <- id_taxa_obs
  filters$year_obs <- year
  filters$order <- "id"
  if (!is.null(within_quebec)) filters$within_quebec <- if (within_quebec) "true" else "false"
  if (!is.null(region_fid)) filters$region_fid <- region_fid

  table_name <- if (is.null(region_fid)) "observations" else "observations_regions"

  if (!geometry) {
    sample <- db_read_table(table_name, schema = "api", limit =1)
    filters$select <- grep("^geom", colnames(sample), value = TRUE, invert = TRUE)
  }

  if (is.null(polygon)) {
    do.call(db_read_table, c(list(table_name = table_name, schema = "api",
                                  output_geometry = geometry), filters))
  } else {
    db_call_function("filter_observations_wkt", schema = "api",
                     output_geometry = geometry,
                     wkt = as_wkt_polygon(polygon),
                     .filters = filters, .paginate = TRUE)
  }
}

as_wkt_polygon <- function(polygon) {
  if (inherits(polygon, "bbox")) polygon <- sf::st_as_sfc(polygon)

  geom <- sf::st_geometry(polygon)
  if (is.na(sf::st_crs(geom))) stop("Bad input: `polygon` has no CRS")

  geom <- geom |>
    sf::st_transform(4326) |>
    sf::st_make_valid() |>
    sf::st_union()

  if (!as.character(sf::st_geometry_type(geom)) %in% c("POLYGON", "MULTIPOLYGON")) {
    stop("Bad input: `polygon` must be a POLYGON or MULTIPOLYGON")
  }
  sf::st_as_text(geom, digits = 15)
}
