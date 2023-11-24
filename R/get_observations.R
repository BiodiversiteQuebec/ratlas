#' Get observations
#'
#' The function downloads and returns a list or dataframe containing
#' observations with a single record per row. The function returns a dataframe
#' containing all records if no parameters are specified (STRONGLY DISCOURAGED
#' DUE TO LARGE AMOUNT OF OBSERVATIONS CONTAINED WITHIN ATLAS).
#' The function filters returned records by attributes corresponding to atlas
#' table columns specified as parameters (ie. `id`, `year_obs`, `id_taxa`,
#' `id_datasets`, `id_variables`, `fid_region`, `etc`) with accepted values 
#' either being scalar or vector for single or multiple records.
#'
#' @param id Optional. `integer` scalar or vector. Returns a dataframe for the
#' observation with the specified id
#' @param year Optional. `integer` scalar or vector. Returns a dataframe for
#' the observations related to the `id_taxa`
#' @param id_taxa Optional. `integer` scalar or vector. Returns a dataframe
#' for the taxa_obs record related to the value. `id_taxa` is translated to
#' `id_taxa_obs`
#' @param fid_region Optional. `integer` scalar or vector. Returns observations
#' for the region with the specified id
#' @param within_quebec Optional. `logical` default NULL. If `TRUE`, returns
#' only observations within Quebec. If `FALSE`, returns only observations
#' outside Quebec. If `NULL`, returns all observations.
#' @param ... Optional. scalar or vector. Returns a dataframe filtered by the
#' atlas `observations` table columns specified as parameter
#' (ie. `id_datasets`, `id_variables`, `month_obs`, `day_obs`)
#' @param .cores Optional. `integer` default `4`. Number of cores used to
#' parallelize and improve rapidity
#' @return `tibble` with rows associated with Atlas observations
#'
#' @import magrittr
#' @export

get_observations <- function(
  id = NULL,
  year = NULL,
  id_taxa = NULL,
  region_fid = NULL,
  geometry = TRUE,
  within_quebec = NULL,
  ...
) {
  query <- list(...)
  query$table_name <- "observations"
  query$schema <- "api"

  if (! is.null(id)) {
    query$id <- id
  }
  if (! is.null(id_taxa)) {
    query$id_taxa_obs <- id_taxa
  }
  if (! is.null(year)) {
    query$year_obs <- year
  }
  if (! is.null(within_quebec)) {
    if (within_quebec) {
      query$within_quebec <- "true"
    } else {
      query$within_quebec <- "false"
    }
  }

  if (! is.null(region_fid)) {
    query$table_name <- "observations_regions"
    query$region_fid <- region_fid
  }

  if (geometry) {
    query$output_geometry <- "true"
  } else {
    query$output_geometry <- "false"

    # List of columns from single call
    sample_query <- query
    sample_query$limit <- 1
    sample <- do.call(db_read_table, sample_query)
    sample_cols <- colnames(sample)
    # Remove cols that starts with 'geom'
    sample_cols <- sample_cols[!grepl("^geom", sample_cols)]
    query$select <- sample_cols
  }

  # Get observations
  observations <- do.call(db_read_table, query)

  return(observations)
}