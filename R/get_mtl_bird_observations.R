#' Get mtl bird observations
#'
#' The function downloads and returns a list or dataframe containing
#' observations with a single record per row. The function returns a dataframe
#' containing all records if no parameters are specified (STRONGLY DISCOURAGED
#' DUE TO LARGE AMOUNT OF OBSERVATIONS CONTAINED WITHIN ATLAS).
#' The function filters returned records by attributes corresponding to atlas
#' table columns specified as parameters (ie. `id`, `year_obs`, `id_taxa`,
#' `id_datasets`, `id_variables`, `etc`) with accepted values either being
#' scalar or vector for single or multiple records.
#' 
#' The absence are infered from all sampling points related to birds observed
#' on Montreal territory whose observations were obtained from datasets
#' defined as `exhaustive`.
#'
#' @param id Optional. `integer` scalar or vector. Returns a dataframe for the
#' observation with the specified id
#' @param year Optional. `integer` scalar or vector. Returns a dataframe for
#' the observations related to the `id_taxa`
#' @param id_taxa Optional. `integer` scalar or vector. Returns a dataframe
#' for the taxa_obs record related to the value. `id_taxa` is translated to
#' `id_taxa_obs`
#' @param ... Optional. scalar or vector. Returns a dataframe filtered by the
#' atlas `observations` table columns specified as parameter
#' (ie. `id_datasets`, `id_variables`, `month_obs`, `day_obs`)
#' @param .cores Optional. `integer` default `4`. Number of cores used to
#' parallelize and improve rapidity
#' @return `tibble` with rows associated with Atlas observations
#'
#' @examples
#' # Returns all observations from 2010 to 2015 for taxa
#' "Leuconotopicus villosus" (`id_taxa` = 6450) from ebird datasets
#' (`id_datasets` = 55:102)
#' obs <- get_observations(
#'  id_taxa = 6450, year = 2010:2015, id_datasets = 55:102)
#' 
#' @export

get_mtl_bird_observations <- function(
  id = NULL,
  year = NULL,
  id_taxa = NULL,
  ...
) {
  query <- list(...)
  query$endpoint <- "mtl_bird_exhaustive_observations"
  query$.schema <- "api"

  if (! is.null(id)) {
    query$id <- id
  }
  if (! is.null(id_taxa)) {
    query$id_taxa_obs <- id_taxa
  }
  if (! is.null(year)) {
    query$year_obs <- year
  }

  observations <- do.call(get_gen, query)
  observations$geom <- geojsonsf::geojson_sf(jsonlite::toJSON(
    observations$geom)
  )$geometry
  return(observations)
}