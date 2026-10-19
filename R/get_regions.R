#' Get regions
#'
#' The function downloads and returns a list or dataframe containing geometries
#' and attributes for each requested region where a row corresponds to a region.
#' Regions are organized by type (ie. Ecological Region, Administrative Region,
#' and Hexagonal Grid) and each region type is organized by scale. It is
#' recommended to explore regions using the Biodiversité Québec Atlas web
#' application to determine the region type and scale of interest.
#'
#' IMPORTANT : Running the function without any parameters will return all
#' regions in the atlas. This can be a very large dataset and may take a long
#' time to download. It is recommended to use the `region_fid` parameter to return a
#' specific region or the `type` parameter to return a specific type of region.
#' It is also possible to exclude the geometry from the results by setting the
#' `geometry` parameter to `FALSE`.
#'
#' @param region_fid Optional. `integer` scalar or vector. Returns a dataframe
#' for the region with the specified region_fid
#' @param type Optional. `character` scalar or vector. Returns a dataframe
#' filtered by the atlas `regions_mv` table column `type` specified as parameter.
#' Accepted values are `admin`, `hex`, `cadre_eco`, 'dom_bio', 'watershed' and 'protected'
#' @param scale Optional. `integer` scalar or vector. Returns a dataframe
#' for the region with the specified scale
#' @param geometry Optional. `boolean` which returns a sf object if TRUE
#' or a tibble if FALSE
#' @param ... Optional. scalar or vector. Returns a dataframe filtered by the
#' atlas `regions_mv` table columns specified as parameter.
#'
#' @return An `sf` object when `geometry = TRUE`, a `tibble` otherwise.
#'
#' @export

get_regions <- function(
  region_fid = NULL,
  type = NULL,
  scale = NULL,
  geometry = TRUE,
  ...
) {
  query <- list(...)
  query$table_name <- "regions_mv"
  query$schema <- "api"

  if (! is.null(region_fid)) {
    query$region_fid <- region_fid
  }

  if (! is.null(type)) {
    query$type <- type
  }

  if (! is.null(scale)) {
    query$scale <- scale
  }

  if (geometry) {
    query$output_geometry <- TRUE
  } else {
    query$output_geometry <- FALSE
    query$select <- c("region_fid", "type", "name", "scale",
                      "scale_desc_fr", "scale_desc_en",
                      "updated_at", "id_datasets")
  }

  do.call(db_read_table, query)
}
