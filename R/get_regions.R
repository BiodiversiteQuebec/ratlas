#' Get regions
#'
#' The function downloads and returns a list or dataframe containing geometries
#' and attributes for each requested region where a row corresponds to a region.
#' Regions are organized by type (ie. Ecological Region, Administrative Region,
#' and Hexagonal Grfid) and each region type is organized by scale. It is
#' recommended to explore regions using the Biodiversité Québec Atlas web
#' application to determine the region type and scale of interest.
#' 
#' IMPORTANT : Running the function without any parameters will return all
#' regions in the atlas. This can be a very large dataset and may take a long
#' time to download. It is recommended to use the `fid` parameter to return a
#' specific region or the `type` parameter to return a specific type of region.
#' It is also possible to exclude the geometry from the results by setting the
#' `geometry` parameter to `FALSE`.
#'
#' @param fid_region Optional. `integer` scalar or vector. Returns a dataframe
#' for the region with the specified fid
#' @param type Optional. `character` scalar or vector. Returns a dataframe
#' filtered by the atlas `regions` table column `type` specified as parameter.
#' Accepted values are `cadre_eco`, `admin`, and `hex`
#' @param zoom Optional. `integer` scalar or vector. Returns a dataframe
#' @param ... Optional. scalar or vector. Returns a dataframe filtered by the
#' atlas `regions` table columns specified as parameter.
#' 
#'
#' @export

get_regions <- function(
  fid = NULL,
  type = NULL,
  scale = NULL,
  geometry = TRUE,
  ...
) {
  query <- list(...)
  query$table_name <- "regions"
  query$schema <- "public"

  if (! is.null(fid)) {
    query$fid <- fid
  }

  if (! is.null(type)) {
    query$type <- type
  }

  if (! is.null(scale)) {
    query$scale <- scale
  }

  if (geometry) {
    query$output_geometry <- "true"
  } else {
    query$output_geometry <- "false"
    query$select <- c("fid", "type", "scale", "scale_desc", "name")
  }

  regions <- do.call(get_table_data, query)
  return(regions)
}