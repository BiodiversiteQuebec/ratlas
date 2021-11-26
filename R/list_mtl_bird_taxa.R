#' List Montreal bird taxa
#'
#' The function downloads and returns a dataframe containing presence and
#' absence (values TRUE or FALSE) with a single record per row for the bird
#' species specified by `id_taxa`.
#'
#' The absence are infered from all sampling points related to birds observed
#' on Montreal territory whose observations were obtained from datasets
#' defined as `exhaustive`.
#'
#' @param rank Optional. `character` Filter by rank, such as `family`, `species`
#' , `class`, etc.
#' @param source_name Optional. `character` Filter by taxonomic source.
#' Accepted values are `ITIS`, `Catalogue of Life` and `GBIF Backbone Taxonomy`
#' @param valid Optional. `logical`
#' @param ... Optional. scalar or vector. Returns a dataframe filtered by the
#' atlas `taxa_ref` table columns specified as parameter
#' @return `tibble` with rows associated with Atlas `taxa_ref` records
#'
#' @examples
#' # Returns all bird species of different sources
#' pres_abs <- get_mtl_bird_presence_absence(rank = "species")
#'
#' @export

list_mtl_bird_taxa <- function(
    rank = NULL,
    source_name = NULL,
    valid = NULL,
    ...
    ) {
  query <- list(...)
  query$endpoint <- "bird_mtl_taxa_ref"
  query$.schema <- "api"

  if (! is.null(rank)) {
    query$rank <- rank
  }
  if (! is.null(source_name)) {
    query$source_name <- source_name
  }
  if (! is.null(valid)) {
    query$valid <- valid
  }


  results <- do.call(get_gen, query)
  return(results)
}