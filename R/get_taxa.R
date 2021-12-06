#' Get taxa
#'
#' The function downloads and returns a list or dataframe containing a taxonomic
#' description for each requested species where a row corresponds to an
#' individual species. The function returns a dataframe for all species if no
#' parameters are specified. The function filters returned taxa record by
#' attributes corresponding to atlas table columns specified as parameters
#' (ie. `id`, `scientific_name`, `col`, `gbif`, `etc`)
#' with accepted values either being scalar or vector for single or multiple
#' records
#'
#' @param id Optional. `integer` scalar or vector. Returns a dataframe for the
#' taxon with the specified id
#' @param scientific_name Optional. `char` scalar or vector. Returns a dataframe
#' for the taxon with the specified scientific name
#' @param ... Optional. scalar or vector. Returns a dataframe filtered by the
#' atlas `taxa` table columns specified as parameter
#' 
#' @return `tibble` with rows associated with Atlas data object
#' 
#' @examples
#' # Returns all available taxa records in atlas
#' taxa <- get_taxa()
#'
#' # Returns all taxa filtered by the column id values
#' taxa <- get_taxa(id = c(188, 201, 294, 392))
#'
#' # Returns taxa record for the scientific name
#' taxa <- get_taxa(scientific_name = "Cyanocitta cristata")
#'
#' # Return taxa filtered by the atlas table column `col`
#' results <- get_taxa(col = 35520954)
#' @import magrittr
#' @export

get_taxa <- function(
  id = NULL,
  scientific_name = NULL,
  ...
) {
  query <- list(...)
  query$endpoint <- "taxa"
  query$.schema <- "api"

  if (! is.null(id)) {
    query$id_taxa_obs <- id
  }
  if (! is.null(scientific_name)) {
    match_taxa <- lapply(scientific_name,
    FUN = function(taxa_name) get_gen(
      "rpc/match_taxa_obs", taxa_name = taxa_name)
    ) %>% dplyr::bind_rows()
    query$id_taxa_obs <- match_taxa$id
  }

  taxa <- do.call(get_gen, query)
  return(taxa)
}