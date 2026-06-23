#' Get taxa
#'
#' The function downloads and returns a list or dataframe containing a taxonomic
#' description for each requested species where a row corresponds to an
#' individual species. The function returns a dataframe for all species if no
#' parameters are specified. The function filters returned taxa record by
#' attributes corresponding to atlas table columns specified as parameters
#' (ie. `id`, `scientific_name`, `group_fr`, `lemv_status`, `etc`)
#' with accepted values either being scalar or vector for single or multiple
#' records
#'
#' @param id Optional. `integer` scalar or vector. Returns a dataframe for the
#' taxon with the specified id
#' @param group_name Optional. `char` scalar or vector, Returns a dataframe for
#' the taxa belonging to the specified group(s). Must be one (or several) of the
#' following accepted values, written exactly in uppercase:
#' `AMPHIBIANS`, `REPTILES`, `BIRDS`, `MAMMALS`, `FISH`, `VASCULAR_PLANTS`,
#' `NON_VASCULAR_PLANTS`, `ARTHROPODS`, `FUNGI`, `ALGAE`, `MOLLUSKS`,
#' `OTHER_INVERTEBRATES`, `MICROORGANISMS`.
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
#' # Return taxa filtered by the atlas table column `group_fr`
#' results <- get_taxa(group_fr = "Amphibiens")
#' @export

GROUPS <- c("AMPHIBIANS", "REPTILES", "BIRDS", "MAMMALS", "FISH",
            "VASCULAR_PLANTS", "NON_VASCULAR_PLANTS", "ARTHROPODS",
            "FUNGI", "ALGAE", "MOLLUSKS", "OTHER_INVERTEBRATES", "MICROORGANISMS")

get_taxa <- function(
  id = NULL,
  group_name = NULL,
  scientific_name = NULL,
  match_name = NULL,
  ...
) {
  query <- list(...)
  query$table_name <- "taxa"
  query$schema <- "api"

  if (is.null(id) && is.null(group_name) && is.null(scientific_name) && is.null(match_name)) {
    stop("Bad input: must provide either id_taxa_obs, a group name or a scientific name")
  }

  if (! is.null(id)) {
    return(db_read_table(table_name = "taxa", schema = "api", id_taxa_obs = id))
  }

  if (! is.null(group_name)) {
    if (!all(group_name %in% GROUPS)) stop(paste("Bad input: group_name must be one of", paste(GROUPS, collapse = ", ")))
    match_group <- lapply(
      group_name,
      function(x) {
        db_call_function(function_name = "get_taxa_from_group", schema = "api", taxa_group_short = x)
      }
    ) |> dplyr::bind_rows()
    return(match_group)
  }

  if (! is.null(scientific_name)) {
    match_taxa <- lapply(
      scientific_name,
      function(x) {
        validated_names <- db_call_function(function_name = "taxa_autocomplete", schema = "atlas_api", name = x)
        if (nrow(validated_names) == 0) {
          warning(paste("No match found for", x))
          return(NULL)
        }
        db_call_function("match_taxa", schema = "api", taxa_name = validated_names$scientific_name[1])
      }
    ) |> dplyr::bind_rows()
    if (nrow(match_taxa) == 0) stop("No match found")
    return(match_taxa)
  }
}