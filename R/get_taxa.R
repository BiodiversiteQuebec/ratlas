#' Get taxa
#'
#' The function downloads and returns a tibble of taxa records matching the
#' input. Each row corresponds to an observed name entry (`id_taxa_obs`) as it
#' appears in source datasets, linked to its `valid_scientific_name`. A single
#' species can therefore span several rows, one per synonym or spelling found
#' in the data. Exactly one of `id_taxa_obs`, `group_name`, `scientific_name`
#' or `vernacular_name` must be provided.
#'
#' @param id_taxa_obs Optional. `integer` scalar or vector. Returns the taxa
#' records with the specified `id_taxa_obs`.
#' @param group_name Optional. `char` scalar or vector, Returns a dataframe for
#' the taxa belonging to the specified group(s). Must be one (or several) of the
#' following accepted values, written exactly in uppercase:
#' `AMPHIBIANS`, `REPTILES`, `BIRDS`, `MAMMALS`, `FISH`, `VASCULAR_PLANTS`,
#' `NON_VASCULAR_PLANTS`, `ARTHROPODS`, `FUNGI`, `ALGAE`, `MOLLUSKS`,
#' `OTHER_INVERTEBRATES`, `MICROORGANISMS`.
#' @param scientific_name Optional. `char` scalar or vector. Returns all taxa
#' records sharing the valid name of each specified scientific name, synonyms
#' included. Names must match exactly (case insensitive).
#' @param vernacular_name Optional. `char` scalar or vector. Returns all taxa
#' records sharing the valid name of each specified vernacular name. Names must
#' match exactly (case insensitive).
#' @param ... Optional. scalar or vector. Additional filters on the returned
#' columns (ie. `rank`, `kingdom`, `sensitive`).
#'
#' @return `tibble` with one row per `id_taxa_obs`.
#'
#' @examples
#' # Returns all taxa filtered by the column id_taxa_obs values
#' taxa <- get_taxa(id_taxa_obs = c(188, 201, 294, 392))
#'
#' # Return taxa filtered by the atlas table column `group_name`
#' results <- get_taxa(group_name = "AMPHIBIANS")
#'
#' # Returns all records linked to the valid name, synonyms included
#' taxa <- get_taxa(scientific_name = "Leuconotopicus villosus")
#'
#' # Return taxa record for the vernacular name
#' taxa <- get_taxa(vernacular_name = "Pic chevelu")
#'
#' @export

get_taxa <- function(
  id_taxa_obs = NULL,
  group_name = NULL,
  scientific_name = NULL,
  vernacular_name = NULL,
  ...
) {
  filters <- list(...)

  if (is.null(id_taxa_obs) && is.null(group_name) && is.null(scientific_name) && is.null(vernacular_name)) {
    stop("Bad input: provide exactly one of `id_taxa_obs`, `group_name`, `scientific_name` or `vernacular_name`")
  }

  if (! is.null(id_taxa_obs)) {
    return(do.call(db_read_table, c(list(table_name = "taxa", schema = "api",
                                         id_taxa_obs = id_taxa_obs), filters)))
  }

  if (! is.null(group_name)) {
    if (!all(group_name %in% GROUPS)) stop(paste("Bad input: group_name must be one of", paste(GROUPS, collapse = ", ")))
    taxa <- lapply(group_name, function(x) {
      db_call_function("get_taxa_from_group", schema = "api", taxa_group_short = x, .filters = filters)
    })
    return(dplyr::distinct(dplyr::bind_rows(taxa)))
  }

  if (! is.null(scientific_name)) {
    return(match_taxa_names(scientific_name, filters = filters))
  }

  if (! is.null(vernacular_name)) {
    return(match_taxa_names(vernacular_name, filters = filters))
  }
}

# Resolves each name through autocomplete: exact matches first, otherwise
# accepts the candidates only if they all point to a single valid name.
# Errors if no name matches, warns for the ones that don't.
match_taxa_names <- function(names, filters = list()) {
  taxa <- lapply(names, function(name) {
    candidates <- db_call_function("taxa_autocomplete", schema = "atlas_api", name = name)
    if (nrow(candidates) == 0) return(NULL)

    is_exact <- tolower(candidates$matched_name) == tolower(name)
    is_exact[is.na(is_exact)] <- FALSE

    if (any(is_exact)) {
      valid_names <- unique(candidates$scientific_name[is_exact])
    } else if (length(unique(candidates$scientific_name)) == 1) {
      valid_names <- unique(candidates$scientific_name)
      message("Matched '", name, "' to '", candidates$matched_name[1], "'")
    } else {
      stop("Bad input: ambiguous name '", name, "', candidates are: ",
           paste(unique(candidates$matched_name), collapse = ", "), call. = FALSE)
    }

    lapply(valid_names, function(x) {
      db_call_function("match_taxa", schema = "api", taxa_name = x, .filters = filters)
    }) |> dplyr::bind_rows()
  })

  found <- !vapply(taxa, is.null, logical(1))
  if (!any(found)) stop("No match found for ", paste(names, collapse = ", "), call. = FALSE)
  if (!all(found)) warning("No match found for ", paste(names[!found], collapse = ", "), call. = FALSE)

  dplyr::distinct(dplyr::bind_rows(taxa))
}
