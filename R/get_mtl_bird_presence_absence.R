#' Get Montreal bird presence absence
#'
#' The function downloads and returns a dataframe containing presence and
#' absence (values TRUE or FALSE) with a single record per row for the bird
#' species specified by `id_taxa`.
#'
#' The absence are infered from all sampling points related to birds observed
#' on Montreal territory whose observations were obtained from datasets
#' defined as `exhaustive`.
#'
#' @param taxa_name. `str` Returns a dataframe
#' for the observations related to the to the`scientific_name` corresponding to
#' any rank.
#' @param .cores Optional. `integer` default `4`. Number of cores used to
#' parallelize and improve rapidity
#' @return `tibble` with rows associated with Atlas observations
#'
#' @examples
#' # Returns all presence absence for the blue jay `Cyanocitta cristata`
#' pres_abs <- get_mtl_bird_presence_absence(taxa_name = "Cyanocitta cristata")
#'
#' @export

get_mtl_bird_presence_absence <- function(
  taxa_name,
  .cores = 4
) {
  query <- list()
  query$endpoint <- "rpc/get_mtl_bird_presence_absence"
  query$.schema <- "api"
  query$.cores <- .cores
  query$taxa_name <- taxa_name

  results <- do.call(get_gen, query)
  return(results)
}