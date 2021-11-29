#' Get bird presence absence
#'
#' The function downloads and returns a dataframe containing presence and
#' absence (values TRUE or FALSE) with a single record per row for the bird
#' species specified by `id_taxa`.
#'
#' The absence are infered from all sampling points related to birds observed
#' on Quebec territory whose observations were obtained from datasets
#' defined as `exhaustive`.
#'
#' @param id_taxa Optional. `integer` scalar or vector. Returns a dataframe
#' for the observations related to the `id_taxa`
#' @param .cores Optional. `integer` default `4`. Number of cores used to
#' parallelize and improve rapidity
#' @return `tibble` with rows associated with Atlas observations
#'
#' @examples
#' # Returns all taxa filtered by the column `id_taxa` values
#' pres_abs <- get_bird_presence_absence(id_taxa = 188)
#'
#' @export

get_bird_presence_absence <- function(
  taxa_name,
  .cores = 4,
  .page_limit = 100000
) {
  query <- list()
  query$endpoint <- "rpc/get_bird_presence_absence"
  query$.schema <- "api"
  query$.cores <- .cores
  query$.page_limit <- .page_limit
  query$.page_parameters <- list(limit = "page_limit", offset = "page_offset")
  query$.n_pages <- ceiling(
    endpoint_range_count(
      "bird_sampling_points", .schema = "api"
      ) / query$.page_limit
    )
  query$taxa_name <- taxa_name

  results <- do.call(get_gen, query)
  return(results)
}