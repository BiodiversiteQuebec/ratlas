#' Get datasets
#'
#' The function downloads and returns a list or dataframe containing a
#' description for each requested dataset where a row corresponds to a
#' record. The function returns a dataframe for all datasets if no
#' parameters are specified. The function filters returned datasets record by
#' attributes corresponding to atlas table columns specified as parameters
#' (ie. `id`, `title`, `source_alias`, `etc`)
#' with accepted values either being scalar or vector for single or multiple
#' records
#'
#' @param id Optional. `integer` scalar or vector. Returns a dataframe for the
#' dataset with the specified id
#' @param ... Optional. scalar or vector. Returns a dataframe filtered by the
#' atlas `datasets` table columns specified as parameter
#'
#' @return `tibble` with rows associated with Atlas data object
#'
#' @examples
#' # Returns all available datasets records in atlas
#' datasets <- get_datasets()
#'
#' # Returns all datasets filtered by the column `source_alias`
#' datasets <- get_datasets(source_alias = "GBIF")
#'
#' Returns all datasets filtered by the column id corresponding to all
#' Placettes-échantillons temporaires datasets
#' datasets <- get_datasets(id = c(  "292302c5-32c2-4e14-b437-23ea9695fafe",
#'  "8d865875-dd4d-41b4-9fee-6161ac73c471",
#'  "be1252cf-385d-48d6-9836-232a8458f402",
#'  "8b793526-db15-451b-bdac-6387825e6178"))
#'
#' @export

get_datasets <- function(
  id = NULL,
  ...
) {
  query <- list(...)
  query$table_name <- "datasets"
  query$schema <- "public"

  if (! is.null(id)) {
    query$id <- id
  }

  datasets <- do.call(db_read_table, query)
  return(datasets)
}