#' Get datasets
#'
#' The function downloads and returns a list or dataframe containing a 
#' description for each requested dataset where a row corresponds to an
#' record. The function returns a dataframe for all datasets if no
#' parameters are specified. The function filters returned datasets record by
#' attributes corresponding to atlas table columns specified as parameters
#' (ie. `id`, `title`, `original_source`, `etc`)
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
#' # Returns all datasets filtered by the column `original_source`
#' datasets <- get_datasets(original_source = "eBird")
#'
#' # Returns all datasets filtered by the column id corresponding to eBird
#' datasets <- get_datasets(id = 55:102)
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

  datasets <- do.call(get_table_data, query)
  return(datasets)
}