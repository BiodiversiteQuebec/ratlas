#' Get timeseries
#'
#' The function downloads and returns a spatial feature (sf) dataframe 
#' containing time-series, for Quebec, where each row corresponds to an 
#' individual population. 
#'
#' @param id Returns a dataframe for the population with the specified id
#' @param id_taxa Returns a dataframe containing time series with species 
#' @param ... Optional. scalar or vector. Returns a dataframe filtered by the
#' atlas `observations` table columns specified as parameter
#'
#' @return A list or dataframe with rows corresponding to individual population.
#' @export
get_timeseries <- function(
  id = NULL,
  id_taxa = NULL,
  ...
) {
  query <- list(...)
  query$table_name <- "time_series"
  query$schema <- "api"

  if (! is.null(id)) {
    query$id <- id
  }
  if (! is.null(id_taxa)) {
    query$id_taxa <- id_taxa
  }

  timeseries <- do.call(get_table_data, query)

  return(timeseries)
}