#' Generic function to directly access data in Atlas database
#'
#' Return data objects stored in tables and views in the Atlas database.
#'
#' This function is designed to interface with a web API deployed with
#' PostgREST.
#'
#' @param table_name `character`. Name of the table to be accessed.
#' @param schema `character` default 'public'. Schema from the database where
#' the table is located. Can be either `public`, `api` or `atlas_api`.
#' @param output_geometry Optional. `logical` default `FALSE`. If `TRUE`,
#' returns an `sf` object using the `geometry` column from the table.
#' @param output_flatten `logical` default `TRUE`. If `TRUE`, returns a
#' `data.frame` object with nested objects flattened.
#' @param limit Optional. `integer` default `NULL`. Maximum number of rows to
#' return. If `NULL`, all rows are returned. From the PostgREST API syntax.
#' @param select Optional. `character` default `NULL`. List of columns to
#' return. All columns are returned if `NULL`. From the PostgREST API syntax.
#' @param ... Additional parameters to be passed as query to the API table_name.
#' @param .host Optional. `character` Atlas API host url.
#' @param .token Optional. `character` Bearer token providing access to the web api.
#' @param .max_active Optional. `integer` default `4`. Number of active connections
#' that are alive at all time to process the request.
#' @param .page_limit Optional. `integer` default `10000`. Maximum number of
#' rows to download per page. This determines how many requests (pages) the
#' download is split into.
#' @param .header `list` Additional headers to provide to the request.
#'
#' @return `tibble` or `sf` with rows associated with Atlas data object.
#' @keywords internal

db_read_table <- function(table_name,
                          schema = "public",
                          output_geometry = FALSE,
                          output_flatten = TRUE,
                          limit = NULL,
                          select = NULL,
                          ...,
                          .host = ATLAS_API_V4_HOST(),
                          .token = ATLAS_API_TOKEN(),
                          .max_active = 4,
                          .page_limit = 10000,
                          .header = list()) {
  # Argument validation
  if (!schema %in% SCHEMA_VALUES) {
    stop("Bad input: Unexpected value for argument `schema`")
  }

  # Set the url
  url <- format_url(endpoint = table_name, host = .host)

  # Prepare query parameters
  query <- postgrest_query_filter(list(...))
  if (!is.null(select)) query$select <- paste0(select, collapse = ",")

  # Prepare header parameters
  header <- format_header(schema, token = .token, method = "GET")

  # Overrride default header with user provided ones
  header <- modifyList(header, .header)

  if (output_geometry) header$`Accept` <- "application/geo+json"

  postgrest_read_paged(url, query, header,
                       limit = limit, output_flatten = output_flatten,
                       page_limit = .page_limit, max_active = .max_active)
}