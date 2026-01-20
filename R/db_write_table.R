SCHEMA_VALUES <- c("public", "api", "atlas_api", "indicators")

#' Generic function to post data into Atlas databases
#'
#' Data records attributes (list names or columns) must corresponds to the
#' table columns it is sent to.
#' (ie. taxa, observations, datasets, etc).
#'
#' This function is designed to interface with a web API deployed with PostgREST
#'
#' @param table_name `character`. Name of the atlas data object destination.
#' Corresponds to the name of a within Atlas Postgresql database, stored within
#' schema `public` ou `api`
#' @param data `list` or `data.frame`. Either a single record as a `list` with
#' names as attributes or multiple records as a `data.frame` with attributes as
#' columns with attibutes corresponding to the destination column tables.
#' @param ... `character` or `numeric` scalar or vector.
#' Additional parameters to provide to the request. May correspond to postgrest
#' http request parameters and syntax.
#' @param schema `character` Schema from the database where is located the data
#' object is located. Accept either values `api` or `public` (default)
#' @param .page_limit `integer` Count of objects returned through pagination
#' @param .token  `character` Bearer token providing access to the web api
#' @param .cores `integer` default `4`. Number of cores used to parallelize and
#' @param .header `list` Additional headers to provide to the request.
#' improve rapidity
#' @export

db_write_table <- function(
    table_name,
    data,
    ...,
    schema = "public",
    host = ATLAS_API_V4_HOST(),
    .page_limit = 50000,
    .token = ATLAS_API_TOKEN(),
    .cores = 4,
    .header = list()) {

  # Argument validation
  if (! schema %in% SCHEMA_VALUES) {
    stop("Bad input: Unexpected value for argument `schema`")
  }

  # Set the url
  url <- format_url(table_name, host)

  # Prepare header parameters
  header <- format_header(schema, .token = .token, method = "POST")

  # Overrride default header with user provided ones
  header <- modifyList(header, .header)

  # Post to the database
  response <-  postgrest_post(url = url, header = header, data = data, params = list(...), page_limit = .page_limit)

  # Check response for error
  postgrest_stop_if_err(response)

  # Parse response using
  out <- postgrest_resp_to_data(response)

  return(out)
}


postgrest_post <- function(url, header, data, params = list(), page_limit = NULL) {
  # Update params with page limit
  if (!is.null(page_limit)) {
    params$limit <- page_limit
  }

  # Prepare the request
  req <- httr2::request(url) |>
    httr2::req_headers(!!!header) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_body_json(data, na = "null")

  # Make the request
  response <- req |>
    httr2::req_error(is_error = \(resp) FALSE) |> # Disable error throwing and catch it after
    httr2::req_perform()

  return(response)
}
