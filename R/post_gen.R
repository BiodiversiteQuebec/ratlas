SCHEMA_VALUES <- c("public", "api", "public_api")

#' Generic function to post data into Atlas databases
#'
#' Data records attributes (list names or columns) must corresponds to the
#' table columns it is sent to.
#' (ie. taxa, observations, datasets, etc).
#'
#' This function is designed to interface with a web API deployed with PostgREST
#'
#' @param endpoint `character`. Name of the atlas data object destination.
#' Corresponds to the name of a within Atlas Postgresql database, stored within
#' schema `public` ou `api`
#' @param data `list` or `data.frame`. Either a single record as a `list` with
#' names as attributes or multiple records as a `data.frame` with attributes as
#' columns with attibutes corresponding to the destination column tables.
#' @param ... `character` or `numeric` scalar or vector.
#' Additional parameters to provide to the request. May correspond to postgrest
#' http request parameters and syntax.
#' @param .schema `character` Schema from the database where is located the data
#' object is located. Accept either values `api` or `public` (default)
#' @param .page_limit `integer` Count of objects returned through pagination
#' @param .token  `character` Bearer token providing access to the web api
#' @param .cores `integer` default `4`. Number of cores used to parallelize and
#' improve rapidity
#' @return 

#' @export

post_gen <- function(
    endpoint,
    data,
    ...,
    .schema = "public",
    .page_limit = 50000,
    .token = ATLAS_API_TOKEN(),
    .cores = 4) {

    # Argument validation
    if (! .schema %in% SCHEMA_VALUES) {
        stop("Bad input: Unexpected value for argument `.schema`")
        }
    # Prepare HTTP request with url, header abd query parameters
    url <- httr::modify_url(ATLAS_API_V2_HOST(),
        path = paste(
            httr::parse_url(ATLAS_API_V2_HOST())$path,
            endpoint, sep = "/"))
    if (is.null(nrow(data))) {
        # Case of a list
        body <- jsonlite::toJSON(data)
    } else {
        # Case of a data.frame
        body <- jsonlite::toJSON(as.data.frame(data))
    }
    
    header <- list(
        Authorization = paste("Bearer", .token),
        `User-Agent` = USER_AGENT(), # defined in zzz.R
        Prefer = "count=planned", # header parameter from postgrest
        `Content-type` = "application/json;charset=UTF-8"
        )
    query <- list(...)
    response <- httr::POST(
        url = url,
        config = do.call(httr::add_headers, header),
        body = body,
        query = query
    )
    postgrest_stop_if_err(response)
    return(NULL)
}

postgrest_stop_if_err <- function(response) {
  if (httr::http_error(response)) {
    stop(httr::message_for_status(response, httr::content(response)$message))
  }
}