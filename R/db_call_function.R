SCHEMA_VALUES <- c("public", "api", "atlas_api")

#' Generic function to access data from functions in Atlas database
#'
#' Return data objects stored obtained through endpoints corresponding to the
#' name of the database function.
#'
#' This function is designed to interface with a web API deployed with PostgREST
#'
#' @param name `character`. Name of the atlas function to be accessed.
#' data. Can be either `api`, `public` or `atlas_api`.
#' @param ... `character` or `numeric` scalar or vector. Arguments to be passed
#' to the function.
#' @param schema Optional. `character` Schema from the database where is located
#' the data. Can be either `api`, `public` or `atlas_api`.
#' @param output_geometry Optional. `logical` default `FALSE`. If `TRUE`,
#' returns an `sf` object using the `geometry` column from the function output.
#' @param output_flatten Optional. `logical` default `FALSE`. If `TRUE`,
#' returns a `data.frame` object with nested objects flattened.
#' @param .token Optional. `character` Bearer token providing access to the web
#' api.
#' @return `tibble` with rows associated with Atlas data object
#' @export

db_call_function <- function(name,
                              schema = "public",
                              output_geometry = FALSE,
                              output_flatten = TRUE,
                              .token = NULL,
                              ...) {
    # Argument validation
    if (!schema %in% SCHEMA_VALUES) {
        stop("Bad input: Unexpected value for argument `schema`")
    }

    if (is.null(.token)) {
        .token <- ATLAS_API_TOKEN()
    }

    # Prefix the function name with 'rpc' if it is not already the case
    # as required by PostgREST syntax for functions
    if (!grepl("^rpc", name)) {
        # Strip name for "/" or "\" characters
        name <- gsub("[/\\\\]", "", name)
        name <- paste("rpc", name, sep = "/")
    }

    # Prepare HTTP request with url, header abd query parameters
    url <- httr::modify_url(ATLAS_API_V4_HOST(),
        path = paste(
            httr::parse_url(ATLAS_API_V4_HOST())$path,
            name,
            sep = "/"
        )
    )
    body <- list(...)
    header <- list(
        Authorization = paste("Bearer", .token),
        `User-Agent` = USER_AGENT(), # defined in zzz.R
        `Content-type` = "application/json;charset=UTF-8",
        `Content-Profile` = schema
    )

    # Create and send the request
    response <- httr2::request(url) |>
           httr2::req_headers(!!!header) |>
           httr2::req_body_json(body) |>
           httr2::req_perform()

    # Stop if error
    postgrest_stop_if_err(response)

    # Parse response
    out <- postgrest_resp_to_data(response, output_flatten = output_flatten)
    return(out)
}