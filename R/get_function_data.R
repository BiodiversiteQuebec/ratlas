SCHEMA_VALUES <- c("public", "api", "public_api", "atlas_api")

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

get_function_data <- function(name,
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
    url <- httr::modify_url(ATLAS_API_V2_HOST(),
        path = paste(
            httr::parse_url(ATLAS_API_V2_HOST())$path,
            name,
            sep = "/"
        )
    )
    data <- list(...)
    header <- list(
        Authorization = paste("Bearer", .token),
        `User-Agent` = USER_AGENT(), # defined in zzz.R
        `Content-type` = "application/json;charset=UTF-8",
        `Content-Profile` = schema
    )

    # Prepare body
    # If data

    if (length(data) > 0) {
        body <- jsonlite::toJSON(data, auto_unbox = TRUE)
    } else {
        body <- NULL
    }

    # Send request
    response <- httr::POST(
        url = url,
        body = body,
        config = do.call(httr::add_headers, header)
    )

    # Stop if error
    postgrest_stop_if_err(response)

    # Parse response
    out <- postgrest_resp_to_data(response, output_flatten = output_flatten)
    return(out)
}