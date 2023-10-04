SCHEMA_VALUES <- c("public", "api", "public_api", "atlas_api")
POSTGREST_QUERY_PARAMETERS <- c(
    "select", "limit", "offset"
)

#' Generic function to access data from tables and views in Atlas database
#'
#' Return data objects stored in tables and views in the Atlas database.
#'
#' This function is designed to interface with a web API deployed with
#' PostgREST.
#'
#' @param table_name `character`. Name of the table to be accessed.
#' @param schema `character` default 'public'. Schema from the database where
#' the table is located.
#' Can be either `public`, `api`, `public_api`, or `atlas_api`.
#' @param ... Additional parameters to be passed as query to the API table_name.
#' @param output_geometry Optional. `logical` default `FALSE`. If `TRUE`,
#' returns an `sf` object using the `geometry` column from the table.
#' @param output_flatten `logical` default `TRUE`. If `TRUE`, returns a
#' `data.frame` object with nested objects flattened.
#' @param limit Optional. `integer` default `NULL`. Maximum number of rows to
#' return. If `NULL`, all rows are returned. From the PostgREST API syntax.
#' @param select Optional. `character` default `NULL`. List of columns to
#' return. All columns are returned if `NULL`. From the PostgREST API syntax.
#' @param .cores Optional. `integer` default `4`. Number of cores to use for
#' parallel processing. If `1`, no parallel processing is used. If `NULL`, the
#' function will automatically determine the number of cores to use based on
#' the number of available cores on the system.
#' @param .n_pages Optional. `integer` default `NULL`. Number of pages to
#' download. ' If `NULL`, the number of pages is estimated from the number of
#' rows in the table.
#' @param .page_limit Optional. `integer` default `500000`. Maximum number of
#' rows to download per page. This parameter is used to estimate the number of
#' pages to download if `.n_pages` is `NULL`.
#' @param .token Optional. `character` Bearer token providing access to the web
#' API. If `NULL`, the function will attempt to read the token from the
#' `ATLAS_API_TOKEN` environment variable.
#' @return `tibble` or `sf` with rows associated with Atlas data object
#' @export

get_table_data <- function(table_name,
                           schema = "public",
                           output_geometry = FALSE,
                           output_flatten = TRUE,
                           limit = NULL,
                           select = NULL,
                           ...,
                           .token = NULL,
                           .cores = 4,
                           .n_pages = NULL,
                           .page_limit = 10000) {
    # Argument validation
    if (!schema %in% SCHEMA_VALUES) {
        stop("Bad input: Unexpected value for argument `schema`")
    }

    if (is.null(.token)) {
        .token <- ATLAS_API_TOKEN()
    }

    # Prepare HTTP request with url, header and query parameters
    url <- httr::modify_url(ATLAS_API_V2_HOST(),
        path = paste(httr::parse_url(ATLAS_API_V2_HOST())$path, table_name, 
        sep = "/")
    )
    query <- postgrest_query_filter(list(...))
    if (!is.null(select)) {
        if (length(select) > 1) {
            select <- paste0(select, collapse = ",")
        }
        query$select <- select
    }
    if (!is.null(limit)) {
        query$limit <- limit
    }

    header <- list(
        Authorization = paste("Bearer", .token),
        `User-Agent` = USER_AGENT(), # defined in zzz.R
        `Content-type` = "application/json;charset=UTF-8",
        `Accept-Profile` = schema
    )

    if (output_geometry) {
        header$`Accept` <- "application/geo+json"
    }

    # Estimate number of pages if not provided
    if (is.null(limit)) limit <- postgrest_get_table_count(url, query, header)
    if (is.null(.n_pages)) .n_pages <- ceiling(limit / .page_limit)

    # Define parallelization parameters
    .cores <- min(.cores, .n_pages)
    debug <- as.logical(Sys.getenv("DEBUG"))
    if (is.na(debug)) debug <- FALSE

    parallel <- (!debug & .cores > 1)

    # Send request in parallel if more than one page
    if (.n_pages <= 1) {
        response <- httr::GET(
            url,
            config = do.call(httr::add_headers, header),
            query = query
        )
        postgrest_stop_if_err(response)
        out <- postgrest_resp_to_data(response, output_flatten = output_flatten)
    } else if (parallel) {
        doParallel::registerDoParallel(cores = .cores)
        on.exit(doParallel::stopImplicitCluster())
        out <- foreach::foreach(
            page = 1:.n_pages,
            .combine = rbind,
            .export = c(
                "postgrest_get_page",
                "postgrest_stop_if_err",
                "postgrest_resp_to_data",
                ".page_limit",
                "output_flatten",
                "output_geometry")
            ) %dopar% {
                response <- postgrest_get_page(
                    url = url,
                    query = query,
                    header = header,
                    page = page,
                    limit = .page_limit
                )
                postgrest_stop_if_err(response)
                postgrest_resp_to_data(response)
            }
    } else {
        query$limit <- format(.page_limit, scientific = FALSE)
        out <- list()
        for (page in 1:.n_pages) {
            response <- postgrest_get_page(
                url = url,
                query = query,
                header = header,
                page = page,
                limit = .page_limit
            )
            postgrest_stop_if_err(response)
            page_out <- postgrest_resp_to_data(
                response,
                output_flatten = output_flatten
            )
            out <- rbind(out, page_out)
        }
    }
    return(out)
}

postgrest_query_filter <- function(parameters) {
    for (name in names(parameters)) {
        if (name == "select" & length(parameters[[name]]) > 1) {
            parameters[[name]] <- paste0(parameters[[name]], collapse = ",")
        }
        if (name %in% POSTGREST_QUERY_PARAMETERS |
            is.null(parameters[[name]])) {
            next
        }
        if (length(parameters[[name]]) > 1) {
            v_array <- paste0(parameters[[name]], collapse = ",")
            parameters[[name]] <- paste0("in.(", v_array, ")", sep = "")
        } else {
            parameters[[name]] <- paste0("eq.", parameters[[name]], sep = "")
        }
    }
    return(parameters)
}

postgrest_get_page <- function(url,
                               query,
                               header,
                               page,
                               limit) {
    offset <- (page - 1) * limit
    query$limit <- format(limit, scientific = FALSE)
    query$offset <- format(offset, scientific = FALSE)

    response <- httr::GET(
        url,
        config = do.call(httr::add_headers, header),
        query = query
    )
    return(response)
}

postgrest_get_table_count <- function(url,
                                      query,
                                      header) {
    header$`Prefer` <- "count=exact"
    query$limit <- 1
    response <- httr::GET(
        url,
        config = do.call(httr::add_headers, header),
        query = query
    )
    postgrest_stop_if_err(response)
    # Parse table count from response header
    tmp <- unlist(
        strsplit(httr::headers(response)$"content-range", split = "\\D")
    )
    range_count <- as.numeric(tmp[3])
    return(range_count)
}