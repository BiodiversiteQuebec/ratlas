SCHEMA_VALUES <- c("public", "api", "atlas_api")
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
#' the table is located. Can be either `public`, `api` or `atlas_api`.
#' @param ... Additional parameters to be passed as query to the API table_name.
#' @param output_geometry Optional. `logical` default `FALSE`. If `TRUE`,
#' returns an `sf` object using the `geometry` column from the table.
#' @param output_flatten `logical` default `TRUE`. If `TRUE`, returns a
#' `data.frame` object with nested objects flattened.
#' @param limit Optional. `integer` default `NULL`. Maximum number of rows to
#' return. If `NULL`, all rows are returned. From the PostgREST API syntax.
#' @param select Optional. `character` default `NULL`. List of columns to
#' return. All columns are returned if `NULL`. From the PostgREST API syntax.
#' @param .max_active Optional. `integer` default `4`. Number of active connections
#' that are alive at all time to process the request.
#' @param .page_limit Optional. `integer` default `10000`. Maximum number of
#' rows to download per page. This determines how many requests (pages) the
#' download is split into.
#' @param .host Optional. `character` Atlas API host url.
#' @param .token Optional. `character` Bearer token providing access to the web api.
#' @param .header `list` Additional headers to provide to the request.
#' @return `tibble` or `sf` with rows associated with Atlas data object
#' @export

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
  url <- format_url(table_name, host = .host)

  # Prepare query parameters
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

  # Prepare header parameters
  header <- format_header(schema, token = .token, method = "GET")

  # Overrride default header with user provided ones
  header <- modifyList(header, .header)

  if (output_geometry) {
    header$`Accept` <- "application/geo+json"
  }

  # Estimate number of pages if not provided
  if (is.null(limit)) limit <- postgrest_get_table_count(url, query, header)
  n_pages <- ceiling(limit / .page_limit)

  page_limit <- min(.page_limit, limit)

  # Build one request per page
  reqs <- lapply(seq_len(n_pages), function(page) {
    postgrest_page_req(url, query, header, page = page, limit = page_limit)
  })

  debug <- isTRUE(as.logical(Sys.getenv("DEBUG")))
  max_active <- if (debug) 1L else max(1L, min(.max_active, n_pages))

  # Perform all requests concurrently
  responses <- httr2::req_perform_parallel(
    reqs,
    on_error = "continue",
    max_active = max_active,
    progress = FALSE
  )

  # Convert each page, surfacing failures clearly
  pages <- lapply(seq_along(responses), function(i) {
    resp <- responses[[i]]
    if (inherits(resp, "condition")) {
      stop("Failed to fetch page ", i, ": ", conditionMessage(resp))
    }
    postgrest_stop_if_err(resp)
    postgrest_resp_to_data(resp, output_flatten = output_flatten)
  })

  out <- do.call(rbind, pages)
  return(out)
}

postgrest_query_filter <- function(parameters) {
  for (name in names(parameters)) {
    if (name == "select" && length(parameters[[name]]) > 1) {
      parameters[[name]] <- paste0(parameters[[name]], collapse = ",")
    }
    if (name %in% POSTGREST_QUERY_PARAMETERS ||
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

postgrest_page_req <- function(url, query, header, page, limit) {
  offset <- (page - 1) * limit
  query$limit <- format(limit, scientific = FALSE)
  query$offset <- format(offset, scientific = FALSE)

  httr2::request(url) |>
    httr2::req_headers(!!!header) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_retry(max_tries = 3, retry_on_failure = TRUE) |>
    httr2::req_error(is_error = ~ FALSE)
}

postgrest_get_table_count <- function(url, query = NULL, header = NULL) {
  if (is.null(query)) {
    query <- list()
  }
  if (is.null(header)) {
    header <- list()
  }

  header$`Prefer` <- "count=exact"
  query$limit <- 1

  response <- httr2::request(url) |>
    httr2::req_headers(!!!header) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_perform()

  # Get the content range from the response header
  content_range <- httr2::resp_headers(response)$`Content-Range`
  range_count <- as.numeric(strsplit(content_range, "/")[[1]][2])

  return(range_count)
}