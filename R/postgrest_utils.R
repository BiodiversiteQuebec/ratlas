postgrest_resp_to_data <- function(response, output_flatten = TRUE) {

  # Check if response is from a POST
  if (response$method == "POST" && length(response$body) == 0) {
    return(response)
  }
  # Get the response body as text
  textresp <- httr2::resp_body_string(response, encoding = "UTF-8")

  # Check the content type of the response
  content_type <- httr2::resp_headers(response)$`content-type`
  if (!is.null(content_type) && grepl("application/geo\\+json", content_type)) {
    data <- sf::st_read(textresp, quiet = TRUE)
    return(data)
  } else if (!is.null(content_type) && grepl("application/json", content_type)) {
    data <- jsonlite::fromJSON(textresp, flatten = output_flatten)
    data <- tibble::as_tibble(data)
    return(data)
  } else {
    return(textresp)
  }
}

postgrest_stop_if_err <- function(response) {
  # Check if the response indicates an error
  if (httr2::resp_is_error(response)) {
    # Extract the error message from the response
    error_content <- httr2::resp_body_json(response)

    # Stop and display the error message
    msg_parts <- c(
      if (!is.null(error_content$code)) paste0("[", error_content$code, "]") else NULL,
      if (!is.null(error_content$message)) error_content$message else NULL,
      if (!is.null(error_content$details)) paste0("Details: ", error_content$details) else NULL,
      if (!is.null(error_content$hint)) paste0("Hint: ", error_content$hint) else NULL
    )
    formatted_message <- paste(Filter(Negate(is.null), msg_parts), collapse = " | ")
    stop("HTTP error: ", response$status_code, " - ", formatted_message)
  }
}

postgrest_read_paged <- function(url, query, header, body = NULL,
                                 limit = NULL, output_flatten = TRUE,
                                 page_limit = 10000, max_active = 4) {

  # Total rows to fetch: user limit if given, otherwise ask the server
  total <- if (!is.null(limit)) limit else
    postgrest_get_table_count(url, query, header, body)

  if (total == 0) return(tibble::tibble())

  n_pages <- ceiling(total / page_limit)
  page_size <- min(page_limit, total)

  reqs <- lapply(seq_len(n_pages), function(page) {
    offset <- (page - 1) * page_size
    postgrest_page_req(url, query, header, body,
                       offset = offset,
                       limit = min(page_size, total - offset))
  })

  debug <- isTRUE(as.logical(Sys.getenv("DEBUG")))
  max_active <- if (debug) 1L else max(1L, min(max_active, n_pages))

  responses <- httr2::req_perform_parallel(
    reqs,
    on_error = "continue",
    max_active = max_active,
    progress = FALSE
  )

  pages <- lapply(seq_along(responses), function(i) {
    resp <- responses[[i]]
    if (inherits(resp, "condition")) {
      stop("Failed to fetch page ", i, ": ", conditionMessage(resp))
    }
    postgrest_stop_if_err(resp)
    postgrest_resp_to_data(resp, output_flatten = output_flatten)
  })

  do.call(rbind,pages)
}

postgrest_escape <- function(x) {
  x <- as.character(x)
  needs <- grepl('[",()]', x)
  x[needs] <- paste0('"', gsub('"', '\\\\"', x[needs]), '"')
  x
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
      v_array <- paste0(postgrest_escape(parameters[[name]]), collapse = ",")
      parameters[[name]] <- paste0("in.(", v_array, ")", sep = "")
    } else {
      parameters[[name]] <- paste0("eq.", postgrest_escape(parameters[[name]]))
    }
  }
  return(parameters)
}

postgrest_page_req <- function(url, query, header, body = NULL, offset, limit) {
  query$limit <- format(limit, scientific = FALSE)
  query$offset <- format(offset, scientific = FALSE)

  req <- httr2::request(url) |>
    httr2::req_headers(!!!header) |>
    httr2::req_url_query(!!!query)
  if (!is.null(body)) req <- httr2::req_body_json(req, body) # makes it a POST

  req |>
    httr2::req_retry(max_tries = 3, retry_on_failure = TRUE) |>
    httr2::req_error(is_error = ~ FALSE)
}

postgrest_get_table_count <- function(url, query = list(), header = list(), body = NULL) {
  header$`Prefer` <- "count=exact"
  query$limit <- 1

  req <- httr2::request(url) |>
    httr2::req_headers(!!!header) |>
    httr2::req_url_query(!!!query)
  if (!is.null(body)) req <- httr2::req_body_json(req, body)

  response <- req |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_perform()
  postgrest_stop_if_err(response)
  content_range <- httr2::resp_headers(response)$`Content-Range`
  as.numeric(strsplit(content_range, "/")[[1]][2])
}