postgrest_resp_to_data <- function(response, output_flatten = TRUE) {
  textresp <- httr::content(response, type = "text", encoding = "UTF-8")

  # If content-type string contains "geo+json", then it is a geojson
  # Otherwise, it is a json
  content_type <- httr::headers(response)$"content-type"
  if (!is.null(content_type) && (grepl("application/geo\\+json", content_type))) {
    data <- sf::st_read(textresp, quiet = TRUE)
    return(data)
  } else if (
    !is.null(content_type) && grepl("application/json", content_type)) {
    data <- jsonlite::fromJSON(textresp, flatten = output_flatten)
    data <- tibble::as_tibble(data)
    return(data)
  } else {
    return(textresp)
  }
}

postgrest_stop_if_err <- function(response) {
  if (httr::http_error(response)) {
    stop(httr::message_for_status(response, httr::content(response)$message))
  }
}
