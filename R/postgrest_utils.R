postgrest_resp_to_data <- function(response, output_flatten = TRUE) {
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
    error_message <- error_content$message

    # Stop and display the error message
    stop("HTTP error: ", response$status_code, " - ", error_message)
  }
}