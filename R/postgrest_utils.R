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
