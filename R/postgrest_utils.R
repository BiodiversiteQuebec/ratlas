postgrest_resp_to_data <- function(response) {
  textresp <- httr::content(response, type = "text", encoding = "UTF-8")

  # If content-type string contains "geo+json", then it is a geojson
  # Otherwise, it is a json
  if (grepl("application/geo\\+json", httr::headers(response)$"content-type")) {
    data <- sf::st_read(textresp, quiet = TRUE)
    return(data)
  } else if (
    grepl("application/json", httr::headers(response)$"content-type")) {
    data <- jsonlite::fromJSON(textresp, simplifyDataFrame = TRUE)
    data <- tibble::as_tibble(data)
    return(data)
  } else {
    stop("Unexpected content-type")
  }
}

postgrest_stop_if_err <- function(response) {
  if (httr::http_error(response)) {
    stop(httr::message_for_status(response, httr::content(response)$message))
  }
}