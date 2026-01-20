format_url <- function(endpoint, host = ATLAS_API_V4_HOST()) {

  # Prepare HTTP request with url, header and query parameters
  # Remove trailing slash from base_uarl if present
  base_url <- gsub("/$", "", host)

  # Construct the URL by appending the table_name to the base URL
  url <- paste0(base_url, "/", endpoint)

  return(url)
}

VALID_METHODS <- c("GET", "POST")
format_header <- function(schema, .token = ATLAS_API_TOKEN(), method = "GET") {

  # If method is not valid, stop
  if (!method %in% VALID_METHODS) {
    stop("Bad input: Unexpected value for argument `method`")
  }

  # Prepare header parameters
  if (method == "GET") {
    profile_key <- "Accept-Profile"
  } else if (method == "POST") {
    profile_key <- "Content-Profile"
  }

  header <- list(
    Authorization = paste("Bearer", .token),
    `User-Agent` = USER_AGENT(), # defined in zzz.R
    `Content-type` = "application/json;charset=UTF-8"
  )

  header[[profile_key]] <- schema

  return(header)
}
