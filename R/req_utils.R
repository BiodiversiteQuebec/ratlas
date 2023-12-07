format_url <- function(endpoint, host = ATLAS_API_V2_HOST()) {

  # Prepare HTTP request with url, header and query parameters
  # Remove trailing slash from base_uarl if present
  base_url <- gsub("/$", "", host)

  # Construct the URL by appending the table_name to the base URL
  url <- paste0(base_url, "/", endpoint)

  return(url)
}

format_header <- function(schema, .token = ATLAS_API_TOKEN()) {

  header <- list(
    Authorization = paste("Bearer", .token),
    `User-Agent` = USER_AGENT(), # defined in zzz.R
    `Content-type` = "application/json;charset=UTF-8",
    `Accept-Profile` = schema
  )

  return(header)
}
