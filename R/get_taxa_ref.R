# Prefered source identifiers : COL(1), GBIF(11), VASCAN(147)
PREF_SOURCE <- c(1, 11, 147)
API_HOST <- "https://verifier.globalnames.org"
API_ENDPOINT <- "api/v1/verifications"

capitalize_taxa <- function(taxas) {
    words <- strsplit(taxas, " ")
    out <- ""
    for (i in 1:length(taxas)) {
        first_word <- paste0(
            toupper(substring(words[[i]][[1]], 1, 1)),
            tolower(substring(words[[i]][[1]], 2)))
        correct_words <- c(first_word, tolower(words[[i]][2:length(words[[i]])]))
        out[[i]] <- paste(correct_words, collapse = " ")
    }
    return(out)
}

#' Get taxa ref
#'
#' The function returns a list of attributes describing the reference
#' source for requested scientific name scalar or vector using fuzzy matching.
#'
#' Based on Global Names Verifier API.
#' [documentation](https://verifier.globalnames.org/api)
#'
#' @param names `char` scalar or vector. Scientific name to be verified.
#' @param sources Optional. `integer` scalar or vector. Taxonomic reference
#' reference database to be searched for. Default for COL, GBIF, VASCAN.
#' See [reference](https://verifier.globalnames.org/data_sources) for list of
#' identifiers.
#' @return `list` with elements related to requested taxa
#'
#' @examples
#' # Returns all taxa references for list of scientific names
#' taxa <- get_taxa_ref(c("Cyanocitta cristata", "Grus Canadensis"))

#' @export

get_taxa_ref <- function(
    names,
    sources = PREF_SOURCE
) {
    names <- paste(capitalize_taxa(names), collapse = "|")
    query <- list(
        pref_sources = paste(sources, collapse = "|"),
        capitalize = "false")

    url <- httr::modify_url(
        API_HOST,
        path = c(API_ENDPOINT, names),
        query = query
    )
    response <- httr::GET(url)

    if (httr::http_error(response)) {
        stop(httr::message_for_status(
            response, httr::content(response)$message)
            )
        }
    text_resp <- httr::content(response, type = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(
        text_resp,
        simplifyDataFrame = FALSE
        )
  return(data)
}