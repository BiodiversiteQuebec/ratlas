ATLAS_API_V1_HOST <- function() "https://atlas.biodiversite-quebec.ca/api/v1"
ATLAS_API_V2_HOST <- function() "https://atlas.biodiversite-quebec.ca/api/v2"
ATLAS_API_V4_HOST <- function() {
  env_host <- Sys.getenv("ATLAS_API_HOST")
  if (nchar(env_host) > 0) {
    return(env_host)
  } else {
    return("https://atlas.biodiversite-quebec.ca/api/v4")
  }
}

ATLAS_API_TOKEN <- function() Sys.getenv("ATLAS_API_TOKEN")

USER_AGENT <- function() "ratlas"

SCHEMA_VALUES <- c("public", "api", "atlas_api")
WRITE_SCHEMA_VALUES <- c("public", "api", "atlas_api", "indicators")

POSTGREST_QUERY_PARAMETERS <- c("select", "limit", "offset")

GROUPS <- c("AMPHIBIANS", "REPTILES", "BIRDS", "MAMMALS", "FISH",
            "VASCULAR_PLANTS", "NON_VASCULAR_PLANTS", "ARTHROPODS",
            "FUNGI", "ALGAE", "MOLLUSKS", "OTHER_INVERTEBRATES", "MICROORGANISMS")