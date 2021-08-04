ATLAS_API_V1_HOST <- "https://atlas.biodiversite-quebec.ca/api/v1"
ATLAS_API_V2_HOST <- "https://atlas.biodiversite-quebec.ca/api/v2"

if (nchar(Sys.getenv("ATLAS_API_HOST")) > 0) {
    ATLAS_API_V2_HOST <- Sys.getenv("ATLAS_API_HOST")
}
ATLAS_API_TOKEN <- Sys.getenv("ATLAS_API_TOKEN")